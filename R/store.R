# Internal helpers for the on-disk store: paths, locking, atomic writes,
# metadata table, hashing and object (de)serialization.

isaves_state <- new.env(parent = emptyenv())
default_algo <- "spookyhash"
store_columns <- c("object", "class", "size", "date", "hash", "comment", "user", "annotation", "algo", "format", "save_id")

# Paths ----------------------------------------------------------------------

is_absolute_path <- function(path){
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

find_root <- function(marker){
  option <- getOption("isaves.root")
  if(!is.null(option)) return(normalizePath(option, winslash = "/", mustWork = FALSE))
  start <- normalizePath(getwd(), winslash = "/")
  current <- start
  repeat{
    if(file.exists(file.path(current, marker)) ||
       file.exists(file.path(current, ".git")) ||
       length(list.files(current, pattern = "\\.Rproj$")) > 0) return(current)
    parent <- dirname(current)
    if(parent == current) return(start)
    current <- parent
  }
}

resolve_store <- function(metadata.file, rds.folder){
  metadata.file <- path.expand(metadata.file)
  rds.folder <- sub("[/\\\\]+$", "", path.expand(rds.folder))
  if(is_absolute_path(metadata.file)){
    meta <- normalizePath(metadata.file, winslash = "/", mustWork = FALSE)
    root <- dirname(meta)
  } else{
    root <- find_root(metadata.file)
    meta <- file.path(root, metadata.file)
  }
  rds <- if(is_absolute_path(rds.folder)) normalizePath(rds.folder, winslash = "/", mustWork = FALSE) else file.path(root, rds.folder)
  if(!is.null(isaves_state$root) && !identical(isaves_state$root, root)){
    message("isaves: now using the store in ", root, " (previously ", isaves_state$root, ")")
  }
  isaves_state$root <- root
  list(root = root, meta = meta, rds = rds)
}

store_file <- function(store, hash, format){
  file.path(store$rds, paste(hash, format, sep = "."))
}

# Locking and atomic writes ---------------------------------------------------

with_store_lock <- function(store, code){
  dir.create(dirname(store$meta), recursive = TRUE, showWarnings = FALSE)
  lock_file <- paste(store$meta, "lock", sep = ".")
  lock <- filelock::lock(lock_file, exclusive = TRUE, timeout = getOption("isaves.lock.timeout", 60000))
  if(is.null(lock)) stop("Could not lock ", lock_file, ": another R session is using this store", call. = FALSE)
  on.exit(filelock::unlock(lock))
  force(code)
}

# The temporary file is created next to the target so that the final rename
# stays on the same volume. On Windows a rename can fail while another process
# has the target open, so it is retried for a short time.
atomic_write <- function(write_fun, file){
  tmp <- tempfile(pattern = ".isaves_tmp_", tmpdir = dirname(file))
  on.exit(unlink(tmp))
  write_fun(tmp)
  for(attempt in 1:40){
    if(suppressWarnings(file.rename(tmp, file))) return(invisible(file))
    Sys.sleep(0.05)
  }
  stop("Could not write ", file, call. = FALSE)
}

# Object files -----------------------------------------------------------------

qs_threads <- function(size = Inf){
  if(size < 1e6) return(1L)
  getOption("isaves.nthreads", max(1L, min(8L, parallel::detectCores(), na.rm = TRUE)))
}

write_object <- function(x, file, format, size = Inf){
  atomic_write(function(tmp){
    if(format == "qs2"){
      threads <- qs_threads(size)
      # Builds of qs2 without TBB only support one thread
      tryCatch(qs2::qs_save(x, tmp, nthreads = threads),
               error = function(e) qs2::qs_save(x, tmp, nthreads = 1L))
    } else{
      saveRDS(x, file = tmp)
    }
  }, file)
}

read_object <- function(file, format){
  if(format == "qs2"){
    threads <- qs_threads(file.size(file))
    tryCatch(qs2::qs_read(file, nthreads = threads),
             error = function(e) qs2::qs_read(file, nthreads = 1L))
  } else{
    readRDS(file)
  }
}

# Target of the promises created by lazy loading. The hash and the algorithm are
# only arguments so that they can be recovered from the unevaluated promise.
read_stored <- function(file, format, hash, algo){
  read_object(file, format)
}

assign_lazy <- function(name, file, format, hash, algo, envir){
  do.call(delayedAssign, list(x = name,
                              value = as.call(list(quote(read_stored), file = file, format = format, hash = hash, algo = algo)),
                              eval.env = environment(read_stored),
                              assign.env = envir))
}

# Environment where substitute() can read the promises of envir. substitute() does
# not look into the global environment, so a copy of its bindings is used instead;
# copying does not force them. Make it once per call: it depends on the number of objects.
promise_reader <- function(envir){
  if(identical(envir, globalenv())) rlang::env_clone(envir, parent = emptyenv()) else envir
}

# Returns the unevaluated read_stored() call of a pending lazy-loaded binding, or NULL
pending_lazy <- function(name, envir, reader = promise_reader(envir)){
  if(!rlang::env_binding_are_lazy(envir, name)) return(NULL)
  expr <- do.call(substitute, list(as.name(name), reader))
  if(is.call(expr) && identical(expr[[1]], quote(read_stored))) expr else NULL
}

# Hashing ----------------------------------------------------------------------

# The byte-code compiler modifies closures the first time they are run, which
# would make an unedited function look like a new version. Hashing its parts
# avoids that. "md5" rows come from older versions of the package, which hashed
# the object directly.
hash_object <- function(x, algo = default_algo){
  if(algo == "md5") return(digest::digest(x))
  if(is.function(x) && !is.primitive(x)) x <- list(formals(x), body(x), environment(x), attributes(x))
  digest::digest(x, algo = algo)
}

# Metadata ---------------------------------------------------------------------

empty_table <- function(){
  data.frame(object = character(0), class = character(0), size = numeric(0),
             date = as.POSIXct(character(0)), hash = character(0), comment = character(0),
             user = character(0), annotation = character(0), algo = character(0),
             format = character(0), save_id = integer(0))
}

empty_snapshots <- function(){
  list(index = data.frame(save_id = integer(0), date = as.POSIXct(character(0)),
                          user = character(0), annotation = character(0)),
       manifest = data.frame(save_id = integer(0), object = character(0), hash = character(0)))
}

add_columns <- function(table, columns){
  for(column in setdiff(columns, names(table))) table[[column]] <- rep(NA, nrow(table))
  table
}

normalize_table <- function(table){
  if(!is.data.frame(table) || !all(c("object", "hash", "date") %in% names(table))) stop("The metadata file is not a valid isaves table", call. = FALSE)
  factors <- vapply(table, is.factor, TRUE)
  table[factors] <- lapply(table[factors], as.character)
  table <- add_columns(table, store_columns)
  table$algo[is.na(table$algo)] <- "md5"
  table$format[is.na(table$format)] <- "rds"
  # Rows filled with NA are left behind by older versions of purge_ws_table()
  invalid <- is.na(table$object) | is.na(table$hash)
  if(any(invalid)) message("isaves: ignoring ", sum(invalid), " invalid row(s) in the metadata table")
  table <- table[!invalid, c(store_columns, setdiff(names(table), store_columns)), drop = FALSE]
  rownames(table) <- NULL
  table
}

read_store <- function(store){
  if(!file.exists(store$meta)) return(list(exists = FALSE, table = empty_table(), snapshots = empty_snapshots()))
  table <- tryCatch(readRDS(store$meta), error = function(e){
    backup <- paste(store$meta, "bak", sep = ".")
    stop("The metadata file ", store$meta, " cannot be read: ", conditionMessage(e),
         if(file.exists(backup)) paste0("\nThe previous version is in ", backup, "; copy it over the metadata file to restore it."),
         call. = FALSE)
  })
  snapshots <- attr(table, "snapshots")
  attr(table, "snapshots") <- NULL
  list(exists = TRUE, table = normalize_table(table), snapshots = if(is.null(snapshots)) empty_snapshots() else snapshots)
}

require_store <- function(store){
  state <- read_store(store)
  if(!state$exists) stop("No metadata found in ", store$meta, call. = FALSE)
  state
}

write_store <- function(store, state){
  table <- state$table
  attr(table, "snapshots") <- state$snapshots
  if(file.exists(store$meta)) file.copy(store$meta, paste(store$meta, "bak", sep = "."), overwrite = TRUE)
  atomic_write(function(tmp) saveRDS(table, file = tmp), store$meta)
}

# NA is not TRUE: missing annotations must not select (or corrupt) any row
select_rows <- function(expr, table, enclos){
  f <- eval(expr, table, enclos)
  if(!is.logical(f)) stop("subset must be a logical expression", call. = FALSE)
  if(length(f) == 1L) f <- rep(f, nrow(table))
  if(length(f) != nrow(table)) stop("subset must have one value per row of the table", call. = FALSE)
  f & !is.na(f)
}

latest_rows <- function(table){
  ord <- order(table$date, seq_len(nrow(table)), decreasing = TRUE)
  table[ord[!duplicated(table$object[ord])], , drop = FALSE]
}

# Compares the objects with their latest stored version without forcing lazy-loaded bindings
workspace_state <- function(items, envir, table){
  latest <- latest_rows(table)
  idx <- match(items, latest$object)
  hash <- character(length(items))
  algo <- character(length(items))
  lazy <- logical(length(items))
  reader <- promise_reader(envir)
  for(i in seq_along(items)){
    pending <- pending_lazy(items[i], envir, reader)
    if(!is.null(pending)){
      hash[i] <- pending$hash
      algo[i] <- pending$algo
      lazy[i] <- TRUE
    } else{
      value <- get(items[i], envir = envir, inherits = FALSE)
      algo[i] <- if(is.na(idx[i])) default_algo else latest$algo[idx[i]]
      hash[i] <- hash_object(value, algo[i])
      if(algo[i] != default_algo && hash[i] != latest$hash[idx[i]]){
        algo[i] <- default_algo
        hash[i] <- hash_object(value, algo[i])
      }
    }
  }
  status <- ifelse(is.na(idx), "new", ifelse(hash == latest$hash[idx], "unchanged", "modified"))
  data.frame(object = items, hash = hash, algo = algo, lazy = lazy, status = status, stringsAsFactors = FALSE)
}

# Snapshots ---------------------------------------------------------------------

resolve_snapshot <- function(snapshot, snapshots){
  index <- snapshots$index
  if(nrow(index) == 0) stop("This store has no snapshots yet", call. = FALSE)
  id <- if(is.numeric(snapshot)){
    index$save_id[match(snapshot, index$save_id)]
  } else if(identical(snapshot, "latest")){
    max(index$save_id)
  } else{
    matches <- index$save_id[!is.na(index$annotation) & index$annotation == snapshot]
    if(length(matches) > 0) max(matches) else NA
  }
  if(length(id) != 1L || is.na(id)) stop("Snapshot not found: ", format(snapshot), call. = FALSE)
  id
}

snapshot_rows <- function(table, snapshots, id){
  manifest <- snapshots$manifest[snapshots$manifest$save_id == id, , drop = FALSE]
  ord <- order(table$date, seq_len(nrow(table)), decreasing = TRUE)
  keys <- paste(table$object[ord], table$hash[ord])
  idx <- match(paste(manifest$object, manifest$hash), keys)
  if(anyNA(idx)) warning("Snapshot ", id, " is incomplete, these versions are no longer stored: ",
                         paste(manifest$object[is.na(idx)], collapse = ", "), call. = FALSE)
  table[ord[idx[!is.na(idx)]], , drop = FALSE]
}

# Removal ------------------------------------------------------------------------

# Removes rows from the table and the files that no remaining row refers to. The
# table is written first so that an interruption can only leave orphan files.
remove_rows <- function(store, state, f){
  removed <- state$table[f, , drop = FALSE]
  previous <- latest_rows(state$table)
  state$table <- state$table[!f, , drop = FALSE]
  write_store(store, state)
  orphan <- removed[!removed$hash %in% state$table$hash & !duplicated(removed$hash), , drop = FALSE]
  unlink(store_file(store, orphan$hash, orphan$format))
  current <- latest_rows(state$table)
  reverted <- previous$object[previous$object %in% current$object &
                                previous$hash != current$hash[match(previous$object, current$object)]]
  if(length(reverted) > 0) message("An older version is now the latest one for: ", paste(reverted, collapse = ", "))
  removed
}
