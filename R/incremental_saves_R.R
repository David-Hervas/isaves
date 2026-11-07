#' Load incremental
#'
#' @description Load, potentially selecting specific objects, the incremental saves in the project folder.
#' All the files are checked and read before assigning anything, so a failed load does not leave the workspace half loaded.
#' @details By default, the latest version of every object ever saved is loaded, even if it was later removed from the
#' workspace. Use \code{snapshot} to restore the exact state of the workspace at the time of a given save.
#' @param subset A logical expression to select specific objects. It can use the columns of \code{\link{ws_ref_table}} and variables of the calling environment. Rows where it evaluates to NA are not selected
#' @param overwrite Should objects already present in \code{envir} be overwritten? If FALSE, a different version is loaded with the date appended to its name
#' @param lazyload Should the objects be lazy loaded? If FALSE, objects of \code{envir} that were lazy loaded and not used yet are read into memory
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder to store all the object files
#' @param envir Environment where objects should be loaded
#' @param snapshot Snapshot to restore: the \code{save_id} or the annotation shown by \code{\link{ws_snapshots}}, or "latest". If NULL, the latest version of each object is used
#' @return Invisibly, a data.frame with the \code{object}, the \code{name} it has in \code{envir} and the \code{action} performed: "loaded", "lazy", "renamed" or "unchanged"
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(envir = e)
#' restored <- new.env()
#' load_incremental(object == "x", envir = restored)
#' restored$x
#' setwd(old)
#' @export
load_incremental <- function(subset, overwrite=FALSE, lazyload=FALSE, metadata.file="ws_table.ref", rds.folder="rds", envir=parent.frame(), snapshot=NULL){
  store <- resolve_store(metadata.file, rds.folder)
  state <- require_store(store)
  candidates <- state$table
  if(!is.null(snapshot)){
    candidates <- snapshot_rows(candidates, state$snapshots, resolve_snapshot(snapshot, state$snapshots))
  }
  f <- select_rows(if(missing(subset)) TRUE else substitute(subset), candidates, parent.frame())
  metadata <- latest_rows(candidates[f, , drop = FALSE])

  files <- store_file(store, metadata$hash, metadata$format)
  absent <- !file.exists(files)
  if(any(absent)) stop("Nothing was loaded, the files of these objects are missing: ", paste(metadata$object[absent], collapse = ", "), call. = FALSE)

  name <- metadata$object
  action <- rep(if(lazyload) "lazy" else "loaded", nrow(metadata))
  reader <- promise_reader(envir)
  for(i in seq_len(nrow(metadata))){
    if(same_version(name[i], metadata$hash[i], metadata$algo[i], envir, reader)){
      action[i] <- "unchanged"
    } else if(exists(name[i], envir = envir, inherits = FALSE) && !overwrite){
      name[i] <- paste(name[i], format(metadata$date[i], "%Y.%m.%d_%H.%M.%S"), sep = "_")
      action[i] <- if(same_version(name[i], metadata$hash[i], metadata$algo[i], envir, reader)) "unchanged" else "renamed"
      if(action[i] == "renamed") name[i] <- free_name(name[i], envir)
    }
    if(action[i] == "unchanged" && !lazyload && !is.null(pending_lazy(name[i], envir, reader))) action[i] <- "loaded"
  }

  to_load <- which(action != "unchanged")
  if(lazyload){
    for(i in to_load) assign_lazy(name[i], files[i], metadata$format[i], metadata$hash[i], metadata$algo[i], envir)
  } else{
    values <- lapply(to_load, function(i) read_object(files[i], metadata$format[i]))
    for(j in seq_along(to_load)) assign(name[to_load[j]], values[[j]], envir = envir)
  }

  if(any(action == "renamed")){
    message("Already present with different content, loaded with a new name: ", paste(name[action == "renamed"], collapse = ", "))
  }
  if(is.null(snapshot) && missing(subset) && nrow(state$snapshots$index) > 0){
    last <- state$snapshots$manifest$object[state$snapshots$manifest$save_id == max(state$snapshots$index$save_id)]
    not_in_last <- setdiff(metadata$object, last)
    if(length(not_in_last) > 0){
      message("Not in the latest snapshot (use snapshot = \"latest\" to leave them out): ", paste(not_in_last, collapse = ", "))
    }
  }
  invisible(data.frame(object = metadata$object, name = name, action = action, row.names = NULL))
}

same_version <- function(name, hash, algo, envir, reader){
  if(!exists(name, envir = envir, inherits = FALSE)) return(FALSE)
  pending <- pending_lazy(name, envir, reader)
  if(!is.null(pending)) return(identical(pending$hash, hash))
  identical(hash_object(get(name, envir = envir, inherits = FALSE), algo), hash)
}

free_name <- function(name, envir){
  candidate <- name
  suffix <- 1L
  while(exists(candidate, envir = envir, inherits = FALSE)){
    suffix <- suffix + 1L
    candidate <- paste(name, suffix, sep = "_")
  }
  candidate
}

# Returns the items without repetitions
check_items <- function(items, envir){
  if(!is.character(items) || anyNA(items) || !all(nzchar(items))){
    stop("items must be a character vector with the names of the objects (to use the objects of an environment, give it as envir)", call. = FALSE)
  }
  items <- unique(items)
  unknown <- items[!vapply(items, exists, TRUE, envir = envir, inherits = FALSE)]
  if(length(unknown) > 0) stop("Objects not found: ", paste(unknown, collapse = ", "), call. = FALSE)
  items
}

#' Save incremental
#'
#' @description Save only the objects that are not already stored in the project folder, recording a snapshot of the workspace.
#' Identical objects share a single file. Lazy-loaded objects that have not been used are not read from disk.
#' @param items Character vector with names of the objects to save (save everything by default). When given, the other objects of the latest snapshot are kept in the new one
#' @param annotation Annotation for current save. It is stored in the new rows and in the snapshot, also when no object has changed
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder to store all the object files
#' @param envir Environment containing the objects to save
#' @param format Format of the object files: "qs2" (fast) or "rds" (does not need any package to be read)
#' @return Invisibly, a data.frame with the new rows of the Work Space Reference Table
#' @importFrom utils object.size
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(annotation = "first version", envir = e)
#' e$x <- 1:20
#' save_incremental(envir = e)
#' ws_ref_table()
#' setwd(old)
#' @export
save_incremental <- function(items = objects(envir = envir), annotation = NA, metadata.file="ws_table.ref", rds.folder="rds", envir = parent.frame(), format = getOption("isaves.format", "qs2")){
  partial <- !missing(items)
  format <- match.arg(format, c("qs2", "rds"))
  if(is.null(annotation)) annotation <- NA
  if(length(annotation) != 1L) stop("annotation must be a single value", call. = FALSE)
  items <- check_items(items, envir)

  store <- resolve_store(metadata.file, rds.folder)
  dir.create(store$rds, recursive = TRUE, showWarnings = FALSE)
  if(!dir.exists(store$rds)) stop("Could not create the folder ", store$rds, call. = FALSE)
  state <- read_store(store)
  if(!state$exists) message("isaves: creating a new store in ", store$root)

  ws <- workspace_state(items, envir, state$table)
  to_save <- ws[ws$status != "unchanged", , drop = FALSE]
  details <- object_details(to_save, envir, state$table, format)
  write_missing_objects(store, to_save, details, envir)

  # Hashing and writing can be slow, so only the update of the table is locked
  new_rows <- with_store_lock(store, {
    state <- read_store(store)
    # Another session may have saved or removed versions since the workspace was compared,
    # so objects that were unchanged then may need a new row now
    current <- latest_rows(state$table)
    pos <- match(ws$object, current$object)
    changed <- is.na(pos) | ws$hash != current$hash[pos]
    late <- changed & !ws$object %in% to_save$object
    if(any(late)){
      to_save <- rbind(to_save, ws[late, , drop = FALSE])
      details <- rbind(details, object_details(ws[late, , drop = FALSE], envir, state$table, format))
    }
    pending <- changed[match(to_save$object, ws$object)]
    write_missing_objects(store, to_save[pending, , drop = FALSE], details[pending, , drop = FALSE], envir)
    save_id <- max(0L, state$snapshots$index$save_id, state$table$save_id, na.rm = TRUE) + 1L
    date <- Sys.time()
    new_rows <- data.frame(
      object = to_save$object[pending], class = details$class[pending], size = details$size[pending], date = rep(date, sum(pending)),
      hash = to_save$hash[pending], comment = details$comment[pending], user = rep(Sys.info()[["user"]], sum(pending)),
      annotation = rep(as.character(annotation), sum(pending)), algo = to_save$algo[pending], format = details$format[pending], save_id = rep(save_id, sum(pending)),
      row.names = NULL)
    state$table <- rbind_table(state$table, new_rows)
    manifest <- data.frame(object = ws$object, hash = ws$hash)
    previous <- state$snapshots$manifest[state$snapshots$manifest$save_id == max(0L, state$snapshots$index$save_id), c("object", "hash"), drop = FALSE]
    if(partial){
      base <- if(nrow(state$snapshots$index) > 0) previous else latest_rows(state$table)[c("object", "hash")]
      manifest <- rbind(base[!base$object %in% manifest$object, , drop = FALSE], manifest)
    }
    same_manifest <- nrow(state$snapshots$index) > 0 && setequal(paste(manifest$object, manifest$hash), paste(previous$object, previous$hash))
    if(nrow(new_rows) > 0 || !is.na(annotation) || (!same_manifest && nrow(manifest) + nrow(previous) > 0)){
      state$snapshots$index <- rbind(state$snapshots$index, data.frame(save_id = save_id, date = date, user = Sys.info()[["user"]], annotation = as.character(annotation)))
      if(nrow(manifest) > 0) state$snapshots$manifest <- rbind(state$snapshots$manifest, data.frame(save_id = save_id, manifest, row.names = NULL))
      write_store(store, state)
    }
    new_rows
  })
  if(nrow(new_rows) > 0) message("Saved ", nrow(new_rows), " object(s)") else message("No new objects to store")
  invisible(new_rows)
}

rbind_table <- function(old, new){
  rbind(old, add_columns(new, names(old))[names(old)])
}

object_details <- function(to_save, envir, table, format){
  n <- nrow(to_save)
  details <- data.frame(class = rep(NA_character_, n), size = rep(NA_real_, n), comment = rep(NA_character_, n), format = rep(format, n))
  for(i in seq_len(n)){
    # A version already stored keeps its file, whatever the current format is
    known <- match(to_save$hash[i], table$hash)
    if(!is.na(known)) details$format[i] <- table$format[known]
    if(to_save$lazy[i]){
      details[i, c("class", "size", "comment")] <- table[known, c("class", "size", "comment")]
    } else{
      value <- get(to_save$object[i], envir = envir, inherits = FALSE)
      details$class[i] <- class(value)[1]
      details$size[i] <- as.numeric(object.size(value))
      if(!is.null(comment(value))) details$comment[i] <- paste(comment(value), collapse = "\n")
    }
  }
  details
}

write_missing_objects <- function(store, to_save, details, envir){
  for(i in which(!duplicated(to_save$hash))){
    file <- store_file(store, to_save$hash[i], details$format[i])
    if(!file.exists(file)){
      if(to_save$lazy[i]) stop("The file of the lazy-loaded object ", to_save$object[i], " is missing", call. = FALSE)
      write_object(get(to_save$object[i], envir = envir, inherits = FALSE), file, details$format[i], details$size[i])
    }
  }
}

#' Work Space Reference Table
#'
#' @description Visualize the work space reference table file on the project's folder
#' @param subset A logical expression to select specific objects. It can use the columns of the table and variables of the calling environment. Rows where it evaluates to NA are not selected
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @return A data.frame with one row per stored version of each object, or NULL if there is no table yet. Tables from older versions of the package may have additional columns, which are kept. The columns are:
#' \describe{
#'   \item{object}{Name of the object.}
#'   \item{class}{First class of the object.}
#'   \item{size}{Size of the object in memory, in bytes, as given by \code{\link[utils]{object.size}}. The file on disk is usually smaller.}
#'   \item{date}{Date and time of the save that created the row.}
#'   \item{hash}{Hash of the content of the object. Together with \code{format}, it gives the name of its file, so versions with the same content share a file.}
#'   \item{comment}{Comment of the object (see \code{\link[base]{comment}}), with the lines separated by a line break, or NA if it has none.}
#'   \item{user}{User who made the save.}
#'   \item{annotation}{Annotation given to \code{\link{save_incremental}} or \code{\link{annotate_ws}}, or NA if it has none.}
#'   \item{algo}{Algorithm used to calculate \code{hash}: "spookyhash", or "md5" for rows saved with versions of the package before 0.3.0. An unchanged object is recognized by hashing it with the algorithm of its latest row.}
#'   \item{format}{Format of the file of the object: "qs2" or "rds". See the \code{format} argument of \code{\link{save_incremental}}.}
#'   \item{save_id}{Number of the save that created the row, the same shown by \code{\link{ws_snapshots}}, or NA for rows saved with versions of the package before 0.3.0.}
#' }
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(envir = e)
#' ws_ref_table(object == "x")
#' setwd(old)
#' @export
ws_ref_table <- function(subset, file="ws_table.ref"){
  store <- resolve_store(file, "rds")
  state <- read_store(store)
  if(!state$exists){
    message("No previous metadata in ", store$root)
    return(invisible(NULL))
  }
  state$table[select_rows(if(missing(subset)) TRUE else substitute(subset), state$table, parent.frame()), , drop = FALSE]
}


#' Purge Work Space Reference Table
#'
#' @description Remove object versions from the Work Space Reference Table and delete the files that no remaining version uses
#' @param subset A logical expression to select specific objects. It is required when \code{remove} is TRUE; use \code{TRUE} to purge everything. Rows where it evaluates to NA are not selected
#' @param remove If TRUE, objects are actually removed. If FALSE, nothing is changed.
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder where the object files are stored
#' @return A data.frame with the selected rows (invisibly if they were removed)
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(envir = e)
#' purge_ws_table(object == "x")
#' purge_ws_table(object == "x", remove = TRUE)
#' setwd(old)
#' @export
purge_ws_table <- function(subset, remove=FALSE, file="ws_table.ref", rds.folder="rds"){
  if(missing(subset) && remove) stop("Purging needs a subset; use subset = TRUE to purge everything", call. = FALSE)
  store <- resolve_store(file, rds.folder)
  expr <- if(missing(subset)) TRUE else substitute(subset)
  enclos <- parent.frame()
  if(!remove){
    state <- require_store(store)
    selected <- state$table[select_rows(expr, state$table, enclos), , drop = FALSE]
    message("Objects that would be purged (use remove = TRUE to purge them): ", nrow(selected))
    return(selected)
  }
  removed <- with_store_lock(store, {
    state <- require_store(store)
    remove_rows(store, state, select_rows(expr, state$table, enclos))
  })
  message("Purged objects: ", nrow(removed))
  invisible(removed)
}


#' Annotate Work Space Reference Table
#'
#' @description Annotate lines in the Work Space Reference Table
#' @param subset A logical expression to select specific objects. Rows where it evaluates to NA are not selected
#' @param annotation Character string with the annotation. Use NA to remove existing annotations
#' @param write If TRUE, annotations are actually written in the file. If FALSE, nothing is changed.
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @return A data.frame with the selected rows and the new annotation (invisibly if it was written)
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(envir = e)
#' annotate_ws(object == "x", "clean data", write = TRUE)
#' setwd(old)
#' @export
annotate_ws <- function(subset, annotation, write=FALSE, file="ws_table.ref"){
  if(missing(annotation) || length(annotation) != 1L) stop("annotation must be a single value", call. = FALSE)
  store <- resolve_store(file, "rds")
  expr <- if(missing(subset)) TRUE else substitute(subset)
  enclos <- parent.frame()
  if(!write){
    state <- require_store(store)
    selected <- state$table[select_rows(expr, state$table, enclos), , drop = FALSE]
    selected$annotation <- rep(as.character(annotation), nrow(selected))
    return(selected)
  }
  invisible(with_store_lock(store, {
    state <- require_store(store)
    f <- select_rows(expr, state$table, enclos)
    state$table$annotation[f] <- as.character(annotation)
    write_store(store, state)
    state$table[f, , drop = FALSE]
  }))
}


#' Workspace status
#'
#' @description Compare the workspace with the store without saving anything and without reading lazy-loaded objects
#' @param items Character vector with names of the objects to compare (everything by default). When given, stored objects that are not in \code{items} are not shown
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder where the object files are stored
#' @param envir Environment containing the objects
#' @return A data.frame with the \code{object}, its \code{status} ("new", "modified", "unchanged" or "not_in_workspace"), its \code{class} and its \code{size} in memory in bytes, and the \code{last_saved} date and \code{annotation} of its latest stored version. The class and size describe the object in the workspace; for an object that is not in the workspace, they describe its latest stored version
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(envir = e)
#' e$x <- 1:20
#' e$y <- "new"
#' ws_status(envir = e)
#' setwd(old)
#' @export
ws_status <- function(items = objects(envir = envir), metadata.file="ws_table.ref", rds.folder="rds", envir = parent.frame()){
  all_items <- missing(items)
  items <- check_items(items, envir)
  store <- resolve_store(metadata.file, rds.folder)
  table <- read_store(store)$table
  ws <- workspace_state(items, envir, table)
  latest <- latest_rows(table)
  shown <- c(ws$object, if(all_items) setdiff(latest$object, ws$object))
  idx <- match(shown, latest$object)
  status <- ws$status[match(shown, ws$object)]
  status[is.na(status)] <- "not_in_workspace"
  # An unchanged object has the same class and size as its stored version, so only
  # the others need to be measured, and lazy-loaded objects are not read
  class <- latest$class[idx]
  size <- latest$size[idx]
  changed <- ws[ws$status != "unchanged", , drop = FALSE]
  details <- object_details(changed, envir, table, NA_character_)
  positions <- match(changed$object, shown)
  class[positions] <- details$class
  size[positions] <- details$size
  data.frame(object = shown, status = status, class = class, size = size, last_saved = latest$date[idx], annotation = latest$annotation[idx])
}


#' Snapshots of the workspace
#'
#' @description List the snapshots recorded by \code{\link{save_incremental}}. Each one can be restored with the \code{snapshot} argument of \code{\link{load_incremental}}
#' @param subset A logical expression to select specific snapshots. Rows where it evaluates to NA are not selected
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @return A data.frame with the \code{save_id}, \code{date}, \code{user} and \code{annotation} of each snapshot, its number of objects and how many of their versions are no longer stored
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(annotation = "first", envir = e)
#' ws_snapshots()
#' setwd(old)
#' @export
ws_snapshots <- function(subset, file="ws_table.ref"){
  store <- resolve_store(file, "rds")
  index <- snapshot_summary(read_store(store))
  index[select_rows(if(missing(subset)) TRUE else substitute(subset), index, parent.frame()), , drop = FALSE]
}


#' Prune old versions
#'
#' @description Keep only the most recent versions of each object in the store
#' @param keep Number of versions to keep for each object
#' @param remove If TRUE, the older versions are actually removed. If FALSE, nothing is changed.
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder where the object files are stored
#' @return A data.frame with the rows selected for removal (invisibly if they were removed)
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' for(i in 1:3){
#'   e$x <- i
#'   save_incremental(envir = e)
#' }
#' prune_ws(keep = 1)
#' prune_ws(keep = 1, remove = TRUE)
#' setwd(old)
#' @export
prune_ws <- function(keep = 1L, remove=FALSE, file="ws_table.ref", rds.folder="rds"){
  if(length(keep) != 1L || is.na(keep) || keep < 1) stop("keep must be at least 1", call. = FALSE)
  store <- resolve_store(file, rds.folder)
  older_versions <- function(table){
    if(nrow(table) == 0) return(logical(0))
    ord <- order(table$date, seq_len(nrow(table)), decreasing = TRUE)
    rank <- integer(nrow(table))
    rank[ord] <- stats::ave(seq_along(ord), table$object[ord], FUN = seq_along)
    rank > keep
  }
  if(!remove){
    state <- require_store(store)
    selected <- state$table[older_versions(state$table), , drop = FALSE]
    message("Versions that would be removed (use remove = TRUE to remove them): ", nrow(selected))
    return(selected)
  }
  removed <- with_store_lock(store, {
    state <- require_store(store)
    remove_rows(store, state, older_versions(state$table))
  })
  message("Removed versions: ", nrow(removed))
  invisible(removed)
}


#' Verify the store
#'
#' @description Check that the metadata table and the object files are consistent
#' @param deep If TRUE, every file is also read and its hash is checked, which can be slow
#' @param remove.orphans If TRUE, files that no row of the table refers to are deleted. Do not use it while another R session is saving in the same store
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder where the object files are stored
#' @return Invisibly, a list with \code{ok}, the rows whose file is \code{missing}, the rows whose file is \code{corrupt} (only if \code{deep}), the \code{orphans} files, and the snapshots with versions no longer stored (\code{incomplete_snapshots})
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' old <- setwd(d)
#' e <- new.env()
#' e$x <- 1:10
#' save_incremental(envir = e)
#' verify_ws(deep = TRUE)
#' setwd(old)
#' @export
verify_ws <- function(deep=FALSE, remove.orphans=FALSE, file="ws_table.ref", rds.folder="rds"){
  store <- resolve_store(file, rds.folder)
  state <- require_store(store)
  table <- state$table
  files <- store_file(store, table$hash, table$format)
  absent <- !file.exists(files)
  on_disk <- list.files(store$rds, pattern = "^(\\.isaves_tmp_.*|[0-9a-f]+\\.(rds|qs2))$", full.names = TRUE, all.files = TRUE)
  orphans <- setdiff(basename(on_disk), basename(files))
  corrupt <- rep(FALSE, nrow(table))
  if(deep){
    distinct <- which(!absent & !duplicated(files))
    bad <- vapply(distinct, function(i){
      tryCatch(hash_object(read_object(files[i], table$format[i]), table$algo[i]) != table$hash[i], error = function(e) TRUE)
    }, TRUE)
    corrupt <- files %in% files[distinct[bad]]
  }
  snapshots <- snapshot_summary(state)
  incomplete <- snapshots$save_id[snapshots$n_missing > 0]
  remaining <- orphans
  if(remove.orphans && length(orphans) > 0){
    # Another session may have recorded some of these files while they were checked
    orphans <- with_store_lock(store, {
      current <- read_store(store)$table
      orphans <- setdiff(orphans, basename(store_file(store, current$hash, current$format)))
      unlink(file.path(store$rds, orphans))
      orphans
    })
    remaining <- character(0)
    message("Deleted ", length(orphans), " orphan file(s)")
  }
  ok <- !any(absent) && !any(corrupt) && length(remaining) == 0 && length(incomplete) == 0
  message(if(ok) "The store is consistent" else sprintf("Problems found: %d row(s) with missing file, %d with corrupt file, %d orphan file(s), %d incomplete snapshot(s)",
                                                        sum(absent), sum(corrupt), length(remaining), length(incomplete)))
  invisible(list(ok = ok, missing = table[absent, , drop = FALSE], corrupt = table[corrupt, , drop = FALSE],
                 orphans = orphans, incomplete_snapshots = incomplete))
}


#' Root of the store
#'
#' @description Show the folder used by the package. Relative paths are resolved from the nearest folder, going up from the working directory, that has the metadata file, an RStudio project or a git repository (otherwise, the working directory). Set \code{options(isaves.root = "path")} to fix it.
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @return The path of the root folder
#' @examples
#' ws_root()
#' @export
ws_root <- function(metadata.file="ws_table.ref"){
  resolve_store(metadata.file, "rds")$root
}
