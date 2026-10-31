# Creates an empty project folder for the duration of the calling test
local_store <- function(env = parent.frame()){
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  withr::local_options(list(isaves.root = NULL), .local_envir = env)
  assign("root", NULL, envir = isaves:::isaves_state)
  file.create(file.path(dir, "project.Rproj"))
  dir
}

# Dates must differ for the order of the versions to be meaningful
save_quiet <- function(...){
  result <- suppressMessages(save_incremental(...))
  Sys.sleep(0.02)
  invisible(result)
}

# Without a subset: a wrapper would change the environment where it is evaluated
load_quiet <- function(...) suppressMessages(load_incremental(...))

# Writes a store as the versions of the package before 0.3.0 did
legacy_store <- function(objects, annotation = NA){
  dir.create("rds", showWarnings = FALSE)
  rows <- lapply(names(objects), function(name){
    hash <- digest::digest(objects[[name]])
    saveRDS(objects[[name]], file.path("rds", paste0(hash, ".rds")))
    data.frame(object = name, class = class(objects[[name]])[1], size = 1, date = Sys.time(), hash = hash,
               comment = NA, user = "someone", annotation = annotation)
  })
  saveRDS(do.call(rbind, rows), "ws_table.ref")
}
