#' Load incremental
#'
#' @description Load, potentially selecting specific objects, all the incremental saves in the project folder
#' @param subset A logical expression to select specific objects
#' @param overwrite Should objects already present in the .GlobalEnvironment be overwritten?
#' @param lazyload Should the objects be lazy loaded?
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder to store all the .rds files
#' @param envir Environment where objects should be loaded
#' @return A loaded workspace in the Global Environment
#' @export
load_incremental <- function(subset, overwrite=FALSE, lazyload=FALSE, metadata.file="ws_table.ref", rds.folder="rds", envir=.GlobalEnv){
  path <- paste("./", rds.folder, "/", sep="")
  metadata_complete <- ws_ref_table(file=metadata.file)
  current_ws <- objects(envir = .GlobalEnv)
  if(missing(subset)) f <- rep(TRUE, nrow(metadata_complete)) else f <- eval(substitute(subset), metadata_complete, baseenv())
  metadata <- metadata_complete[f, ]
  load_files <- rev(unique(metadata$hash[order(metadata$date, decreasing = TRUE)][!duplicated(metadata$object[order(metadata$date, decreasing = TRUE)])]))
  invisible(lapply(load_files, function(x){
    if(metadata$object[metadata$hash == x] %in% current_ws &&
       !overwrite &&
       x != digest::digest(get(current_ws[current_ws == metadata$object[metadata$hash == x]], envir=.GlobalEnv))){
      object_name <- gsub(":|-", ".", gsub(" ", "_", paste(metadata$object[metadata$hash == x], "_", metadata$date[metadata$hash == x], sep="")))
    } else{
      object_name <- metadata$object[metadata$hash == x]
    }
    if(lazyload){
      delayedAssign(object_name, readRDS(paste(path, x, ".rds", sep="")), assign.env = envir)
    } else{
      assign(object_name, readRDS(paste(path, x, ".rds", sep="")), envir = envir)
    }
  }))
}

#' Save incremental
#'
#' @description Save only the objects from the workspace that are not already stored in other .rds files in the project folder
#' @param items Character vector with names of the objects to save (save everything by default)
#' @param annotation Annotation for current save
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder to store all the .rds files
#' @return An incremental save of the workspace is stored in .rds files on the project's folder
#' @importFrom utils object.size
#' @export
save_incremental <- function(items = objects(envir = .GlobalEnv), annotation = NA, metadata.file="ws_table.ref", rds.folder="rds"){
  path <- paste("./", rds.folder, "/", sep="")
  if(!dir.exists(path)) dir.create(path)
  current_ws <- items
  old_metadata <- ws_ref_table(file = metadata.file)
  hashes <- sapply(current_ws, function(.x) digest::digest(get(.x)))
  to_save <- current_ws[!hashes %in% old_metadata$hash & !duplicated(hashes)]
  if(length(to_save) > 0){
    lapply(to_save, function(.x){
      saveRDS(get(.x), file=paste(path, digest::digest(get(.x)), ".rds", sep=""))
    })

    metadata <- do.call(rbind,
                        lapply(to_save,
                               function(.x){ data.frame(object=.x,
                                                        class=class(get(.x))[1],
                                                        size=as.numeric(object.size(get(.x))),
                                                        date=Sys.time(),
                                                        hash=digest::digest(get(.x)),
                                                        comment=ifelse(!is.null(comment(get(.x))), comment(get(.x)), NA),
                                                        user=Sys.info()[["user"]],
                                                        annotation=annotation)}))

    if(file.exists(metadata.file)){
      metadata <- rbind(
        data.frame(c(old_metadata, sapply(setdiff(names(metadata), names(old_metadata)), function(x) NA))),
        data.frame(c(metadata, sapply(setdiff(names(old_metadata), names(metadata)), function(x) NA)))
      )
    }
    saveRDS(metadata, file=metadata.file)
  } else{
    cat("No new objects to store")
  }
}

#' Work Space Reference Table
#'
#' @description Visualize the work space reference table file on the project's folder
#' @param subset A logical expression to select specific objects
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @return A data.frame with the information of the different objects in all the .RData files of the folder
#' @export
ws_ref_table <- function(subset, file="ws_table.ref"){
  metadata <- suppressWarnings(tryCatch(readRDS(file), error=function(e) cat("No previous metadata")))
  if(!is.null(metadata)){
    if(missing(subset)) f <- rep(TRUE, nrow(metadata)) else f <- eval(substitute(subset), metadata, baseenv())
    metadata[f, ]
  }
}


#' Purge Work Space Reference Table
#'
#' @description Remove objects from the saved .RData files and update the Work Space Reference Table
#' @param subset A logical expression to select specific objects
#' @param remove If TRUE, objects are actually removed. If FALSE, a list of objects that would be removed is returned.
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @param rds.folder Name of the folder where .rds files are stored
#' @return .RData files are updated removing the selected objects and the Work Space Reference Table is updated.
#' @export
purge_ws_table <- function(subset, remove=FALSE, file="ws_table.ref", rds.folder="rds"){
  path <- paste("./", rds.folder, "/", sep="")
  metadata_complete <- ws_ref_table(file=file)
  if(missing(subset)) f <- rep(TRUE, nrow(metadata_complete)) else f <- eval(substitute(subset), metadata_complete, baseenv())
  metadata <- metadata_complete[f,]
  if(remove){
    lapply(metadata$hash, function(x){
      file.remove(paste(path, x, ".rds", sep=""))
    })
    metadata <- metadata_complete[!f,]
    saveRDS(metadata, file=file)
  }
  cat("Purged objects: \n")
  print(metadata_complete[f,])
}
