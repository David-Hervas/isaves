#' Load incremental
#'
#' @description Load, potentially selecting specific objects, all the incremental saves in the project folder
#' @param subset A logical expression to select specific objects
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @return A loaded workspace in the Gloval Environment
#' @export
load_incremental <- function(subset, metadata.file="ws_table.ref", overwrite=FALSE){  #Avisar cuando hay objetos con el mismo nombre?
  metadata_complete <- ws_ref_table(metadata.file)
  current_ws <- objects(envir = .GlobalEnv)
  if(missing(subset)) f <- rep(TRUE, nrow(metadata_complete)) else f <- eval(substitute(subset), metadata_complete, baseenv())
  metadata <- metadata_complete[f, ]
  if(!overwrite & any(objects(envir = .GlobalEnv) %in% metadata$object)) stop(paste("Same object name in current Global Environment and .RData file:", objects(envir = .GlobalEnv)[objects(envir = .GlobalEnv) %in% metadata$object]))
  load_files <- unique(metadata$file[order(metadata$date, decreasing = TRUE)][!duplicated(metadata$object[order(metadata$date, decreasing = TRUE)])])
  to_load <- paste(rev(load_files), ".RData", sep="")
  lapply(to_load, load, envir = .GlobalEnv)
  if(!all(metadata_complete$hash[metadata_complete$file %in% load_files] %in% metadata$hash)){
    to_remove <- metadata_complete$object[(!metadata_complete$object %in% metadata$object) &
                                            !(metadata_complete$object %in% current_ws)]
    rm(list=to_remove, envir = .GlobalEnv)
  }
}

#' Save incremental
#'
#' @description Save only the objects from the workspace that are not already stored in other .RData files in the project folder
#' @param file Name of the .RData file to store the saved workspace
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @return An incremental save of the workspace is stored in a file on the project's folder
#' @export
save_incremental <- function(file, metadata.file="ws_table.ref"){
  current_ws <- objects(envir = .GlobalEnv)
  old_metadata <- ws_ref_table(metadata.file)
  if(file %in% old_metadata$file) stop("Filename already exists. If you want to remove files or objects please use the purge_ws_table() function")
  save(list=current_ws[!sapply(current_ws, function(.x) digest::digest(get(.x))) %in% old_metadata$hash], file=paste(file, ".RData", sep=""))
  metadata <- do.call(rbind,
                      lapply(current_ws[!sapply(current_ws, function(.x) digest::digest(get(.x))) %in% old_metadata$hash],
                             function(.x){ data.frame(file=file,
                                                     object=.x,
                                                     class=class(get(.x))[1],
                                                     size=as.numeric(object.size(get(.x))),
                                                     date=Sys.time(),
                                                     hash=digest::digest(get(.x)),
                                                     comment=ifelse(!is.null(comment(get(.x))), comment(get(.x)), NA))}))
  if(file.exists(metadata.file)){
    metadata <- rbind(old_metadata, metadata)
  }
  save(metadata, file=metadata.file)
}

#' Work Space Reference Table
#'
#' @description Visualize the work space reference table file on the project's folder
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @return A data.frame with the information of the different objects in all the .RData files of the folder
#' @export
ws_ref_table <- function(file="ws_table.ref"){
  suppressWarnings(tryCatch(load(file), error=function(e) cat("No previous metadata")))
  if(exists("metadata")) metadata
}


#' Purge Work Space Reference Table
#'
#' @description Remove objects from the saved .RData files and update the Work Space Reference Table
#' @param subset A logical expression to select specific objects
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @param remove If TRUE, objects are actually removed. If FALSE, a list of objects that would be removed is returned.
#' @return .RData files are updated removing the selected objects and the Work Space Reference Table is updated.
#' @export
purge_ws_table <- function(subset, file="ws_table.ref", remove=FALSE){
  metadata_complete <- ws_ref_table(file)
  if(missing(subset)) f <- rep(TRUE, nrow(metadata_complete)) else f <- eval(substitute(subset), metadata_complete, baseenv())
  metadata <- metadata_complete[f,]
  if(remove){
    lapply(unique(metadata$file), function(.x){
      e <- local({load(paste(.x, ".RData", sep="")); environment()})
      rm(list=metadata$object[metadata$file == .x], envir = e)
      save(list=objects(envir = e), file=paste(.x, ".RData", sep=""))
    })
  }
  metadata <- metadata_complete[!f,]
  save(list="metadata", file=file)
  cat("Purged objects: \n")
  print(metadata_complete[f,])
}
