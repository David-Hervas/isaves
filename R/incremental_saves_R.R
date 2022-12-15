#' Load incremental
#'
#' @description Load, potentially selecting specific objects, all the incremental saves in the project folder
#' @param subset A logical expression to select specific objects
#' @param metadata.file Name of the file in the folder containing the information about the saved workspaces
#' @return A loaded workspace in the Gloval Environment
#' @export
load_incremental <- function(subset, metadata.file="ws_table.ref"){  #Avisar cuando hay objetos con el mismo nombre?
  metadata_complete <- ws_ref_table(metadata.file)
  if(missing(subset)) f <- rep(TRUE, nrow(metadata_complete)) else f <- eval(substitute(subset), metadata_complete, baseenv())
  metadata <- metadata_complete[f, ]
  load_files <- unique(metadata$file[order(metadata$date, decreasing = TRUE)][!duplicated(metadata$object[order(metadata$date, decreasing = TRUE)])])
  to_load <- paste(load_files, ".RData", sep="")
  lapply(to_load, load, envir = .GlobalEnv)
  if(!all(metadata_complete$hash[metadata_complete$file %in% load_files] %in% metadata$hash)){
    to_remove <- metadata_complete$object[!metadata_complete$object %in% metadata$object]
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
  save(list=current_ws[!sapply(current_ws, function(x) digest::digest(get(x))) %in% old_metadata$hash], file=paste(file, ".RData", sep=""))
  metadata <- do.call(rbind,
                      lapply(current_ws[!sapply(current_ws, function(x) digest::digest(get(x))) %in% old_metadata$hash],
                             function(x){ data.frame(file=file,
                                                     object=x,
                                                     class=class(get(x))[1],
                                                     size=as.numeric(object.size(get(x))),
                                                     date=Sys.time(),
                                                     hash=digest::digest(get(x)),
                                                     comment=ifelse(!is.null(comment(get(x))), comment(get(x)), NA))}))
  if(file.exists(metadata.file)){
    metadata <- rbind(old_metadata, metadata)
  }
  save(metadata, file=metadata.file)
}

#' Work Space Reference table
#'
#' @description Visualize the work space reference table file on the project's folder
#' @param file Name of the file in the folder containing the information about the saved workspaces
#' @return A data.frame with the information of the different objects in all the .RData files of the folder
#' @export
ws_ref_table <- function(file="ws_table.ref"){
  suppressWarnings(tryCatch(load(file), error=function(e) cat("No previous metadata")))
  if(exists("metadata")) metadata
}