% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incremental_saves_R.R
\name{ws_ref_table}
\alias{ws_ref_table}
\title{Work Space Reference Table}
\usage{
ws_ref_table(subset, file = "ws_table.ref")
}
\arguments{
\item{subset}{A logical expression to select specific objects}

\item{file}{Name of the file in the folder containing the information about the saved workspaces}
}
\value{
A data.frame with the information of the different objects in all the .RData files of the folder
}
\description{
Visualize the work space reference table file on the project's folder
}
