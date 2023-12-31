% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataObject.R
\name{Class}
\alias{Class}
\title{Set an new DataClass(environment)}
\usage{
Class(class)
}
\arguments{
\item{class}{character class name}
}
\value{
enviroment(DataClass)
}
\description{
Set an new environment to store important data in a project
}
\examples{
# # create DATA
# AA <- Class(class = "AA")
#
# # list info
# AA$catinfo()
#
# # add variable
# AA$new(T_DATA, "id", 1000, "X SYMOBOL list")
# AA$new(envir = T_DATA, variable = "id", value = 1000, info = "hacd")
#
# # list variable
# AA$id
#
# # delete variable
# AA$delete(XENADB, "phen")
}
