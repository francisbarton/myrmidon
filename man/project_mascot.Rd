% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_mascot.R
\name{project_mascot}
\alias{project_mascot}
\title{Get a cute emoji mascot for your latest project}
\usage{
project_mascot(alliterative = FALSE)
}
\arguments{
\item{alliterative}{Boolean. \code{TRUE} will try to return a name that starts
with the same letter as the (randomly) chosen emoji. \code{FALSE} by default.}
}
\description{
... or even just for your current R session
}
\examples{
project_mascot() |>
  glue::glue_data("{mascot_name} the {mascot_label} {mascot_emoji}")

}
