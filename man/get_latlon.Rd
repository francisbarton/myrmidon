% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_latlon.R
\name{get_latlon}
\alias{get_latlon}
\title{Use doogal.co.uk API To Get Postcode Data}
\usage{
get_latlon(postcode)
}
\arguments{
\item{postcode}{a UK postcode}
}
\description{
Deprecated - use \code{get_postcode_data()} instead
Doesn't accept a vector of codes all at once, so use with purrr::map_dfr()
}
