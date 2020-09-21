#' Find and Import RDS Files
#'
#' A quick way to read a load of Rds files into the global env.
#' The folder to search within can be specified, otherwise it will find all files
#' within the root/working directory (recursively, by default)
#'
#' @param folder a sub-folder to only search within (default is "" ie working directory)
#' @param extension a filename extension to import (single values only)
#' @param recursive whether to search through folders recursively (default is TRUE)
#'
#' @return object(s) in the global env.
#'
#' @importFrom here here
#' @importFrom stringr str_extract
#'
#' @export
#'
#' @examples
#' import_rds("rds_data")
import_rds <- function(folder = "", extension = "Rds", recursive = TRUE) {
  files <- list.files(
    path = here::here(folder),
    pattern = paste0("\\.", extension, "$"),
    recursive = recursive,
    full.names = TRUE,
    include.dirs = FALSE
  )

  for (file in files) {
    out <- stringr::str_extract(file, paste0("[^/]+(?=\\.", extension, "$)"))

    x <- readRDS(here::here(folder, file))

    assign(out, x, envir = .GlobalEnv)
  }
}
