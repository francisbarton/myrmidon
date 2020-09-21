#' Shortcut to Save an Object as an RDS File
#'
#' @param x Object to save (can be piped in)
#' @param dir Directory to store RDS files ('rds_data' by default)
#'
#' @importFrom here here
#' @importFrom rlang enexpr
#'
#' @return Creates an RDS file
#' @export
#'
save_it <- function(x, dir = "rds_data") {
  dirhere <- here::here(dir)
  filenm <- paste0(deparse(rlang::enexpr(x)), ".Rds")

  if (!dir.exists(dirhere)) {
    dir.create(dirhere)
  }

  saveRDS(x, here::here(dir, filenm))
}
