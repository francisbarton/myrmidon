#' Set your R library paths to whatever you choose
#'
#' Taken from Miles McBain's blog post at
#' https://www.milesmcbain.com/posts/hacking-r-library-paths/
#' @param libs A vector of library paths
#' @export
set_lib_paths <- function(libs) {
  libs <- normalizePath(libs, mustWork = TRUE)

  shim_fun <- .libPaths
  shim_env <- new.env(parent = environment(shim_fun))
  shim_env$.Library <- character()
  shim_env$.Library.site <- character()

  environment(shim_fun) <- shim_env
  shim_fun(libs)
}
