# based on original function from Mauro Lepore's {handy} package -
# https://github.com/maurolepore/handy/blob/31d51816903193ab131bce10da641fbf8e21c469/R/open.R#L1:L8
# Grazie Mauro!

#' Provides easy edit access within RStudio to recent projects list
#' Windows only
#' @export
edit_rstudio_recent_projects <- function() {
  if (Sys.info()[["sysname"]] == "Windows") {
    Sys.getenv("LOCALAPPDATA") |>
      file.path("RStudio", "monitored", "lists", "project_mru") |>
      rstudioapi::navigateToFile()
  }
  invisible(TRUE)
}
