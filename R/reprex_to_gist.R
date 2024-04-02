#' Easily post reprexes from selected text to a GitHub gist
#' 
#' If no selected text is found for the reprex input then the currently visible
#' open file, and then the system clipboard, will be tried in that order.
#' 
#' From https://github.com/tidyverse/reprex/issues/190#issuecomment-817313938
#' by Mickaël Canouil
#' 
#' @export
reprex_to_gist <- function() {
  input <- reprex:::rstudio_selection()
  if (input == character(0)) input <- reprex:::rstudio_file()
  if (input == character(0)) input <- reprex:::ingest_clipboard()
  if (all(input == "")) cli::cli_abort("No input found for reprex")
  
  output <- reprex::reprex(input = input, style = TRUE, html_preview = FALSE)
  codename <- sub("-", "_", sample(reprex:::adjective_animal, 1L))
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_id <- glue::glue("{codename}_{ts}.R")
  post_data <- list(list(content = paste(input, collapse = "\n"))) |>
    rlang::set_names(file_id)


  # Post the source code as a gist
  gist_info <- gh::gh(
    "POST /gists",
    description = "reprex code",
    files = post_data,
    public = TRUE
  )

  # Post the reprex output as a comment on the gist
  gist_comment <- gh::gh(
    gist_info[["comments_url"]],
    .method = "POST",
    body = paste(output, collapse = "\n")
  )

  gist_base <- "https://gist.github.com"
  my_gh <- gh::gh_whoami()[["login"]]
  gist_id <- gist_info[["id"]]
  gist_url <- glue::glue("{gist_base}/{my_gh}/{gist_id}")

  cli::cli_alert_success("reprex posted to {gist_url}")
  utils::writeClipboard(gist_url)
  utils::browseURL(gist_url)
  invisible(TRUE)
}