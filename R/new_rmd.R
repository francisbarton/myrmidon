#' Create a new RMarkdown draft based on a template
#' @inheritParams rmarkdown::draft
#' @export
new_rmd <- function(file = "index.Rmd", template = "myrmidon2", package = "myrmidon", create_dir = FALSE, edit = TRUE) {

  while (file.exists(file)) {
    file <- readline("that file already exists. please enter a new filename: ")
  }
  rmarkdown::draft(file = file,
                   template = template,
                   package = package,
                   create_dir = create_dir,
                   edit = edit)
}
