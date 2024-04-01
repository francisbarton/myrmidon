#' Register a font for use in PDF creation
#'
#' Based on work by Konrad Rudolph here
#' https://github.com/klmr/ggplots/blob/main/fonts.r
#'
#' @param name character. The name of the font (or the font filename) you
#' want to register
#'
#' @export


register_pdf_font <- function(name) {

  assertthat::assert_that(requireNamespace("extrafontdb"), msg = "It looks like the extrafontdb package is not installed.")

  font_paths <- function(name, path) {
    list.files(path, pattern = paste0("^", name, ".*.afm.gz"), ignore.case = TRUE)
  }
  name <- sub(" ", "", name)
  extrafontdb_path <- system.file('metrics', package = 'extrafontdb', mustWork = TRUE)

  assertthat::assert_that(!rlang::is_empty(font_paths(name, extrafontdb_path)),
                          msg = "No fonts found starting with this name. Try removing spaces or replacing them with hyphens.")


  font_spec <- grDevices::Type1Font(name, font_paths(name, extrafontdb_path))
  font_args <- rlang::set_names(list(font_spec), name)
  rlang::inject(grDevices::pdfFonts(!!!font_args))

  invisible(TRUE)
}
