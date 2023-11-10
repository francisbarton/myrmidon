#' @export
flowview <- function() {
  lst <- rstudioapi::getSourceEditorContext()
  doc_id <- lst[["id"]]
  cur_row <- lst[["selection"]][[1]][["range"]][["start"]][["row"]]
  row_start <- rstudioapi::document_position(cur_row, 1)
  rstudioapi::insertText(row_start, "  View() # ", doc_id)
  invisible(TRUE)
}
