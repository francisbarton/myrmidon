#' @title Shortcut to `glimpse` an object
#'
#' @return Nothing, per se
#' @export
#'

# I want to rewrite this so it just performs the action (glimpse/View) rather
# than adding in any code
pipeToGlimpse <- function() {
  rstudioapi::insertText(" %>% glimpse()")
}
