#' Converts R colours to hex codes
#' 
#' Adapted from https://github.com/talgalili/gplots/blob/master/R/col2hex.R
#' @param x character A vector of R colour names from base::colours()
#' @examples col2hex("seagreen4")
#' @export
col2hex <- \(x) `names<-`(tolower(rgb(t(col2rgb(x)), maxColorValue = 255)), x)
