#' @export
myr_prompt_light <- function(unicode = TRUE) {
  stopifnot(is.logical(unicode))

  prompt::set_prompt(
    function(expr, value, ok, visible) {
      chk <- if (ok) {
        crayon::green$bold("\u2713 ")
        } else {
        crayon::red$bold("\u2718 ")
      }
      cat(
        prompt_location(unicode),
        prompt_git_branch(unicode),
        chk
      )
    })
  invisible(TRUE)
}

#' @export
myr_prompt_no_unicode <- function() {
  prompt::set_prompt(
    function(expr, value, ok, visible) {
      cat(
        prompt_rstudio(),
        prompt_memuse(unicode = FALSE),
        prompt_location(unicode = FALSE),
        prompt_git_branch(unicode = FALSE)
      )
    })
  invisible(TRUE)
}


#' @export
myr_prompt_medium <- function() {
  prompt::set_prompt(
    function(expr, value, ok, visible) {
      chk <- if (ok) {
        crayon::green$bold("\u2713 ")
      } else {
        crayon::red$bold("\u2718 ")
      }
      cat(
        prompt_rstudio(),
        prompt_moon(),
        prompt_uptime(),
        prompt_location(unicode = TRUE),
        prompt_git_branch(unicode = TRUE),
        prompt_git_status(),
        chk
      )
    })
  invisible(TRUE)
}

#' @export
myr_prompt_heavy <- function() {
  prompt::set_prompt(
    function(expr, value, ok, visible) {
      chk <- if (ok) {
        crayon::green$bold("\u2713 ")
      } else {
        crayon::red$bold("\u2718 ")
      }
      cat(
        prompt_rstudio(),
        prompt_moon(),
        prompt_uptime(),
        prompt_memuse(unicode = TRUE),
        prompt_location(unicode = TRUE),
        prompt_git_branch(unicode = TRUE),
        prompt_git_status(),
        chk
      )
    })
  invisible(TRUE)
}
