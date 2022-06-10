prompt_rstudio <- function(col = "darkslateblue") {
  rstd <- RStudio.Version()$release_name
  crayon::style(rstd, col)
}

prompt_location <- function(col = "whitesmoke", icon = TRUE) {
  location <- basename(getwd())
  if (icon) icon <- "\U1F4C2" else icon <- NULL
  paste0(icon, crayon::style(location, col))
}

prompt_git <- function() {
  if (prompt::is_git_dir()) {
    git_ab <- gert::git_ahead_behind()

    if (git_ab$ahead > 0 ) {
      git_ahead <- paste0(" ", git_ab$ahead, crayon::style("\u25B2", "slateblue1"))
    } else git_ahead <- NULL
    if (git_ab$behind > 0 ) {
      git_behind <- paste0(" ", git_ab$behind, crayon::style("\u25BC", "tomato1"))
    } else git_behind <- NULL

    paste0(
      crayon::style("\uE0A0", "orange"),
      crayon::style(gert::git_branch(), "orange"),
      # prompt::git_dirty(),
      git_ahead, git_behind)
  } else {
    NULL
  }
}


prompt_moon <- function() {
  moons <- c(
    "\U1F311", "\U1F312",  "\U1F313", "\U1F314",
    "\U1F315", "\U1F316",  "\U1F317", "\U1F318"
  )
  moon_phase <- round(suncalc::getMoonIllumination()$phase * 8) + 1
  moons[moon_phase]
}

prompt_memuse <- function() {

  col_blocks <- c(
    crayon::style("\u2582", "darkolivegreen3"),
    crayon::style("\u2584", "darkolivegreen3"),
    crayon::style("\u2586", "darkolivegreen3"),
    crayon::style("\u2588", "orangered3")
  )
  gry_blocks <- c(
    crayon::style("\u2582", "grey67"),
    crayon::style("\u2584", "grey67"),
    crayon::style("\u2586", "grey67"),
    crayon::style("\u2588", "grey67")
  )

  mem <- ceiling(ps::ps_system_memory()$percent * 4)
  paste0(
    paste0(col_blocks[seq(mem)], collapse = ""),
    paste0(gry_blocks[setdiff(seq(4), seq(mem))], collapse = "")
  )
}

prompt_pkgs <- function() {
  n <- length(names(utils::old.packages()[,1]))
  paste0("\U1F4E6", n)
}

prompt_uptime <- function(prefix = "up: ") {
  rstime <- get0("rstime", .prompt_env, ifnotfound = NULL)
    if (!is.null(rstime)) {
      uptime <- difftime(Sys.time(), rstime, units = "auto")
      paste0(
        prefix,
        signif(as.double(uptime), 2),
        substr(units(uptime), 1, 1)
      )
    }
  }


prompt_toggl <- function(add_time = TRUE) {
  if (require(togglr)) {
    toggl <- togglr::get_current()$description
    if (is.null(toggl)) {
      toggl_status <- crayon::red$bold("\u2718")
    } else {
      toggl_time <- if (add_time) {
        paste0(
          signif(
            as.double(
              togglr::get_current_duration() / 60000,
              units = "mins"), 2),
          "m")
      } else NULL
      toggl_status <- paste0(
        crayon::style(toggl, "mediumorchid3"), " ",
        crayon::style(toggl_time, "darkolivegreen3"))
    }
    paste0(
      crayon::style("\u23FB", "mediumorchid1"),
      " ",
      toggl_status)
  }
}

#' Reset console prompt, with options.
#'
#' @export
prompt_reset <- function(type = c("min", "max")) {
  switch(type,
         "min" = {
           .prompt_env$type <- "min"
           prompt::set_prompt(
           function(expr, value, ok, visible) {
             chk <- ifelse(ok, crayon::green$bold("\u2713"), crayon::red$bold("\u2718"))
             cat(chk, prompt_uptime(), "> ")
           })},
         "max" = {
           .prompt_env$type <- "max"
           prompt::set_prompt(
           function(expr, value, ok, visible) {
             chk <- ifelse(ok, crayon::green$bold("\u2713"), crayon::red$bold("\u2718"))
             cat(
               prompt_rstudio(),
               prompt_pkgs(),
               prompt_location(),
               prompt_git(),
               # prompt_uptime(),
               # prompt_toggl(),
               chk,
               "\u27A4",
               "",
               sep = " "
             )})}
  )
  invisible(NULL)
}

#' @export
prompt_toggle <- function() {
  type <- get0("type", .prompt_env, ifnotfound = "max")
  other <- setdiff(c("max", "min"), type)
  prompt_reset(other)
}