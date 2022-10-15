prompt_rstudio <- function(col = "darkslateblue") {

  rstd <- get0("rstd", .prompt_env, ifnotfound = NULL)

  if (is.null(rstd)) {
    rstd <- tryCatch(
      RStudio.Version()$release_name, error = function(e) NULL)
    rlang::env_bind(.prompt_env, rstd = rstd)
  }

  if (!is.null(rstd)) crayon::style(rstd, col) else NULL
}




prompt_location <- function(unicode, col = "whitesmoke") {
  location <- basename(getwd())
  icon <- if(unicode) "\U1F4C2" else NULL
  paste0(icon, crayon::style(location, col))
}





prompt_git <- function(unicode) {
  if (prompt::is_git_dir()) {
    if (unicode) {
      ab <- NULL
      if (nrow(gert::git_remote_list()) > 0) {
        git_ab <- gert::git_ahead_behind()

        if (git_ab$ahead > 0 ) {
          git_ahead <- paste0(" ", git_ab$ahead, crayon::style("\u25B2", "slateblue1"))
        } else git_ahead <- NULL
        if (git_ab$behind > 0 ) {
          git_behind <- paste0(" ", git_ab$behind, crayon::style("\u25BC", "tomato1"))
        } else git_behind <- NULL

        ab <- paste0(git_ahead, git_behind)
      }

      paste0(
        crayon::style("\uE0A0", "orange"),
        crayon::style(gert::git_branch(), "orange"),
        ab)
    } else {
      crayon::style(gert::git_branch(), "orange")
    }
  }
  else NULL
}



prompt_memuse <- function(unicode) {

  mem_pct <- ps::ps_system_memory()$percent
  mem <- ceiling(mem_pct * 3)

  if (unicode) {
    col_blocks <- c(
      crayon::style("\u2582", "palegreen4"),
      crayon::style("\u2585", "palegreen4"),
      crayon::style("\u2588", "orangered3")
    )
    gry_blocks <- c(
      crayon::style("\u2582", "grey40"),
      crayon::style("\u2585", "grey40"),
      crayon::style("\u2588", "grey40")
    )

    paste0(
      paste0(col_blocks[seq(mem)], collapse = ""),
      paste0(gry_blocks[setdiff(seq(3), seq(mem))], collapse = "")
    )
  } else {
    colours <- c(rep("palegreen4", 2), "orangered3")
    crayon::style(
      scales::percent(mem_pct, accuracy = 0.1),
      colours[mem])
  }
}



prompt_pkgs <- function(unicode) {
  n <- length(utils::old.packages()[,1])
  if (n > 0) {
    if (unicode) paste0("\U1F4E6", n)
    else n
  }
}


prompt_moon <- function(unicode) {
  if (requireNamespace("suncalc", quietly = TRUE) & unicode) {
    moon_emoji <- c(
    "\U1F316", "\U1F317", "\U1F318", "\U1F311",
    "\U1F312", "\U1F313", "\U1F314", "\U1F315"
  )
  moon_phase <- get0("moon_phase", .prompt_env, ifnotfound = suncalc::getMoonIllumination()$phase)
  # moon_phase <- rlang::env_cache(.prompt_env, "moon_phase", suncalc::getMoonIllumination()$phase)
  if (!is.null(moon_phase)) {
    moon_phase <- round(moon_phase * length(moon_emoji))
    if (moon_phase == 0) moon_phase <- 8
    moon_emoji[moon_phase]
  } else NULL
  }
}



prompt_uptime <- function(prefix = "up: ") {
  rstime <- get0("rstime", .prompt_env, ifnotfound = Sys.time())
  # rstime <- rlang::env_cache(.prompt_env, "rstime", Sys.time())

  uptime <- difftime(Sys.time(), rstime, units = "auto")
  paste0(
    prefix,
    signif(as.double(uptime), 2),
    substr(units(uptime), 1, 1)
  )
}




prompt_toggl <- function(unicode, add_time = TRUE) {

  if (requireNamespace("togglr", quietly = TRUE)) {

    toggl_desc <- togglr::get_current()$description

    if (is.null(toggl_desc)) {
      if (unicode) toggl_status <- crayon::red$bold("\u2718")
      else NULL
    }

    if (!is.null(toggl_desc)) {
      if (add_time) {
        toggl_time <- paste0(
          signif(
            as.double(
              togglr::get_current_duration() / 60000,
              units = "mins"), 2),
          "m")

        toggl_status <- paste0(
          crayon::style(toggl_desc, "mediumorchid3"),
          " ",
          crayon::style(toggl_time, "darkolivegreen3"))
      } else {
        toggl_status <- crayon::style(toggl_desc, "mediumorchid3")
      }

      if (unicode) {
        toggl_status <- paste0(
          crayon::style("\u23FB", "mediumorchid1"),
          " ",
          toggl_status)
      }
    }

    toggl_status
  } else NULL
}



#' My custom prompt
#'
#' @param unicode whether to use unicode characters in the prompt.
#' @export
my_prompt <- function(unicode = FALSE) {

  stopifnot(is.logical(unicode))
  rlang::env_bind(.prompt_env, use_unicode = unicode)

  prompt::set_prompt(
    function(expr, value, ok, visible) {
      chk <- if (unicode) {
        if (ok) crayon::green$bold("\u2713 ") else crayon::red$bold("\u2718 ")
      } else " "

      cat(
        prompt_rstudio(),
        prompt_moon(unicode),
        prompt_uptime(),
        # prompt_memuse(unicode), # issue with ps::ps_system_memory() currently
        prompt_pkgs(unicode),
        prompt_location(unicode),
        prompt_git(unicode),
        prompt_toggl(unicode),
        chk
      )})

  invisible(NULL)
}

#' Toggle unicode usage on/off in my custom prompt `my_prompt()`
#'
#' @export
switch_my_prompt <- function() {
  using_unicode <- get0("use_unicode", .prompt_env, ifnotfound = FALSE)
  my_prompt(!using_unicode)
}

