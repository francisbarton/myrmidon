prompt_rstudio <- function(prompt_env = .myr_env) {
  rstd <- tryCatch(rstudioapi::versionInfo(), error = \(e) NULL)
  if (!is.null(rstd)) {
    clr <- get0("myr_prompt_col3", prompt_env, ifnotfound = "darkslateblue")
    crayon::style(rstd[["release_name"]], clr)
  } else NULL
}



prompt_location <- function(unicode = TRUE, prompt_env = .myr_env) {
  clr <- get0("myr_prompt_col1", prompt_env, ifnotfound = "whitesmoke")
  location <- basename(getwd())
  icon <- if (unicode) {
    if (file.exists(here::here("DESCRIPTION"))) "\U1F4E6\u2009"
    else "\U1F4C2\u2009"
  } else NULL
  paste0(icon, crayon::style(location, clr))
}




prompt_git_branch <- function(unicode = TRUE, prompt_env = .myr_env) {
  if (prompt::is_git_dir()) {
    clr <- get0("myr_prompt_col2", prompt_env, ifnotfound = "orange")

    if (unicode) {
      paste0(
        crayon::style("\uE0A0", clr),
        crayon::style(gert::git_branch(), clr))
    } else {
      crayon::style(gert::git_branch(), clr)
    }
  } else NULL
}



prompt_git_status <- function() {
  if (prompt::is_git_dir()) {
    if (nrow(gert::git_remote_list()) > 0L) {
      git_ab <- gert::git_ahead_behind()

      if (git_ab[["ahead"]] > 0L ) {
        git_ahead <- paste0(" ", git_ab[["ahead"]],
          crayon::style("\u25B2", "slateblue1"))
      } else git_ahead <- NULL

      if (git_ab[["behind"]] > 0L ) {
        git_behind <- paste0(" ", git_ab[["behind"]],
          crayon::style("\u25BC", "tomato1"))
      } else git_behind <- NULL

      if (!any(is.null(c(git_ahead, git_behind)))) {
        paste0(git_ahead, git_behind)
      } else NULL
    } else NULL
  } else NULL
}


prompt_memuse <- function(unicode) {
  mem_pct <- ps::ps_system_memory()[["percent"]] / 100
  mem <- ceiling(mem_pct * 4)

  if (unicode) {
    col_blocks <- c(
      crayon::style("\u2582", "palegreen4"),
      crayon::style("\u2584", "palegreen4"),
      crayon::style("\u2586", "orange"),
      crayon::style("\u2588", "orangered3")
    )
    gry_blocks <- c(
      crayon::style("\u2582", "grey40"),
      crayon::style("\u2584", "grey40"),
      crayon::style("\u2586", "grey40"),
      crayon::style("\u2588", "grey40")
    )
    paste0(
      paste0(col_blocks[seq(mem)], collapse = ""),
      paste0(gry_blocks[setdiff(seq(4L), seq(mem))], collapse = "")
    )
  } else {
    colours <- c(rep("palegreen4", 2L), "orange", "orangered3")
    crayon::style(
      round(mem_pct * 100, digits = 1L),
      colours[mem])
  }
}



prompt_pkgs <- function(unicode) {
  n <- length(unname(utils::old.packages()[, "Package"]))
  if (n > 0L) {
    if (unicode) paste0("\U1F4E6", n) else n
  } else NULL
}


prompt_moon <- function(prompt_env = .myr_env) {
  if (requireNamespace("suncalc", quietly = TRUE)) {
    moon_emoji <- c(
      "\U1F311", "\U1F312", "\U1F313", "\U1F314",
      "\U1F315", "\U1F316", "\U1F317", "\U1F318",
      "\U1F311"
    )
    moon_phase <- prompt_env |>
      rlang::env_cache("moon_phase", suncalc::getMoonIllumination()[["phase"]])
    if (!is.null(moon_phase)) {
      moon_phase <- round(moon_phase * (length(moon_emoji) - 1L)) + 1L
      moon_emoji[[moon_phase]]
    } else NULL
  }
}



prompt_uptime <- function(prefix = "up: ") {
  start_time <- rlang::env_cache(.myr_env, "start_time", Sys.time())
  uptime <- difftime(Sys.time(), start_time, units = "auto")
  paste0(
    prefix,
    signif(as.double(uptime), 2L),
    substr(units(uptime), 1L, 1L)
  )
}




prompt_toggl <- function(unicode, add_time = TRUE) {

  if (requireNamespace("togglr", quietly = TRUE)) {
    toggl_desc <- togglr::get_current()[["description"]]

    if (is.null(toggl_desc)) {
      if (unicode) toggl_status <- crayon::red$bold("\u2718")
      else NULL
    }
    if (!is.null(toggl_desc)) {
      if (add_time) {
        toggl_time <- paste0(
          signif(
            as.double(
              togglr::get_current_duration() / 60000L,
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
