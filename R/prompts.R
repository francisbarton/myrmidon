prompt_rstudio <- function() {

  # Won't work at first prompt as it runs from .Rprofile
  # before R is aware of RStudio
  rstd <- tryCatch(
    RStudio.Version()$release_name, error = function(e) NULL)

  if (!is.null(rstd)) {
    colour <- get0("myr_prompt_col3", .myr_prompt_env,
                   ifnotfound = "darkslateblue")
    crayon::style(rstd, colour)
  } else NULL
}




prompt_location <- function(unicode) {
  colour <- get0("myr_prompt_col1", .myr_prompt_env,
                 ifnotfound = "whitesmoke")
  location <- basename(getwd())
  icon <- if(unicode) {
    if (file.exists(here::here("DESCRIPTION"))) "\U1F4E6"
    else "\U1F4C2"
  } else NULL
  paste0(icon, crayon::style(location, colour))
}





prompt_git_branch <- function(unicode) {
  if (prompt::is_git_dir()) {
    colour <- get0("myr_prompt_col2", .myr_prompt_env,
                   ifnotfound = "orange")

    if (unicode) {
      paste0(
        crayon::style("\uE0A0", colour),
        crayon::style(gert::git_branch(), colour))
    } else {
      crayon::style(gert::git_branch(), colour)
    }
  } else NULL
}



prompt_git_status <- function() {
  if (prompt::is_git_dir() & nrow(gert::git_remote_list()) > 0) {
    git_ab <- gert::git_ahead_behind()

    if (git_ab$ahead > 0 ) {
      git_ahead <- paste0(" ", git_ab$ahead,
                          crayon::style("\u25B2", "slateblue1"))
      } else git_ahead <- NULL

    if (git_ab$behind > 0 ) {
      git_behind <- paste0(" ", git_ab$behind,
                           crayon::style("\u25BC", "tomato1"))
      } else git_behind <- NULL

    if (!is.null(c(git_ahead, git_behind))) {
      paste0(git_ahead, git_behind)
      } else NULL
  } else NULL
}


prompt_memuse <- function(unicode) {
  mem_pct <- ps::ps_system_memory()$percent / 100
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
      paste0(gry_blocks[setdiff(seq(4), seq(mem))], collapse = "")
    )
  } else {
    colours <- c(rep("palegreen4", 2), "orange", "orangered3")
    crayon::style(
      round(mem_pct * 100, digits = 1),
      colours[mem])
  }
}



prompt_pkgs <- function(unicode) {
  n <- length(unname(utils::old.packages()[, "Package"]))
  if (n > 0) {
    if (unicode) paste0("\U1F4E6", n) else n
  } else NULL
}


prompt_moon <- function() {
  if (requireNamespace("suncalc", quietly = TRUE)) {
    moon_emoji <- c(
      "\U1F311", "\U1F312", "\U1F313", "\U1F314",
      "\U1F315", "\U1F316", "\U1F317", "\U1F318",
      "\U1F311"
    )
    moon_phase <- rlang::env_cache(.myr_prompt_env, "moon_phase",
            suncalc::getMoonIllumination()$phase)
    if (!is.null(moon_phase)) {
      moon_phase <- round(moon_phase * (length(moon_emoji) -1)) + 1
      moon_emoji[[moon_phase]]
    } else NULL
  }
}



prompt_uptime <- function(prefix = "up: ") {
  start_time <- rlang::env_cache(.myr_prompt_env, "start_time",
                                 Sys.time())

  uptime <- difftime(Sys.time(), start_time, units = "auto")
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
