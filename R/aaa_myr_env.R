.myr_env <- rlang::new_environment()

populate_myr_env <- function() {
  # some variables that myrmidon::myr_prompt_*() can use
  rlang::env_bind_lazy(
    .myr_env,
    # time when R session started
    start_time = Sys.time(),
    moon_phase = suncalc::getMoonIllumination()[["phase"]],
    myr_prompt_col1 = "whitesmoke",
    myr_prompt_col2 = "orange",
    myr_prompt_col3 = "darkseagreen1"
  )
}