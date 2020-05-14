pipeToGlimpse <- function() {
  rstudioapi::insertText(" %>% glimpse()")
}

# could this not just be written as:

# pipeToGlimpse <- rstudioapi::insertText(" %>% glimpse()")
# ???

# I mean pipeToGlimpse isn't a function of anything.
# It's just a wrapper
