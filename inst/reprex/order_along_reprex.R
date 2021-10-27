# order_along reprex
library(dplyr, warn.conflicts = FALSE)

sw_select <- starwars %>%
  select(1,3,10,11)

#' Three ways that don't do what I want:
sw_select %>%
  group_by(species) %>%
  arrange(desc(mass), .by_group = TRUE) %>%
  ungroup() %>%
  head(10)

sw_select %>%
  split( ~  species) %>%
  purrr::map_dfr(~ arrange(., desc(mass))) %>%
  head(10)

sw_select %>%
  arrange(desc(mass)) %>%
  group_by(species) %>%
  group_split() %>%
  purrr::reduce(bind_rows) %>%
  head(10)

#' One way that does:
sw_select %>%
  group_by(species) %>%
  slice_max(mass) %>%
  arrange(desc(mass)) %>%
  ungroup() %>%
  select(species) %>%
  left_join(arrange(sw_select, desc(mass))) %>%
  select(all_of(colnames(sw_select))) %>%
  head(10)

sw_select %>%
  order_along(order_along = species, sort_by = mass, desc = TRUE) %>%
  head(10)

