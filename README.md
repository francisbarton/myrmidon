# myrmidon

<!-- badges: start -->
[![R-CMD-check](https://github.com/francisbarton/myrmidon/workflows/R-CMD-check/badge.svg)](https://github.com/francisbarton/myrmidon/actions)
<!-- badges: end -->

## Personal functions and templates, from the whimsical to the useful to the rather experimental.

> The Myrmidons of Greek myth were known for their loyalty to their leaders, so that in pre-industrial Europe the word "myrmidon" carried many of the same connotations that "robot" does today. [wikipedia](https://en.m.wikipedia.org/wiki/Myrmidons#Modern_Myrmidons)

<figure style="margin: 30px;">
  <img src="inst/img/bad_myrmidon.png" width="33%" alt="Myrmidon cartoon by Horacio Gonzalez. A 'centaur' ant is drawn, with an ant's body and legs, and a humanoid torso, wearing a breastplate and helmet in an Ancient Greek style, and carrying a spear in its right hand and a sword in its left. (It also has its six ant legs)." />
  <figcaption><em>Myrmidon cartoon – represented as a half-ant, half-human warrior creature – by <a href="https://lostinbrittany.org/blog/2006/09/28/monstres-pour-la-grece-fantastique/">Horacio Gonzalez</a>.</em></figcaption>
 </figure>

> What do ants best represent in the cosmos? [Carol] Anelli reports [E.O.] Wilson insisting that they "do it all" and dominate on a number of fronts. They are exceedingly diverse -- predators, farmers, architects -- and are highly social. [Delahoyde & Hughes, 2008](https://web.archive.org/web/20080224072456/http://www.wsu.edu/~delahoyd/myrmidons.html)

### Functions

Some useful functions you will find in here:

* postcode_data_join: Get postcode data from postcodes.io and join it to an existing df
* batch_it: Turn a vector into a batched list
* patch_join: (AKA coalesce join) Join 2 data frames, using values in `y` to patch NAs in `x`
* project_mascot: Get a cute emoji mascot for your latest project - or just for your current R session
* reprex_to_gist: Send reprex code and output to a new GH gist (based on Mickaël Canouil's code [here][mc-gist])
* theme_linen and theme_clay: my personal ggplot2 themes
* save_it: Wrapper for saveRDS - a quick way to save an object to an `Rds` file
* extract_col_types: A helper for `col_types` in `readr::read_csv()` _& co._
* hcl_msoa_names: A simple function to get the latest [House of Commons Library MSOA Names][hcl-msoa]
* commonest: a function that returns the mode (most common value) from a vector
* gmaps_to_sf: Convert Google Maps lat-lon data to a geospatial `sf` object
* year_dates: Create vectors of dates for one or more years (and/or months)
* bbox_to_poly: Convert a bbox to an `sf` geospatial polygon (rectangle)

[mc-gist]: https://github.com/tidyverse/reprex/issues/190#issuecomment-817313938
[hcl-msoa]: https://houseofcommonslibrary.github.io/msoanames/

### Examples

#### postcode_data_join

```r
postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")
test_df1 <- dplyr::tibble(place = paste0("place_", 1:3), postcode = postcodes)
postcode_data_join(test_df1)
```


### HTML5 RMarkdown template

*details, details, details...*
=======


```
