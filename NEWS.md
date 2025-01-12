# 0.6.18 (10 September 2024)

* Add `theme_linen()` and `theme_clay()` ggplot2 themes
* Add function to create `.myr_prompt_env` on package load
* Add `chars()` helper function
* Review function documentation and imports
* We still have a lot of dependencies (33!)
* `get_daily_welsh_word()` is still glitching due to inconsistencies in source data structure from day to day
* Remove `how_old_am_i()` function which was only ever a little toy as I was learning stuff many years ago - it's not useful now
* Little bit of housekeeping on `myr_colours` list
* Update DESCRIPTION

# 0.6.15 (26 March 2024)

* Added `suggest_postcode_fixes()` function and simplified `postcode_data_join()` function as a result.

# 0.6.13 (11 May 2023)

* Moved my prompt functions to be RStudio add-ins

# 0.6.12 (10 April 2023)

* Added auto_reprex function and RStudio add-in

# 0.6.11 (7 April 2023)

* Apparently I don't update this file very often...
* Added transpose_tbl_wider()
* Added patch_join()
* Various improvements to hcl_msoa_names()

# 0.6.7 (1 August 2022)

* Added `get_daily_welsh_word()` function just for fun
* Bit of an RSS reader (non-robust) in there too
* Improvements to `my_prompt()` [`prompts.R`] particularly around Unicode handling

# 0.6.6 (10 January 2022)

* Further fixes to the `narrow` function in `postcode_data_join`
* `postcode_data_join` gets an example in its manpage...
* ... and `README.md` finally gets a little bit of love.

# 0.6.5 (7 January 2022)

* Added `narrow` option to `postcode_data_join` to reduce columns in the return.
* Added `add_msoa_names` function to facilitate using the [HoCL MSOA Names resource](https://houseofcommonslibrary.github.io/msoanames/).

# 0.6.4 (7 January 2022)

* Added new function `bb_to_poly`
* Fixed an issue with `batch_it` example code (and simplified the example)
* Further attempts to fix `postcode_data_join` and reduce CMD check notes
* Added `.data` pronoun to scripts to reduce `devtools::check()` Notes
* Discovered weird issue with `commonest` tests

# 0.6.3 (30 November 2021)

* Created new function postcode_data_join for mutating a data frame with a column of postcodes, to add postcode data from the postcodes.io API. I actually wrote tests!

# 0.6.2 (23 November 2021)

* Moved `ons_age_profiles` data object and `generate_stats()` out to a new package/repo called co-useful-data

# 0.6.1 (8 November 2021)

* Further work on Rmd template and documented some hitherto undocumented functions
* Created `ons_age_profiles` data object and added `generate_stats` function to produce age banded stat summaries from this data.

# 0.6 (29 October 2021)

* developed new RMarkdown template `myrmidon2`, with the aim to be HTML5 compatible and get rid of certain options in default `html_document()` that I don't agree with.
* added `knit_to_html5` and `new_rmd()` functions

# 0.5.5 (18 August 2021)

* add `gmaps_to_sf` function

# 0.5.4 (25 June 2021)

* added new function `expand_sf_bbox` which automates creating a bigger (or smaller) bbox - as an `sf` object - from an existing bboxable object
* re-purposed `utils_pipe.R` as general package doc and renamed to `myrmidon-package.R`, adding in `globalVariables` check as used by Jenny Bryan (see https://stackoverflow.com/a/57755406/5168907)

# 0.5.3

* fixed annoying problem with `get_postcode_data()` return that was erroring
* changed return style argument from minimal (Boolean) to an option of 3 character strings (moderate, minimal or full)
* added "quiet" option as a parameter to `batch_it()`
* `purrr`-ified the batching algorithm in `batch_it()`, replacing `for()` loop

# 0.5.2

* added `batch_it.R` and `batch_it_simple.R`
* played around with dates and `lubridate`

# 0.5.1

* added `download_files.R` to handle multiple URLs
* rejigged `import_rds.R` into a single function
* un-exported the assignment pipe addin function
* improved roxygen documentation for some functions
* fixed issue where I had not understood the Title line in roxygen :-/

# 0.5.0

* Added `save_it.R`
* removed `pipeToView.R` as not a good thing

# 0.4.0

* Added a `NEWS.md` file to track changes to the package.
* Created `download_file` wrapper script
* Added documentation for `download_file`
* used roxygen and `devtools::document()` for the first time.
* Package passes all build checks and is now at 0.4.0.
* Added MIT Licence
