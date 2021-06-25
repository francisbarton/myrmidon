# myrmidon 0.5.4 (25 June 2021)

* added new function `expand_sf_bbox` which automates creating a bigger (or smaller) bbox - as an `sf` object - from an existing bboxable object
* re-purposed `utils_pipe.R` as general package doc and renamed to `myrmidon-package.R`, adding in `globalVariables` check as used by Jenny Bryan (see https://stackoverflow.com/a/57755406/5168907)

# myrmidon 0.5.3

* fixed annoying problem with `get_postcode_data()` return that was erroring
* changed return style argument from minimal (Boolean) to an option of 3 character strings (moderate, minimal or full)
* added "quiet" option as a parameter to `batch_it()`
* `purrr`-ified the batching algorithm in `batch_it()`, replacing `for()` loop

# myrmidon 0.5.2

* added `batch_it.R` and `batch_it_simple.R`
* played around with dates and `lubridate`

# myrmidon 0.5.1

* added `download_files.R` to handle multiple URLs
* rejigged `import_rds.R` into a single function
* un-exported the assignment pipe addin function
* improved roxygen documentation for some functions
* fixed issue where I had not understood the Title line in roxygen :-/

# myrmidon 0.5.0

* Added `save_it.R`
* removed `pipeToView.R` as not a good thing

# myrmidon 0.4.0

* Added a `NEWS.md` file to track changes to the package.
* Created `download_file` wrapper script
* Added documentation for `download_file`
* used roxygen and `devtools::document()` for the first time.
* Package passes all build checks and is now at 0.4.0.
* Added MIT Licence
