
# myrmidon

<!-- badges: start -->
[![R-CMD-check](https://github.com/francisbarton/myrmidon/workflows/R-CMD-check/badge.svg)](https://github.com/francisbarton/myrmidon/actions)
<!-- badges: end -->

Personal functions and templates, from the whimsical to the useful to the rather experimental.

> The Myrmidons of Greek myth were known for their loyalty to their leaders, so that in pre-industrial Europe the word "myrmidon" carried many of the same connotations that "robot" does today. [wikipedia](https://en.m.wikipedia.org/wiki/Myrmidons#Modern_Myrmidons)

![Myrmidon cartoon by Horacio Gonzalez. A "centaur" ant is drawn, with an ant's body and legs, and a human torso, wearing a breastplate and helmet in an Ancient Greek style, and carrying a spear in its right hand and a sword in its left. (It also has its six ant legs).](bad_myrmidon.png)

Myrmidon cartoon by [Horacio Gonzalez][hg].

[hg]: https://lostinbrittany.org/blog/2006/09/28/monstres-pour-la-grece-fantastique/


>  We hear about these ant-men in Metamorphoses and in Homer's Iliad, where they are Achilles' warriors. Ovid gives the origin: Aeacus, father of Peleus and Telamon and grandfather of Ajax, comes to an uninhabited island and prays to Jove for a populace. He dreams that ants, "grain-bearers," he saw on a tree magically transform into men. He awakens to his son Telamon bringing word that masses of people have inexplicably shown up, as Aeacus says, "greeting me as a ruler" (Ovid 172).
>  
> What do ants best represent in the cosmos? [Carol] Anelli reports [E.O.] Wilson insisting that they "do it all" and dominate on a number of fronts. They are exceedingly diverse -- predators, farmers, architects -- and are highly social. [Delahoyde & Hughes, 2008](https://web.archive.org/web/20080224072456/http://www.wsu.edu/~delahoyd/myrmidons.html)

Some useful functions you will find in here:

* get_postcode_data: Use postcodes.io To Get Postcode Metadata
* batch_it: Turn A Long Vector Into A Batched List
* save_it: Wrapper for saveRDS - Shortcut to Save an Object as an RDS File
* gmaps_to_sf: Convert Google Maps lat-lon Data to a Geospatial sf object
* year_dates: Create vectors of dates for one or more years (and/or months)


## Examples

### postcode_data_join
```r
postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")
test_df1 <- dplyr::tibble(place = paste0("place_", 1:3), postcode = postcodes)
postcode_data_join(test_df1, fix_invalid = TRUE, narrow = FALSE)
```
