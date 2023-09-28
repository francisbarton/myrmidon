#' @export
random_tube_route <- function() {

  tube_lines <- jsonlite::fromJSON("https://api.tfl.gov.uk/Line/Mode/tube") |>
    purrr::pluck("id") |>
    c(
      jsonlite::fromJSON("https://api.tfl.gov.uk/Line/Mode/elizabeth-line") |>
        purrr::pluck("id")
    )

  line_id <- tube_lines |>
    sample(1)

  tfl_req <- httr2::request("https://api.tfl.gov.uk/")
  tfl_app_key <- Sys.getenv("TFL_API_KEY")

  line_stop_ids <- tfl_req |>
    httr2::req_url_path_append("Line") |>
    httr2::req_url_path_append(line_id) |>
    httr2::req_url_path_append("StopPoints") |>
    httr2::req_url_query(app_key = tfl_app_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::map_chr("id")

  stop1 <- sample(seq_along(line_stop_ids), 1)

  if (stop1 <= length(line_stop_ids) / 2) {
    stop2 <- min(stop1 + sample(seq(4L), 1), length(line_stop_ids))
  } else {
    stop2 <- max(stop1 - sample(seq(4L), 1), 1L)
  }

  stop1_id <- line_stop_ids[stop1]
  stop2_id <- line_stop_ids[stop2]

  if (line_id == "elizabeth") {
    journey_mode <- "elizabeth-line"
  } else journey_mode <- "tube"

  route_data <- tfl_req |>
    httr2::req_url_path_append("Journey") |>
    httr2::req_url_path_append("JourneyResults") |>
    httr2::req_url_path_append(stop1_id) |>
    httr2::req_url_path_append("to") |>
    httr2::req_url_path_append(stop2_id) |>
    httr2::req_url_query(journeyPreference = "LeastInterchange") |>
    httr2::req_url_query(mode = journey_mode) |>
    httr2::req_url_query(app_key = tfl_app_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # route_durations <- route_data |>
  #   purrr::pluck("journeys") |>
  #   purrr::map_int("duration")

  route_legs <- route_data |>
    purrr::pluck("journeys") |>
    purrr::map("legs") |>
    lengths()

  selected_route <- route_data |>
    purrr::pluck("journeys") |>
    # purrr::keep(\(x) x[["duration"]] == min(route_durations)) |>
    purrr::keep(\(x) length(x[["legs"]]) == min(route_legs)) |>
    purrr::pluck(1) |>
    purrr::pluck("legs", 1)

  depart_stn <- selected_route |>
    purrr::pluck("departurePoint", "commonName") |>
    stringr::str_remove(" Rail Station$| Underground Station$") |>
    stringr::str_remove(" \\(.*$")
  arrive_stn <- selected_route |>
    purrr::pluck("arrivalPoint", "commonName") |>
    stringr::str_remove(" Rail Station$| Underground Station$") |>
    stringr::str_remove(" \\(.*$")
  direction <- selected_route |>
    purrr::pluck("instruction", "detailed") |>
    stringr::str_replace(" line", " Line")
  stop_points <- selected_route |>
    purrr::pluck("path", "stopPoints") |>
    purrr::map_chr("name") |>
    stringr::str_remove(" Rail Station$| Underground Station$") |>
    stringr::str_remove(" \\(.*$")
  route_line_id <- selected_route |>
    purrr::pluck("routeOptions", 1, "lineIdentifier", "id")


  if (length(stop_points) > 4) {
    arrive_stn <- stop_points[4]
    stop_points <- stop_points[1:3]
  }

  stop_points <- setdiff(stop_points, arrive_stn) |>
    stringr::str_flatten_comma(", and ")

  if (length(stop_points)) {
    stop_points <- paste0(", via ", stop_points)
  } else {
    stop_points <- ""
  }

  # Slightly amended from https://londonmymind.com/london-tube-colors/
  line_colours <- c(
    bakerloo           = "#b36305",
    central            = "#e32017",
    circle             = "#ddd300",
    district           = "#20984a",
    elizabeth          = "#6950a1",
    `hammersmith-city` = "#f3a9bb",
    jubilee            = "#a0a5a9",
    metropolitan       = "#9b0056",
    northern           = "#202020",
    piccadilly         = "#003688",
    victoria           = "#0098d4",
    `waterloo-city`    = "#95cdba"
  )

  # direction_line <- stringr::str_extract(direction, "^.+ Line") |>
  #   crayon::style(line_colours[[route_line_id]]) |>
  #   crayon::style("bold")
  twds <- paste0(
    " (——⦿ ",
    crayon::style(stringr::str_extract(direction, "(?<=towards ).*"), "bold"),
    ")") |>
    crayon::style(line_colours[[route_line_id]])

  glue::glue("{depart_stn} to {arrive_stn}{stop_points}{twds}")
}
