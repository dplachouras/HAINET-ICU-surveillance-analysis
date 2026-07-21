# Date helpers ----------------------------------------------------------

parse_mixed_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  iso_date <- as.Date(x, format = "%Y-%m-%d")
  day_month_year_date <- as.Date(x, format = "%d/%m/%Y")

  dplyr::coalesce(iso_date, day_month_year_date)
}

hicu_parse_mixed_date <- function(x) {
  parse_mixed_date(x)
}