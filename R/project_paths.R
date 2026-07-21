#' Find project root directory
#'
#' @return Absolute path to the project root.
#' @keywords internal
hicu_find_project_root <- function() {
  rprojroot::find_root(
    rprojroot::has_file("HAINET_ICU.Rproj") | rprojroot::has_dir(".git")
  )
}

#' Get default reporting year
#'
#' @return Character scalar year.
#' @keywords internal
hicu_default_year <- function() {
  Sys.getenv("HAINET_YEAR", unset = "2023")
}

#' Build default output directory path
#'
#' @param year Character year value.
#' @return Absolute path to outputs/<year>.
#' @keywords internal
hicu_default_output_dir <- function(year = hicu_default_year()) {
  file.path(hicu_find_project_root(), "outputs", year)
}

#' Build default raw data directory path
#'
#' @param year Character year value.
#' @return Absolute path to data/raw/<year>.
#' @keywords internal
hicu_default_data_dir <- function(year = hicu_default_year()) {
  file.path(hicu_find_project_root(), "data", "raw", year)
}
