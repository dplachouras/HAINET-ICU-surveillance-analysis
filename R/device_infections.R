#' Classify intubation-associated pneumonia cases
#'
#' @param patient_infections Patient-infection table.
#' @param exposure Patient exposure table.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Named list with classified cases and unit counts.
#' @export
classify_iap_cases <- function(patient_infections, exposure, patient_unit = NULL) {
  infection_rows <- patient_infections |>
    dplyr::select(
      dplyr::any_of(c("Id", "UnitId", "InfectionSite", "DateOfOnset"))
    ) |>
    dplyr::mutate(DateOfOnset = hicu_parse_mixed_date(.data$DateOfOnset))

  exposure_rows <- prepare_exposures(exposure, patient_unit = patient_unit) |>
    dplyr::select("Id", "ExpType", "DateExpStart", "DateExpEnd")

  cases <- merge(infection_rows, exposure_rows, by = "Id") |>
    dplyr::filter(
      grepl("PN", .data$InfectionSite),
      .data$ExpType == "INT",
      .data$DateOfOnset > .data$DateExpStart,
      .data$DateOfOnset < .data$DateExpEnd + 3
    ) |>
    dplyr::distinct(.data$Id, .data$DateOfOnset, .keep_all = TRUE) |>
    dplyr::group_by(.data$Id) |>
    dplyr::arrange(.data$DateOfOnset, .by_group = TRUE) |>
    dplyr::mutate(
      previous_onset = dplyr::lag(.data$DateOfOnset),
      days_since_previous = as.numeric(.data$DateOfOnset - .data$previous_onset)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(is.na(.data$days_since_previous) | .data$days_since_previous > 7) |>
    as.data.frame()

  unit_counts <- cases |>
    dplyr::count(.data$UnitId, name = "IAP") |>
    as.data.frame()

  list(cases = cases, unit_counts = unit_counts)
}

#' Classify central-line associated BSI cases
#'
#' @param patient_infections Patient-infection table.
#' @param exposure Patient exposure table.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Named list with classified cases and unit counts.
#' @export
classify_cvc_associated_bsi <- function(
    patient_infections,
    exposure,
    patient_unit = NULL
) {
  infection_rows <- patient_infections |>
    dplyr::select(
      dplyr::any_of(c(
        "Id", "RecordId", "UnitId", "InfectionSite", "BSIOrigin",
        "DateOfOnset", "InfectionOutcome"
      ))
    ) |>
    dplyr::mutate(DateOfOnset = hicu_parse_mixed_date(.data$DateOfOnset))

  exposure_rows <- prepare_exposures(exposure, patient_unit = patient_unit) |>
    dplyr::select("Id", "ExpType", "DateExpStart", "DateExpEnd")

  cases <- merge(infection_rows, exposure_rows, by = "Id") |>
    dplyr::filter(
      grepl("CRI3|BSI", .data$InfectionSite),
      grepl("C-CVC|^C$|UNK|UO|N/A", .data$BSIOrigin),
      .data$ExpType == "CVC",
      .data$DateOfOnset > .data$DateExpStart + 1,
      .data$DateOfOnset < .data$DateExpEnd + 2,
      .data$DateExpEnd - .data$DateExpStart > 1
    ) |>
    dplyr::distinct(.data$Id, .data$DateOfOnset, .keep_all = TRUE) |>
    dplyr::mutate(clabsi = TRUE) |>
    as.data.frame()

  unit_counts <- cases |>
    dplyr::count(.data$UnitId, name = "CVCASBSI") |>
    as.data.frame()

  list(cases = cases, unit_counts = unit_counts)
}