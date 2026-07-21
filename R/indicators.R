hicu_empty_country_deno_7d <- function() {
  data.frame(
    ReportingCountry = character(),
    UnitSize_median = numeric(),
    NumPatDays7Days_mean_median = numeric(),
    NumRegNurseHours7Days_mean_median = numeric(),
    NumNursingAssistHours7Days_mean_median = numeric(),
    NumAlcoholHandRubLiters_median = numeric(),
    NumPatientDaysPrevYear_median = numeric()
  )
}

hicu_indicator_codes <- function() {
  c("ASTREV72H", "CVCSITDRES", "INTCUFPRES", "INTORDECON", "INTPOSNSUP")
}

hicu_indicator_value_columns <- function() {
  as.vector(outer(
    c("IndNumCompliant", "IndNumObservations", "IndPerc"),
    hicu_indicator_codes(),
    paste,
    sep = "_"
  ))
}

hicu_empty_country_ind <- function() {
  out <- data.frame(ReportingCountry = character())
  for (column in hicu_indicator_value_columns()) {
    out[[paste0(column, "_mean")]] <- numeric()
  }
  out$n_units <- integer()
  out
}

#' Summarise country 7-day denominator indicators
#'
#' @param denominator_standard Standard protocol denominator table.
#' @param denominator_light Light protocol denominator table.
#' @param units Combined unit table with `RecordId` and country metadata.
#'
#' @return Country-level 7-day structure denominator table.
#' @export
summarise_country_deno_7d <- function(
    denominator_standard,
    denominator_light,
    units
) {
  denominator <- dplyr::bind_rows(denominator_standard, denominator_light)
  if (nrow(denominator) == 0) {
    return(hicu_empty_country_deno_7d())
  }

  unit_metadata <- units |>
    dplyr::transmute(
      ParentId = .data$RecordId,
      ReportingCountry = as.character(.data$ReportingCountry),
      NumAlcoholHandRubLiters = as.numeric(.data$NumAlcoholHandRubLiters),
      NumPatientDaysPrevYear = as.numeric(.data$NumPatientDaysPrevYear),
      UnitSize = as.numeric(dplyr::na_if(as.character(.data$UnitSize), "UNK")),
      UnitSpecialty = as.factor(.data$UnitSpecialty)
    )

  denominator_units <- denominator |>
    dplyr::left_join(unit_metadata, by = "ParentId") |>
    dplyr::filter(!is.na(.data$AuditStart), .data$AuditStart != "N/A") |>
    dplyr::mutate(
      AuditStart = hicu_parse_mixed_date(.data$AuditStart),
      AuditEnd = hicu_parse_mixed_date(.data$AuditEnd),
      PeriodStart = hicu_parse_mixed_date(.data$PeriodStart),
      PeriodEnd = hicu_parse_mixed_date(.data$PeriodEnd),
      audit_days = as.numeric(.data$AuditEnd - .data$AuditStart),
      period_days = as.numeric(.data$PeriodEnd - .data$PeriodStart),
      dplyr::across(
        dplyr::any_of(c(
          "NumPatDays7Days", "NumRegNurseHours7Days",
          "NumNursingAssistHours7Days", "NumUnitAdmission2d"
        )),
        as.numeric
      )
    )

  if (nrow(denominator_units) == 0) {
    return(hicu_empty_country_deno_7d())
  }

  denominator_by_unit <- denominator_units |>
    dplyr::group_by(
      .data$ParentId,
      .data$ReportingCountry,
      .data$UnitSize,
      .data$UnitSpecialty,
      .data$NumAlcoholHandRubLiters,
      .data$NumPatientDaysPrevYear
    ) |>
    dplyr::summarise(
      NumPatDays7Days_mean = mean(.data$NumPatDays7Days, na.rm = TRUE),
      NumRegNurseHours7Days_mean = mean(.data$NumRegNurseHours7Days, na.rm = TRUE),
      NumNursingAssistHours7Days_mean = mean(
        .data$NumNursingAssistHours7Days,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::distinct()

  denominator_by_unit |>
    dplyr::select(
      "ParentId", "ReportingCountry", "UnitSize", "UnitSpecialty",
      "NumPatDays7Days_mean", "NumRegNurseHours7Days_mean",
      "NumNursingAssistHours7Days_mean", "NumAlcoholHandRubLiters",
      "NumPatientDaysPrevYear"
    ) |>
    dplyr::distinct() |>
    dplyr::select(-dplyr::any_of(c("ParentId", "UnitSpecialty"))) |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        list(median = ~ stats::median(.x, na.rm = TRUE))
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(ReportingCountry = as.character(.data$ReportingCountry)) |>
    as.data.frame()
}

#' Summarise country process indicators
#'
#' @param indicator_standard Standard protocol process indicator table.
#' @param indicator_light Light protocol process indicator table.
#' @param denominator_standard Standard protocol denominator table.
#' @param denominator_light Light protocol denominator table.
#' @param units Combined unit table with `RecordId` and country metadata.
#'
#' @return Country-level chart-review/direct-observation indicator table.
#' @export
summarise_country_process_indicators <- function(
    indicator_standard,
    indicator_light,
    denominator_standard,
    denominator_light,
    units
) {
  indicator_columns <- c("IndNumCompliant", "IndNumObservations")
  indicator_standard <- indicator_standard |>
    dplyr::mutate(dplyr::across(dplyr::any_of(indicator_columns), as.character))
  indicator_light <- indicator_light |>
    dplyr::mutate(dplyr::across(dplyr::any_of(indicator_columns), as.character))
  indicators <- dplyr::bind_rows(indicator_standard, indicator_light)
  denominator <- dplyr::bind_rows(denominator_standard, denominator_light) |>
    dplyr::select("RecordId", "ParentId") |>
    dplyr::rename(denominator_id = "RecordId", unit_id = "ParentId")

  if (nrow(indicators) == 0 || nrow(denominator) == 0) {
    return(hicu_empty_country_ind())
  }

  unit_country <- units |>
    dplyr::transmute(
      RecordId = .data$RecordId,
      ReportingCountry = as.character(.data$ReportingCountry)
    ) |>
    dplyr::rename(unit_id = "RecordId")

  indicator_clean <- indicators |>
    dplyr::left_join(denominator, by = c("ParentId" = "denominator_id")) |>
    dplyr::filter(
      .data$IndNumObservations != "UNK",
      .data$IndNumCompliant != "UNK"
    ) |>
    dplyr::mutate(
      IndNumCompliant = as.numeric(.data$IndNumCompliant),
      IndNumObservations = as.numeric(.data$IndNumObservations),
    ) |>
    dplyr::filter(.data$IndNumObservations != 0) |>
    dplyr::mutate(
      IndNumCompliant = dplyr::if_else(
        .data$IndNumObservations < .data$IndNumCompliant,
        .data$IndNumObservations,
        .data$IndNumCompliant
      ),
      IndPerc = round(.data$IndNumCompliant / .data$IndNumObservations * 100, 1)
    )

  if (nrow(indicator_clean) == 0) {
    return(hicu_empty_country_ind())
  }

  indicator_wide <- indicator_clean |>
    dplyr::select(-dplyr::any_of("RecordId")) |>
    tidyr::pivot_wider(
      names_from = "IndicatorCode",
      values_from = c("IndNumCompliant", "IndNumObservations", "IndPerc")
    )

  for (column in hicu_indicator_value_columns()) {
    if (!column %in% names(indicator_wide)) {
      indicator_wide[[column]] <- NA_real_
    }
  }

  indicator_wide |>
    dplyr::left_join(unit_country, by = "unit_id") |>
    dplyr::select(
      "ParentId", "ReportingCountry",
      dplyr::all_of(hicu_indicator_value_columns())
    ) |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        list(mean = ~ mean(.x, na.rm = TRUE))
      ),
      n_units = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(ReportingCountry = as.character(.data$ReportingCountry)) |>
    as.data.frame()
}