#' Prepare exposure records
#'
#' @param exposure Patient exposure table.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Exposure records with parsed dates clipped to unit stay dates.
#' @export
prepare_exposures <- function(exposure, patient_unit = NULL) {
  exposure_out <- exposure |>
    dplyr::select(
      dplyr::any_of(c(
        "Id", "ParentId", "RecordId", "UnitId", "ExpType", "DateExpStart",
        "DateExpEnd", "DateUnitAdmission", "DateUnitDischarge", "expdays"
      ))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "DateExpStart", "DateExpEnd", "DateUnitAdmission",
          "DateUnitDischarge"
        )),
        hicu_parse_mixed_date
      )
    )

  if (!"Id" %in% names(exposure_out) && "ParentId" %in% names(exposure_out)) {
    exposure_out$Id <- exposure_out$ParentId
  }

  if (!is.null(patient_unit) && !"DateUnitAdmission" %in% names(exposure_out)) {
    patient_dates <- patient_unit |>
      dplyr::select(
        dplyr::any_of(c(
          "RecordId.x", "RecordId", "UnitId", "DateUnitAdmission",
          "DateUnitDischarge"
        ))
      )
    if ("RecordId.x" %in% names(patient_dates)) {
      names(patient_dates)[names(patient_dates) == "RecordId.x"] <- "Id"
    } else if ("RecordId" %in% names(patient_dates)) {
      names(patient_dates)[names(patient_dates) == "RecordId"] <- "Id"
    }
    exposure_out <- merge(exposure_out, patient_dates, by = "Id")
    exposure_out <- exposure_out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c("DateUnitAdmission", "DateUnitDischarge")),
          hicu_parse_mixed_date
        )
      )
  }

  exposure_out <- exposure_out |>
    dplyr::filter(!is.na(.data$DateExpStart), !is.na(.data$DateExpEnd)) |>
    dplyr::mutate(
      original_exp_start = .data$DateExpStart,
      DateExpStart = pmin(.data$DateExpStart, .data$DateExpEnd),
      DateExpEnd = pmax(.data$original_exp_start, .data$DateExpEnd)
    )

  if (all(c("DateUnitAdmission", "DateUnitDischarge") %in% names(exposure_out))) {
    exposure_out <- exposure_out |>
      dplyr::filter(
        !(.data$DateExpEnd > .data$DateUnitDischarge &
          .data$DateExpStart > .data$DateUnitDischarge),
        !(.data$DateExpEnd < .data$DateUnitAdmission &
          .data$DateExpStart < .data$DateUnitAdmission),
        !is.na(.data$DateUnitAdmission),
        !is.na(.data$DateUnitDischarge)
      ) |>
      dplyr::mutate(
        DateExpStart = pmax(.data$DateExpStart, .data$DateUnitAdmission),
        DateExpEnd = pmin(.data$DateExpEnd, .data$DateUnitDischarge)
      )
  }

  if (!"expdays" %in% names(exposure_out)) {
    exposure_out$expdays <- as.numeric(exposure_out$DateExpEnd - exposure_out$DateExpStart + 1)
  } else {
    exposure_out$expdays <- as.numeric(exposure_out$expdays)
  }

  exposure_out |>
    dplyr::select(-dplyr::any_of("original_exp_start"))
}

#' Prepare exposure records for a device type
#'
#' @param exposure Patient exposure table.
#' @param exposure_type Device exposure type to keep.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Exposure records with parsed dates and positive exposure days.
#' @export
prepare_device_exposures <- function(
    exposure,
    exposure_type = "CVC",
    patient_unit = NULL
) {
  prepare_exposures(exposure, patient_unit = patient_unit) |>
    dplyr::filter(.data$ExpType == .env$exposure_type, .data$expdays > 0)
}

#' Summarise exposure days by ICU
#'
#' @param exposure Patient exposure table.
#' @param exposure_type Device exposure type to keep.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Table with one row per ICU and summed exposure days.
#' @export
summarise_unit_exposure_days <- function(
    exposure,
    exposure_type = "CVC",
    patient_unit = NULL
) {
  prepare_device_exposures(
    exposure,
    exposure_type = exposure_type,
    patient_unit = patient_unit
  ) |>
    dplyr::select("UnitId", "expdays") |>
    dplyr::group_by(.data$UnitId) |>
    dplyr::summarise(
      unitexpdays = sum(.data$expdays, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.data.frame()
}

#' Build unit and country exposure-day outputs
#'
#' @param unit_aggregates Unit-level infection aggregate with patient days.
#' @param exposure Patient exposure table.
#' @param exposure_type Device exposure type to keep.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Named list with unit and country exposure-day tables.
#' @export
build_device_exposure_outputs <- function(
    unit_aggregates,
    exposure,
    exposure_type = "CVC",
    patient_unit = NULL
) {
  unit_exposure_days <- summarise_unit_exposure_days(
    exposure,
    exposure_type = exposure_type,
    patient_unit = patient_unit
  )

  if (!"NumPatDaysUnit2d" %in% names(unit_aggregates) &&
      "lengthofstay" %in% names(unit_aggregates)) {
    unit_aggregates$NumPatDaysUnit2d <- unit_aggregates$lengthofstay
  }

  unit_exposure <- merge(
    unit_aggregates,
    unit_exposure_days,
    by.x = "RecordId",
    by.y = "UnitId"
  )

  country_exposure <- unit_exposure |>
    dplyr::select(
      "ReportingCountry",
      "unitexpdays",
      "NumPatDaysUnit2d"
    ) |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      countryexpdays = sum(.data$unitexpdays, na.rm = TRUE),
      patientdays = sum(as.numeric(.data$NumPatDaysUnit2d), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      utilrate = round(.data$countryexpdays / .data$patientdays, digits = 2)
    ) |>
    as.data.frame()

  list(
    unit_exposure = unit_exposure,
    country_exposure = country_exposure
  )
}

#' Build intubation-day outputs
#'
#' @param incidence_input Combined incidence input table.
#' @param exposure Patient exposure table.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#'
#' @return Combined incidence input with intubation days and percent intubated.
#' @export
build_intubation_exposure_output <- function(
    incidence_input,
    exposure,
    patient_unit = NULL
) {
  intubation_days <- summarise_unit_exposure_days(
    exposure,
    exposure_type = "INT",
    patient_unit = patient_unit
  )
  names(intubation_days)[names(intubation_days) == "unitexpdays"] <- "expdays"
  intubation_days$RecordId <- intubation_days$UnitId
  intubation_days$UnitId <- NULL

  out <- merge(incidence_input, intubation_days, by = "RecordId", all.x = TRUE)
  out$percintub <- round(
    as.numeric(out$expdays) / as.numeric(out$NumPatDaysUnit2d) * 100,
    digits = 2
  )

  out
}

#' Build urinary catheter exposure-day outputs
#'
#' @param unit_aggregates Unit-level infection aggregate with patient days.
#' @param exposure Patient exposure table.
#' @param patient_unit Optional patient-unit table used to add admission and
#'   discharge dates when raw exposure data do not already include them.
#' @param excluded_countries Countries excluded from urinary catheter summaries.
#' @param min_unit_exposure_days Minimum unit urinary catheter days retained.
#'
#' @return Named list with unit and country urinary catheter outputs.
#' @export
build_urinary_catheter_outputs <- function(
    unit_aggregates,
    exposure,
    patient_unit = NULL,
    excluded_countries = c("FR", "BE", "MT", "UK"),
    min_unit_exposure_days = 10
) {
  outputs <- build_device_exposure_outputs(
    unit_aggregates,
    exposure,
    exposure_type = "UC",
    patient_unit = patient_unit
  )

  if (!"UTI" %in% names(outputs$unit_exposure)) {
    outputs$unit_exposure$UTI <- 0
  }

  unit_exposure <- outputs$unit_exposure |>
    dplyr::filter(!.data$ReportingCountry %in% .env$excluded_countries) |>
    dplyr::filter(.data$unitexpdays >= .env$min_unit_exposure_days) |>
    dplyr::mutate(
      utidevadj = round(1000 * .data$UTI / .data$unitexpdays, digits = 2)
    ) |>
    as.data.frame()

  if (nrow(unit_exposure) == 0) {
    return(list(
      unit_exposure = unit_exposure,
      country_exposure = data.frame(
        ReportingCountry = character(),
        countryexpdays = numeric(),
        patientdays = numeric(),
        CAUTIn = numeric(),
        aggr = numeric(),
        meaninc = numeric(),
        cauti25pct = numeric(),
        cautimedian = numeric(),
        cauti75pct = numeric(),
        utilrate = numeric()
      )
    ))
  }

  country_exposure <- unit_exposure |>
    dplyr::select(
      "ReportingCountry",
      "unitexpdays",
      "NumPatDaysUnit2d",
      "UTI",
      "utidevadj"
    ) |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      countryexpdays = sum(as.numeric(.data$unitexpdays), na.rm = TRUE),
      patientdays = sum(as.numeric(.data$NumPatDaysUnit2d), na.rm = TRUE),
      CAUTIn = sum(.data$UTI, na.rm = TRUE),
      aggr = round(1000 * sum(.data$UTI, na.rm = TRUE) /
        sum(.data$unitexpdays, na.rm = TRUE), digits = 1),
      meaninc = mean(.data$utidevadj, na.rm = TRUE),
      cauti25pct = round(stats::quantile(.data$utidevadj, probs = 0.25, na.rm = TRUE), digits = 2),
      cautimedian = round(stats::median(.data$utidevadj, na.rm = TRUE), digits = 2),
      cauti75pct = round(stats::quantile(.data$utidevadj, probs = 0.75, na.rm = TRUE), digits = 2),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      utilrate = round(.data$countryexpdays / .data$patientdays, digits = 2)
    ) |>
    as.data.frame()

  list(
    unit_exposure = unit_exposure,
    country_exposure = country_exposure
  )
}