#' Prepare combined ICU unit records
#'
#' @param unit Standard protocol unit table.
#' @param unit_light Light protocol unit table.
#'
#' @return Combined unit table with country/network recoding and global IDs.
#' @export
prepare_haiicu_units <- function(unit, unit_light) {
  unit_standard <- unit

  unit_standard$ReportingCountry <- as.character(unit_standard$ReportingCountry)
  unit_standard$ReportingCountry[unit_standard$ReportingCountry == "IT"] <-
    unit_standard$DataSource[unit_standard$ReportingCountry == "IT"]

  combined <- dplyr::bind_rows(unit_standard, unit_light)
  combined$ReportingCountry <- as.factor(combined$ReportingCountry)
  combined <- dplyr::mutate(
    combined,
    UnitSize = dplyr::if_else(
      as.character(.data$UnitSize) %in% c("99", "86"),
      "UNK",
      as.character(.data$UnitSize)
    ),
    UnitIdGlobal = as.factor(paste(.data$ReportingCountry, .data$HospitalId, .data$UnitId)),
    HospitalIdGlobal = as.factor(paste(.data$ReportingCountry, .data$HospitalId))
  )

  combined
}

#' Prepare patient-unit records
#'
#' @param patient Patient table.
#' @param unit Standard protocol unit table.
#'
#' @return Patient table joined to standard unit records.
#' @export
prepare_patient_units <- function(patient, unit) {
  patient_out <- patient |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("DateUnitAdmission", "DateUnitDischarge")),
        hicu_parse_mixed_date
      )
    ) |>
    dplyr::filter(
      .data$DateUnitDischarge > .data$DateUnitAdmission + 1,
      !is.na(.data$DateUnitDischarge)
    ) |>
    dplyr::mutate(
      los = as.numeric(.data$DateUnitDischarge - .data$DateUnitAdmission + 1),
      UnitId = .data$ParentId
    ) |>
    dplyr::filter(.data$los < 366)

  unit_out <- unit
  unit_out$ReportingCountry <- as.character(unit_out$ReportingCountry)
  unit_out$ReportingCountry[unit_out$ReportingCountry == "IT"] <-
    unit_out$DataSource[unit_out$ReportingCountry == "IT"]
  unit_out$UnitId <- unit_out$RecordId

  merge(patient_out, unit_out, by = "UnitId")
}

#' Prepare patient-infection records
#'
#' @param patient_unit Patient-unit table from `prepare_patient_units()`.
#' @param infection Patient infection table.
#'
#' @return Patient-infection table with infection flags and length of stay.
#' @export
prepare_patient_infections <- function(patient_unit, infection) {
  infection_out <- infection
  infection_out$Id <- infection_out$ParentId
  infection_out$InfectionId <- infection_out$RecordId

  patient_unit_out <- patient_unit
  patient_unit_out$Id <- patient_unit_out$RecordId.x

  merged <- merge(patient_unit_out, infection_out, by = "Id", all = TRUE)
  if ("RecordId.x" %in% names(merged)) {
    names(merged)[names(merged) == "RecordId.x"] <- "RecordId"
  }
  names(merged) <- make.unique(names(merged), sep = "_")

  merged <- merged |>
    dplyr::filter(!is.na(.data$UnitId)) |>
    dplyr::mutate(
      InfectionId = dplyr::coalesce(.data$InfectionId, .data$RecordId),
      hasHai = !is.na(.data$InfectionSite),
      dupl_pat = duplicated(.data$Id)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("DateUnitAdmission", "DateUnitDischarge", "DateOfOnset")),
        hicu_parse_mixed_date
      ),
      lengthofstay = .data$DateUnitDischarge - .data$DateUnitAdmission + 1,
      losPN = dplyr::if_else(
        .data$ReportingCountry == "IT-GiViTI" & grepl("PN", .data$InfectionSite),
        as.numeric(.data$DateOfOnset - .data$DateUnitAdmission + 1),
        as.numeric(.data$DateUnitDischarge - .data$DateUnitAdmission + 1)
      )
    ) |>
    dplyr::filter(.data$lengthofstay < 366) |>
    unique()

  merged
}

#' Aggregate standard protocol infections by ICU
#'
#' @param patient_infections Patient-infection table.
#' @param unit Standard protocol unit table.
#'
#' @return Standard protocol unit infection aggregate.
#' @export
aggregate_standard_unit_infections <- function(patient_infections, unit) {
  unit_inc <- patient_infections |>
    dplyr::transmute(
      Id = .data$Id,
      UnitId = .data$UnitId,
      InfectionSite = .data$InfectionSite,
      lengthofstay = .data$lengthofstay,
      BSIOrigin = .data$BSIOrigin,
      BSI = grepl("BSI", .data$InfectionSite),
      PN = grepl("PN", .data$InfectionSite),
      UTI = grepl("UTI", .data$InfectionSite),
      CRI3 = grepl("CRI3", .data$InfectionSite),
      PRBSI = grepl("BSI|CRI3", .data$InfectionSite) &
        !grepl("^S-.*", .data$BSIOrigin),
      dupl = duplicated(.data$Id)
    ) |>
    dplyr::mutate(lengthofstay = dplyr::if_else(.data$dupl, NA, .data$lengthofstay)) |>
    dplyr::group_by(.data$UnitId) |>
    dplyr::summarise(
      BSI = sum(.data$BSI, na.rm = TRUE),
      PN = sum(.data$PN, na.rm = TRUE),
      UTI = sum(.data$UTI, na.rm = TRUE),
      CRI3 = sum(.data$CRI3, na.rm = TRUE),
      PRBSI = sum(.data$PRBSI, na.rm = TRUE),
      lengthofstay = sum(as.numeric(.data$lengthofstay), na.rm = TRUE),
      .groups = "drop"
    )

  merge(unit, unit_inc, by.x = "RecordId", by.y = "UnitId", all.x = TRUE)
}

#' Aggregate light protocol infections by ICU
#'
#' @param unit_light Light protocol unit table.
#' @param denominator_light Light protocol denominator table.
#' @param infection_light Light protocol infection table.
#'
#' @return Light protocol unit infection aggregate.
#' @export
aggregate_light_unit_infections <- function(
    unit_light,
    denominator_light,
    infection_light
) {
  unitlight_inc <- infection_light |>
    dplyr::select(
      dplyr::any_of(c("ParentId", "RecordId", "InfectionSite", "BSIOrigin"))
    ) |>
    dplyr::mutate(
      BSI = grepl("BSI", .data$InfectionSite),
      PN = grepl("PN", .data$InfectionSite),
      UTI = grepl("UTI", .data$InfectionSite),
      CRI3 = grepl("CRI3", .data$InfectionSite),
      PRBSI = grepl("BSI|CRI3", .data$InfectionSite) &
        !grepl("^S-.*", .data$BSIOrigin)
    ) |>
    dplyr::group_by(.data$ParentId) |>
    dplyr::summarise(
      BSI = sum(.data$BSI, na.rm = TRUE),
      PN = sum(.data$PN, na.rm = TRUE),
      UTI = sum(.data$UTI, na.rm = TRUE),
      CRI3 = sum(.data$CRI3, na.rm = TRUE),
      PRBSI = sum(.data$PRBSI, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(RecordId = "ParentId")

  unitlight_inf <- merge(denominator_light, unitlight_inc, by = "RecordId", all.x = TRUE)
  unitlight_inf$RecordId <- unitlight_inf$ParentId

  merge(unit_light, unitlight_inf, by = "RecordId", all.x = TRUE)
}

#' Build the combined incidence input table
#'
#' @param standard_unit_infections Standard protocol unit infection aggregate.
#' @param light_unit_infections Light protocol unit infection aggregate.
#'
#' @return Combined incidence input equivalent to historical `haiicuall2`.
#' @export
build_haiicuall2 <- function(standard_unit_infections, light_unit_infections) {
  base_columns <- c(
    "RecordId", "ReportingCountry", "HospitalSize", "HospitalType",
    "UnitSize", "UnitSpecialty", "UnitPercentIntub", "NumPatDaysUnit2d",
    "BSI", "PN", "UTI", "CRI3", "PRBSI"
  )

  standard <- standard_unit_infections |>
    dplyr::mutate(NumPatDaysUnit2d = .data$lengthofstay) |>
    dplyr::select(dplyr::any_of(base_columns))

  light <- light_unit_infections |>
    dplyr::mutate(NumPatDaysUnit2d = dplyr::coalesce(.data$NumPatDaysUnit2d, .data$NumPatDaysUnit)) |>
    dplyr::select(dplyr::any_of(base_columns))

  out <- dplyr::bind_rows(light, standard) |>
    dplyr::mutate(
      RecordId = as.factor(.data$RecordId),
      UTI_incdens = round(.data$UTI / .data$NumPatDaysUnit2d * 1000, digits = 2),
      BSI_incdens = round((.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d * 1000, digits = 2),
      PN_incdens = round(.data$PN / .data$NumPatDaysUnit2d * 1000, digits = 2)
    )

  out
}

#' Build the core functional HAIICU workflow slice
#'
#' @param inputs Named list returned by `load_haiicu_inputs()`.
#'
#' @return Named list of core prepared outputs and incidence tables.
#' @export
build_core_workflow_outputs <- function(inputs) {
  unit <- inputs$unit
  unit_light <- inputs$unit_light
  patient <- inputs$patient
  infection <- inputs$infection
  denominator <- inputs$denominator
  indicator <- inputs$indicator
  indicator_light <- inputs$indicator_light
  antimicrobial <- inputs$antimicrobial
  denominator_light <- inputs$denominator_light
  infection_light <- inputs$infection_light

  haiicu_level1_all <- prepare_haiicu_units(unit, unit_light)
  haiicu_pt_unit <- prepare_patient_units(patient, unit)
  country_demogr <- summarise_country_demographics(haiicu_pt_unit)
  haiicu_pt_inf_all <- prepare_patient_infections(haiicu_pt_unit, infection)
  haiicu_level1_inf <- aggregate_standard_unit_infections(haiicu_pt_inf_all, unit)
  haiicu_unitlight_all <- aggregate_light_unit_infections(
    unit_light,
    denominator_light,
    infection_light
  )
  haiicuall2 <- build_haiicuall2(haiicu_level1_inf, haiicu_unitlight_all)
  incidence_tables <- build_incidence_tables(haiicuall2)
  unit_patient_days <- haiicu_level1_inf |>
    dplyr::transmute(UnitId = .data$RecordId, patdays = .data$lengthofstay)
  antimicrobial_outputs <- NULL
  if (!is.null(antimicrobial)) {
    antimicrobial_outputs <- summarise_antimicrobial_use(
      antimicrobial,
      patient,
      haiicu_level1_all,
      unit_patient_days
    )
  }
  indicator_outputs <- list()
  if (!is.null(denominator)) {
    indicator_outputs$country_deno_7d <- summarise_country_deno_7d(
      denominator,
      denominator_light,
      haiicu_level1_all
    )
  }
  if (!is.null(indicator) && !is.null(indicator_light) && !is.null(denominator)) {
    indicator_outputs$country_ind <- summarise_country_process_indicators(
      indicator,
      indicator_light,
      denominator,
      denominator_light,
      haiicu_level1_all
    )
  }
  cvc_exposure <- build_device_exposure_outputs(
    haiicu_level1_inf,
    inputs$exposure,
    patient_unit = haiicu_pt_unit
  )
  intubation_exposure <- build_intubation_exposure_output(
    haiicuall2,
    inputs$exposure,
    patient_unit = haiicu_pt_unit
  )
  urinary_catheter_exposure <- build_urinary_catheter_outputs(
    haiicuall2,
    inputs$exposure,
    patient_unit = haiicu_pt_unit
  )
  prbsi_outputs <- build_prbsi_table(
    cvc_exposure$unit_exposure,
    haiicu_pt_unit,
    intubation_exposure = intubation_exposure,
    min_intubation_days = 20
  )
  cvc_associated_bsi <- classify_cvc_associated_bsi(
    haiicu_pt_inf_all,
    inputs$exposure,
    patient_unit = haiicu_pt_unit
  )
  device_bsi_outputs <- build_device_bsi_tables(
    cvc_exposure$unit_exposure,
    haiicu_pt_inf_all,
    haiicu_pt_unit,
    cvc_associated_counts = cvc_associated_bsi$unit_counts
  )

  c(
    list(
      haiicu_level1_all = haiicu_level1_all,
      haiicu_pt_unit = haiicu_pt_unit,
      country_demogr = country_demogr,
      haiicu_pt_inf_all = haiicu_pt_inf_all,
      haiicu_level1_inf = haiicu_level1_inf,
      haiicu_unitlight_all = haiicu_unitlight_all,
      haiicu_unit_expcvc = cvc_exposure$unit_exposure,
      haiicu_country_expcvc = cvc_exposure$country_exposure,
      haiicuall_percintub = intubation_exposure,
      haiicu_unit_expuc = urinary_catheter_exposure$unit_exposure,
      haiicu_country_expuc = urinary_catheter_exposure$country_exposure,
      haiicuall2 = haiicuall2
    ),
    antimicrobial_outputs[c("country_ab_table", "country_ab_ind")],
    indicator_outputs[c("country_deno_7d", "country_ind")],
    prbsi_outputs[c("prbsi_table")],
    device_bsi_outputs[c(
      "haiicu_unit_bsidevadj", "bsidevadj_bycountry",
      "bsidevadj_totcritable_bycountry", "haiicu_unit_bsidevadj_totcritable",
      "bsidevadj_cri3table_bycountry", "bsidevadj_cvcasbsitable_bycountry",
      "eu_cvcasbsi", "clabsi_bycountry", "haiicu_unit_bsidevadj_cvcasbsitable",
      "haiicu_unit_bsidevadj_clabsitable", "CRBSItable"
    )],
    incidence_tables
  )
}