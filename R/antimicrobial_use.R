#' Prepare antimicrobial treatment records
#'
#' @param antimicrobial Antimicrobial treatment table.
#' @param patient Patient table with ICU admission/discharge dates.
#' @param unit_country Unit-level country table with `RecordId` and
#'   `ReportingCountry`.
#'
#' @return Antimicrobial treatment records clipped to ICU stay dates.
#' @export
prepare_antimicrobial_records <- function(antimicrobial, patient, unit_country) {
  antimicrobial_out <- antimicrobial |>
    dplyr::select(dplyr::any_of(c(
      "ParentId", "DateAntimicrobialStart", "DateAntimicrobialEnd",
      "ATCCode", "AntimicrobialIndication"
    ))) |>
    dplyr::rename(pt_id = "ParentId")

  patient_out <- patient |>
    dplyr::select(
      dplyr::any_of(c(
        "RecordId", "ParentId", "DateUnitAdmission", "DateUnitDischarge"
      ))
    ) |>
    dplyr::rename(pt_id = "RecordId", unit_id = "ParentId") |>
    dplyr::mutate(
      DateUnitAdmission = hicu_parse_mixed_date(.data$DateUnitAdmission),
      DateUnitDischarge = hicu_parse_mixed_date(.data$DateUnitDischarge)
    ) |>
    dplyr::filter(
      .data$DateUnitDischarge > .data$DateUnitAdmission + 1,
      !is.na(.data$DateUnitDischarge)
    )

  unit_country_out <- unit_country |>
    dplyr::transmute(
      RecordId = .data$RecordId,
      ReportingCountry = as.character(.data$ReportingCountry)
    ) |>
    dplyr::rename(unit_id = "RecordId")

  merge(patient_out, antimicrobial_out, by = "pt_id", all = TRUE) |>
    dplyr::mutate(
      DateAntimicrobialStart = hicu_parse_mixed_date(.data$DateAntimicrobialStart),
      DateAntimicrobialEnd = hicu_parse_mixed_date(.data$DateAntimicrobialEnd)
    ) |>
    dplyr::filter(
      !is.na(.data$DateAntimicrobialEnd),
      !is.na(.data$DateUnitDischarge),
      !is.na(.data$DateAntimicrobialStart),
      !is.na(.data$DateUnitAdmission)
    ) |>
    dplyr::mutate(
      DateAntimicrobialEnd = pmin(.data$DateAntimicrobialEnd, .data$DateUnitDischarge),
      DateAntimicrobialStart = pmax(.data$DateAntimicrobialStart, .data$DateUnitAdmission),
      treatmdays = as.numeric(.data$DateAntimicrobialEnd - .data$DateAntimicrobialStart + 1)
    ) |>
    dplyr::filter(.data$treatmdays > 0) |>
    dplyr::left_join(unit_country_out, by = "unit_id") |>
    dplyr::filter(!is.na(.data$ReportingCountry)) |>
    as.data.frame()
}

hicu_antimicrobial_unit_summary <- function(antimicrobial_records) {
  if (nrow(antimicrobial_records) == 0) {
    return(data.frame(
      unit_id = character(),
      N_ab = integer(),
      Carb = integer(),
      Carb_d = numeric(),
      piptaz = integer(),
      piptaz_d = numeric(),
      Ceph12 = integer(),
      Ceph12_d = numeric(),
      Ceph34 = integer(),
      Ceph34_d = numeric(),
      FQ = integer(),
      FQ_d = numeric(),
      Glycop = integer(),
      Glycop_d = numeric(),
      Polymyx = integer(),
      Polymyx_d = numeric(),
      treatmdays_tot = numeric(),
      treatmdays_tot_ind = numeric(),
      empiric = numeric(),
      directed = numeric(),
      prophylactic = numeric(),
      selective = numeric(),
      other = numeric(),
      ReportingCountry = character()
    ))
  }

  antimicrobial_records |>
    dplyr::group_by(.data$unit_id) |>
    dplyr::summarise(
      N_ab = dplyr::n(),
      Carb = sum(grepl("J01DH..", .data$ATCCode)),
      Carb_d = sum(.data$treatmdays[grepl("J01DH..", .data$ATCCode)]),
      piptaz = sum(grepl("J01CR05", .data$ATCCode)),
      piptaz_d = sum(.data$treatmdays[grepl("J01CR05", .data$ATCCode)]),
      Ceph12 = sum(grepl("J01DB..|J01DC..", .data$ATCCode)),
      Ceph12_d = sum(.data$treatmdays[grepl("J01DB..|J01DC..", .data$ATCCode)]),
      Ceph34 = sum(grepl("J01DD..|J01DG..", .data$ATCCode)),
      Ceph34_d = sum(.data$treatmdays[grepl("J01DD..|J01DG..", .data$ATCCode)]),
      FQ = sum(grepl("J01MA..", .data$ATCCode)),
      FQ_d = sum(.data$treatmdays[grepl("J01MA", .data$ATCCode)]),
      Glycop = sum(grepl("J01XA..", .data$ATCCode)),
      Glycop_d = sum(.data$treatmdays[grepl("J01XA", .data$ATCCode)]),
      Polymyx = sum(grepl("J01XB..", .data$ATCCode)),
      Polymyx_d = sum(.data$treatmdays[grepl("J01XB", .data$ATCCode)]),
      treatmdays_tot = sum(.data$treatmdays, na.rm = TRUE),
      treatmdays_tot_ind = sum(
        .data$treatmdays[.data$AntimicrobialIndication != "UNK"],
        na.rm = TRUE
      ),
      empiric = sum(.data$treatmdays[.data$AntimicrobialIndication == "E"], na.rm = TRUE),
      directed = sum(.data$treatmdays[.data$AntimicrobialIndication == "M"], na.rm = TRUE),
      prophylactic = sum(.data$treatmdays[.data$AntimicrobialIndication == "P"], na.rm = TRUE),
      selective = sum(.data$treatmdays[.data$AntimicrobialIndication == "S"], na.rm = TRUE),
      other = sum(.data$treatmdays[.data$AntimicrobialIndication == "O"], na.rm = TRUE),
      ReportingCountry = dplyr::first(.data$ReportingCountry),
      .groups = "drop"
    ) |>
    as.data.frame()
}

#' Summarise antimicrobial use by country
#'
#' @param antimicrobial Antimicrobial treatment table.
#' @param patient Patient table with ICU admission/discharge dates.
#' @param unit_country Unit-level country table with `RecordId` and
#'   `ReportingCountry`.
#' @param unit_patient_days Unit-level patient-day table with `UnitId` and
#'   `patdays`.
#'
#' @return Named list with `country_ab_table`, `country_ab_ind`, and `unit_ab`.
#' @export
summarise_antimicrobial_use <- function(
    antimicrobial,
    patient,
    unit_country,
    unit_patient_days
) {
  antimicrobial_records <- prepare_antimicrobial_records(
    antimicrobial,
    patient,
    unit_country
  )

  unit_ab <- hicu_antimicrobial_unit_summary(antimicrobial_records) |>
    dplyr::left_join(unit_patient_days, by = c("unit_id" = "UnitId")) |>
    dplyr::mutate(
      patdays = as.numeric(.data$patdays),
      carb_td = round(.data$Carb_d / .data$patdays * 100, digits = 2),
      piptaz_td = round(.data$piptaz_d / .data$patdays * 100, digits = 2),
      ceph34_td = round(.data$Ceph34_d / .data$patdays * 100, digits = 2),
      fq_td = round(.data$FQ_d / .data$patdays * 100, digits = 2),
      glycop_td = round(.data$Glycop_d / .data$patdays * 100, digits = 2),
      polymyx_td = round(.data$Polymyx_d / .data$patdays * 100, digits = 2)
    )

  if (nrow(unit_ab) == 0) {
    return(list(
      country_ab_table = data.frame(
        ReportingCountry = character(),
        N_ab_mean = numeric(),
        treatmdays_tot_mean = numeric(),
        carb_td_mean = numeric(),
        piptaz_td_mean = numeric(),
        ceph34_td_mean = numeric(),
        fq_td_mean = numeric(),
        glycop_td_mean = numeric(),
        polymyx_td_mean = numeric()
      ),
      country_ab_ind = data.frame(
        ReportingCountry = character(),
        emp = numeric(),
        dir = numeric(),
        proph = numeric(),
        selec = numeric(),
        other = numeric()
      ),
      unit_ab = as.data.frame(unit_ab)
    ))
  }

  country_ab_ind <- unit_ab |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      emp = sum(.data$empiric, na.rm = TRUE) /
        sum(as.numeric(.data$treatmdays_tot_ind), na.rm = TRUE),
      dir = sum(.data$directed, na.rm = TRUE) /
        sum(as.numeric(.data$treatmdays_tot_ind), na.rm = TRUE),
      proph = sum(.data$prophylactic, na.rm = TRUE) /
        sum(as.numeric(.data$treatmdays_tot_ind), na.rm = TRUE),
      selec = sum(.data$selective, na.rm = TRUE) /
        sum(as.numeric(.data$treatmdays_tot_ind), na.rm = TRUE),
      other = sum(.data$other, na.rm = TRUE) /
        sum(as.numeric(.data$treatmdays_tot_ind), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(ReportingCountry = as.character(.data$ReportingCountry)) |>
    as.data.frame()

  country_ab_table <- unit_ab |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      N_ab_mean = mean(as.numeric(.data$N_ab), na.rm = TRUE),
      treatmdays_tot_mean = mean(as.numeric(.data$treatmdays_tot), na.rm = TRUE),
      carb_td_mean = mean(as.numeric(.data$carb_td), na.rm = TRUE),
      piptaz_td_mean = mean(as.numeric(.data$piptaz_td), na.rm = TRUE),
      ceph34_td_mean = mean(as.numeric(.data$ceph34_td), na.rm = TRUE),
      fq_td_mean = mean(as.numeric(.data$fq_td), na.rm = TRUE),
      glycop_td_mean = mean(as.numeric(.data$glycop_td), na.rm = TRUE),
      polymyx_td_mean = mean(as.numeric(.data$polymyx_td), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(ReportingCountry = as.character(.data$ReportingCountry)) |>
    as.data.frame()

  list(
    country_ab_table = country_ab_table,
    country_ab_ind = country_ab_ind,
    unit_ab = as.data.frame(unit_ab)
  )
}