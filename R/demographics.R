#' Summarise patient demographics by country
#'
#' @param patient_unit Patient-unit table from `prepare_patient_units()`.
#'
#' @return Country-level demographic summary equivalent to `country_demogr`.
#' @export
summarise_country_demographics <- function(patient_unit) {
  patient_unit |>
    dplyr::mutate(
      Age = as.numeric(.data$Age),
      SapsII = as.numeric(.data$SapsII),
      los = as.numeric(.data$los)
    ) |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      N_pat = dplyr::n(),
      patdays = sum(.data$los, na.rm = TRUE),
      avg_los = round(sum(.data$los, na.rm = TRUE) / dplyr::n(), digits = 1),
      Gender_F_pc = hicu_pct(.data$Gender == "F", dplyr::n()),
      Age_median = round(stats::median(.data$Age, na.rm = TRUE), digits = 1),
      SapsII_median = round(stats::median(.data$SapsII, na.rm = TRUE), digits = 1),
      Origin_HOSP_pc = hicu_pct(.data$PatientOrigin == "HOSP", dplyr::n()),
      Trauma_pc = hicu_pct(.data$Trauma == "Y", dplyr::n()),
      TypeAdm_med = hicu_pct(.data$TypeOfAdmission == "MED", dplyr::n()),
      TypeAdm_ssur = hicu_pct(.data$TypeOfAdmission == "SSUR", dplyr::n()),
      TypeAdm_usur = hicu_pct(.data$TypeOfAdmission == "USUR", dplyr::n()),
      Intub_pc = hicu_pct(.data$Intubation == "Y", dplyr::n()),
      UrinCath_pc = hicu_pct(.data$UrinaryCatheter == "Y", dplyr::n()),
      CVC_pc = hicu_pct(.data$CVC == "Y", dplyr::n()),
      ImpImmun_pc = hicu_prc(sum(.data$ImpairedImmunity == "Y", na.rm = TRUE) / dplyr::n()),
      AntimicrUnit_pc = hicu_pct(.data$AntimicrobialInUnit == "Y", dplyr::n()),
      Outcome_D_pc = hicu_prc(sum(.data$OutcomeUnit == "D", na.rm = TRUE) / dplyr::n()),
      .groups = "drop"
    ) |>
    as.data.frame()
}