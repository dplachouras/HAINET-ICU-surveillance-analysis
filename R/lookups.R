#' Country/network display labels
#'
#' @return Named character vector mapping surveillance country/network codes to
#'   display labels.
#' @export
hicu_country_labels <- function() {
  c(
    AT = "Austria", BE = "Belgium", CZ = "Czech Republic", DE = "Germany",
    EE = "Estonia", ES = "Spain", FR = "France", HU = "Hungary",
    IT = "Italy", LT = "Lithuania", LU = "Luxembourg", MT = "Malta",
    PL = "Poland", PT = "Portugal", RO = "Romania", SK = "Slovakia",
    UK = "United Kingdom",
    "IT-SPIN-UTI" = "Italy-SPIN-UTI",
    "IT-GiViTI" = "Italy-GiViTI",
    ITSPINUTI = "Italy-SPIN-UTI",
    ITGiViTI = "Italy-GiViTI"
  )
}

#' Report table display labels
#'
#' @return Named character vector mapping report column names to labels.
#' @export
hicu_report_var_labels <- function() {
  c(
    ReportingCountry = "Country/Network",
    n_BSI = "BSI episodes (n)",
    n_PN = "Pneumonia episodes (n)",
    n_IAP = "IAP episodes (n)",
    n_NumPtDays = "Patient-days (n)",
    n_cvcdays = "Catheter-days (n)",
    n_expdays = "Intubation-days (n)",
    cvcuse = "CVC use (catheter-days per 100 patient-days)",
    intubuse = "Intubation use (intubation-days per 100 patient-days)",
    n_clabsi = "CLABSI episodes (n)",
    cvcdays = "CVC-days (n)",
    n_totcrbsi = "Total CRBSI episodes (n)",
    n_CRI3 = "CRI3 episodes (n)",
    nricu = "ICUs (n)",
    nrpat = "Patients (n)",
    avglos = "Average length of stay (days)",
    avgcrbsirate = "Mean",
    crbsirate25pct = "25th percentile",
    crbsiratemedian = "Median",
    crbisrate75pct = "75th percentile",
    countrclabsiinc = "Mean CLABSI incidence",
    NumPatDaysUnit2d = "Patient-days",
    aggrinc = "Aggregated",
    aggr_inc = "Aggregated",
    avgclabsirate = "Mean",
    avgiaprate = "Mean",
    clabsirate25pct = "25th percentile",
    iaprate25pct = "25th percentile",
    clabsiratemedian = "Median",
    iapratemedian = "Median",
    clabsirate75pct = "75th percentile",
    iaprate75pct = "75th percentile",
    BSIinc = "Aggregated",
    PNinc = "Aggregated",
    UTIinc = "Aggregated",
    meanBSIinc = "Mean",
    meanPNinc = "Mean",
    meanUTIinc = "Mean",
    pct25 = "25th percentile",
    median = "Median",
    pct75 = "75th percentile",
    N = "ICUs (n)",
    unit_size_median = "ICU size (median no. beds)",
    Spec_med = "Medical",
    Spec_sur = "Surgical",
    Spec_mix = "Mixed",
    Spec_coro = "Coronary",
    Spec_ounk = "Other/unknown",
    N_pat = "Patients (n)",
    patdays = "Patient-days (n)",
    avg_los = "Average length of stay (days)",
    Gender_F_pc = "Females (%)",
    Age_median = "Median age (years)",
    SapsII_median = "SAPS II score median",
    Origin_HOSP_pc = "Patient from hospital (%)",
    Trauma_pc = "Trauma (%)",
    TypeAdm_med = "Medical",
    TypeAdm_ssur = "Scheduled surgery",
    TypeAdm_usur = "Urgent surgery",
    Intub_pc = "Intubation (%)",
    UrinCath_pc = "Urinary catheter (%)",
    CVC_pc = "Central vascular catheter (%)",
    ImpImmun_pc = "Impaired immunity (%)",
    Outcome_D_pc = "Mortality (%)",
    MRSA = "S. aureus, meticillin resistance (MRSA, %)",
    VRE = "Enterococcus spp., vancomycin resistance (%)",
    CEFRPS = "P. aeruginosa, ceftazidime resistance (%)",
    C3GREC = "E. coli, third-generation cephalosporin resistance (%)",
    C3GRKP = "Klebsiella spp., third-generation cephalosporin resistance (%)",
    C3GRENT = "Enterobacter spp., third-generation cephalosporin resistance (%)",
    CRKP = "Klebsiella spp., carbapenem resistance (%)",
    CREC = "E. coli, carbapenem resistance (%)",
    CRENT = "Enterobacter spp., carbapenem resistance (%)",
    CRPS = "P. aeruginosa, carbapenem resistance (%)",
    CRAB = "Acinetobacter baumannii, carbapenem resistance (%)"
  )
}

#' Label country/network values
#'
#' @param data Data frame containing a country/network column.
#' @param country_col Name of the country/network column.
#' @param labels Named character vector of labels.
#'
#' @return `data` with labelled country/network values where labels are known.
#' @export
hicu_label_country_values <- function(
    data,
    country_col = "ReportingCountry",
    labels = hicu_country_labels()
) {
  if (!country_col %in% names(data)) {
    return(data)
  }

  dplyr::mutate(
    data,
    !!country_col := dplyr::recode(.data[[country_col]], !!!labels)
  )
}

#' Label country/network columns
#'
#' @param data Data frame with country/network codes as column names.
#' @param labels Named character vector of labels.
#'
#' @return `data` with country/network columns renamed where labels are known.
#' @export
hicu_label_country_columns <- function(data, labels = hicu_country_labels()) {
  dplyr::rename_with(
    data,
    ~ ifelse(.x %in% names(labels), unname(labels[.x]), .x)
  )
}

#' Sort table rows with EU/EEA last
#'
#' @param data Data frame containing a country/network column.
#' @param country_col Name of the country/network column.
#' @param eu_label Label used for the EU/EEA row.
#'
#' @return Data frame sorted by country/network with EU/EEA last.
#' @export
hicu_sort_with_eu_last <- function(
    data,
    country_col = "ReportingCountry",
    eu_label = "EU/EEA"
) {
  if (!country_col %in% names(data)) {
    return(data)
  }

  data |>
    dplyr::arrange(
      .data[[country_col]] == eu_label,
      .data[[country_col]]
    )
}