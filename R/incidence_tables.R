#' Build annual incidence tables from ICU-level aggregated data
#'
#' @param haiicuall2 Data frame equivalent to the historical `haiicuall2`
#'   object, including `ReportingCountry`, `PN`, `BSI`, `CRI3`, `UTI`, and
#'   `NumPatDaysUnit2d`.
#' @param eu_exclusions Named list of country exclusions used in EU/EEA rows.
#'   Defaults follow current production logic.
#'
#' @return Named list with country and EU incidence tables:
#'   `PNinc_country`, `BSIinc_country`, `UTIinc_country`, `PNinc_EU`,
#'   `BSIinc_EU`, `UTIinc_EU`.
#' @export
build_incidence_tables <- function(
    haiicuall2,
    eu_exclusions = list(
      PN = c("DE"),
      BSI = c("DE"),
      UTI = c("DE", "FR")
    )
) {
  required_cols <- c("ReportingCountry", "PN", "BSI", "CRI3", "UTI", "NumPatDaysUnit2d")
  missing_cols <- setdiff(required_cols, names(haiicuall2))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  hicu_quantile <- function(x, probs) {
    unname(stats::quantile(x, probs = probs, na.rm = TRUE))
  }

  h <- dplyr::mutate(
    haiicuall2,
    ReportingCountry = as.character(.data$ReportingCountry),
    NumPatDaysUnit2d = as.numeric(.data$NumPatDaysUnit2d)
  )

  pn_country <- h |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      n_PN = sum(.data$PN, na.rm = TRUE),
      n_NumPtDays = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      PNinc = round(1000 * sum(.data$PN, na.rm = TRUE) / sum(.data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      meanPNinc = round(mean(1000 * .data$PN / .data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      pct25 = round(hicu_quantile(1000 * .data$PN / .data$NumPatDaysUnit2d, 0.25), digits = 2),
      median = round(hicu_quantile(1000 * .data$PN / .data$NumPatDaysUnit2d, 0.5), digits = 2),
      pct75 = round(hicu_quantile(1000 * .data$PN / .data$NumPatDaysUnit2d, 0.75), digits = 2),
      .groups = "drop"
    )

  bsi_country <- h |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      n_BSI = sum(.data$BSI + .data$CRI3, na.rm = TRUE),
      n_NumPtDays = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      BSIinc = round(1000 * (sum(.data$BSI, na.rm = TRUE) + sum(.data$CRI3, na.rm = TRUE)) / sum(.data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      meanBSIinc = round(mean(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      pct25 = round(hicu_quantile(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, 0.25), digits = 2),
      median = round(hicu_quantile(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, 0.5), digits = 2),
      pct75 = round(hicu_quantile(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, 0.75), digits = 2),
      .groups = "drop"
    )

  uti_country <- h |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      n_UTI = sum(.data$UTI, na.rm = TRUE),
      n_NumPtDays = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      UTIinc = round(1000 * sum(.data$UTI, na.rm = TRUE) / sum(.data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      meanUTIinc = round(mean(1000 * .data$UTI / .data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      pct25 = round(hicu_quantile(1000 * .data$UTI / .data$NumPatDaysUnit2d, 0.25), digits = 2),
      median = round(hicu_quantile(1000 * .data$UTI / .data$NumPatDaysUnit2d, 0.5), digits = 2),
      pct75 = round(hicu_quantile(1000 * .data$UTI / .data$NumPatDaysUnit2d, 0.75), digits = 2),
      .groups = "drop"
    )

  pn_eu <- h |>
    dplyr::filter(!.data$ReportingCountry %in% eu_exclusions$PN) |>
    dplyr::summarise(
      n_PN = sum(.data$PN, na.rm = TRUE),
      n_NumPtDays = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      PNinc = round(1000 * sum(.data$PN, na.rm = TRUE) / sum(.data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      meanPNinc = round(mean(1000 * .data$PN / .data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      pct25 = round(hicu_quantile(1000 * .data$PN / .data$NumPatDaysUnit2d, 0.25), digits = 2),
      median = round(hicu_quantile(1000 * .data$PN / .data$NumPatDaysUnit2d, 0.5), digits = 2),
      pct75 = round(hicu_quantile(1000 * .data$PN / .data$NumPatDaysUnit2d, 0.75), digits = 2)
    )

  bsi_eu <- h |>
    dplyr::filter(!.data$ReportingCountry %in% eu_exclusions$BSI) |>
    dplyr::summarise(
      n_BSI = sum(.data$BSI + .data$CRI3, na.rm = TRUE),
      n_NumPtDays = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      BSIinc = round(1000 * (sum(.data$BSI, na.rm = TRUE) + sum(.data$CRI3, na.rm = TRUE)) / sum(.data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      meanBSIinc = round(mean(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      pct25 = round(hicu_quantile(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, 0.25), digits = 2),
      median = round(hicu_quantile(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, 0.5), digits = 2),
      pct75 = round(hicu_quantile(1000 * (.data$BSI + .data$CRI3) / .data$NumPatDaysUnit2d, 0.75), digits = 2)
    )

  uti_eu <- h |>
    dplyr::filter(!.data$ReportingCountry %in% eu_exclusions$UTI) |>
    dplyr::summarise(
      n_UTI = sum(.data$UTI, na.rm = TRUE),
      n_NumPtDays = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      UTIinc = round(1000 * sum(.data$UTI, na.rm = TRUE) / sum(.data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      meanUTIinc = round(mean(1000 * .data$UTI / .data$NumPatDaysUnit2d, na.rm = TRUE), digits = 2),
      pct25 = round(hicu_quantile(1000 * .data$UTI / .data$NumPatDaysUnit2d, 0.25), digits = 2),
      median = round(hicu_quantile(1000 * .data$UTI / .data$NumPatDaysUnit2d, 0.5), digits = 2),
      pct75 = round(hicu_quantile(1000 * .data$UTI / .data$NumPatDaysUnit2d, 0.75), digits = 2)
    )

  list(
    PNinc_country = pn_country,
    BSIinc_country = bsi_country,
    UTIinc_country = uti_country,
    PNinc_EU = pn_eu,
    BSIinc_EU = bsi_eu,
    UTIinc_EU = uti_eu
  )
}

#' Build primary BSI incidence table by country
#'
#' @param unit_cvc_exposure Unit-level CVC exposure table with `PRBSI`,
#'   `unitexpdays`, and patient days.
#' @param patient_unit Patient-unit table used to count unit admissions.
#' @param intubation_exposure Optional unit intubation exposure table. When
#'   provided with `min_intubation_days`, units are restricted to those meeting
#'   the legacy IAP-density eligibility rule.
#' @param min_unit_admissions Minimum number of unit admissions retained.
#'   Defaults to 10, equivalent to the legacy `adm > 9` filter.
#' @param min_intubation_days Optional minimum intubation days retained.
#'
#' @return Named list with `prbsi_table` and `unit_prbsi_table`.
#' @export
build_prbsi_table <- function(
    unit_cvc_exposure,
    patient_unit,
    intubation_exposure = NULL,
    min_unit_admissions = 10,
    min_intubation_days = NULL
) {
  required_cols <- c(
    "RecordId", "ReportingCountry", "unitexpdays", "PRBSI",
    "NumPatDaysUnit2d"
  )
  missing_cols <- setdiff(required_cols, names(unit_cvc_exposure))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  unit_admissions <- patient_unit |>
    dplyr::count(.data$UnitId, name = "adm")

  unit_prbsi_table <- unit_cvc_exposure |>
    dplyr::mutate(
      RecordId = as.character(.data$RecordId),
      PRBSI = as.numeric(.data$PRBSI),
      NumPatDaysUnit2d = as.numeric(.data$NumPatDaysUnit2d),
      unitexpdays = as.numeric(.data$unitexpdays)
    ) |>
    dplyr::left_join(unit_admissions, by = c("RecordId" = "UnitId")) |>
    dplyr::filter(.data$adm >= .env$min_unit_admissions)

  if (!is.null(intubation_exposure) && !is.null(min_intubation_days)) {
    eligible_units <- intubation_exposure |>
      dplyr::mutate(
        RecordId = as.character(.data$RecordId),
        expdays = as.numeric(.data$expdays)
      ) |>
      dplyr::filter(.data$expdays >= .env$min_intubation_days) |>
      dplyr::pull("RecordId")

    unit_prbsi_table <- unit_prbsi_table |>
      dplyr::filter(.data$RecordId %in% .env$eligible_units)
  }

  unit_prbsi_table <- unit_prbsi_table |>
    dplyr::mutate(
      prbsiinc = round(.data$PRBSI / .data$NumPatDaysUnit2d * 1000, digits = 2)
    ) |>
    as.data.frame()

  if (nrow(unit_prbsi_table) == 0) {
    return(list(
      prbsi_table = data.frame(
        ReportingCountry = character(),
        cvcdays = numeric(),
        cvcuse = numeric(),
        pt_days = numeric(),
        n_prbsi = numeric(),
        aggrinc = numeric(),
        avgprbsirate = numeric(),
        prbsirate25pct = numeric(),
        prbsiratemedian = numeric(),
        prbisrate75pct = numeric()
      ),
      unit_prbsi_table = unit_prbsi_table
    ))
  }

  prbsi_table <- unit_prbsi_table |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      cvcdays = sum(.data$unitexpdays, na.rm = TRUE),
      cvcuse = round(
        1000 * mean(.data$unitexpdays, na.rm = TRUE) /
          mean(.data$NumPatDaysUnit2d, na.rm = TRUE),
        digits = 0
      ),
      pt_days = sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
      n_prbsi = sum(.data$PRBSI, na.rm = TRUE),
      aggrinc = round(
        1000 * sum(.data$PRBSI, na.rm = TRUE) /
          sum(.data$NumPatDaysUnit2d, na.rm = TRUE),
        digits = 2
      ),
      avgprbsirate = round(mean(.data$prbsiinc, na.rm = TRUE), digits = 2),
      prbsirate25pct = round(
        stats::quantile(.data$prbsiinc, probs = 0.25, na.rm = TRUE),
        digits = 2
      ),
      prbsiratemedian = round(stats::median(.data$prbsiinc, na.rm = TRUE), digits = 2),
      prbisrate75pct = round(
        stats::quantile(.data$prbsiinc, probs = 0.75, na.rm = TRUE),
        digits = 2
      ),
      .groups = "drop"
    ) |>
    as.data.frame()

  list(
    prbsi_table = prbsi_table,
    unit_prbsi_table = unit_prbsi_table
  )
}

hicu_unit_patient_stats <- function(patient_unit) {
  stay_column <- intersect(c("los", "lengthofstay"), names(patient_unit))[1]
  if (is.na(stay_column)) {
    stop("Missing required columns: los or lengthofstay", call. = FALSE)
  }

  patient_unit |>
    dplyr::mutate(
      los = as.numeric(.data[[stay_column]])
    ) |>
    dplyr::group_by(.data$UnitId) |>
    dplyr::summarise(
      adm = dplyr::n(),
      lengthofstay = sum(.data$los, na.rm = TRUE),
      avglos = round(.data$lengthofstay / .data$adm, digits = 2),
      .groups = "drop"
    )
}

hicu_bsi_origin_counts <- function(patient_infections) {
  patient_infections |>
    dplyr::filter(grepl("CRI3|BSI", .data$InfectionSite)) |>
    dplyr::group_by(.data$UnitId) |>
    dplyr::summarise(
      crbsin = sum(grepl("^C.*", .data$BSIOrigin), na.rm = TRUE),
      prbsi = sum(
        grepl("BSI|CRI3", .data$InfectionSite) &
          !grepl("^S-.*", .data$BSIOrigin),
        na.rm = TRUE
      ),
      crin = sum(.data$InfectionSite != "BSI", na.rm = TRUE),
      cvcbsi = sum(
        .data$InfectionSite == "CRI3-CVC" |
          .data$BSIOrigin == "C-CVC" |
          .data$BSIOrigin == "C",
        na.rm = TRUE
      ),
      cri3 = sum(.data$InfectionSite == "CRI3-CVC", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(totcrbsi = .data$crbsin + .data$crin)
}

hicu_empty_device_bsi_outputs <- function(unit_table) {
  list(
    haiicu_unit_bsidevadj = unit_table,
    bsidevadj_bycountry = data.frame(
      ReportingCountry = character(),
      countrbsidai = numeric()
    ),
    bsidevadj_totcritable_bycountry = data.frame(
      ReportingCountry = character(),
      n_cvcdays = numeric(),
      cvcuse = numeric(),
      n_totcrbsi = numeric(),
      aggrinc = numeric(),
      avgcrbsirate = numeric(),
      crbsirate25pct = numeric(),
      crbsiratemedian = numeric(),
      crbisrate75pct = numeric()
    ),
    haiicu_unit_bsidevadj_totcritable = unit_table,
    bsidevadj_cri3table_bycountry = data.frame(
      ReportingCountry = character(),
      cvcdays = numeric(),
      cvcuse = numeric(),
      n_CRI3 = numeric(),
      aggrinc = numeric(),
      avgcrbsirate = numeric(),
      crbsirate25pct = numeric(),
      crbsiratemedian = numeric(),
      crbisrate75pct = numeric()
    ),
    bsidevadj_cvcasbsitable_bycountry = data.frame(
      ReportingCountry = character(),
      n_cvcdays = numeric(),
      cvcuse = numeric(),
      n_clabsi = numeric(),
      aggrinc = numeric(),
      avgclabsirate = numeric(),
      clabsirate25pct = numeric(),
      clabsiratemedian = numeric(),
      clabsirate75pct = numeric()
    ),
    eu_cvcasbsi = data.frame(
      n_cvcdays = numeric(),
      cvcuse = numeric(),
      n_clabsi = numeric(),
      aggrinc = numeric(),
      avgclabsirate = numeric(),
      clabsirate25pct = numeric(),
      clabsiratemedian = numeric(),
      clabsirate75pct = numeric()
    ),
    clabsi_bycountry = data.frame(
      ReportingCountry = character(),
      countrclabsiinc = numeric()
    ),
    haiicu_unit_bsidevadj_cvcasbsitable = unit_table,
    haiicu_unit_bsidevadj_clabsitable = unit_table,
    CRBSItable = data.frame(
      ReportingCountry = character(),
      nricu = integer(),
      nrpat = numeric(),
      avglos = numeric(),
      cvcuse = numeric(),
      avgcrbsirate = numeric(),
      crbsirate25pct = numeric(),
      crbsiratemedian = numeric(),
      crbisrate75pct = numeric()
    )
  )
}

#' Build device-adjusted BSI tables
#'
#' @param unit_cvc_exposure Unit-level CVC exposure table.
#' @param patient_infections Patient-infection table.
#' @param patient_unit Patient-unit table used for admissions and stay length.
#' @param cvc_associated_counts Optional table with `UnitId` and `CVCASBSI`.
#' @param min_unit_admissions Minimum number of unit admissions retained.
#'
#' @return Named list of CRBSI, CRI3, and CLABSI unit/country tables.
#' @export
build_device_bsi_tables <- function(
    unit_cvc_exposure,
    patient_infections,
    patient_unit,
    cvc_associated_counts = NULL,
    min_unit_admissions = 10
) {
  required_cols <- c("RecordId", "ReportingCountry", "unitexpdays", "NumPatDaysUnit2d")
  missing_cols <- setdiff(required_cols, names(unit_cvc_exposure))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!"CRI3" %in% names(unit_cvc_exposure)) {
    unit_cvc_exposure$CRI3 <- NA_real_
  }

  unit_stats <- hicu_unit_patient_stats(patient_unit)
  bsi_counts <- hicu_bsi_origin_counts(patient_infections)

  unit_table <- unit_cvc_exposure |>
    dplyr::mutate(
      RecordId = as.character(.data$RecordId),
      NumPatDaysUnit2d = as.numeric(.data$NumPatDaysUnit2d),
      unitexpdays = as.numeric(.data$unitexpdays)
    ) |>
    dplyr::left_join(bsi_counts, by = c("RecordId" = "UnitId")) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("totcrbsi", "cvcbsi", "cri3", "prbsi")),
        ~ tidyr::replace_na(as.numeric(.x), 0)
      ),
      CRI3 = dplyr::coalesce(
        as.numeric(.data[["CRI3"]]),
        as.numeric(.data$cri3)
      ),
      devadjinc = round(.data$cvcbsi / .data$unitexpdays * 1000, digits = 2),
      devadjcri3 = round(.data$cri3 / .data$unitexpdays * 1000, digits = 2),
      prbsiinc = round(.data$prbsi / .data$NumPatDaysUnit2d * 1000, digits = 2)
    ) |>
    as.data.frame()

  unit_filtered <- unit_table |>
    dplyr::left_join(unit_stats, by = c("RecordId" = "UnitId")) |>
    dplyr::filter(.data$adm >= .env$min_unit_admissions) |>
    as.data.frame()

  if (nrow(unit_filtered) == 0) {
    return(hicu_empty_device_bsi_outputs(unit_filtered))
  }

  if (is.null(cvc_associated_counts)) {
    cvc_associated_counts <- data.frame(UnitId = character(), CVCASBSI = numeric())
  }

  unit_clabsi <- unit_table |>
    dplyr::left_join(
      cvc_associated_counts,
      by = c("RecordId" = "UnitId")
    ) |>
    dplyr::left_join(
      unit_stats |>
        dplyr::select("UnitId", "lengthofstay", "adm", "avglos"),
      by = c("RecordId" = "UnitId")
    ) |>
    dplyr::mutate(
      CVCASBSI = tidyr::replace_na(as.numeric(.data$CVCASBSI), 0),
      clabsiinc = round(.data$CVCASBSI / .data$unitexpdays * 1000, digits = 2)
    ) |>
    dplyr::filter(.data$adm >= .env$min_unit_admissions) |>
    as.data.frame()

  country_totcrbsi <- unit_filtered |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      n_cvcdays = sum(.data$unitexpdays, na.rm = TRUE),
      cvcuse = round(
        1000 * mean(.data$unitexpdays, na.rm = TRUE) /
          mean(.data$NumPatDaysUnit2d, na.rm = TRUE),
        digits = 0
      ),
      n_totcrbsi = sum(.data$totcrbsi, na.rm = TRUE),
      aggrinc = round(
        1000 * sum(.data$totcrbsi, na.rm = TRUE) /
          sum(.data$unitexpdays, na.rm = TRUE),
        digits = 2
      ),
      avgcrbsirate = round(mean(.data$devadjinc, na.rm = TRUE), digits = 2),
      crbsirate25pct = round(
        stats::quantile(.data$devadjinc, probs = 0.25, na.rm = TRUE),
        digits = 2
      ),
      crbsiratemedian = round(stats::median(.data$devadjinc, na.rm = TRUE), digits = 2),
      crbisrate75pct = round(
        stats::quantile(.data$devadjinc, probs = 0.75, na.rm = TRUE),
        digits = 2
      ),
      .groups = "drop"
    ) |>
    as.data.frame()

  country_cri3 <- unit_filtered |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      cvcdays = sum(.data$unitexpdays, na.rm = TRUE),
      cvcuse = round(
        1000 * mean(.data$unitexpdays, na.rm = TRUE) /
          mean(.data$NumPatDaysUnit2d, na.rm = TRUE),
        digits = 0
      ),
      n_CRI3 = sum(.data$CRI3, na.rm = TRUE),
      aggrinc = round(
        1000 * sum(.data$CRI3, na.rm = TRUE) /
          sum(.data$unitexpdays, na.rm = TRUE),
        digits = 2
      ),
      avgcrbsirate = round(mean(.data$devadjcri3, na.rm = TRUE), digits = 2),
      crbsirate25pct = round(
        stats::quantile(.data$devadjcri3, probs = 0.25, na.rm = TRUE),
        digits = 2
      ),
      crbsiratemedian = round(stats::median(.data$devadjcri3, na.rm = TRUE), digits = 2),
      crbisrate75pct = round(
        stats::quantile(.data$devadjcri3, probs = 0.75, na.rm = TRUE),
        digits = 2
      ),
      .groups = "drop"
    ) |>
    as.data.frame()

  country_clabsi <- unit_clabsi |>
    dplyr::group_by(.data$ReportingCountry) |>
    dplyr::summarise(
      n_cvcdays = sum(.data$unitexpdays, na.rm = TRUE),
      cvcuse = round(
        1000 * mean(.data$unitexpdays, na.rm = TRUE) /
          mean(as.numeric(.data$lengthofstay), na.rm = TRUE),
        digits = 0
      ),
      n_clabsi = sum(.data$CVCASBSI, na.rm = TRUE),
      aggrinc = round(
        1000 * sum(.data$CVCASBSI, na.rm = TRUE) /
          sum(.data$unitexpdays, na.rm = TRUE),
        digits = 2
      ),
      avgclabsirate = round(mean(.data$clabsiinc, na.rm = TRUE), digits = 2),
      clabsirate25pct = round(
        stats::quantile(.data$clabsiinc, probs = 0.25, na.rm = TRUE),
        digits = 2
      ),
      clabsiratemedian = round(stats::median(.data$clabsiinc, na.rm = TRUE), digits = 2),
      clabsirate75pct = round(
        stats::quantile(.data$clabsiinc, probs = 0.75, na.rm = TRUE),
        digits = 2
      ),
      .groups = "drop"
    ) |>
    as.data.frame()

  eu_cvcasbsi <- unit_clabsi |>
    dplyr::summarise(
      n_cvcdays = sum(.data$unitexpdays, na.rm = TRUE),
      cvcuse = round(
        1000 * mean(.data$unitexpdays, na.rm = TRUE) /
          mean(as.numeric(.data$lengthofstay), na.rm = TRUE),
        digits = 0
      ),
      n_clabsi = sum(.data$CVCASBSI, na.rm = TRUE),
      aggrinc = round(
        1000 * sum(.data$CVCASBSI, na.rm = TRUE) /
          sum(.data$unitexpdays, na.rm = TRUE),
        digits = 2
      ),
      avgclabsirate = round(mean(.data$clabsiinc, na.rm = TRUE), digits = 2),
      clabsirate25pct = round(
        stats::quantile(.data$clabsiinc, probs = 0.25, na.rm = TRUE),
        digits = 2
      ),
      clabsiratemedian = round(stats::median(.data$clabsiinc, na.rm = TRUE), digits = 2),
      clabsirate75pct = round(
        stats::quantile(.data$clabsiinc, probs = 0.75, na.rm = TRUE),
        digits = 2
      )
    ) |>
    as.data.frame()

  list(
    haiicu_unit_bsidevadj = unit_table,
    bsidevadj_bycountry = unit_filtered |>
      dplyr::group_by(.data$ReportingCountry) |>
      dplyr::summarise(
        countrbsidai = round(mean(.data$devadjinc, na.rm = TRUE), digits = 2),
        .groups = "drop"
      ) |>
      as.data.frame(),
    bsidevadj_totcritable_bycountry = country_totcrbsi,
    haiicu_unit_bsidevadj_totcritable = unit_filtered,
    bsidevadj_cri3table_bycountry = country_cri3,
    bsidevadj_cvcasbsitable_bycountry = dplyr::bind_rows(
      country_clabsi,
      dplyr::mutate(eu_cvcasbsi, ReportingCountry = "EU/EEA") |>
        dplyr::select("ReportingCountry", dplyr::everything())
    ),
    eu_cvcasbsi = eu_cvcasbsi,
    clabsi_bycountry = unit_clabsi |>
      dplyr::group_by(.data$ReportingCountry) |>
      dplyr::summarise(
        countrclabsiinc = round(mean(.data$clabsiinc, na.rm = TRUE), digits = 2),
        .groups = "drop"
      ) |>
      as.data.frame(),
    haiicu_unit_bsidevadj_cvcasbsitable = unit_clabsi,
    haiicu_unit_bsidevadj_clabsitable = unit_clabsi,
    CRBSItable = unit_filtered |>
      dplyr::mutate(cvcuse = round(100 * .data$unitexpdays / .data$NumPatDaysUnit2d, digits = 2)) |>
      dplyr::group_by(.data$ReportingCountry) |>
      dplyr::summarise(
        nricu = dplyr::n(),
        nrpat = sum(.data$adm, na.rm = TRUE),
        avglos = round(mean(.data$avglos, na.rm = TRUE), digits = 2),
        cvcuse = round(mean(.data$cvcuse, na.rm = TRUE), digits = 2),
        avgcrbsirate = round(mean(.data$devadjinc, na.rm = TRUE), digits = 2),
        crbsirate25pct = round(
          stats::quantile(.data$devadjinc, probs = 0.25, na.rm = TRUE),
          digits = 2
        ),
        crbsiratemedian = round(stats::median(.data$devadjinc, na.rm = TRUE), digits = 2),
        crbisrate75pct = round(
          stats::quantile(.data$devadjinc, probs = 0.75, na.rm = TRUE),
          digits = 2
        ),
        .groups = "drop"
      ) |>
      as.data.frame()
  )
}
