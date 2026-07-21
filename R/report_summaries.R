hicu_report_rate <- function(numerator, denominator, multiplier = 1, digits = 2) {
  if (length(denominator) == 0 || is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }

  round(multiplier * numerator / denominator, digits = digits)
}

hicu_report_range <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(list(min = NA_real_, max = NA_real_))
  }

  list(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}

hicu_first_value <- function(data, column, default = NA_real_) {
  if (is.null(data) || !column %in% names(data) || nrow(data) == 0) {
    return(default)
  }

  data[[column]][[1]]
}

hicu_top_isolates <- function(data, isolate_col = "Isolate", count = 4) {
  if (is.null(data) || !isolate_col %in% names(data) || nrow(data) == 0) {
    return(character())
  }

  head(as.character(data[[isolate_col]]), count)
}

hicu_format_isolate_list <- function(isolates) {
  isolates <- isolates[!is.na(isolates) & nzchar(isolates)]

  if (length(isolates) == 0) {
    return(NA_character_)
  }

  if (length(isolates) == 1) {
    return(isolates)
  }

  paste(
    paste(head(isolates, -1), collapse = ", "),
    tail(isolates, 1),
    sep = " and "
  )
}

hicu_amr_indicator_summary <- function(data, labels = hicu_report_var_labels()) {
  if (is.null(data) || ncol(data) == 0) {
    return(list(count = 0L, labels = character(), text = NA_character_))
  }

  indicator_names <- setdiff(names(data), "ReportingCountry")
  indicator_names <- indicator_names[indicator_names %in% names(labels)]
  indicator_labels <- unname(labels[indicator_names])

  list(
    count = length(indicator_labels),
    labels = indicator_labels,
    text = hicu_format_isolate_list(indicator_labels)
  )
}

#' Prepare scalar summaries for Quarto report narrative
#'
#' @param report_data Named list returned by `prepare_report_data()`.
#'
#' @return Named list of scalar summaries used by the Quarto report narrative.
#' @export
hicu_prepare_report_summaries <- function(report_data) {
  units <- report_data$haiicu_level1_all
  patient_units <- report_data$haiicu_pt_unit
  infections <- report_data$haiicu_pt_inf_all
  incidence_units <- report_data$haiicuall2

  unique_patient_rows <- if (!is.null(infections) && "dupl_pat" %in% names(infections)) {
    !infections$dupl_pat
  } else {
    rep(TRUE, nrow(infections))
  }

  patient_days <- if (!is.null(infections) && "lengthofstay" %in% names(infections)) {
    sum(as.numeric(infections$lengthofstay[unique_patient_rows]), na.rm = TRUE)
  } else {
    NA_real_
  }

  patients_with_hai <- if (!is.null(infections) && all(c("Id", "hasHai") %in% names(infections))) {
    length(unique(infections$Id[infections$hasHai %in% TRUE]))
  } else {
    NA_integer_
  }

  patient_count <- if (!is.null(patient_units)) {
    nrow(patient_units)
  } else if (!is.null(infections)) {
    sum(unique_patient_rows, na.rm = TRUE)
  } else {
    NA_integer_
  }

  unit_size_range <- if (!is.null(units) && "UnitSize" %in% names(units)) {
    hicu_report_range(suppressWarnings(as.numeric(as.character(units$UnitSize))))
  } else {
    list(min = NA_real_, max = NA_real_)
  }

  pneumonia_rows <- if (!is.null(infections) && "InfectionSite" %in% names(infections)) {
    grepl("PN", infections$InfectionSite)
  } else {
    logical()
  }
  bsi_rows <- if (!is.null(infections) && "InfectionSite" %in% names(infections)) {
    grepl("BSI|CRI3", infections$InfectionSite)
  } else {
    logical()
  }
  uti_rows <- if (!is.null(infections) && "InfectionSite" %in% names(infections)) {
    grepl("UTI", infections$InfectionSite)
  } else {
    logical()
  }

  pn_cases <- sum(pneumonia_rows, na.rm = TRUE)
  bsi_cases <- sum(bsi_rows, na.rm = TRUE)
  uti_cases <- sum(uti_rows, na.rm = TRUE)
  pn_top_isolates <- hicu_top_isolates(report_data$PNtable_group_top10_pc)
  bsi_top_isolates <- hicu_top_isolates(report_data$BSItable_group_top10_pc)
  uti_top_isolates <- hicu_top_isolates(report_data$UTItable_group_top10_pc)
  amr_indicators <- hicu_amr_indicator_summary(
    hicu_prepare_amr_report_table(report_data$resist)
  )

  list(
    participation = list(
      country_count = dplyr::n_distinct(units$ReportingCountry, na.rm = TRUE),
      hospital_count = dplyr::n_distinct(units$HospitalIdGlobal, na.rm = TRUE),
      icu_count = dplyr::n_distinct(units$UnitIdGlobal, na.rm = TRUE),
      unit_size_median = stats::median(
        suppressWarnings(as.numeric(as.character(units$UnitSize))),
        na.rm = TRUE
      ),
      unit_size_min = unit_size_range$min,
      unit_size_max = unit_size_range$max,
      patient_count = patient_count,
      patients_with_hai = patients_with_hai,
      patients_with_hai_percent = hicu_report_rate(
        patients_with_hai,
        patient_count,
        multiplier = 100,
        digits = 1
      )
    ),
    pneumonia = list(
      cases = pn_cases,
      intubation_associated_percent = if (!is.null(infections) && "Intubation" %in% names(infections)) {
        hicu_report_rate(
          sum(infections$Intubation == "Y" & pneumonia_rows, na.rm = TRUE),
          pn_cases,
          multiplier = 100,
          digits = 1
        )
      } else {
        NA_real_
      },
      patient_percent = hicu_report_rate(
        pn_cases,
        sum(unique_patient_rows, na.rm = TRUE),
        multiplier = 100,
        digits = 2
      ),
      incidence = hicu_report_rate(
        pn_cases,
        patient_days,
        multiplier = 1000,
        digits = 2
      ),
      eu_incidence = hicu_first_value(report_data$PNinc_EU, "PNinc"),
      top_isolates = pn_top_isolates,
      top_isolates_text = hicu_format_isolate_list(pn_top_isolates)
    ),
    bloodstream = list(
      cases = bsi_cases,
      patient_percent = hicu_report_rate(
        bsi_cases,
        sum(unique_patient_rows, na.rm = TRUE),
        multiplier = 100,
        digits = 2
      ),
      mean_icu_incidence = if (!is.null(incidence_units) && "BSI_incdens" %in% names(incidence_units)) {
        round(mean(incidence_units$BSI_incdens, na.rm = TRUE), digits = 2)
      } else {
        NA_real_
      },
      eu_incidence = hicu_first_value(report_data$BSIinc_EU, "BSIinc"),
      top_isolates = bsi_top_isolates,
      top_isolates_text = hicu_format_isolate_list(bsi_top_isolates)
    ),
    urinary_tract = list(
      cases = uti_cases,
      patient_percent = hicu_report_rate(
        uti_cases,
        sum(unique_patient_rows, na.rm = TRUE),
        multiplier = 100,
        digits = 2
      ),
      mean_icu_incidence = if (!is.null(incidence_units) && "UTI_incdens" %in% names(incidence_units)) {
        round(mean(incidence_units$UTI_incdens, na.rm = TRUE), digits = 2)
      } else {
        NA_real_
      },
      eu_incidence = hicu_first_value(report_data$UTIinc_EU, "UTIinc"),
      top_isolates = uti_top_isolates,
      top_isolates_text = hicu_format_isolate_list(uti_top_isolates)
    ),
    antimicrobial_resistance = list(
      indicator_count = amr_indicators$count,
      indicator_labels = amr_indicators$labels,
      indicator_text = amr_indicators$text
    )
  )
}