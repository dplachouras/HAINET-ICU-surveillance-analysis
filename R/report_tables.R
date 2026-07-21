#' Append an EU/EEA row to a country/network table
#'
#' @param country_data Country/network data frame.
#' @param eu_data One-row EU/EEA data frame.
#' @param country_col Name of the country/network column.
#' @param eu_label Label to use for the EU/EEA row.
#'
#' @return `country_data` with an EU/EEA row appended last.
#' @export
hicu_append_eu_row <- function(
    country_data,
    eu_data,
    country_col = "ReportingCountry",
    eu_label = "EU/EEA"
) {
  country_out <- country_data
  eu_out <- eu_data

  eu_out[[country_col]] <- eu_label

  for (column_name in setdiff(names(country_out), names(eu_out))) {
    eu_out[[column_name]] <- NA
  }

  eu_out <- eu_out[, names(country_out), drop = FALSE]

  for (column_name in names(country_out)) {
    target_class <- class(country_out[[column_name]])[[1]]
    eu_out[[column_name]] <- switch(
      target_class,
      integer = suppressWarnings(as.integer(eu_out[[column_name]])),
      numeric = suppressWarnings(as.numeric(eu_out[[column_name]])),
      character = as.character(eu_out[[column_name]]),
      factor = factor(as.character(eu_out[[column_name]]), levels = levels(country_out[[column_name]])),
      eu_out[[column_name]]
    )
  }

  dplyr::bind_rows(country_out, eu_out)
}

#' Label report table columns
#'
#' @param data Report table data frame.
#' @param labels Named character vector mapping column names to labels.
#'
#' @return `data` with labelled columns where labels are known.
#' @export
hicu_label_report_columns <- function(data, labels = hicu_report_var_labels()) {
  dplyr::rename_with(
    data,
    ~ ifelse(.x %in% names(labels), unname(labels[.x]), .x)
  )
}

hicu_prepare_amr_report_table <- function(data) {
  raw_columns <- c(
    "ReportingCountry",
    "ResultIsolate",
    "Isolate",
    "Antibiotic",
    "SIR"
  )

  if (all(raw_columns %in% names(data))) {
    return(summarise_resistance_indicators(data))
  }

  data
}

#' Prepare a generic report table
#'
#' @param data Report table data frame.
#' @param country_col Name of the country/network column.
#' @param label_country_values Whether to label country/network row values.
#' @param label_country_columns Whether to label country/network column names.
#' @param label_columns Whether to label known report variable names.
#' @param sort_eu Whether to sort rows with the EU/EEA row last.
#' @param eu_label Label used for the EU/EEA row.
#'
#' @return Prepared report table data frame.
#' @export
hicu_prepare_report_table <- function(
    data,
    country_col = "ReportingCountry",
    label_country_values = TRUE,
    label_country_columns = TRUE,
    label_columns = TRUE,
    sort_eu = TRUE,
    eu_label = "EU/EEA"
) {
  out <- as.data.frame(data)

  if (isTRUE(label_country_values) && country_col %in% names(out)) {
    out <- hicu_label_country_values(out, country_col = country_col)
  }

  if (isTRUE(sort_eu) && country_col %in% names(out)) {
    out <- hicu_sort_with_eu_last(
      out,
      country_col = country_col,
      eu_label = eu_label
    )
  }

  if (isTRUE(label_country_columns)) {
    out <- hicu_label_country_columns(out)
  }

  if (isTRUE(label_columns)) {
    out <- hicu_label_report_columns(out)
  }

  out
}

#' Prepare all available Quarto report tables
#'
#' @param report_data Named list returned by `prepare_report_data()`.
#'
#' @return Named list of prepared tables for `reports/haineticu-report.qmd`.
#' @export
hicu_prepare_report_tables <- function(report_data) {
  tables <- list()

  add_table <- function(object_name, table_name) {
    if (!is.null(report_data[[object_name]])) {
      tables[[table_name]] <<- hicu_prepare_report_table(report_data[[object_name]])
    }
  }

  add_incidence_table <- function(country_name, eu_name, table_name) {
    if (!is.null(report_data[[country_name]])) {
      tables[[table_name]] <<- hicu_prepare_report_table(
        hicu_prepare_incidence_table(
          report_data[[country_name]],
          report_data[[eu_name]]
        ),
        label_country_values = FALSE
      )
    }
  }

  add_table("country_unit_table", "icu_characteristics")
  add_table("country_demogr", "patient_demographics")
  add_table("InfOutc", "infection_outcomes")

  add_incidence_table("PNinc_country", "PNinc_EU", "pn_incidence")
  add_table("IAPtable", "iap")
  add_table("PNtable_group_top10_pc", "pn_microorganisms")

  add_incidence_table("BSIinc_country", "BSIinc_EU", "bsi_incidence")
  add_table("prbsi_table", "prbsi")
  add_table("cri3table", "cri3")
  add_table("CRBSItable", "catheter_related_bsi")
  add_table("totcrbsitable", "total_crbsi")
  add_table("haiicu_unit_bsidevadj_totcritable", "unit_total_crbsi")
  add_table("cvcasbsitable", "cvc_associated_bsi")
  add_table("eu_cvcasbsi", "eu_cvc_associated_bsi")
  add_table("clabsi_bycountry", "clabsi_by_country")
  add_table("haiicu_unit_bsidevadj_cvcasbsi", "unit_cvc_associated_bsi")
  add_table("BSItable_group_top10_pc", "bsi_microorganisms")

  add_incidence_table("UTIinc_country", "UTIinc_EU", "uti_incidence")
  add_table("UTItable_group_top10_pc", "uti_microorganisms")

  if (!is.null(report_data$resist)) {
    tables$antimicrobial_resistance <- hicu_prepare_report_table(
      hicu_prepare_amr_report_table(report_data$resist)
    )
  }
  add_table("country_ab_table", "antimicrobial_groups")
  add_table("country_ab_ind", "antimicrobial_indications")
  add_table("country_deno_7d", "structure_indicators")
  add_table("country_ind", "process_indicators")

  tables
}

#' Prepare an incidence table for reporting
#'
#' @param country_data Country/network incidence data frame.
#' @param eu_data Optional one-row EU/EEA incidence data frame.
#' @param country_col Name of the country/network column.
#' @param eu_label Label to use for the EU/EEA row.
#'
#' @return Labelled incidence data frame, with EU/EEA row appended if supplied.
#' @export
hicu_prepare_incidence_table <- function(
    country_data,
    eu_data = NULL,
    country_col = "ReportingCountry",
    eu_label = "EU/EEA"
) {
  out <- hicu_label_country_values(country_data, country_col = country_col)

  if (!is.null(eu_data)) {
    eu_out <- hicu_label_country_values(eu_data, country_col = country_col)
    out <- hicu_append_eu_row(
      out,
      eu_out,
      country_col = country_col,
      eu_label = eu_label
    )
  }

  hicu_sort_with_eu_last(out, country_col = country_col, eu_label = eu_label)
}