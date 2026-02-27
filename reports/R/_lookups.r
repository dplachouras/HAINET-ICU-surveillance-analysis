country_lut <- c(
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

# Recode values in a country column (e.g. ReportingCountry)
label_country_values <- function(df, col = "ReportingCountry") {
  dplyr::mutate(df, !!col := dplyr::recode(.data[[col]], !!!country_lut))
}

# Rename columns that are country codes (e.g. AT, BE, DE as column names)
label_country_columns <- function(df) {
  dplyr::rename_with(df, ~ ifelse(.x %in% names(country_lut), country_lut[.x], .x))
}

var_label_lut <- c(
  ReportingCountry = "Country/Network",
  n_BSI = "BSI episodes (n)",
  n_NumPtDays = "Patient-days (n)",
  BSIinc = "BSI incidence per 1,000 patient-days",
  n_cvcdays = "Catheter-days (n)",
  cvcuse = "CVC use (catheter-days per 100 patient-days)",
  n_clabsi = "CLABSI episodes (n)",
  NumPatDaysUnit2d = "Patient-days",
  aggrinc = "Aggregated",
  avgclabsirate = "Mean",
  clabsirate25pct = "25th percentile",
  clabsiratemedian = "Median",
  clabsirate75pct = "75th percentile",
  BSIinc = "Aggregated",
  meanBSIinc = "Mean",
  pct25 = "25th percentile",
  median = "Median",
  pct75 = "75th percentile"
)

apply_gt_labels <- function(gt_tbl, data, lut = var_label_lut) {
  keep <- lut[names(lut) %in% names(data)]
  if (length(keep) == 0) return(gt_tbl)
  gt::cols_label(gt_tbl, .list = as.list(keep))
}

add_incidence_spanner <- function(
    gt_tbl,
    data,
    cols,
    title = "Incidence density",
    unit = "(episodes per 1 000 patient-days)"
) {
  spanner_cols <- intersect(cols, names(data))
  if (length(spanner_cols) == 0) return(gt_tbl)

  gt::tab_spanner(
    gt_tbl,
    label = gt::md(paste0("**", title, "**<br>", unit)),
    columns = dplyr::all_of(spanner_cols)
  )
}

inc_cols_std <- c("inc", "meanInc", "pct25", "median", "pct75")  # if standardized
inc_cols_bsi <- c("BSIinc", "meanBSIinc", "pct25", "median", "pct75")
inc_cols_pn <- c("PNinc", "meanPNinc", "pct25", "median", "pct75")
inc_cols_iap <- c("IAPinc", "meanIAPinc", "pct25", "median", "pct75")
inc_cols_clabsi <- c("aggrinc", "avgclabsirate", "clabsirate25pct", "clabsiratemedian", "clabsirate75pct")
inc_cols_uti <- c("UTIinc", "meanUTIinc", "pct25", "median", "pct75")   

style_gt_common <- function(
    gt_tbl,
    data,
    country_col = "ReportingCountry",
    eu_label = "EU/EEA",
    decimals_non_integer = 2
) {
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]

  int_like <- num_cols[vapply(data[num_cols], function(x) {
    x <- x[!is.na(x)]
    length(x) > 0 && all(abs(x - round(x)) < .Machine$double.eps^0.5)
  }, logical(1))]

  dec_like <- setdiff(num_cols, int_like)

  # Only format columns that actually exist in this gt table
  gt_cols <- names(gt_tbl[["_data"]])
  int_like <- intersect(int_like, gt_cols)
  dec_like <- intersect(dec_like, gt_cols)

  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels(columns = everything())
    ) |>
    gt::cols_align(align = "center", columns = everything()) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        rows = .data[[country_col]] == eu_label,
        columns = everything()
      )
    )

  if (length(int_like)) {
    gt_tbl <- gt_tbl |>
      gt::fmt_number(columns = dplyr::all_of(int_like), decimals = 0, sep_mark = " ", dec_mark = ".")
  }
  if (length(dec_like)) {
    gt_tbl <- gt_tbl |>
      gt::fmt_number(columns = dplyr::all_of(dec_like), decimals = decimals_non_integer, sep_mark = " ", dec_mark = ".")
  }

  gt_tbl
}
