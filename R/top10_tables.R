# Top microorganism country tables -------------------------------------

build_top10_country_tables <- function(df,
                                       isolate_col = "Isolate",
                                       total_col = "total",
                                       totalpc_col = "totalpc",
                                       country_name_lut = c(
                                         AT = "Austria", BE = "Belgium",
                                         CZ = "Czech Republic", DE = "Germany",
                                         EE = "Estonia", ES = "Spain",
                                         FR = "France", HU = "Hungary",
                                         IT = "Italy", LT = "Lithuania",
                                         LU = "Luxembourg", MT = "Malta",
                                         PL = "Poland", PT = "Portugal",
                                         RO = "Romania", SK = "Slovakia",
                                         UK = "United Kingdom",
                                         "IT-SPIN-UTI" = "Italy-SPIN-UTI",
                                         "IT-GiViTI" = "Italy-GiViTI",
                                         ITSPINUTI = "Italy-SPIN-UTI",
                                         ITGiViTI = "Italy-GiViTI"
                                       )) {
  country_cols <- df |>
    dplyr::select(
      dplyr::where(is.numeric),
      -dplyr::any_of(c(total_col, totalpc_col)),
      -dplyr::matches("pc$")
    ) |>
    names()

  country_pc_cols <- df |>
    dplyr::select(dplyr::matches("pc$"), -dplyr::any_of(totalpc_col)) |>
    names()

  summary_tbl <- df |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(country_cols), ~ sum(.x, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      total = rowSums(dplyr::across(dplyr::all_of(country_cols)), na.rm = TRUE)
    )

  totals_tbl <- df |>
    dplyr::select(dplyr::all_of(country_cols), dplyr::any_of(total_col)) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum")
    )

  pc_tbl <- df |>
    dplyr::select(
      dplyr::all_of(isolate_col),
      dplyr::all_of(country_pc_cols),
      dplyr::any_of(totalpc_col)
    ) |>
    dplyr::mutate(dplyr::across(-dplyr::all_of(isolate_col), ~ tidyr::replace_na(.x, 0)))

  pc_keys <- sub("pc$", "", country_pc_cols)
  display_names <- ifelse(
    is.na(country_name_lut[pc_keys]),
    pc_keys,
    unname(country_name_lut[pc_keys])
  )

  names(pc_tbl) <- c(
    isolate_col,
    display_names,
    if (totalpc_col %in% names(pc_tbl)) "total"
  )

  list(
    summary = summary_tbl,
    totals = totals_tbl,
    pc = pc_tbl,
    country_cols = country_cols,
    country_pc_cols = country_pc_cols
  )
}