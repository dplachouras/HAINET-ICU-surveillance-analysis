#' Recode microorganism isolate codes
#'
#' @param result_isolate Character vector of TESSy `ResultIsolate` codes.
#' @param infection_group Infection group. BSI maps staphylococci more broadly
#'   to coagulase-negative staphylococci before the specific S. aureus rule.
#'
#' @return Character vector of display isolate names.
#' @export
recode_microorganism_isolate <- function(
    result_isolate,
    infection_group = c("PN", "BSI", "UTI")
) {
  infection_group <- match.arg(infection_group)
  isolate <- ifelse(!grepl("_", result_isolate), substr(result_isolate, 1, 3), NA)

  isolate[grepl("PSEAER", result_isolate)] <- "Pseudomonas aeruginosa"
  if (infection_group == "BSI") {
    isolate[grepl("STA...", result_isolate)] <- "Coagulase-negative staphylococci"
  }
  isolate[grepl("STAAUR", result_isolate)] <- "Staphylococcus aureus"
  isolate[grepl("KLE...", result_isolate)] <- "Klebsiella spp."
  isolate[grepl("ESCCOL", result_isolate)] <- "Escherichia coli"
  isolate[grepl("CAN...", result_isolate)] <- "Candida spp."
  isolate[grepl("STEMAL", result_isolate)] <- "Stenotrophomonas maltofphilia"
  isolate[grepl("ENB...", result_isolate)] <- "Enterobacter spp."
  isolate[grepl("ACI...", result_isolate)] <- "Acinetobacter spp."
  isolate[grepl("ENC...", result_isolate)] <- "Enterococcus spp."
  isolate[grepl("SER...", result_isolate)] <- "Serratia spp."
  isolate[grepl("PRT...", result_isolate)] <- "Proteus spp."

  isolate
}

#' Prepare microorganism records for top-country tables
#'
#' @param microorganism_records Data frame with `ResultIsolate` and
#'   `ReportingCountry` columns.
#' @param infection_group Infection group used for isolate recoding.
#' @param excluded_result_isolates Sentinel isolate values excluded from tables.
#' @param excluded_isolates Recoded isolate labels excluded from tables.
#'
#' @return Prepared microorganism records with an `Isolate` column.
#' @export
prepare_microorganism_records <- function(
    microorganism_records,
    infection_group = c("PN", "BSI", "UTI"),
    excluded_result_isolates = c("_NOEXA", "_STERI", "_NA", "_NONI"),
    excluded_isolates = character()
) {
  infection_group <- match.arg(infection_group)
  microorganism_records |>
    dplyr::mutate(
      Isolate = recode_microorganism_isolate(
        .data$ResultIsolate,
        infection_group = infection_group
      )
    ) |>
    dplyr::filter(!.data$ResultIsolate %in% .env$excluded_result_isolates) |>
    dplyr::filter(!.data$Isolate %in% .env$excluded_isolates) |>
    dplyr::filter(!is.na(.data$Isolate)) |>
    as.data.frame()
}

#' Build top microorganism country tables
#'
#' @param microorganism_records Data frame with `ResultIsolate` and
#'   `ReportingCountry` columns.
#' @param infection_group Infection group used for isolate recoding.
#' @param n Number of top isolates to keep.
#' @param excluded_result_isolates Sentinel isolate values excluded from tables.
#' @param excluded_isolates Recoded isolate labels excluded from tables.
#'
#' @return Named list from `build_top10_country_tables()`.
#' @export
build_microorganism_top_country_tables <- function(
    microorganism_records,
    infection_group = c("PN", "BSI", "UTI"),
    n = 10,
    excluded_result_isolates = c("_NOEXA", "_STERI", "_NA", "_NONI"),
    excluded_isolates = character()
) {
  prepared <- prepare_microorganism_records(
    microorganism_records,
    infection_group = infection_group,
    excluded_result_isolates = excluded_result_isolates,
    excluded_isolates = excluded_isolates
  )

  grouped <- prepared |>
    dplyr::group_by(.data$Isolate, .data$ReportingCountry) |>
    dplyr::summarise(n_isol = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = "ReportingCountry",
      values_from = "n_isol",
      values_fill = 0
    ) |>
    dplyr::mutate(total = rowSums(dplyr::pick(-dplyr::all_of("Isolate")), na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::slice_head(n = n)

  country_cols <- grouped |>
    dplyr::select(dplyr::where(is.numeric), -dplyr::any_of("total")) |>
    names()

  for (country_col in country_cols) {
    pc_col <- paste0(make.names(country_col), "pc")
    grouped[[pc_col]] <- 100 * round(
      grouped[[country_col]] / sum(grouped[[country_col]], na.rm = TRUE),
      digits = 3
    )
  }
  grouped$totalpc <- 100 * round(grouped$total / sum(grouped$total, na.rm = TRUE), digits = 3)

  build_top10_country_tables(grouped)
}

#' Recode AMR isolate labels
#'
#' @param result_isolate Character vector of TESSy `ResultIsolate` codes.
#'
#' @return Character vector of AMR isolate group labels.
#' @export
recode_resistance_isolate <- function(result_isolate) {
  isolate <- rep(NA_character_, length(result_isolate))
  isolate[grepl("PSEAER", result_isolate)] <- "Pseudomonas aeruginosa"
  isolate[grepl("STA...", result_isolate)] <- "Coagulase-negative staphylococci"
  isolate[grepl("STAAUR", result_isolate)] <- "Staphylococcus aureus"
  isolate[grepl("KLE...", result_isolate)] <- "Klebsiellas"
  isolate[grepl("ESCCOL", result_isolate)] <- "Escherichia coli"
  isolate[grepl("CAN...", result_isolate)] <- "Candida spp."
  isolate[grepl("STEMAL", result_isolate)] <- "Stenotrophomonas maltofphilia"
  isolate[grepl("ENB...", result_isolate)] <- "Enterobacter"
  isolate[grepl("ACI...", result_isolate)] <- "Acinetobacter spp."
  isolate[grepl("ENC...", result_isolate)] <- "Enterococcus"
  isolate[grepl("SER...", result_isolate)] <- "Serratia spp."
  isolate[grepl("PRT...", result_isolate)] <- "Proteus spp."
  isolate[grepl("CIT...", result_isolate)] <- "Citrobacter spp."
  isolate
}

#' Prepare AMR resistance records
#'
#' @param resistance_records Resistance result table.
#' @param reporting_country Optional infection-level country mapping with
#'   `InfectionId` and `ReportingCountry` columns.
#' @param exclude_countries Countries excluded from AMR summaries.
#'
#' @return Prepared resistance records with normalized antibiotic/isolate codes.
#' @export
prepare_resistance_records <- function(
    resistance_records,
    reporting_country = NULL,
    exclude_countries = "DE"
) {
  out <- resistance_records |>
    dplyr::mutate(
      Isolate = recode_resistance_isolate(.data$ResultIsolate),
      Antibiotic = dplyr::case_when(
        grepl("CAZ|CTX", .data$Antibiotic) ~ "C3G",
        grepl("VAN", .data$Antibiotic) ~ "GLY",
        grepl("ESBL", .data$Antibiotic) ~ "C3G",
        grepl("IPM|MEM", .data$Antibiotic) ~ "CAR",
        TRUE ~ .data$Antibiotic
      )
    )

  if (!is.null(reporting_country)) {
    country_map <- reporting_country |>
      dplyr::select("InfectionId", "ReportingCountry") |>
      dplyr::mutate(InfectionId = as.character(.data$InfectionId))
    out <- out |>
      dplyr::mutate(ParentId = as.character(.data$ParentId)) |>
      dplyr::left_join(country_map, by = c("ParentId" = "InfectionId"))
  }

  out |>
    dplyr::mutate(uniq = !duplicated(.data$ParentId)) |>
    dplyr::filter(!is.na(.data$ReportingCountry)) |>
    dplyr::filter(!.data$ReportingCountry %in% .env$exclude_countries) |>
    dplyr::filter(.data$SIR != "UNK") |>
    as.data.frame()
}

hicu_resistance_percent <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  round(100 * numerator / denominator, digits = 1)
}

hicu_resistance_summary_row <- function(records) {
  data.frame(
    MRSA = hicu_resistance_percent(
      sum(records$ResultIsolate == "STAAUR" & records$SIR %in% c("R", "IR") & records$Antibiotic %in% c("OXA", "MET"), na.rm = TRUE),
      sum(records$ResultIsolate == "STAAUR" & records$Antibiotic %in% c("OXA", "MET"), na.rm = TRUE)
    ),
    VRE = hicu_resistance_percent(
      sum(records$Isolate == "Enterococcus" & records$SIR %in% c("R", "IR") & records$Antibiotic %in% c("GLY", "_NOTEST"), na.rm = TRUE),
      sum(records$Isolate == "Enterococcus" & records$Antibiotic %in% c("GLY"), na.rm = TRUE)
    ),
    CEFRPS = hicu_resistance_percent(
      sum(records$ResultIsolate == "PSEAER" & records$SIR == "R" & records$Antibiotic == "C3G", na.rm = TRUE),
      sum(records$ResultIsolate == "PSEAER" & records$Antibiotic %in% c("C3G"), na.rm = TRUE)
    ),
    C3GREC = hicu_resistance_percent(
      sum(records$ResultIsolate == "ESCCOL" & records$SIR == "R" & records$Antibiotic == "C3G", na.rm = TRUE),
      sum(records$ResultIsolate == "ESCCOL" & records$Antibiotic %in% c("C3G"), na.rm = TRUE)
    ),
    C3GRKP = hicu_resistance_percent(
      sum(records$Isolate == "Klebsiellas" & records$SIR == "R" & records$Antibiotic == "C3G", na.rm = TRUE),
      sum(records$Isolate == "Klebsiellas" & records$Antibiotic %in% c("C3G"), na.rm = TRUE)
    ),
    C3GRENT = hicu_resistance_percent(
      sum(records$Isolate == "Enterobacter" & records$SIR == "R" & records$Antibiotic == "C3G", na.rm = TRUE),
      sum(records$Isolate == "Enterobacter" & records$Antibiotic %in% c("C3G"), na.rm = TRUE)
    ),
    CRKP = hicu_resistance_percent(
      sum(records$Isolate == "Klebsiellas" & records$SIR == "R" & records$Antibiotic == "CAR", na.rm = TRUE),
      sum(records$Isolate == "Klebsiellas" & records$Antibiotic %in% c("CAR"), na.rm = TRUE)
    ),
    CREC = hicu_resistance_percent(
      sum(records$ResultIsolate == "ESCCOL" & records$SIR == "R" & records$Antibiotic == "CAR", na.rm = TRUE),
      sum(records$ResultIsolate == "ESCCOL" & records$Antibiotic %in% c("CAR"), na.rm = TRUE)
    ),
    CRENT = hicu_resistance_percent(
      sum(records$Isolate == "Enterobacter" & records$SIR == "R" & records$Antibiotic == "CAR", na.rm = TRUE),
      sum(records$Isolate == "Enterobacter" & records$Antibiotic %in% c("CAR"), na.rm = TRUE)
    ),
    CRPS = hicu_resistance_percent(
      sum(records$ResultIsolate == "PSEAER" & records$SIR == "R" & records$Antibiotic == "CAR", na.rm = TRUE),
      sum(records$ResultIsolate == "PSEAER" & records$Antibiotic %in% c("CAR"), na.rm = TRUE)
    ),
    CRAB = hicu_resistance_percent(
      sum(records$ResultIsolate == "ACIBAU" & records$SIR == "R" & records$Antibiotic == "CAR", na.rm = TRUE),
      sum(records$ResultIsolate == "ACIBAU" & records$Antibiotic %in% c("CAR"), na.rm = TRUE)
    )
  )
}

#' Summarise AMR resistance indicators
#'
#' @param resistance_records Prepared resistance records from
#'   `prepare_resistance_records()`.
#' @param by_country Whether to return one row per reporting country.
#'
#' @return Data frame of resistance indicator percentages.
#' @export
summarise_resistance_indicators <- function(resistance_records, by_country = TRUE) {
  if (!by_country) {
    return(hicu_resistance_summary_row(resistance_records))
  }

  split_records <- split(resistance_records, resistance_records$ReportingCountry)
  out <- do.call(
    rbind,
    lapply(names(split_records), function(country) {
      cbind(
        data.frame(ReportingCountry = country),
        hicu_resistance_summary_row(split_records[[country]])
      )
    })
  )
  row.names(out) <- NULL
  out
}