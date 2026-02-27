

library(dplyr)
library(knitr)
library(rprojroot)

# Find repo root by marker file/folder
repo_root <- rprojroot::find_root(
  rprojroot::has_file("HAINET_ICU.Rproj") | rprojroot::has_dir(".git")
)

OUTPUT_YEAR <- "2023"
OUTPUT_DIR <- file.path(repo_root, "outputs", OUTPUT_YEAR)

source(file.path(repo_root, "R", "haiicu_functions.r"), encoding = "UTF-8")


# Read an output file by name from outputs/<year>
# Handles both:
# - true RDS files (even if they have a .Rda extension in this project)
# - true .Rda/.RData files saved with save()
read_output_obj <- function(file, object = NULL, output_dir = OUTPUT_DIR) {
  path <- file.path(output_dir, file)

  # First try readRDS (your current pipeline seems to use readRDS on many *.Rda files)
  rds_try <- try(readRDS(path), silent = TRUE)
  if (!inherits(rds_try, "try-error")) {
    return(rds_try)
  }

  # Fallback: load() into isolated env
  e <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = e)

  if (!is.null(object)) {
    if (!exists(object, envir = e, inherits = FALSE)) {
      stop(sprintf("Object '%s' not found in %s. Loaded: %s",
                   object, basename(path), paste(loaded_names, collapse = ", ")))
    }
    return(get(object, envir = e, inherits = FALSE))
  }

  # If only one object was loaded, return it directly
  if (length(loaded_names) == 1) {
    return(get(loaded_names[[1]], envir = e, inherits = FALSE))
  }

  # If multiple objects, return as named list
  mget(loaded_names, envir = e, inherits = FALSE)
}





haiicu_intub <- readRDS(file.path(OUTPUT_DIR, "haiicu_percintub.Rda"))

haiicudenscountr<-readRDS(file.path(OUTPUT_DIR,"haiicuiapdens.Rda"))
#load("haiicu_level1_all.Rda")
load(file.path(OUTPUT_DIR,"haiicu_level1_all.Rda"))
allunits<-haiicu_level1_all
#attach(haiicu_level1_all)
haiicu_pt<-load(file.path(OUTPUT_DIR,"haiicu_pt_unit.Rda"))
haiicu_pt_inf_all<-readRDS(file.path(OUTPUT_DIR,"haiicu_pt_inf_all.Rda"))
haiicuall2<-readRDS(file.path(OUTPUT_DIR,"haiicuall2.Rda"))
IAPtable<-readRDS(file.path(OUTPUT_DIR,"IAPtable.Rda"))
eu_iap<-readRDS(file.path(OUTPUT_DIR,"eu_iap.Rda"))
unit_clabsi<-readRDS(file.path(OUTPUT_DIR,"unit_clabsi.Rda"))
haiicu_unitall_bsi<-readRDS(file.path(OUTPUT_DIR,"haiicu_unitall_bsi.Rda"))
PNinc_country<-readRDS(file.path(OUTPUT_DIR,"PNinc_country.Rda"))
BSIinc_country<-readRDS(file.path(OUTPUT_DIR,"BSIinc_country.Rda"))
UTIinc_country<-readRDS(file.path(OUTPUT_DIR,"UTIinc_country.Rda"))
PNinc_EU<-readRDS(file.path(OUTPUT_DIR,"PNinc_EU.Rda"))
BSIinc_EU<-readRDS(file.path(OUTPUT_DIR,"BSIinc_EU.Rda"))
UTIinc_EU<-readRDS(file.path(OUTPUT_DIR,"UTIinc_EU.Rda"))
#haiicu_unitall_bsi<-filter(haiicu_unitall_bsi,InfectionSite=="CRI1"|InfectionSite=="CRI2")
haiicu_unit_expcvc<-readRDS(file.path(OUTPUT_DIR,"haiicu_unit_expcvc.Rda"))
haiicu_country_expcvc<-readRDS(file.path(OUTPUT_DIR,"haiicu_country_expcvc.Rda"))
haiicu_unit_bsidevadj<-readRDS(file.path(OUTPUT_DIR,"haiicu_unit_bsidevadj.Rda"))
bsidevadj_bycountry<-readRDS(file.path(OUTPUT_DIR,"bsidevadj_bycountry.Rda"))
CRBSItable<-readRDS(file.path(OUTPUT_DIR,"CRBSItable.Rda"))
totcrbsitable<-readRDS(file.path(OUTPUT_DIR,"totcrbsitable.Rda"))
haiicu_unit_bsidevadj_totcritable<-readRDS(file.path(OUTPUT_DIR,"unit_crbsi_table.Rda"))
bsidevadj_totcritable_bycountry<-readRDS(file.path(OUTPUT_DIR,"totcrbsitable.Rda"))
BSItable_group_top10_pc<-readRDS(file.path(OUTPUT_DIR,"BSItable_group_top10_pc.Rda"))           
haiicu_unitall_uti<-readRDS(file.path(OUTPUT_DIR,"haiicu_unitall_uti.Rda"))
haiicu_unit_expuc<-readRDS(file.path(OUTPUT_DIR,"haiicu_unit_expuc.Rda"))
haiicu_country_expuc<-readRDS(file.path(OUTPUT_DIR,"haiicu_country_expuc.Rda"))
UTItable_group_top10_pc<-readRDS(file.path(OUTPUT_DIR,"UTItable_group_top10_pc.Rda"))
haiicu_pt_uti<-readRDS(file.path(OUTPUT_DIR,"haiicu_pt_uti.Rda"))
haiicu_aggr_uti<-readRDS(file.path(OUTPUT_DIR,"haiicu_aggr_uti.Rda"))
PNtable_group_top10_pc<-readRDS(file.path(OUTPUT_DIR,"PNtable_group_top10_pc.Rda"))
haiicu_aggr<-readRDS(file.path(OUTPUT_DIR,"haiicu_aggr.Rda"))
resist<-readRDS(file.path(OUTPUT_DIR,"resist.Rda"))
country_demogr<-readRDS(file.path(OUTPUT_DIR,"country_demogr.Rda"))
country_unit_table<-readRDS(file.path(OUTPUT_DIR,"country_unit_table.Rda"))
cri3table<-readRDS(file.path(OUTPUT_DIR,"cri3table.Rda"))
cvcasbsitable<-readRDS(file.path(OUTPUT_DIR,"cvcasbsitable.Rda"))
eu_cvcasbsi<-readRDS(file.path(OUTPUT_DIR,"eu_cvcasbsi.Rda"))
prbsi_table<-readRDS(file.path(OUTPUT_DIR,"prbsi_table.Rda"))
clabsi_bycountry<-readRDS(file.path(OUTPUT_DIR,"clabsi_bycountry.Rda"))
haiicu_unit_bsidevadj_cvcasbsi<-readRDS(file.path(OUTPUT_DIR,"haiicu_unit_bsidevadj_cvcasbsitable.Rda"))
country_ab_table<-readRDS(file.path(OUTPUT_DIR,"country_ab_table.Rda"))
country_ab_ind<-readRDS(file.path(OUTPUT_DIR,"country_ab_ind.Rda"))
country_deno_7d<-readRDS(file.path(OUTPUT_DIR,"country_deno_7d.Rda"))
country_ind<-readRDS(file.path(OUTPUT_DIR,"country_ind.Rda"))
InfOutc<-readRDS(file.path(OUTPUT_DIR,"InfOutc.Rda"))

#EU rows for tables
# ---- Table harmonisation helpers (country + EU row + incidence columns) ----

# Safe recode of country values if country_lut exists (from _lookups.r)
recode_country_if_available <- function(df, country_col = "ReportingCountry") {
  if (!exists("country_lut", inherits = TRUE)) return(df)
  if (!country_col %in% names(df)) return(df)
  dplyr::mutate(df, !!country_col := dplyr::recode(.data[[country_col]], !!!country_lut))
}

# Ensure EU row has the same schema/types as country table and append as last row
append_eu_row <- function(country_df, eu_df, country_col = "ReportingCountry", eu_label = "EU/EEA") {
  cdf <- country_df
  edf <- eu_df

  # If EU table has one row with same conceptual fields but different names, harmonise before calling this wrapper.
  if (!country_col %in% names(edf)) {
    # If missing country column in EU row, create it
    edf[[country_col]] <- eu_label
  } else {
    edf[[country_col]] <- eu_label
  }

  # Add missing columns to EU row
  for (nm in setdiff(names(cdf), names(edf))) {
    edf[[nm]] <- NA
  }

  # Keep only columns in country table order
  edf <- edf[, names(cdf), drop = FALSE]

  # Coerce EU row column types to match country table types
  for (nm in names(cdf)) {
    target_class <- class(cdf[[nm]])[1]
    edf[[nm]] <- switch(
      target_class,
      integer = suppressWarnings(as.integer(edf[[nm]])),
      numeric = suppressWarnings(as.numeric(edf[[nm]])),
      character = as.character(edf[[nm]]),
      factor = factor(as.character(edf[[nm]]), levels = levels(cdf[[nm]])),
      edf[[nm]]
    )
  }

  dplyr::bind_rows(cdf, edf)
}

# One-call prep for incidence tables
prepare_incidence_table <- function(country_df, eu_df = NULL, country_col = "ReportingCountry", eu_label = "EU/EEA") {
  out <- recode_country_if_available(country_df, country_col = country_col)
  if (!is.null(eu_df)) {
    eu_prepped <- recode_country_if_available(eu_df, country_col = country_col)
    out <- append_eu_row(out, eu_prepped, country_col = country_col, eu_label = eu_label)
  }
  out
}

PNinc_country_tbl   <- prepare_incidence_table(PNinc_country, PNinc_EU)
BSIinc_country_tbl  <- prepare_incidence_table(BSIinc_country, BSIinc_EU)
IAP_tbl             <- prepare_incidence_table(IAPtable, eu_iap)         
CLABSI_tbl          <- prepare_incidence_table(cvcasbsitable, eu_cvcasbsi)    
UTIinc_country_tbl  <- prepare_incidence_table(UTIinc_country, UTIinc_EU)