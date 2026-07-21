#' Load report objects from annual outputs
#'
#' @return Named character vector mapping report object names to filenames.
#' @export
report_output_files <- function() {
  c(
    haiicu_intub = "haiicu_percintub.Rda",
    haiicudenscountr = "haiicuiapdens.Rda",
    haiicu_level1_all = "haiicu_level1_all.Rda",
    haiicu_pt_unit = "haiicu_pt_unit.Rda",
    haiicu_pt_inf_all = "haiicu_pt_inf_all.Rda",
    haiicuall2 = "haiicuall2.Rda",
    IAPtable = "IAPtable.Rda",
    eu_iap = "eu_iap.Rda",
    unit_clabsi = "unit_clabsi.Rda",
    haiicu_unitall_bsi = "haiicu_unitall_bsi.Rda",
    PNinc_country = "PNinc_country.Rda",
    BSIinc_country = "BSIinc_country.Rda",
    UTIinc_country = "UTIinc_country.Rda",
    PNinc_EU = "PNinc_EU.Rda",
    BSIinc_EU = "BSIinc_EU.Rda",
    UTIinc_EU = "UTIinc_EU.Rda",
    haiicu_unit_expcvc = "haiicu_unit_expcvc.Rda",
    haiicu_country_expcvc = "haiicu_country_expcvc.Rda",
    haiicu_unit_bsidevadj = "haiicu_unit_bsidevadj.Rda",
    bsidevadj_bycountry = "bsidevadj_bycountry.Rda",
    CRBSItable = "CRBSItable.Rda",
    totcrbsitable = "totcrbsitable.Rda",
    haiicu_unit_bsidevadj_totcritable = "unit_crbsi_table.Rda",
    BSItable_group_top10_pc = "BSItable_group_top10_pc.Rda",
    haiicu_unitall_uti = "haiicu_unitall_UTI.Rda",
    haiicu_unit_expuc = "haiicu_unit_expuc.Rda",
    haiicu_country_expuc = "haiicu_country_expuc.Rda",
    UTItable_group_top10_pc = "UTItable_group_top10_pc.Rda",
    haiicu_pt_uti = "haiicu_pt_uti.Rda",
    haiicu_aggr_uti = "haiicu_aggr_uti.Rda",
    PNtable_group_top10_pc = "PNtable_group_top10_pc.Rda",
    haiicu_aggr = "haiicu_aggr.Rda",
    resist = "resist.Rda",
    country_demogr = "country_demogr.Rda",
    country_unit_table = "country_unit_table.Rda",
    cri3table = "cri3table.Rda",
    cvcasbsitable = "cvcasbsitable.Rda",
    eu_cvcasbsi = "eu_cvcasbsi.Rda",
    prbsi_table = "prbsi_table.Rda",
    clabsi_bycountry = "clabsi_bycountry.Rda",
    haiicu_unit_bsidevadj_cvcasbsi = "haiicu_unit_bsidevadj_cvcasbsitable.Rda",
    country_ab_table = "country_ab_table.Rda",
    country_ab_ind = "country_ab_ind.Rda",
    country_deno_7d = "country_deno_7d.Rda",
    country_ind = "country_ind.Rda",
    InfOutc = "InfOutc.Rda"
  )
}

#' Prepare report data from annual outputs
#'
#' @param year Reporting year. Defaults to `HAINET_YEAR` or "2023".
#' @param output_dir Output directory. Defaults to outputs/<year>.
#' @param object_files Named character vector mapping object names to filenames.
#'
#' @return Named list of loaded objects.
prepare_report_data <- function(
    year = hicu_default_year(),
    output_dir = hicu_default_output_dir(year),
    object_files = report_output_files()
) {
  if (!dir.exists(output_dir)) {
    stop("Output directory not found: ", output_dir, call. = FALSE)
  }

  loaded <- lapply(object_files, function(file) {
    hicu_read_output_object(file = file, output_dir = output_dir)
  })

  loaded$year <- year
  loaded$output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  loaded
}
