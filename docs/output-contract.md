# HAI-Net ICU Annual Output Contract

Last updated: 2026-07-17

This contract records the historical annual output files produced by
`scripts/legacy/haiicu_cleaning.R` and the report surfaces that read them. It is
the regression target for the package refactor: preserve filenames and
report-facing columns unless a cleanup is explicitly recorded here.

Do not include raw row-level surveillance records in this document.

## Saved Annual Outputs

| Filename | Saved object or meaning | Notes |
| --- | --- | --- |
| `haiicu_level1_all.Rda` | `haiicu_level1_all` | Combined standard/light ICU-level records. |
| `haiicu_pt_unit.Rda` | `haiicu_pt_unit` | Patient records joined to unit data. |
| `haiicu_pt_inf_all.Rda` | `haiicu_pt_inf_all` | Patient-unit-infection merged data. |
| `haiicu_level1_inf.Rda` | `haiicu_level1_inf` | Standard-protocol ICU infection aggregates. |
| `haiicu_unitlight_all.Rda` | `haiicu_unitlight_all` | Light-protocol ICU infection aggregates. |
| `haiicuall.Rda` | `haiicuall` | Combined light/standard incidence input. |
| `haiicuall2.Rda` | `haiicuall2` | Combined light/standard incidence input with incidence fields. |
| `PNinc_country.Rda` | `PNinc_country` | Pneumonia incidence by country/network. |
| `BSIinc_country.Rda` | `BSIinc_country` | BSI incidence by country/network. |
| `UTIinc_country.Rda` | `UTIinc_country` | UTI incidence by country/network. |
| `PNinc_EU.Rda` | `PNinc_EU` | EU/EEA pneumonia incidence summary. |
| `BSIinc_EU.Rda` | `BSIinc_EU` | EU/EEA BSI incidence summary. |
| `UTIinc_EU.Rda` | `UTIinc_EU` | EU/EEA UTI incidence summary. |
| `haiicuall_percintub.Rda` | `haiicuall_intub` | Intubation percentage input. |
| `haiicu_percintub.Rda` | `haiicu_percintub` | Intubation percentages. |
| `haiicuiapdens.Rda` | `haiicudenscountr` | IAP density/unit-country input. |
| `IAPtable.Rda` | `IAPtable` | IAP incidence table. |
| `eu_iap.Rda` | `eu_iap` | EU/EEA IAP summary. |
| `PNtable_group_top10_pc.Rda` | `PNtable_group_top10_pc` | Pneumonia top-10 microorganism percentages. |
| `haiicu_aggr.Rda` | `haiicu_aggr` | Saved twice; later save overwrites after BSI/PRBSI fields are added. |
| `haiicu_unitall_bsi.Rda` | `haiicu_unitall_bsi` | BSI unit-level/infection input. |
| `haiicu_unit_expcvc.Rda` | `haiicu_unit_expcvc` | CVC exposure by unit. |
| `haiicu_country_expcvc.Rda` | `haiicu_country_expcvc` | CVC exposure by country/network. |
| `haiicu_unit_bsidevadj.Rda` | `haiicu_unit_bsidevadj` | Device-adjusted BSI rate by ICU. |
| `clabsi_bycountry.Rda` | `clabsi_bycountry` | CLABSI by country/network. |
| `haiicu_unit_bsidevadj_cvcasbsitable.Rda` | `haiicu_unit_bsidevadj_cvcasbsitable` | CVC-associated BSI table by unit. |
| `unit_clabsi.Rda` | `haiicu_unit_bsidevadj_clabsitable` | Unit CLABSI table. |
| `bsidevadj_bycountry.Rda` | `bsidevadj_bycountry` | Device-adjusted BSI by country/network. |
| `totcrbsitable.Rda` | `bsidevadj_totcritable_bycountry` | Total CRBSI table by country/network. |
| `unit_crbsi_table.Rda` | `haiicu_unit_bsidevadj_totcritable` | Unit CRBSI table. |
| `prbsi_table.Rda` | `prbsiinc_table_bycountry` | PRBSI incidence by country/network. |
| `cri3table.Rda` | `bsidevadj_cri3table_bycountry` | CRI3 table by country/network. |
| `cvcasbsitable.Rda` | `bsidevadj_cvcasbsitable_bycountry` | CVC-associated BSI table by country/network. |
| `eu_cvcasbsi.Rda` | `eu_cvcasbsi` | EU/EEA CVC-associated BSI summary. |
| `CRBSItable.Rda` | `CRBSItable` | CRBSI table. |
| `BSItable_group_top10_pc.Rda` | `BSItable_group_top10_pc` | BSI top-10 microorganism percentages. |
| `haiicu_pt_uti.Rda` | `haiicu_pt_uti` | Patient UTI data. |
| `haiicu_aggr_uti.Rda` | `haiicu_aggr_uti` | Aggregated UTI data. |
| `haiicu_unitall_UTI.Rda` | `haiicu_unitall_UTI` | UTI unit-level data. |
| `haiicu_unit_expuc.Rda` | `haiicu_unit_expuc` | Urinary catheter exposure by unit. |
| `haiicu_country_expuc.Rda` | `haiicu_country_expuc` | Urinary catheter exposure by country/network. |
| `UTItable_group_top10_pc.Rda` | `UTItable_group_top10_pc` | UTI top-10 microorganism percentages. |
| `resist.Rda` | `resist` | AMR/resistance summary. |
| `InfOutc.Rda` | `InfOutc` | Infection outcomes. |
| `country_unit_table.Rda` | `country_unit_table` | Country/network ICU characteristics. |
| `haiicu_pt_inf_all_full.Rda` | `haiicu_pt_inf_all` | Written to `outputs/<year>` and also directly to the working directory by a plain `saveRDS()` call. |
| `country_demogr.Rda` | `country_demogr` | Country/network patient demographics. |
| `country_ab_table.Rda` | `country_ab_table` | Antimicrobial-use table. |
| `country_ab_ind.Rda` | `country_ab_ind` | Antimicrobial-use indicators. |
| `country_deno_7d.Rda` | `country_deno_7d` | 7-day denominator table. |
| `country_ind.Rda` | `country_ind` | Structure/process indicators. |

## Report Reads

### Legacy R Markdown report

`scripts/legacy/haineticuReport.Rmd` reads these output files:

`haiicu_percintub.Rda`, `haiicuiapdens.Rda`, `haiicu_level1_all.Rda`,
`haiicu_pt_unit.Rda`, `haiicu_pt_inf_all.Rda`, `haiicuall2.Rda`,
`IAPtable.Rda`, `unit_clabsi.Rda`, `haiicu_unitall_bsi.Rda`,
`PNinc_country.Rda`, `BSIinc_country.Rda`, `UTIinc_country.Rda`,
`PNinc_EU.Rda`, `BSIinc_EU.Rda`, `UTIinc_EU.Rda`,
`haiicu_unit_expcvc.Rda`, `haiicu_country_expcvc.Rda`,
`haiicu_unit_bsidevadj.Rda`, `bsidevadj_bycountry.Rda`, `CRBSItable.Rda`,
`totcrbsitable.Rda`, `unit_crbsi_table.Rda`,
`BSItable_group_top10_pc.Rda`, `haiicu_unitall_uti.Rda`,
`haiicu_unit_expuc.Rda`, `haiicu_country_expuc.Rda`,
`UTItable_group_top10_pc.Rda`, `haiicu_pt_uti.Rda`,
`haiicu_aggr_uti.Rda`, `PNtable_group_top10_pc.Rda`, `haiicu_aggr.Rda`,
`resist.Rda`, `country_demogr.Rda`, `country_unit_table.Rda`,
`cri3table.Rda`, `cvcasbsitable.Rda`, `prbsi_table.Rda`,
`clabsi_bycountry.Rda`, `haiicu_unit_bsidevadj_cvcasbsitable.Rda`,
`country_ab_table.Rda`, `country_ab_ind.Rda`, `country_deno_7d.Rda`,
`country_ind.Rda`, and `InfOutc.Rda`.

### Quarto helper setup

`reports/R/_report_setup.r` reads the same broad set plus `eu_iap.Rda` and
`eu_cvcasbsi.Rda`.

### Canonical Quarto report

`reports/haineticu-report.qmd` uses `prepare_report_data()`,
`hicu_prepare_report_tables()`, and `hicu_prepare_report_summaries()` to render
package-prepared participation, incidence, aggregate BSI annex, microbiology,
AMR, antimicrobial-use, infection-outcome, and structure/process indicator
content. Source-level report migration is complete; `hicu_prepare_report_summaries()`
still needs roxygen export/doc regeneration once R execution is observable.

## Known Mismatches And Risks

- Case mismatch: cleaning saves `haiicu_unitall_UTI.Rda`, while report loaders
  read `haiicu_unitall_uti.Rda`. This works on typical Windows file systems but
  is not portable.
- `haiicu_pt_inf_all_full.Rda` is written once through the output helper and
  once directly to the current working directory.
- `reports/R/_report_setup.r` assigns `haiicu_pt <- load(...)`, which stores the
  character vector returned by `load()` rather than the loaded object.
- Saved but not currently read by the specified report surfaces:
  `haiicu_level1_inf.Rda`, `haiicu_unitlight_all.Rda`, `haiicuall.Rda`,
  `haiicuall_percintub.Rda`, and `haiicu_pt_inf_all_full.Rda`.

## Experimental Boundaries

The annual output-producing contract effectively ends at `country_ind.Rda`.
After that point, the legacy script builds additional master data frames and
contains ad hoc trend summaries, logistic regression models, APACHE-adjusted
variants, XGBoost/caret/pROC/SHAP diagnostics, and feature-importance work.
Those sections do not save annual report outputs and should remain outside
package load scope unless explicitly promoted later.

## 2023 Output Metadata Snapshot

The following metadata were read from `outputs/2023` without printing row-level
records. Column lists are truncated where tables are wide.

| Object | Filename | Class | Dimensions | Columns |
| --- | --- | --- | --- | --- |
| `haiicu_intub` | `haiicu_percintub.Rda` | data.frame | 655 x 16 | RecordId, ReportingCountry, ... |
| `haiicudenscountr` | `haiicuiapdens.Rda` | data.frame | 604 x 12 | UnitId, ReportingCountry, ... |
| `haiicu_level1_all` | `haiicu_level1_all.Rda` | data.frame | 1623 x 26 | RecordId, RecordType, Record..., ... |
| `haiicu_pt_unit` | `haiicu_pt_unit.Rda` | data.frame | 108746 x 55 | UnitId, RecordId.x, ParentI..., ... |
| `haiicu_pt_inf_all` | `haiicu_pt_inf_all.Rda` | data.frame | 112055 x 55 | Id, UnitId, RecordId, Acute..., ... |
| `haiicuall2` | `haiicuall2.Rda` | data.frame | 1623 x 16 | RecordId, ReportingCountry, ... |
| `IAPtable` | `IAPtable.Rda` | tbl_df, tbl, data.frame | 11 x 9 | ReportingCountry, n_IAP, n_..., ... |
| `eu_iap` | `eu_iap.Rda` | data.frame | 1 x 8 | n_IAP, n_expdays, intubuse, ... |
| `unit_clabsi` | `unit_clabsi.Rda` | data.frame | 604 x 32 | RecordId, ReportingCountry, ... |
| `haiicu_unitall_bsi` | `haiicu_unitall_bsi.Rda` | data.frame | 6481 x 5 | RecordId, ParentId, Infecti..., ... |
| `PNinc_country` | `PNinc_country.Rda` | tbl_df, tbl, data.frame | 12 x 8 | ReportingCountry, n_PN, n_N..., ... |
| `BSIinc_country` | `BSIinc_country.Rda` | tbl_df, tbl, data.frame | 12 x 8 | ReportingCountry, n_BSI, n_..., ... |
| `UTIinc_country` | `UTIinc_country.Rda` | tbl_df, tbl, data.frame | 12 x 8 | ReportingCountry, n_UTI, n_..., ... |
| `PNinc_EU` | `PNinc_EU.Rda` | data.frame | 1 x 7 | n_PN, n_NumPtDays, PNinc, meanPNinc, pct25, median, pct75 |
| `BSIinc_EU` | `BSIinc_EU.Rda` | data.frame | 1 x 7 | n_BSI, n_NumPtDays, BSIinc, meanBSIinc, pct25, median, pct75 |
| `UTIinc_EU` | `UTIinc_EU.Rda` | data.frame | 1 x 7 | n_UTI, n_NumPtDays, UTIinc, meanUTIinc, pct25, median, pct75 |
| `BSItable_group_top10_pc` | `BSItable_group_top10_pc.Rda` | tbl_df, tbl, data.frame | 10 x 14 | Isolate, Austria, Estonia, ... |
| `UTItable_group_top10_pc` | `UTItable_group_top10_pc.Rda` | tbl_df, tbl, data.frame | 10 x 12 | Isolate, Austria, Estonia, ... |
| `PNtable_group_top10_pc` | `PNtable_group_top10_pc.Rda` | tbl_df, tbl, data.frame | 10 x 13 | Isolate, Austria, Estonia, ... |
| `country_demogr` | `country_demogr.Rda` | tbl_df, tbl, data.frame | 10 x 18 | ReportingCountry, N_pat, pat..., ... |
| `country_unit_table` | `country_unit_table.Rda` | tbl_df, tbl, data.frame | 12 x 8 | ReportingCountry, N, unit_size_median, Spec_med, Spec_sur, Spec_mix, Spec_coro, Spec_ounk |
| `country_ab_table` | `country_ab_table.Rda` | tbl_df, tbl, data.frame | 8 x 9 | ReportingCountry, N_ab_mean..., ... |
| `country_ab_ind` | `country_ab_ind.Rda` | tbl_df, tbl, data.frame | 8 x 6 | ReportingCountry, emp, dir, ... |
| `country_deno_7d` | `country_deno_7d.Rda` | tbl_df, tbl, data.frame | 3 x 7 | ReportingCountry, UnitSize_..., ... |
| `country_ind` | `country_ind.Rda` | tbl_df, tbl, data.frame | 1 x 17 | ReportingCountry, IndNumCom..., ... |
| `InfOutc` | `InfOutc.Rda` | grouped_df, tbl_df, tbl, data.frame | 7 x 6 | ReportingCountry, A, DDEFRE..., ... |