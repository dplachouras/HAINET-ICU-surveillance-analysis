# AGENTS.md

Guidance for AI coding agents working in this repository.

## Repository Purpose

This repository supports the annual ECDC HAI-Net ICU surveillance workflow for healthcare-associated infections acquired in intensive care units in Europe. The main workflow is:

1. Download or place annual TESSy HAIICU CSV exports in `data/raw/<year>/`.
2. Clean and analyse the annual data with `R/haiicu_cleaning.R`.
3. Save derived annual tables and objects to `outputs/<year>/`.
4. Render the annual epidemiological report from `R/haineticuReport.Rmd` using `R/render_haineticu.R`.
5. Continue development of the Quarto HTML report under `reports/` as a separate, newer reporting surface.

The tracked code is the analytical workflow. Raw data, generated outputs, rendered sites, and Quarto build artifacts are intentionally untracked.

## Project Structure

- `README.txt` documents the high-level annual production steps.
- `HAINET_ICU.Rproj` is the RStudio project marker. It uses UTF-8 and two-space indentation.
- `R/data_download.R` downloads HAIICU data through `haidatamanager` into `data/HAIICU/`.
- `R/haiicu_functions.r` contains shared helpers for percentages, quantiles, mixed date parsing, year-scoped data reads, output reads, output writes, and top-10 microorganism tables.
- `R/haiicu_cleaning.R` is the main cleaning and analysis script. It reads CSV files from `data/raw/<year>/`, creates annual derived tables, and writes `outputs/<year>/` objects.
- `R/haineticuReport.Rmd` is the main R Markdown epidemiological report.
- `R/render_haineticu.R` renders the report to Word or HTML and writes it to `reports/html_AER/`.
- `reports/_quarto.yml` and `reports/Reports/<year>/*.qmd` are the under-development Quarto website report.
- `reports/R/_report_setup.r`, `_lookups.r`, `_plot_helpers.r`, `_plot_theme.r`, and `_report_metrics.r` are shared helpers for the Quarto report.
- `data/`, `outputs/`, `reports/_site/`, `.quarto/`, and Quarto notebook artifacts are ignored by git.

## Commands

Run commands from the repository root unless a script explicitly says otherwise.

```powershell
# Optional: choose the reporting year; defaults to 2023 in current scripts
$env:HAINET_YEAR = "2023"

# Main annual cleaning and analysis
Rscript R/haiicu_cleaning.R

# Render the annual R Markdown report
Rscript R/render_haineticu.R word
Rscript R/render_haineticu.R html
```

For Quarto development, use the `reports/` project and verify paths carefully because this report is still under development:

```powershell
quarto render reports
```

## Data And Output Conventions

- Use `year <- Sys.getenv("HAINET_YEAR", unset = "2023")` for year-scoped work unless the user asks for a fixed year.
- Use `DATA_DIR <- here("data", "raw", year)` and `OUTPUT_DIR <- here("outputs", year)` in R scripts that read source CSVs or write derived outputs.
- Prefer `read_data_csv()`, `read_data_fread()`, `read_output_rds()`, `load_output_rds()`, `save_output()`, and `save_output_rds()` from `R/haiicu_functions.r` instead of open-coded file paths.
- Many output files use a `.Rda` extension even when written with `saveRDS()`. Use the existing helper functions because they handle both true RDS files and loaded RData-style objects where needed.
- Do not commit raw surveillance extracts, generated `.Rda`/`.RDS` outputs, rendered reports, or Quarto build artifacts.
- Treat surveillance data as sensitive operational data. Avoid printing row-level patient or hospital records unless the user explicitly needs a diagnostic excerpt.

## R Coding Style

- Follow the existing R style: two-space indentation, UTF-8, `<-` assignment, and pipe-based `dplyr` workflows.
- Keep changes local and practical. This codebase is an annual production workflow, so avoid broad refactors unless they directly reduce risk in the requested task.
- Prefer existing package patterns: `dplyr`, `tidyr`, `ggplot2`, `data.table`, `here`, `rmarkdown`, `flextable`, `gt`, `knitr`, and `rprojroot` where already used.
- Use `here()` or repository-root detection instead of `setwd()` or fragile relative paths in new production code.
- Use `parse_mixed_date()` for date fields that may arrive as ISO or `dd/mm/yyyy` strings.
- Preserve ECDC surveillance terminology and field names such as `ReportingCountry`, `RecordId`, `ParentId`, `InfectionSite`, `BSIOrigin`, `NumPatDaysUnit2d`, `IAP`, `CLABSI`, and `PRBSI` unless a schema change is intentional.
- Base R `merge()`, `table()`, `grepl()`, `duplicated()`, and direct column assignment are common here. Do not rewrite these patterns just for style.
- Prefer explicit joins and selected columns before merges when adding new tables. Check duplicate identifiers before aggregating patient-level or infection-level data.
- When calculating country or EU/EEA indicators, be explicit about exclusions such as Germany, France, Belgium, Malta, the UK, Czech Republic, or Italian networks when the surrounding code already applies them.
- Keep comments concise and operational. Existing section headers such as `#Exposure data -------` and short data-check comments are acceptable.

## Epidemiological Guardrails

- Do not replace surveillance definitions with generic simplifications. Definitions for IAP, CLABSI, CRI3, PRBSI, device days, patient days, and exclusion rules are report-critical.
- Do not silently change annual defaults, country exclusions, infection definitions, resistance mappings, denominator calculations, or date-window logic.
- Keep country/network handling explicit. Existing code recodes Italy into `IT-GiViTI` and `IT-SPIN-UTI`, excludes some countries from specific denominators, and appends EU/EEA rows in selected tables.
- If changing a case definition, denominator rule, exclusion, microbiology mapping, or resistance mapping, add a clear operational comment and validate the affected table outputs.

## Report Conventions

- The R Markdown report reads precomputed objects from `outputs/<year>/`; do not duplicate heavy cleaning logic in report text.
- Use `make_report_table()` in `R/haineticuReport.Rmd` for report tables so Word and HTML output remain compatible.
- Keep narrative calculations tied to the same saved objects as the tables they describe.
- The Quarto report uses shared setup and lookup helpers in `reports/R/`; add report-wide table labels, country labels, and styling there rather than repeating them in every `.qmd` page.
- Keep generated report content reproducible from the saved annual outputs. If a report needs a new indicator, add it to the cleaning pipeline first, then read it in the report.

## Validation Expectations

- For changes to cleaning logic, run at least:

```powershell
$env:HAINET_YEAR = "2023"
Rscript R/haiicu_cleaning.R
```

- For changes to `R/haineticuReport.Rmd`, `R/render_haineticu.R`, or report table helpers, run the narrow render relevant to the change:

```powershell
$env:HAINET_YEAR = "2023"
Rscript R/render_haineticu.R html
```

- For changes to `R/haiicu_functions.r`, run a narrow source check when possible:

```powershell
Rscript -e "source('R/haiicu_functions.r')"
```

- For Quarto report changes, render the Quarto project or the touched page when possible:

```powershell
quarto render reports
```

- If full validation is slow because it regenerates annual outputs, run the smallest script or render that exercises the touched code and clearly state what was and was not validated.

## Agent Operating Rules

- Do not restructure the annual workflow without asking. The file layout encodes the production order and report dependencies.
- Avoid broad refactors of `R/haiicu_cleaning.R`. It is a production script with many dependent output names consumed by the report.
- Preserve output file names when possible because report scripts load them by exact name.
- Before changing a derived table name or column name, search both the cleaning script and report files for downstream uses.
- Do not overwrite user-edited generated files unless the task is explicitly to regenerate them.
- Do not change `.gitignore` to track `data/`, `outputs/`, or generated site artifacts without explicit approval.
- When adding dependencies, prefer packages already used in the repository. Add a new package only when it materially improves maintainability or report quality.
