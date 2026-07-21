# HAI-Net ICU Surveillance Workflow

Updated: 21 July 2026

## Production steps

1. Download data using `scripts/legacy/data_download.R`, use `scripts/legacy/data_download_selected_countries.R` for the selected-country variant, or place annual TESSy extracts into `data/raw/<year>/`.
2. Run annual cleaning and output generation:

	`Rscript scripts/run_cleaning.R`

3. Render the canonical Quarto annual report from package code:

	`Rscript -e "devtools::load_all(); render_report(output_format = 'html')"`

	`Rscript -e "devtools::load_all(); render_report(output_format = 'docx')"`

4. Run unit tests:

	`Rscript -e "devtools::test()"`

5. Regenerate package documentation after roxygen changes:

	`Rscript -e "devtools::document()"`

## Notes

- Default year is read from `HAINET_YEAR` and falls back to `2023`.
- Exploratory scripts are in `experiments/` and are not part of the production pipeline.
- Legacy executable workflow scripts are in `scripts/legacy/`; package-safe functions live in `R/`.
- Data-download scripts are legacy executable scripts and are kept out of `R/` so package loading does not trigger downloads.
- `scripts/legacy/render_haineticu.R` remains as a compatibility wrapper; new code should call `render_report()` directly.
- `run_haiicu_workflow(write_outputs = FALSE)` runs the extracted synthetic-testable core without writing output files.
- The canonical Quarto report source migration is complete. `hicu_prepare_report_summaries()` still needs roxygen documentation regeneration and a focused report test/render pass once R terminal execution is observable again.
- Raw data and generated outputs remain untracked.