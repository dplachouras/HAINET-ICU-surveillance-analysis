## Run from shell:
## Rscript R/render_haineticu.R word
## Rscript R/render_haineticu.R html

library(here)
library(rmarkdown)

args <- commandArgs(trailingOnly = TRUE)
requested_format <- if (length(args) >= 1) args[1] else Sys.getenv("HAINET_OUTPUT_FORMAT", unset = "word_document")
requested_format <- tolower(trimws(requested_format))

format_map <- c(
  "word" = "word_document",
  "word_document" = "word_document",
  "docx" = "word_document",
  "html" = "html_document",
  "html_document" = "html_document"
)

output_format <- format_map[requested_format]
if (is.na(output_format)) {
  stop("Unsupported format '", requested_format, "'. Use 'word_document'/'docx' or 'html_document'/'html'.")
}

year <- Sys.getenv("HAINET_YEAR", unset = "2023")
out_dir <- normalizePath(here("reports", "html_AER"), winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

output_ext <- if (output_format == "word_document") "docx" else "html"
output_file <- paste0(year, "_HAIICU_Report.", output_ext)
target_path <- file.path(out_dir, output_file)
tmp_dir <- tempdir()
tmp_output_path <- file.path(tmp_dir, output_file)

if (file.exists(tmp_output_path)) {
  file.remove(tmp_output_path)
}

if (file.exists(target_path)) {
  file.remove(target_path)
}

render(
  input = here("R", "haineticuReport.Rmd"),
  output_dir = tmp_dir,
  output_format = output_format,
  output_file = output_file,
  encoding = "UTF-8",
  clean = TRUE,
  intermediates_dir = tmp_dir
)

if (file.exists(tmp_output_path)) {
  file.copy(tmp_output_path, target_path, overwrite = TRUE)
}

