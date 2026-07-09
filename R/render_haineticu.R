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
out_dir <- here("reports", "html_AER")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

output_ext <- if (output_format == "word_document") "docx" else "html"
output_file <- paste0(year, "_HAIICU_Report.", output_ext)

render(
  input = here("R", "haineticuReport.Rmd"),
  output_dir = out_dir,
  output_format = output_format,
  output_file = output_file,
  encoding = "UTF-8"
)

