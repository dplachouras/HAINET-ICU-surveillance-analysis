library(here)
library(rmarkdown)

year <- Sys.getenv("HAINET_YEAR", unset = "2023")
out_dir <- here("reports", "html_AER")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

render(
  here("R", "haineticuReport.Rmd"),
  output_dir = out_dir,
  output_file = paste0(year, "_HAIICU_Report.html"),
  encoding = "UTF-8"
)