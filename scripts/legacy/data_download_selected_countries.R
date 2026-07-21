library(haidatamanager)

data <- collect_surveillance_data(
  subject = "HAIICU",
  years = 2023,
  countries = c("EE", "MT", "SK"),
  output_dir = "data/HAIICU"
)