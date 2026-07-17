library(haidatamanager)
data <- collect_surveillance_data(
  subject = "HAIICU",
  years = 2023,
  output_dir = "data/HAIICU"
)