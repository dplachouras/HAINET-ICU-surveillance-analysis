# Helper functions ---------------------------------------------------------

helper_write_inputs_to_dir <- function(data_dir, inputs) {
  input_files <- haiicu_input_files()
  utils::write.csv(inputs$unit, file.path(data_dir, input_files[["unit"]]), row.names = FALSE)
  utils::write.csv(inputs$unit_light, file.path(data_dir, input_files[["unit_light"]]), row.names = FALSE)
  utils::write.csv(inputs$patient, file.path(data_dir, input_files[["patient"]]), row.names = FALSE)
  utils::write.csv(inputs$infection, file.path(data_dir, input_files[["infection"]]), row.names = FALSE)
  utils::write.csv(
    inputs$denominator,
    file.path(data_dir, input_files[["denominator"]]),
    row.names = FALSE
  )
  utils::write.csv(
    inputs$denominator_light,
    file.path(data_dir, input_files[["denominator_light"]]),
    row.names = FALSE
  )
  utils::write.csv(
    inputs$infection_light,
    file.path(data_dir, input_files[["infection_light"]]),
    row.names = FALSE
  )
  utils::write.csv(
    inputs$indicator,
    file.path(data_dir, input_files[["indicator"]]),
    row.names = FALSE
  )
  utils::write.csv(
    inputs$indicator_light,
    file.path(data_dir, input_files[["indicator_light"]]),
    row.names = FALSE
  )
  utils::write.csv(inputs$exposure, file.path(data_dir, input_files[["exposure"]]), row.names = FALSE)
  utils::write.csv(
    inputs$antimicrobial,
    file.path(data_dir, input_files[["antimicrobial"]]),
    row.names = FALSE
  )

  optional_inputs <- setdiff(
    names(input_files),
    c(
      "unit", "unit_light", "patient", "infection", "denominator_light",
      "infection_light", "exposure", "antimicrobial", "denominator",
      "indicator", "indicator_light"
    )
  )
  for (input_name in optional_inputs) {
    utils::write.csv(
      data.frame(RecordId = character()),
      file.path(data_dir, input_files[[input_name]]),
      row.names = FALSE
    )
  }
}

# Tests for run_haiicu_workflow() -----------------------------------------

testthat::test_that("run_haiicu_workflow supports synthetic no-write runs", {
  withr::local_tempdir()
  data_dir <- file.path(tempdir(), "raw")
  output_dir <- file.path(tempdir(), "outputs")
  dir.create(data_dir)
  helper_write_inputs_to_dir(data_dir, helper_haiicu_inputs())

  result <- run_haiicu_workflow(
    year = "2023",
    data_dir = data_dir,
    output_dir = output_dir,
    write_outputs = FALSE
  )

  testthat::expect_identical(result$year, "2023")
  testthat::expect_contains(names(result), "haiicuall2")
  testthat::expect_contains(names(result), "PNinc_country")
  testthat::expect_contains(names(result), "BSIinc_country")
  testthat::expect_contains(names(result), "UTIinc_country")
  testthat::expect_contains(names(result), "country_ab_table")
  testthat::expect_contains(names(result), "country_ab_ind")
  testthat::expect_contains(names(result), "country_deno_7d")
  testthat::expect_contains(names(result), "country_ind")
  testthat::expect_contains(names(result), "prbsi_table")
  testthat::expect_contains(names(result), "bsidevadj_totcritable_bycountry")
  testthat::expect_contains(names(result), "bsidevadj_cri3table_bycountry")
  testthat::expect_contains(names(result), "bsidevadj_cvcasbsitable_bycountry")
  testthat::expect_false(dir.exists(output_dir))
})