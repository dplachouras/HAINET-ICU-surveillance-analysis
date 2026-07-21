# Tests for hicu_parse_mixed_date() and hicu_read_output_object() ---------

testthat::test_that("hicu_parse_mixed_date parses mixed date formats", {
  x <- c("2023-01-31", "01/02/2023", "", NA)
  out <- hicu_parse_mixed_date(x)

  testthat::expect_s3_class(out, "Date")
  testthat::expect_identical(as.character(out[[1]]), "2023-01-31")
  testthat::expect_identical(as.character(out[[2]]), "2023-02-01")
  testthat::expect_true(is.na(out[[3]]))
  testthat::expect_true(is.na(out[[4]]))
})

testthat::test_that("hicu_read_output_object reads RDS and RData", {
  testthat::skip_on_cran()

  withr::local_tempdir()
  tmp <- tempdir()

  rds_path <- file.path(tmp, "obj.rda")
  expected_rds <- data.frame(a = 1:2)
  saveRDS(expected_rds, rds_path)

  out_rds <- hicu_read_output_object("obj.rda", output_dir = tmp)
  testthat::expect_equal(out_rds, expected_rds)

  rdata_path <- file.path(tmp, "obj2.rda")
  expected_rdata <- list(x = 5L)
  save(expected_rdata, file = rdata_path)

  out_rdata <- hicu_read_output_object("obj2.rda", output_dir = tmp)
  testthat::expect_equal(out_rdata, expected_rdata)
})
