# Helper functions ---------------------------------------------------------

helper_device_patient_infections <- function() {
  data.frame(
    Id = c("P1", "P2", "P3", "P4", "P5"),
    RecordId = c("I1", "I2", "I3", "I4", "I5"),
    UnitId = c("U1", "U1", "U1", "U2", "U2"),
    InfectionSite = c("PN", "PN", "BSI", "BSI", "BSI"),
    BSIOrigin = c("", "", "C-CVC", "S-PUL", "UNK"),
    DateOfOnset = c(
      "2023-01-04", "2023-01-08", "2023-01-04", "2023-01-04",
      "2023-01-02"
    )
  )
}

helper_device_exposures <- function() {
  data.frame(
    RecordId = c("E1", "E2", "E3", "E4", "E5"),
    ParentId = c("P1", "P2", "P3", "P4", "P5"),
    ExpType = c("INT", "INT", "CVC", "CVC", "CVC"),
    DateExpStart = c(
      "2023-01-02", "2023-01-01", "2023-01-01", "2023-01-01",
      "2023-01-01"
    ),
    DateExpEnd = c(
      "2023-01-04", "2023-01-04", "2023-01-05", "2023-01-05",
      "2023-01-03"
    )
  )
}

# Tests for classify_iap_cases() ------------------------------------------

testthat::test_that("classify_iap_cases applies intubation onset window", {
  result <- classify_iap_cases(
    helper_device_patient_infections(),
    helper_device_exposures()
  )

  expected_counts <- data.frame(
    UnitId = "U1",
    IAP = 1L
  )

  testthat::expect_identical(result$cases$Id, "P1")
  testthat::expect_equal(result$unit_counts, expected_counts)
})

# Tests for classify_cvc_associated_bsi() ---------------------------------

testthat::test_that("classify_cvc_associated_bsi applies CVC BSI window", {
  result <- classify_cvc_associated_bsi(
    helper_device_patient_infections(),
    helper_device_exposures()
  )

  expected_counts <- data.frame(
    UnitId = "U1",
    CVCASBSI = 1L
  )

  testthat::expect_identical(result$cases$Id, "P3")
  testthat::expect_true(result$cases$clabsi)
  testthat::expect_equal(result$unit_counts, expected_counts)
})