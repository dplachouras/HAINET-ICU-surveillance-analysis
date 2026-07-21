# Tests for normalize_report_format() -------------------------------------

testthat::test_that("normalize_report_format maps supported aliases", {
  testthat::expect_identical(normalize_report_format("html"), "html")
  testthat::expect_identical(normalize_report_format("html_document"), "html")
  testthat::expect_identical(normalize_report_format("word"), "docx")
  testthat::expect_identical(normalize_report_format("docx"), "docx")
})

testthat::test_that("normalize_report_format rejects unsupported values", {
  testthat::expect_error(
    normalize_report_format("pdf"),
    "Unsupported output format"
  )
})

# Tests for render_report() -----------------------------------------------

testthat::test_that("render_report builds docx output path", {
  withr::local_tempdir()

  report_file <- file.path(tempdir(), "report.qmd")
  output_dir <- file.path(tempdir(), "reports")
  writeLines("---\ntitle: Test\n---\n", report_file)

  captured_args <- NULL
  captured_work_dir <- NULL
  testthat::local_mocked_bindings(
    hicu_run_quarto = function(args, quiet = TRUE, work_dir = NULL) {
      captured_args <<- args
      captured_work_dir <<- work_dir
      output_file <- args[[which(args == "--output") + 1L]]
      file.create(file.path(dirname(report_file), output_file))
      0L
    },
    .package = "HAINETICU"
  )

  rendered <- render_report(
    year = "2024",
    output_format = "docx",
    report_file = report_file,
    output_dir = output_dir
  )

  testthat::expect_true(file.exists(rendered))
  testthat::expect_identical(basename(rendered), "2024_HAIICU_Report.docx")
  testthat::expect_identical(captured_args[[2]], basename(report_file))
  testthat::expect_identical(
    captured_work_dir,
    normalizePath(dirname(report_file), winslash = "/", mustWork = TRUE)
  )
  testthat::expect_contains(captured_args, "--to")
  testthat::expect_identical(
    captured_args[[which(captured_args == "--to") + 1L]],
    "docx"
  )
  testthat::expect_false("--output-dir" %in% captured_args)
  testthat::expect_false(
    file.exists(file.path(dirname(report_file), "2024_HAIICU_Report.docx"))
  )
})

testthat::test_that("render_report rejects stale existing output", {
  withr::local_tempdir()

  report_file <- file.path(tempdir(), "report.qmd")
  output_dir <- file.path(tempdir(), "reports")
  output_file <- file.path(output_dir, "2024_HAIICU_Report.docx")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("---\ntitle: Test\n---\n", report_file)
  writeLines("stale", output_file)

  testthat::local_mocked_bindings(
    hicu_run_quarto = function(args, quiet = TRUE, work_dir = NULL) {
      0L
    },
    .package = "HAINETICU"
  )

  testthat::expect_error(
    render_report(
      year = "2024",
      output_format = "docx",
      report_file = report_file,
      output_dir = output_dir
    ),
    "Quarto render failed"
  )
  testthat::expect_false(file.exists(output_file))
})

testthat::test_that("render_report moves Quarto project site output", {
  withr::local_tempdir()

  report_dir <- file.path(tempdir(), "reports")
  report_file <- file.path(report_dir, "report.qmd")
  output_dir <- file.path(tempdir(), "html_AER")
  site_dir <- file.path(report_dir, "_site")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("---\ntitle: Test\n---\n", report_file)

  testthat::local_mocked_bindings(
    hicu_run_quarto = function(args, quiet = TRUE, work_dir = NULL) {
      output_file <- args[[which(args == "--output") + 1L]]
      file.create(file.path(site_dir, output_file))
      0L
    },
    .package = "HAINETICU"
  )

  rendered <- render_report(
    year = "2024",
    output_format = "docx",
    report_file = report_file,
    output_dir = output_dir
  )

  testthat::expect_true(file.exists(rendered))
  testthat::expect_identical(dirname(rendered), normalizePath(output_dir, winslash = "/"))
  testthat::expect_false(file.exists(file.path(site_dir, "2024_HAIICU_Report.docx")))
})
