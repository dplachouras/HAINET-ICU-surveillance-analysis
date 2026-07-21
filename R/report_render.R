#' Normalize report output format
#'
#' @param output_format User-provided output format.
#' @return One of "html" or "docx".
#' @keywords internal
normalize_report_format <- function(output_format = "html") {
  format_map <- c(
    "html" = "html",
    "html_document" = "html",
    "word" = "docx",
    "word_document" = "docx",
    "docx" = "docx"
  )

  key <- tolower(trimws(output_format))
  normalized <- unname(format_map[key])

  if (length(normalized) == 0 || is.na(normalized)) {
    stop(
      "Unsupported output format '", output_format,
      "'. Use one of: html, html_document, docx, word, word_document.",
      call. = FALSE
    )
  }

  normalized
}

hicu_run_quarto <- function(args, quiet = TRUE, work_dir = NULL) {
  old_dir <- NULL
  if (!is.null(work_dir)) {
    old_dir <- getwd()
    setwd(work_dir)
    on.exit(setwd(old_dir), add = TRUE)
  }

  suppressWarnings(
    system2(
      "quarto",
      args = args,
      stdout = if (quiet) FALSE else "",
      stderr = if (quiet) FALSE else ""
    )
  )
}

#' Render the canonical Quarto report
#'
#' @param year Reporting year. Defaults to `HAINET_YEAR` or "2023".
#' @param output_format Output format. Supported: html, docx.
#' @param report_file Path to Quarto report file.
#' @param output_dir Directory where rendered reports are copied.
#' @param quiet Whether to suppress Quarto command output.
#'
#' @return The absolute path of the rendered report file.
#' @export
render_report <- function(
    year = hicu_default_year(),
    output_format = "html",
    report_file = file.path(hicu_find_project_root(), "reports", "haineticu-report.qmd"),
    output_dir = file.path(hicu_find_project_root(), "reports", "html_AER"),
    quiet = TRUE
) {
  if (!file.exists(report_file)) {
    stop("Report file not found: ", report_file, call. = FALSE)
  }

  format_normalized <- normalize_report_format(output_format)
  output_ext <- if (identical(format_normalized, "docx")) "docx" else "html"

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  output_file <- paste0(year, "_HAIICU_Report.", output_ext)
  render_dir <- dirname(normalizePath(report_file, winslash = "/", mustWork = TRUE))
  rendered_path <- file.path(render_dir, output_file)
  project_rendered_path <- file.path(render_dir, "_site", output_file)
  output_path <- file.path(normalizePath(output_dir, winslash = "/", mustWork = FALSE), output_file)
  if (file.exists(rendered_path)) {
    unlink(rendered_path)
  }
  if (file.exists(project_rendered_path)) {
    unlink(project_rendered_path)
  }
  if (file.exists(output_path)) {
    unlink(output_path)
  }

  args <- c(
    "render",
    basename(report_file),
    "--to",
    format_normalized,
    "--output",
    output_file
  )

  status <- hicu_run_quarto(args, quiet = quiet, work_dir = render_dir)

  rendered_candidate <- if (file.exists(rendered_path)) {
    rendered_path
  } else {
    project_rendered_path
  }

  if (!identical(status, 0L) || !file.exists(rendered_candidate)) {
    stop(
      "Quarto render failed. Ensure Quarto is installed and the report compiles: ",
      normalizePath(report_file, winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }

  if (!identical(normalizePath(dirname(rendered_candidate), winslash = "/", mustWork = TRUE), normalizePath(output_dir, winslash = "/", mustWork = FALSE))) {
    if (!file.rename(rendered_candidate, output_path)) {
      file.copy(rendered_candidate, output_path, overwrite = TRUE)
      unlink(rendered_candidate)
    }
  }

  if (!file.exists(output_path)) {
    stop(
      "Rendered report could not be moved to output directory: ",
      normalizePath(output_dir, winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }

  normalizePath(output_path, winslash = "/", mustWork = TRUE)
}
