#' Run the HAIICU cleaning workflow
#'
#' This transitional wrapper executes the extracted functional core when
#' `write_outputs = FALSE` and the existing production script when historical
#' output files should be written.
#'
#' @param year Reporting year. Defaults to `HAINET_YEAR` or "2023".
#' @param data_dir Raw data directory. Defaults to data/raw/<year>.
#' @param output_dir Output directory. Defaults to outputs/<year>.
#' @param write_outputs Logical flag kept for API compatibility.
#' @param script_path Path to the current legacy production script.
#'
#' @return A named list with `year`, `data_dir`, `output_dir`, and `script_path`.
#' @export
run_haiicu_workflow <- function(
    year = hicu_default_year(),
    data_dir = hicu_default_data_dir(year),
    output_dir = hicu_default_output_dir(year),
    write_outputs = TRUE,
    script_path = NULL
) {
  if (!isTRUE(write_outputs)) {
    inputs <- load_haiicu_inputs(year = year, data_dir = data_dir)
    outputs <- build_core_workflow_outputs(inputs)

    return(c(
      list(
        year = year,
        data_dir = normalizePath(data_dir, winslash = "/", mustWork = FALSE),
        output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
        script_path = if (is.null(script_path)) {
          NA_character_
        } else {
          normalizePath(script_path, winslash = "/", mustWork = FALSE)
        }
      ),
      outputs
    ))
  }

  if (is.null(script_path)) {
    script_path <- file.path(
      hicu_find_project_root(), "scripts", "legacy", "haiicu_cleaning.R"
    )
  }

  if (!file.exists(script_path)) {
    stop("Workflow script not found: ", script_path, call. = FALSE)
  }

  old_year <- Sys.getenv("HAINET_YEAR", unset = "")
  on.exit(Sys.setenv(HAINET_YEAR = old_year), add = TRUE)

  Sys.setenv(HAINET_YEAR = year)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  workflow_env <- new.env(parent = parent.frame())
  workflow_env$DATA_DIR <- data_dir
  workflow_env$OUTPUT_DIR <- output_dir

  sys.source(script_path, envir = workflow_env)

  incidence_tables <- NULL
  if (exists("haiicuall2", envir = workflow_env, inherits = FALSE)) {
    incidence_tables <- build_incidence_tables(get("haiicuall2", envir = workflow_env, inherits = FALSE))
  }

  list(
    year = year,
    data_dir = normalizePath(data_dir, winslash = "/", mustWork = FALSE),
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    script_path = normalizePath(script_path, winslash = "/", mustWork = TRUE),
    incidence_tables = incidence_tables
  )
}
