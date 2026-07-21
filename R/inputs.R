#' HAIICU raw input filenames
#'
#' @return Named character vector of expected raw TESSy export filenames.
#' @export
haiicu_input_files <- function() {
  c(
    unit = "1.HAIICU.csv",
    unit_light = "1.HAIICULIGHT.csv",
    denominator = "2.HAIICU$Denom.csv",
    patient = "2.HAIICU$Pt.csv",
    denominator_light = "2.HAIICULIGHT$Deno.csv",
    indicator = "3.HAIICU$Denom$Ind.csv",
    antimicrobial = "3.HAIICU$Pt$Am.csv",
    exposure = "3.HAIICU$Pt$Exp.csv",
    infection = "3.HAIICU$Pt$Inf.csv",
    indicator_light = "3.HAIICULIGHT$Deno$Ind.csv",
    infection_light = "3.HAIICULIGHT$Deno$Inf.csv",
    resistance = "4.HAIICU$Pt$Inf$Res.csv",
    resistance_light = "4.HAIICULIGHT$Deno$Inf$Res.csv"
  )
}

#' Load HAIICU raw input tables
#'
#' @param year Reporting year. Defaults to `HAINET_YEAR` or "2023".
#' @param data_dir Raw data directory. Defaults to `data/raw/<year>`.
#' @param input_files Named character vector of expected raw filenames.
#' @param reader Function used to read each CSV file.
#' @param optional_inputs Names of input files allowed to be missing.
#' @param ... Additional arguments passed to `reader`.
#'
#' @return Named list of input tables.
#' @export
load_haiicu_inputs <- function(
    year = hicu_default_year(),
    data_dir = hicu_default_data_dir(year),
    input_files = haiicu_input_files(),
    reader = utils::read.csv,
    optional_inputs = character(),
    ...
) {
  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir, call. = FALSE)
  }

  missing_inputs <- names(input_files)[
    !file.exists(file.path(data_dir, input_files)) &
      !names(input_files) %in% optional_inputs
  ]

  if (length(missing_inputs) > 0) {
    stop(
      "Missing required input files: ",
      paste(input_files[missing_inputs], collapse = ", "),
      call. = FALSE
    )
  }

  loaded <- lapply(names(input_files), function(input_name) {
    path <- file.path(data_dir, input_files[[input_name]])
    if (!file.exists(path)) {
      return(NULL)
    }
    reader(path, ...)
  })

  names(loaded) <- names(input_files)
  loaded[!vapply(loaded, is.null, logical(1))]
}