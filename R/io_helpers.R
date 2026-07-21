# Input/output helpers --------------------------------------------------

read_data_csv <- function(file, data_dir = get0("DATA_DIR", ifnotfound = "."), ...) {
  utils::read.csv(file.path(data_dir, file), ...)
}

read_data_fread <- function(file, data_dir = get0("DATA_DIR", ifnotfound = "."), ...) {
  data.table::fread(file.path(data_dir, file), ...)
}

read_output_rds <- function(file,
                            dir = get0("OUTPUT_DIR", ifnotfound = "."),
                            envir = new.env(),
                            ...) {
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  tryCatch(
    readRDS(path, ...),
    error = function(e) {
      vars <- load(path, envir = envir)
      if (length(vars) == 1) {
        return(envir[[vars]])
      }
      mget(vars, envir = envir)
    }
  )
}

hicu_read_output_object <- function(file, output_dir, envir = new.env(), ...) {
  path <- file.path(output_dir, file)

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  tryCatch(
    readRDS(path, ...),
    error = function(e) {
      vars <- load(path, envir = envir)
      if (length(vars) == 1) {
        return(envir[[vars]])
      }
      mget(vars, envir = envir)
    }
  )
}

load_output_rds <- function(file, output_dir = get0("OUTPUT_DIR", ifnotfound = "."), ...) {
  envir <- new.env()
  load(file.path(output_dir, file), envir = envir)
  get(ls(envir)[1], envir = envir)
}

save_output <- function(..., file, output_dir = get0("OUTPUT_DIR", ifnotfound = ".")) {
  save(..., file = file.path(output_dir, file))
}

save_output_rds <- function(object,
                            file,
                            output_dir = get0("OUTPUT_DIR", ifnotfound = "."),
                            ...) {
  saveRDS(object, file = file.path(output_dir, file), ...)
}