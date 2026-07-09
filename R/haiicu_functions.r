# This file contains functions used in the analysis of the HAIICU data.

#function definitions --------
prc<-function(x){return(100*round(x,digits=3))}
pct<-function(x,tot){prc(sum(x,na.rm=TRUE)/tot)}
unfactor<-function(x){as.numeric(as.character(x))}
p25<-function(x){quantile(x,c(0.25),na.rm=TRUE)}
p75<-function(x){quantile(x,c(0.75),na.rm=TRUE)}


read_data_csv <- function(file, ...) read.csv(file.path(DATA_DIR, file), ...)
read_data_fread <- function(file, ...) data.table::fread(file.path(DATA_DIR, file), ...)
read_output_rds <- function(file, dir = OUTPUT_DIR, envir = new.env(), ...) {
  path <- file.path(dir, file)
  if (!file.exists(path)) stop("File not found: ", path)

  tryCatch(
    readRDS(path, ...),
    error = function(e) {
      vars <- load(path, envir = envir)
      if (length(vars) == 1) {
        return(envir[[vars]])
      }
      return(mget(vars, envir = envir))
    }
  )
}
load_output_rds <- function(file, ...) {  
  e <- new.env()  
  load(file.path(OUTPUT_DIR, file), envir = e) 
   get(ls(e)[1], envir = e)
   }
save_output <- function(..., file) save(..., file = file.path(OUTPUT_DIR, file))
save_output_rds <- function(object, file, ...) saveRDS(object, file = file.path(OUTPUT_DIR, file), ...)

parse_mixed_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  # Try ISO first, then dd/mm/yyyy
  d1 <- as.Date(x, format = "%Y-%m-%d")
  d2 <- as.Date(x, format = "%d/%m/%Y")
  
  coalesce(d1, d2)
}

#top 10 microorganism by country tables function -----
build_top10_country_tables <- function(df,
                                       isolate_col = "Isolate",
                                       total_col = "total",
                                       totalpc_col = "totalpc",
                                       country_name_lut = c(
                                         AT = "Austria", BE = "Belgium", CZ = "Czech Republic",
                                         DE = "Germany", EE = "Estonia", ES = "Spain",
                                         FR = "France", HU = "Hungary", IT = "Italy",
                                         LT = "Lithuania", LU = "Luxembourg", MT = "Malta",
                                         PL = "Poland", PT = "Portugal", RO = "Romania",
                                         SK = "Slovakia", UK = "United Kingdom",
                                         "IT-SPIN-UTI" = "Italy-SPIN-UTI",
                                         "IT-GiViTI" = "Italy-GiViTI",
                                         ITSPINUTI = "Italy-SPIN-UTI",
                                         ITGiViTI = "Italy-GiViTI"
                                       )) {

  # Numeric country count columns (exclude totals and percentage columns)
  country_cols <- df %>%
    dplyr::select(where(is.numeric), -dplyr::any_of(c(total_col, totalpc_col)), -dplyr::matches("pc$")) %>%
    names()

  # Percentage columns
  country_pc_cols <- df %>%
    dplyr::select(dplyr::matches("pc$"), -dplyr::any_of(totalpc_col)) %>%
    names()

  summary_tbl <- df %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(country_cols), ~ sum(.x, na.rm = TRUE))) %>%
    dplyr::mutate(total = rowSums(dplyr::across(dplyr::all_of(country_cols)), na.rm = TRUE))

  totals_tbl <- df %>%
    dplyr::select(dplyr::all_of(country_cols), dplyr::any_of(total_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"))

  pc_tbl <- df %>%
    dplyr::select(dplyr::all_of(isolate_col), dplyr::all_of(country_pc_cols), dplyr::any_of(totalpc_col)) %>%
    dplyr::mutate(dplyr::across(-dplyr::all_of(isolate_col), ~ tidyr::replace_na(.x, 0)))

  # Rename % columns to full country names
  pc_keys <- sub("pc$", "", country_pc_cols)
  display_names <- ifelse(
    is.na(country_name_lut[pc_keys]),
    pc_keys,
    unname(country_name_lut[pc_keys])
  )

  names(pc_tbl) <- c(
    isolate_col,
    display_names,
    if (totalpc_col %in% names(pc_tbl)) "total"
  )

  list(
    summary = summary_tbl,
    totals = totals_tbl,
    pc = pc_tbl,
    country_cols = country_cols,
    country_pc_cols = country_pc_cols
  )
}
