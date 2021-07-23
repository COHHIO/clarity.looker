# Supporting functions ----
# Mon Jul 19 16:05:32 2021

#' @title Retrieve the HUD Export item file path on disk
#' @description Get the full file path for a HUD Export item given a directory `path`
#' @param x \code{(character)} The HUD CSV Export item name
#' @param path \code{(character)} The directory path in which to search
#' @section Export_Items:
#' Available HUD Export Items are
#' \itemize{
#'   \item{Affiliation}
#'   \item{Assessment}
#'   \item{AssessmentQuestions}
#'   \item{AssessmentResults}
#'   \item{Client}
#'   \item{CurrentLivingSituation}
#'   \item{Disabilities}
#'   \item{EmploymentEducation}
#'   \item{Enrollment}
#'   \item{EnrollmentCoC}
#'   \item{Event}
#'   \item{Exit}
#'   \item{Export}
#'   \item{Funder}
#'   \item{HealthAndDV}
#'   \item{IncomeBenefits}
#'   \item{Inventory}
#'   \item{Organization}
#'   \item{Project}
#'   \item{ProjectCoC}
#'   \item{Services}
#'   \item{User}
#'   \item{YouthEducationStatus}
#' }
#' @return \code{(character)} The full file path(s)
#' @export

hud_filename <- function(x, path = "data") {
  .file <- list.files(path, pattern = x, full.names = TRUE, recursive = FALSE)
  purrr::when(.file,
              rlang::is_empty(.) ~ stop(x, ": file not found. Please retrieve full dataset."),
              length(.) > 1 ~ stop("Found:\n", paste0(basename(.file), collapse = "\n", "\n"),"Please check ",path," to ensure only a single file with name ",x," is present"))
  .file
}

#' @title Gather last updated times for on-disk files
#' @description Check the last modified time for the hud_exports specified
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @return \code{(POSIXct)} Last modified time
#' @export

hud_last_updated <- function(x, path = "data") {
  file.info(hud_filename(x, path))$mtime
}



#' @title Load a HUD Export item from disk
#' @description Load the named HUD Export item from the `path` provided
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @return \code{(POSIXct)} Last modified time
#' @export

hud_load <- function(x, path = "data") {
  .file <- hud_filename(x, path)
  .ext <- .mode(stringr::str_extract(.file, "(?<=\\.)[A-Za-z]+$"))
  import_fn <- switch(.ext,
                      csv = readr::read_csv,
                      feather = feather::read_feather)
  .args <- list(.file)
  if (.ext == "csv")
    .args$col_types <- .hud_export[[x]]$col_types

  do.call(import_fn, .args)
}


hud_rename_strings <- function(x) {
  x %>%
    stringr::str_replace_all("(?<!a)[Ii][Dd]$", "ID") %>%
    stringr::str_remove("^Enrollment ") %>%
    stringr::str_replace_all("[Cc][Oo][Cc]", "CoC") %>%
    stringr::str_replace_all("^[Zz][Ii][Pp]$", "ZIP") %>%
    stringr::str_replace_all("(?<=rk)p(?=lace)", "P")
}



hud_rename <- function(x, .nm) {
  if (is.null(x))
    return(NULL)
  x %>%
    dplyr::rename_with(.fn = ~ {
      # All column names are prefixed with the HUD CSV Export BETA report name from Looker - with spaces between capitalized words. This is removed
      out <-
        trimws(stringr::str_remove(.x, stringr::fixed(paste0(.nm, " ")))) %>%
        hud_rename_strings()


      if (all(is.na(out)))
        out <- .x
      out
    })
}

#' @title Write object to the *data* directory as feather file
#' @description Writes a \code{tibble/data.frame} as a feather file to the path
#' @param .data \code{(tibble/data.frame)} The object to write to feather
#' @param path \code{(character vector)} A character vector of the directory path to be passed to \link[base]{file.path}. If the filename is appended IE "Export.feather", the file will be named as such. Otherwise the name of the `.data` object will be used IE, if an object named `Affiliation` containing the Affiliation data is passed to `.data`, the resulting file will be [path]/Affiliation.feather.
#' @return A success message at the console
#' @export

hud_feather <- function(.data, path = "data") {
  fn <-
    rlang::exec(file.path,
                !!!path,
                !!!ifelse(
                  stringr::str_detect(path, "feather$"),
                  path,
                  paste0(deparse(rlang::enexpr(.data)), ".feather")
                ))
  feather::write_feather(.data, fn)
  cli::cli_alert_success(paste0(fn, " saved"))
}


