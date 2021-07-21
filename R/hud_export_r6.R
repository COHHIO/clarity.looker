
# Supporting functions ----
# Mon Jul 19 16:05:32 2021
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

call_csv <- function(look_type = "disk", write = FALSE) {
  fetch(deparse(match.call()[[1]][[3]]),
        look_type,
        write,
        self$.__enclos_env__)
}


#' @title Call HUD Export Items from the Clarity Looker API
#' @description Calls the Clarity Looker HUD CSV Export  (BETA) API to return to the HUD Export Items on various time ranges via pre-constructed Looks.
#' @export
hud_export <- R6::R6Class(
  "hud_export",
  public = rlang::exec(
    rlang::list2,!!!purrr::map(.hud_export, ~ call_csv),
    #' @description initialize the Looker API connection given the path to the ini configuration file.
    #' @param configFile \code{(character)} Path to the Looker *.ini* configuration file. Only the directory path is needed if the file is entitled *Looker.ini*
    initialize = function(configFile) {
      self$sdk <- lookr::LookerSDK$new(configFile = ifelse(
        stringr::str_detect(configFile, "ini$"),
        file.path(configFile),
        file.path(configFile, "Looker.ini")
      ))
    },
    #' @description Close the Looker API Connection
    close = function() {
      self$sdk$on_connection_closed()
    }
  ),
  lock_objects = FALSE,
  private = list(item = .hud_export),
)


#' @title Retrieve data from disk or the API
#' @description Determines the appropriate location from which to retrieve HUD Export data
#' @param x \code{(character)} The HUD Export item to retrieve. One of:
#' \itemize{
#'   \item{Affiliation}
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
#' }
#' @param look_type \code{(character)} The look type to retrieve. One of:
#' \itemize{
#'   \item{year2}{Two Complete Years}
#'   \item{s2020}{Since the beginning of 2020}
#'   \item{daily}{Created or updated in the last complete day}
#' }
#' @param write \code{(logical)} Whether to write the raw data from the API and the renamed data to the data/API folder
#' @return \code{(tibble)} The HUD Export item requested.

fetch <- function(x,
                  look_type,
                  write = FALSE,
                  ee) {
  .y <- x
  .x <- ee$private$item[[x]]
  .nm <- .x$api_nm %||% .y

  if (look_type == "disk")
    .data <- try(feather::read_feather(file.path("data", "API", paste0(.y, ".feather"))), silent = TRUE)

  .data_error <- inherits(.data, "try-error")
  if (.data_error || look_type == "daily") {
    if (.data_error && look_type == "disk")
      look_type <- "s2020"
    message(.y, ": fetching data")
    if (is.null(.x$look[look_type]))
      return(NULL)
    # Rename col_types to match the way they appear coming from the API
    names(.x$col_types) <- paste0(.nm, " ", names(.x$col_types)) %>%
      stringr::str_replace_all("(?<!a)[I][D]$", "Id")
    .data <-
      ee$self$sdk$runLook(.x$look[look_type],
                          "csv",
                          as = "parsed",
                          col_types = .x$col_types)
    message(.y, ": data retrieved")
    if (nrow(.data) %in% c(0, 500))
      stop(.y, " row count is ", nrow(.data))
  }

  if (any(stringr::str_detect(names(.data), paste0("^", .nm, "\\s")))) {
    .data <- hud_rename(.data, .nm)
  }

  if (write && look_type != "disk") {
    fp <- file.path("data", "API", paste0(.y, ".feather"))
    feather::write_feather(.data, fp)
    message(.y, ": data written to ", fp)
  }

  return(.data)
}

#' @title Write object to the *data* directory
#' @description Writes a \code{tibble/data.frame} as a feather file to the *data* directory using the name of the object as the file name.
#' @param x \code{(tibble/data.frame)} The object to write to feather
#' @param path \code{(character vector)} A character vector of the directory path to be passed to \link[base]{file.path}
#' @return A success message at the console
#' @export
to_feather <- function(x, path = "data") {
  fn <-
    rlang::exec(file.path,
                !!!path,
                !!!ifelse(
                  stringr::str_detect(path, "feather$"),
                  path,
                  paste0(deparse(rlang::enexpr(x)), ".feather")
                ))
  feather::write_feather(x, fn)
  cli::cli_alert_success(paste0(fn, " saved"))
}
