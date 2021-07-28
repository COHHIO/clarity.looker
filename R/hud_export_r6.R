
call_data <- function(look_type = "disk", path = file.path("data"), .write = FALSE) {
  fetch(hud_formatted(deparse(match.call()[[1]][[3]])),
        look_type,
        path,
        .write,
        self$.__enclos_env__)
}

update_data <- function(x, look_type = "daily", path = "data", .write = TRUE, self) {
  x <- hud_formatted(deparse(match.call()[[1]][[3]]))
  last_updated <- hud_last_updated(x, path)
  old_data <- hud_load(x, path)
  do_update <- last_updated < Sys.Date()
  if (do_update) {
    message(x, " last updated: ", last_updated, ". Updating...")
    new_data <- fetch(x,
                      look_type,
                      path,
                      .write = FALSE,
                      self$.__enclos_env__)
    modifications <- purrr::map2_lgl(new_data$DateCreated, new_data$DateUpdated, ~!identical(.x,.y))
    if (any(modifications)) {
      #TODO Merge based on ProjectID, ClientID, EnrollmentID, Etc
      row_matches <- slider::slide_dbl(new_data[modifications,], ~{
        knn_row(.x, old_data)
      })
      old_data[row_matches,] <- new_data[modifications,]
    }

    updated_data <- dplyr::distinct(dplyr::bind_rows(old_data, new_data[!modifications,]))
    if (.write) {
      hud_feather(updated_data, x)
    }
  } else {
    updated_data <- old_data
  }
  return(updated_data)
}

#' @title Retrieve data from disk or the API
#' @description Determines the appropriate location from which to retrieve HUD Export data
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
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
                  path,
                  .write = FALSE,
                  ee) {
  .y <- x
  .x <- ee$private$item[[x]]
  .nm <- .x$api_nm %||% .y

  if (look_type == "disk") {
    .data <- try(hud_load(x, path), silent = TRUE)
  }


  .data_error <- inherits(get0(".data", inherits = FALSE), c("try-error", "NULL"))
  if (.data_error || look_type == "daily") {
    if (.data_error && look_type == "disk")
      look_type <- "since2019"
    message(.y, ": fetching data")
    if (is.null(.x$look[look_type]))
      return(NULL)
    # Rename col_types to match the way they appear coming from the API
    names(.x$col_types) <- paste0(.nm, " ", names(.x$col_types))
    if (.y != "Services")
      names(.x$col_types) <- names(.x$col_types) %>%
      stringr::str_replace_all("(?<!a)[I][D]$", "Id")
    .data <-
      ee$self$api$runLook(.x$look[look_type],
                          "csv",
                          as = "parsed",
                          col_types = .x$col_types)
    if (names(.data)[1] == "message")
      stop(purrr::imap_chr(.data, ~paste0(.y,": ",.x, "\n")))
    message(.y, ": data retrieved")
    if (nrow(.data) %in% c(0, 500))
      stop(.y, " row count is ", nrow(.data))
  }

  if (any(stringr::str_detect(names(.data), paste0("^", .nm, "\\s")))) {
    .data <- hud_rename(.data, .nm)
  }

  if (.write && look_type != "disk") {
    fp <- file.path(path, paste0(.y, ".feather"))
    hud_feather(.data, fp)
  }

  return(.data)
}

#' @title Call HUD Export Items from the Clarity Looker API
#' @description Calls the Clarity Looker HUD CSV Export  (BETA) API to return to the HUD Export Items on various time ranges via pre-constructed Looks.
#' @export
hud_export <- R6::R6Class(
  "hud_export",
  public = rlang::exec(
    rlang::list2,
    !!!purrr::map(.hud_export, ~ call_data),
    update = rlang::list2(!!!purrr::map(.hud_export, ~ update_data)),
    #' @description Pull all Export items with associate Looks
    #' @inheritParams hud_filename
    get_all = function(path) {
      if (!dir.exists(path))
        file_path_create(path)
      purrr::iwalk(.hud_export, ~rlang::eval_bare(rlang::expr(
        self[[!!.y]](
          path = path,
          .write = TRUE
        )
      )))
    },
    #' @description Run daily update for all HUD Export items on disk
    #' @inheritParams hud_filename
    update_all = function(path = "data",
                          skip = c(
                            "Assessment",
                            "AssessmentQuestions",
                            "AssessmentResults",
                            "Services",
                            "User",
                            "YouthEducationStatus"
                          )) {
      to_update <- names(.hud_export) %>% {.[!. %in% skip]}
      purrr::walk(to_update, ~ rlang::eval_bare(rlang::expr(
        self$update[[!!.x]](
          path = path,
          .write = TRUE,
          self = self
        )
      )))

      all(purrr::map_lgl(
        c("Client",
          "Enrollment",
          "Export",
          #"Services",
          "Exit"),
        ~ hud_last_updated(.x, path = path) >= Sys.Date()
      ))
    },

    #' @description initialize the Looker API connection given the path to the ini configuration file.
    #' @param configFile \code{(character)} Path to the Looker *.ini* configuration file. Only the directory path is needed if the file is entitled *Looker.ini*
    initialize = function(configFile) {
      self$api <- lookr::LookerSDK$new(configFile = ifelse(
        stringr::str_detect(configFile, "ini$"),
        file.path(configFile),
        file.path(configFile, "Looker.ini")
      ))
    },
    #' @description Close the Looker API Connection
    close = function() {
      self$api$on_connection_closed()
    }
  ),
  lock_objects = FALSE,
  private = list(item = .hud_export),
)


