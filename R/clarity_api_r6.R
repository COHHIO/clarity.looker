


#' @title Retrieve data from disk or the API
#' @description Determines the appropriate location from which to retrieve HUD Export data
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @param look_type \code{(character)} The look type to retrieve. One of:
#' \itemize{
#'   \item{year2}{Two Complete Years}
#'   \item{since2020}{Since the beginning of 2020}
#'   \item{daily}{Created or updated in the last complete day}
#' }
#' @param write \code{(logical)} Whether to write the raw data from the API and the renamed data to the data/API folder
#' @return \code{(tibble)} The HUD Export item requested.

call_data <-
  function(look_type = "disk",
           path = self$dirs$export,
           .write = FALSE,
           details = FALSE) {
    .data_nm <- hud_formatted(deparse(match.call()[[1]][[3]]))
    # For extras, spdats, public data entities amend the path based on the function name
    .is_export <- .data_nm %in% names(.hud_export)
    if (!.is_export)
      path = self$dirs[[stringr::str_extract(.data_nm, paste0(paste0(
        "(?<=\\_)", purrr::map_chr(dirs[-1], basename), "$"
      ), collapse = "|"))]]
    fetch(.data_nm,
          look_type,
          path,
          .write,
          details,
          self$.__enclos_env__)
  }


update_data <-
  function(x,
           look_type = "daily",
           path = self$dirs$export,
           .write = TRUE) {
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
      modifications <-
        purrr::map2_lgl(new_data$DateCreated,
                        new_data$DateUpdated,
                        ~ !identical(.x, .y))
      if (any(modifications)) {
        #TODO Merge based on ProjectID, ClientID, EnrollmentID, Etc
        row_matches <- slider::slide_dbl(new_data[modifications, ], ~ {
          knn_row(.x, old_data)
        })
        old_data[row_matches, ] <- new_data[modifications, ]
      }

      updated_data <-
        dplyr::distinct(dplyr::bind_rows(old_data, new_data[!modifications, ]))
      if (.write && !identical(old_data, updated_data)) {
        hud_feather(updated_data, file.path(path, paste0(x, ".feather")))
      }
    } else {
      updated_data <- old_data
    }
    return(updated_data)
  }

dirs <- list(
  export = "data/export",
  public = "data/public",
  spm = "data/spm",
  extras = "data/extras"
)

# Use dput on this and copy to the R6 class otherwise utils::getSrcref will bug in roxygen2
.init <- rlang::new_function(
  body = base::quote({
    self$api <- lookr::LookerSDK$new(configFile = ifelse(
      stringr::str_detect(configFile, "ini$"),
      file.path(configFile),
      file.path(configFile, "Looker.ini")
    ))
    self$dirs <- dirs
    rlang::env_bind(self,!!!purrr::map(.hud_export, ~ private$call_data))
    rlang::env_bind(self,!!!purrr::map(.hud_extras, ~ private$call_data))
    self$update = rlang::list2(
      !!!purrr::map(.hud_export, ~ private$update_data),!!!purrr::map(.hud_extras, ~ private$update_data)
    )
  }),
  args = rlang::pairlist2(configFile = , dirs = rlang::expr(!!dirs))
)


fetch <- function(x,
                  look_type,
                  path,
                  .write = FALSE,
                  details = FALSE,
                  ee) {
  .x <- ee$private$item[[x]]
  .nm <- .x$api_nm %||% x


  if (details || stringr::str_detect(x, "extras$")) {
    if (missing(look_type) || look_type == "disk") {
      .look_type = "since2019"
    } else {
      .look_type = look_type
    }


    .look_type <- purrr::when(.look_type,
                is.character(.) ~ .x$look[.look_type],
                ~ .look_type)
    .description <-
      ee$self$api$getLook(.look_type)
  }


  if (!details) {
    if (look_type == "disk" && !.write) {
      .data <- try(hud_load(x, path), silent = TRUE)
    }


    .data_error <-
      inherits(get0(".data", inherits = FALSE), c("try-error", "NULL"))
    if (.data_error || look_type == "daily") {
      if (.data_error && look_type == "disk")
        look_type <- "since2019"
      if (is.null(.x$look[look_type]))
        return(NULL)
      message(x, ": fetching data")
      .look_args <- list(.x$look[look_type],
                         "csv",
                         as = "parsed")
      # Rename col_types to match the way they appear coming from the API
      if (!is.null(names(.x$col_types)) && stringr::str_detect(x, "extras$", negate = TRUE)) {
        names(.x$col_types) <- paste0(.nm, " ", names(.x$col_types))
        if (!x %in% c("Services", "Client", "Enrollment")) {
          # Add both versions of ID columns (ID/Id) since it is non uniform
          .is_id <- .x$col_types %>%
            {stringr::str_detect(names(.), "[Ii][Dd]$")}
          .x$col_types <- c(.x$col_types,
                            .x$col_types[.is_id] %>% {setNames(., nm = stringr::str_replace_all(names(.), "(?<!a)[I][D]$", "Id"))})
        }

      } else if (stringr::str_detect(x, "extras$")) {

        .look_args$col_names <- stringr::str_split(.description$description,  "\\,\\s")[[1]]
        .look_args$col_types <- purrr::map_chr(stringr::str_subset(.look_args$col_names, "ID$") |> {\(x) {setNames(x, x)}}(), ~ "c")
        .look_args$skip <- 1
      }

      .look_args$col_types <-  .x$col_types
      .data <-
        do.call(ee$self$api$runLook, .look_args)
      if (names(.data)[1] == "message")
        stop(purrr::imap_chr(.data, ~ paste0(x, ": ", .x, "\n")))
      message(x, ": data retrieved")
      if (nrow(.data) %in% c(0, 500, 5000) && look_type != "daily")
        stop(x,
             " row count is ",
             nrow(.data),
             ". Row limits could be limiting data.")
    }

    if (any(stringr::str_detect(names(.data), paste0("^", .nm, "\\s")))) {
      .data <- hud_rename(.data, .nm)
    }

    if (.write && look_type != "disk") {
      if (stringr::str_detect(path, "feather$", negate = TRUE)) {
        fp <- file.path(path, paste0(x, ".feather"))
      } else {
        fp <- path
      }
      hud_feather(.data, fp)
    }
  } else {
    .data <- .description
  }


  return(.data)
}





#' @title Call HUD Export Items & Extras from the Clarity Looker API
#' @description Calls the Clarity Looker API HUD CSV Export  (BETA) & LookML models to return to the HUD Export Items & other datasets on various time ranges via pre-constructed Looks. See `?fetch` for details on using all data methods.
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @param look_type \code{(character)} The look type to retrieve. One of:
#' \itemize{
#'   \item{year2}{Two Complete Years}
#'   \item{since2020}{Since the beginning of 2020}
#'   \item{daily}{Created or updated in the last complete day}
#' }
#' @param write \code{(logical)} Whether to write the raw data from the API and the renamed data to the data/API folder
#' @include hud_export.R
#' @include hud_extras.R
#' @export
clarity_api <- R6::R6Class(
  "clarity_api",
  lock_objects = FALSE,
  public = rlang::exec(
    rlang::list2,
    #' @description Pull all Export items with associate Looks
    #' @inheritParams hud_filename
    #' @param skip \code{(character)} of items to skip
    get_export = function(path = self$dirs$export, .write = TRUE, skip = c("Assessment",
                                                            "AssessmentQuestions",
                                                            "AssessmentResults",
                                                            "Services",
                                                            "YouthEducationStatus")) {
      if (!dir.exists(path))
        UU::mkpath(path)
      to_fetch <- names(.hud_export) %>% {
        .[!. %in% skip]
      }
      purrr::walk(to_fetch, ~ rlang::eval_bare(rlang::expr(self[[!!.x]](
        path = path,
        .write = .write
      ))))
    },
    #' @description Run daily update for all HUD Export items on disk
    #' @inheritParams hud_filename
    #' @param skip \code{(character)} of items to skip
    update_export = function(path = self$dirs$export,
                             skip = c("Assessment",
                                      "AssessmentQuestions",
                                      "AssessmentResults",
                                      "Services",
                                      "YouthEducationStatus")) {
      to_update <- names(.hud_export) %>% {
        .[!. %in% skip]
      }
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
          "Services",
          "Exit"),
        ~ hud_last_updated(.x, path = path) >= Sys.Date()
      ))
    },
    #' @description Pull all Extra items with associate Looks
    #' @inheritParams hud_filename
    get_extras = function(look_type = "since2019",path = self$dirs$extras, .write = TRUE) {
      if (!dir.exists(path))
        UU::mkpath(path)
      purrr::iwalk(.hud_extras, ~ rlang::eval_bare(rlang::expr(self[[!!.y]](
        look_type = look_type,
        path = path,
        .write = .write
      ))))
    },
    #' @description Run daily update for all HUD Export items on disk
    #' @inheritParams hud_filename
    update_extras = function(path = self$dirs$extras,
                             skip = NULL) {
      to_update <- names(.hud_extras) %>% {
        .[!. %in% skip]
      }
      purrr::walk(to_update, ~ rlang::eval_bare(rlang::expr(
        self$update[[!!.x]](
          path = path,
          .write = TRUE,
          self = self
        )
      )))

      all(purrr::map_lgl(
        c(
          "Client_extras",
          "Project_extras",
          "Enrollment_extras",
          "Services_extras"
        ),
        ~ hud_last_updated(.x, path = path) >= Sys.Date()
      ))
    },


    #' @description initialize the Looker API connection given the path to the ini configuration file.
    #' @param configFile \code{(character)} Path to the Looker *.ini* configuration file. Only the directory path is needed if the file is entitled *Looker.ini*
    #' @param dirs \code{(named list)} of default directory paths for where to store the feather files for the following data types:
    #' \itemize{
    #'   \item{\code{export}}{ The HUD Export items **Default**: *data/export*}
    #'   \item{\code{public}}{ The public items **Default**: *data/public*}
    #'   \item{\code{spm}}{ The SPM items **Default**: *data/spm*}
    #'   \item{\code{extras}}{ The HUD Extras (custom items) **Default**: *data/extras*}
    #' }
    #' This is optional and the path can be provided to individual methods as needed.

    initialize = function (configFile,
                           dirs = list(
                             export = "data/export",
                             public = "data/public",
                             spm = "data/spm",
                             extras = "data/extras"
                           ))
    {
      self$api <-
        lookr::LookerSDK$new(configFile = ifelse(
          stringr::str_detect(configFile,
                              "ini$"),
          file.path(configFile),
          file.path(configFile,
                    "Looker.ini")
        ))
      self$dirs <- dirs
      rlang::env_bind(self,!!!purrr::map(.hud_export, ~ private$call_data))
      rlang::env_bind(self,!!!purrr::map(.hud_extras, ~ private$call_data))
      self$update = rlang::list2(
        !!!purrr::map(.hud_export, ~ private$update_data),!!!purrr::map(.hud_extras, ~
                                                                          private$update_data)
      )
    },
    #' @description Close the Looker API Connection
    close = function() {
      self$api$on_connection_closed()
    }
  ),
  private = rlang::list2(
    item = rlang::list2(!!!.hud_export,!!!.hud_extras),
    call_data = call_data,
    update_data = update_data
  )
)


