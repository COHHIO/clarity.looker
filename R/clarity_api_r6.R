# dirs ----
# Fri Aug 20 22:31:44 2021
#' @title Default directory tree for specific data types
#' @description COHHIO specific directory tree _Recommended_
#' @export
dirs = list(
  export = "data/export",
  public = "data/public",
  spm = "data/spm",
  extras = "data/extras"
)

# Use dput on this and copy to the R6 class otherwise utils::getSrcref will bug in roxygen2
.init <- rlang::new_function(
  body = base::quote({

    self$api <-
      lookr::LookerSDK$new(configFile = ifelse(
        stringr::str_detect(configFile,
                            "ini$"),
        file.path(configFile),
        file.path(configFile,
                  "Looker.ini")
      ))
    private$folder_info <- list()
    if (UU::is_legit(export_folder)) {
      export_folder <- fetch_folder(export_folder, self)
      private$folder_info$export <- export_folder
      rlang::env_bind(self, !!!purrr::map(folder_looks(self$folders[[export_folder]]), ~ private$call_data))
    }

    if (UU::is_legit(daily_folder)) {
      daily_folder <- fetch_folder(daily_folder, self)
      private$folder_info$daily_folder <- daily_folder
    }
    if (UU::is_legit(look_folders)) {
      purrr::walk(look_folders, fetch_folder, self = self)
      # Bind additional folders
      purrr::iwalk(self$folders[look_folders], ~{
        looks <- purrr::map_chr(.x$looks, "title") |> rlang::set_names() |> purrr::map(~private$call_data)
        self[[.y]] <- rlang::list2(!!!looks)
      })
    }

    self$dirs <- dirs



  }),
  args = rlang::pairlist2(configFile = ,
                          export_folder = "HUD Export",
                          daily_folder = 9711,
                          look_folders = c("HUD Extras"),
                          dirs = rlang::expr(!!dirs))
)

api_col_types <- function(col_types, add_prefixes) {
  .is_id <-  stringr::str_detect(names(col_types), "[Ii][Dd]$")
  col_types <- col_types[.is_id] |>
    {\(x) {setNames(x, nm = stringr::str_replace_all(names(x), "(?<!a)[I][D]$", "Id"))}}() |>
    c(col_types) |>
    {\(x) {rlang::set_names(x, paste0(add_prefixes, " ", names(x)))}}()

}
#' @title Retrieve data from disk or the API
#' @description Determines the appropriate location from which to retrieve HUD Export data
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @param from_disk \code{(logical)} Attempt to retrieve the data from disk in the folder specified by `path`?
#' @param .write \code{(logical)} Whether to write a feather file to `path`
#' @param details \code{(logical)} Return look info. See [getLook](https://docs.looker.com/reference/api-and-integration/api-reference/v3.1/look#get_look)
#' @param daily_update Update the Export with the data that has been added or modified over the last 24 hour period (12a - 12p).
#' @inheritDotParams readr::read_csv
#' @return \code{(tibble)} The HUD Export item requested.

call_data <-
  function(from_disk = TRUE,
           path = self$dirs$export,
           .write = FALSE,
           details = FALSE,
           daily_update = FALSE,
           ...) {
    .data_nm <- hud_formatted(deparse(match.call()[[1]][[3]]))
    .is_export <- .data_nm %in% names(.hud_export)

    if (.is_export) {
      .idx <- private$folder_info$export
    } else if (daily_update) {
      .idx <- private$folder_info$daily
    } else {
      .idx <- hud_formatted(deparse(match.call()[[1]][[2]][[3]]))
    }
    id <- look_id_from_folder(.data_nm, self$folders[[.idx]])

    .arg_path <- deparse(rlang::enexpr(path))
    # If the file path was entered manually, don't do this
    if (!stringr::str_detect(.arg_path, stringr::fixed(.Platform$file.sep)))
      if (!.is_export) {
        # For extras, spdats, public data entities amend the path based on the function name
        path = self$dirs[[stringr::str_extract(.data_nm, paste0(paste0(
          "(?<=\\_)", purrr::map_chr(dirs[-1], basename), "$"
        ), collapse = "|"))]]
      }

    # Check if data exists and is loadable
    if (from_disk && !.write) {
      .data <- try(hud_load(.data_nm, path), silent = TRUE)
    }
    if (UU::is_legit(.data) && from_disk)
      return(.data)

    # Instantiate arguments to runLook
    .args <- list(id)
    if (details || stringr::str_detect(.data_nm, "extras$")) {
      look_info <-
        self$api$getLook(id)
      .args$col_types <- col_types_from_col_names(col_names_from_look_vis_config(look_info))
    } else if (.is_export) {
      spec <- .hud_export[[.data_nm]]
      # api_nm must be used because the API name prefix is sometimes formatted differently than the actual Export item name
      .args$col_types <- spec$col_types

    }
    .args$col_types <- rlang::exec(readr::cols, !!!as.list(.args$col_types))
    .args <- rlang::list2(!!!.args,
                          resultFormat = "csv",
                          as = "parsed"
    )
    .to_runLook <- rlang::dots_list(..., .named = TRUE)
    .args <- purrr::list_modify(.args, !!!.to_runLook)

    if (!daily_update) {
      if (!details) {

        .data_error <-
          inherits(get0(".data", inherits = FALSE), c("try-error", "NULL"))
        # if it doesn't load, update look_type to fetch again
        if (.data_error)
          from_disk <- FALSE

        # call the API
        if (!from_disk) {
          message(.data_nm, ": fetching data")
          .data <-
            rlang::exec(self$api$runLook,
                    !!!.args,
                    queryParams = list(limit = -1,
                                       apply_vis = TRUE))
          # Naming
          if (!is.null(.args$col_names)) {
            attr(.data, "api_names") <- .data[1,]
            .data <- .data[-1,]
          } else if (.is_export) {
            # If col_names not used, use hud_rename
            .data <- hud_rename(.data, spec$api_nm %||% .data_nm, names(spec$col_types))
          }
          # Error messages
          check_api_data(.data, .data_nm, daily_update)
          message(.data_nm, ": data retrieved")
        }

        if (.write) {
          if (stringr::str_detect(path, "feather$", negate = TRUE)) {
            fp <- file.path(path, paste0(.data_nm, ".feather"))
          } else {
            fp <- path
          }
          hud_feather(.data, fp)
        }
      } else {
        # if just retrieving details
        .data <- look_info
      }
    } else {
      # if daily
      last_updated <- hud_last_updated(.data_nm, path)
      old_data <- hud_load(.data_nm, path)
      do_update <- last_updated < Sys.Date()
      if (do_update) {
        message(.data_nm, " last updated: ", last_updated, ". Updating...")
        new_data <-
          do.call(self$api$runLook, .args)
        api_names = new_data[1,]
        new_data <- rlang::set_attrs(new_data, api_names = api_names)
        new_data <- new_data[-1,]
        check_api_data(.data, .data_nm, daily_update)
        updated_data <- update_data(new_data, old_data)
        # The bind_rows in updated_data will remove the attributes so they must be re-added
        new_data <- rlang::set_attrs(new_data, api_names = api_names)
        if (.write && !identical(old_data, updated_data)) {
          hud_feather(updated_data, file.path(path, paste0(.data_nm, ".feather")))
        }
      } else {
        updated_data <- old_data
      }
      .data <- updated_data
    }
    .data
  }

update_data <- function(new_data, old_data) {
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
}

check_api_data <- function(.data, .data_nm, daily_update) {
  if (names(.data)[1] == "message")
    stop(purrr::imap_chr(.data, ~ paste0(.data_nm, ": ", .x, "\n")))
  if (nrow(.data) %in% c(0, 500, 5000) && !daily_update)
    stop(.data_nm,
         " row count is ",
         nrow(.data),
         ". Row limits could be limiting data.")

  if (!is.null(attr(.data, "api_names"))) {
    purrr::walk2(attr(.data, "api_names"), names(.data), ~{
      if (!agrepl(.y, hud_rename_strings(.x, .data_nm), max.distance = .2))
        rlang::warn(paste0("Possible column mismatch:\n", .x, " = ", .y))
    })
  }

}

col_names_from_look_description <- function(look_info) {
  stringr::str_split(look_info$description,  "\\,\\s")[[1]]
}

col_names_from_look_vis_config <- function(look_info) {
  look_info[["query"]][["vis_config"]][["series_labels"]][look_info[["query"]][["fields"]]]
}

col_types_from_col_names <- function(col_names) {
  rlang::set_names(col_names) |> purrr::imap_chr(~{
    purrr::when(.x,
               stringr::str_detect(., "ID$") ~ "c",
               stringr::str_detect(., "^APCounties") ~ "c",
               stringr::str_detect(., "Type$") ~ "i",
               stringr::str_detect(., "(?:^Date)|(?:Date$)") ~ "D",
               ~ "?")

  })
}

fetch_folder <- function(.x, self) {
  if (is.character(.x)) {
    folder <- self$api$folders("search", name = .x)[[1]]
  } else if (is.numeric(.x)) {
    folder <- self$api$folders("get", .x)
    .x <- folder$name
  }

  self$folders[[.x]] <- folder
  .x
}



#' @title Call HUD Export Items & Extras from the Clarity Looker API
#' @description Calls the Clarity Looker API HUD CSV Export  (BETA) & LookML models to return to the HUD Export Items & other datasets on various time ranges via pre-constructed Looks. See `?call_data` for details on using all data methods.
#' @details Methods for calling Export items and all looks contained within specified folders are dynamically added on initialization.
#' @inheritSection hud_filename Export_Items
#' @param .write \code{(logical)} Whether to write data as feather file to `path`
#' @param details \code{(logical)} Return look info. See [getLook](https://docs.looker.com/reference/api-and-integration/api-reference/v3.1/look#get_look)
#' @param daily_update Update the Export with the data that has been added or modified over the last 24 hour period (12a - 12p).
#' @param path \code{(character)} The directory path from which to load stored data
#' @param skip \code{(character)} of Look titles to skip
#' @inheritDotParams readr::read_csv
#' @include hud_export.R
#' @include hud_extras.R
#' @export
clarity_api <- R6::R6Class(
  "clarity_api",
  lock_objects = FALSE,
  cloneable = FALSE,
  public = rlang::exec(
    rlang::list2,
    #' @description initialize the Looker API connection given the path to the ini configuration file.
    #' @param configFile \code{(character)} Path to the Looker *.ini* configuration file. Only the directory path is needed if the file is entitled *Looker.ini*
    #' @param export_folder \code{(numeric)} ID of the folder containing Export looks.
    #' @param daily_folder \code{(numeric)} ID of the folder containing Export look data added or modified in the past 24 hours (12a-12p)
    #' @param look_folders \code{(character/numeric)} list of names or numeric IDs of the additional folders containing relevant looks. Numeric IDs are recommended.
    #' @param dirs \code{(named list)} of default directory paths for where to store the feather files for the following data types:
    #' \itemize{
    #'   \item{\code{export}}{ The HUD Export items **Default**: *data/export*}
    #'   \item{\code{public}}{ The public items **Default**: *data/public*}
    #'   \item{\code{spm}}{ The SPM items **Default**: *data/spm*}
    #'   \item{\code{extras}}{ The HUD Extras (custom items) **Default**: *data/extras*}
    #' }
    #' This is optional and the path can be provided to individual methods as needed.

    initialize = function (configFile, export_folder = 9862, daily_folder = 9711,
                           look_folders = 9874, dirs = list(export = "data/export",
                                                                    public = "data/public", spm = "data/spm",
                                                                    extras = "data/extras"))
    {
      self$api <- lookr::LookerSDK$new(configFile = ifelse(stringr::str_detect(configFile,
                                                                               "ini$"), file.path(configFile), file.path(configFile,
                                                                                                                         "Looker.ini")))
      private$folder_info <- list()
      if (UU::is_legit(export_folder)) {
        export_folder <- fetch_folder(export_folder, self)
        private$folder_info$export <- export_folder
        rlang::env_bind(self, !!!purrr::map(folder_looks(self$folders[[export_folder]]),
                                            ~private$call_data))
      }
      if (UU::is_legit(daily_folder)) {
        daily_folder <- fetch_folder(daily_folder, self)
        private$folder_info$daily_folder <- daily_folder
      }
      if (UU::is_legit(look_folders)) {
        purrr::walk(look_folders, fetch_folder, self = self)
        purrr::iwalk(self$folders[!names(self$folders) %in% unlist(private$folder_info)], ~{

          looks <- purrr::map(rlang::set_names(purrr::map_chr(.x$looks,
                                                              "title")), ~private$call_data)
          self[[.y]] <- rlang::list2(!!!looks)
        })
      }
      self$dirs <- dirs
    },
    #' @description Pull all Export items with associate Looks
    get_export = function(path = self$dirs$export, .write = TRUE, skip = c("Assessment",
                                                                           "AssessmentQuestions",
                                                                           "AssessmentResults",
                                                                           "Services",
                                                                           "YouthEducationStatus")) {
      if (!dir.exists(path))
        UU::mkpath(path)
      to_fetch <- names(folder_looks(self$folders[[private$folder_info$export]])) |>
        {\(x) {x[!x %in% skip]}}() |>
          rlang::set_names()

      purrr::map(to_fetch, ~ rlang::eval_bare(rlang::expr(self[[!!.x]](
        path = path,
        .write = .write
      ))))
    },
    #' @description Pull all Looks associated with a folder
    #' @param folder \code{(folder)} Folder object from `folders` field
    #' @param details \code{(logical)} Whether to return Look info for the looks in the specified folder
    #' @param .write \code{(logical)} Whether to write the folder looks to disk
    #' @param path \code{(character)} If `.write = TRUE`, where to write the files too.
    #' @param skip \code{(character)} vector of the names of looks in the folder to skip
    #' @return \code{(list)} of specified looks in folder
    get_folder_looks = function(folder, details = FALSE, .write = FALSE, path, skip) {
      if (!dir.exists(path))
        UU::mkpath(path)
      .args <- list(
        .write = .write,
        details = details,
        path = path
      )
      if (!missing(skip)) {
        looks <- purrr::keep(folder$looks, ~!.x$title %in% skip)
      } else {
        looks <- folder$looks
      }

      looks <- rlang::set_names(looks, names(folder_looks(folder)))
      if (folder$name == private$folder_info$export) {
        out <- purrr::imap(looks, ~{
            fn <- rlang::call2(self[[!!.x$title]], !!!.args)
            rlang::eval_bare(fn)
          })
      } else {
        out <- purrr::imap(looks, ~ {
          fn <- rlang::call2(rlang::expr(self[[!!folder$name]][[!!.x$title]]), !!!.args)
          rlang::eval_bare(fn)
        })
      }

      out
    },
    #' @field folders `{lookr}` folder data stored here
    folders = list(),
    #' @description Close the Looker API Connection
    finalize = function() {
      self$api$on_connection_closed()
    },

  ),
  private = rlang::list2(
    call_data = call_data,
    folder_info = list()
  )
)


