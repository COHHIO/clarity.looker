
is_legit <- function(x) {
  !(is.null(x) || rlang::is_empty(x) || is.na(x))
}


.mode <- function(.) {
  .u <- unique(.)
  tab <- tabulate(match(., .u))
  .u[tab == max(tab)]
}

file_path_create <- function(path) {
  if (!file.exists(path)) {
    message(paste0(path, " does not exist. Creating..."))
    .path <- stringr::str_split(path, paste0("\\", .Platform$file.sep))[[1]]
    .wd <- stringr::str_split(getwd(), paste0("\\", .Platform$file.sep))[[1]]
    .path <- .path[!.path %in% .wd]
    purrr::walk(purrr::accumulate(.path, ~{
      .p <- paste0(.x,.Platform$file.sep,.y)
    }), ~{
      if (!dir.exists(.x)) dir.create(.x)
    })
  }
}
#' @title Provide the appropriate file read/write function
#' @description Return the appropriate read or write function given an object or `ext`ension as a character
#' @param object to determine the appropriate function for writing to disk
#' @param ext \code{(character)} file extension to determine the appropriate reading function
#' @return \code{(function)}
#' @export
file_io_fn <- function(object, ext) {
  if (!missing(ext))
    purrr::when(
    ext,
    !clarity.looker::is_legit(.) ~ stop(x, " not found"),
    length(.) > 1 ~ stop("Duplicate files found for ", x),
    grepl("csv", ., ignore.case = TRUE) ~ readr::read_csv,
    grepl("feather", ., ignore.case = TRUE) ~ feather::read_feather,
    grepl("rds", ., ignore.case = TRUE) ~ readRDS
    )
  else
    purrr::when(object,
                inherits(., "data.frame") ~ feather::write_feather,
                !inherits(., "data.frame") ~ saveRDS)
}

#' @title Provide the appropriate file extension for a given object
#' @param object to determine the appropriate function for writing to disk
#' @return \code{(character)}
#' @export
file_io_ext <- function(object) {
  purrr::when(object,
              inherits(., "data.frame") ~ ".feather",
              !inherits(., "data.frame") ~ ".rds")
}
