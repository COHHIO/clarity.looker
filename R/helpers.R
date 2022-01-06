# Supporting functions ----
# Mon Jul 19 16:05:32 2021

hud_formatted <- function(x) {
  x <- stringr::str_extract(x, "[A-Za-z\\_\\s\\.\\-]+")
}

hud_regex <- function(x) {
  purrr::when(stringr::str_detect(x, "\\.[A-Za-z0-9]{1,10}$"),
              isTRUE(.) ~ x,
              ~ paste0("^",UU::ext(x, strip = TRUE))
  )
}

#' @title Retrieve all Look IDs from a folder
#' @description Retrieve the Look IDS from a folder with their corresponding names
#' @param folder \code{(folder)} folder object
#' @return \code{(named character)} vector of Look IDs
#' @export
folder_looks <- function(folder) {
  purrr::map_int(folder$looks, "id") |> rlang::set_names(purrr::map_chr(folder$looks, "title"))
}

#' @title Retrieve Look info from a folder
#' @description Retrieve Look Info from a folder for a specific look name
#' @inheritParams folder_looks
#' @param looks \code{(character)} of the Look names for which to retrieve info for
#' @return \code{(named list)} of Look Infos
#' @export
look_id_from_folder <- function(looks, folder) {
  out <- purrr::keep(folder$looks, ~.x$title %in% looks)
  if (length(out) > length(looks))
    stop("Multiple looks matching a particular name")
  rlang::set_names(purrr::map_int(out, "id"), purrr::map_chr(out, "title"))
}


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

  if (!file.exists(x)) {
    .file <- UU::list.files2(path, pattern = hud_regex(x), full.names = TRUE, recursive = FALSE)
  } else {
    .file <- x
  }

  .file
}

#' @title Gather last updated times for on-disk files
#' @description Check the last modified time for the hud_exports specified
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @return \code{(POSIXct)} Last modified time
#' @export

hud_last_updated <- function(x, path = "data") {
  if (!missing(x)) {
    do.call(c, purrr::map(rlang::set_names(x), ~file.info(.x)$mtime)) |> sort(decreasing = TRUE)
  } else {
    do.call(c, purrr::map(rlang::set_names(UU::list.files2(path)), purrr::possibly(~file.info(.x)$mtime, lubridate::NA_POSIXct_), path = path))
  }

}


#' @title Extract a previously downloaded HUD Export archive
#'
#' @param browser_dl_folder \code{(character)} path to the browser's download folder or the file to extract
#' @param extract_path \code{(character)} path to the folder where the archive is to be extracted
#' @param delete_archive \code{(logical)} Delete the archive after extracting?
#' @param moment \code{(POSIXct/Date)} The time point which the archive creation time should be greater than to ensure it's recent.
#' @param wait \code{(Duration)} to wait for the file to appear in the download directory. Relevant when using browser automation.
#' @return \code{(logical)} as to whether the extraction was successful
#' @export

hud_export_extract <- function(browser_dl_folder = "~/../Downloads", extract_path = file.path("data", "export"), delete_archive = TRUE, moment = Sys.Date(), wait = lubridate::minutes(2)) {
  downloads <- path.expand(browser_dl_folder)
  extract_path <- path.expand(extract_path)
  if (!(stringr::str_detect(downloads, "(?:7z$)|(?:zip$)") && file.exists(downloads))) {
    dls <- list.files(downloads, full.names = TRUE, pattern = "^hudx")
    dl_times <- do.call(c, purrr::map(dls, ~file.info(.x)$mtime))
    if (!UU::is_legit(dl_times))
      cli::cli_alert(paste0("No HUD Export found in ", path.expand(downloads), " waiting ", wait))
    wait = lubridate::now() + wait
    .recent <- dl_times > moment
    while (!any(.recent) && Sys.time() < wait) {
      Sys.sleep(5)
      dls <- list.files(downloads, full.names = TRUE, pattern = "^hudx")
      dl_times <- do.call(c, purrr::map(dls, ~file.info(.x)$mtime))
      .recent <- dl_times > moment
    }
  } else {
    f <- downloads
  }

  if (any(get0(".recent", inherits = FALSE)))
    f <- dls[.recent]


  if (UU::is_legit(f)) {
    UU::mkpath(extract_path)
    .last_update <- mean(hud_last_updated(extract_path), na.rm = TRUE)
    if (UU::is_legit(.last_update) && .last_update < mean(archive::archive(f)$date, na.rm = TRUE))
      archive::archive_extract(f, extract_path)
    else
      cli::cli_inform("Current export is already up to date. No extraction performed.")
  } else
    cli::cli_alert("No HUD Export found in {.path {downloads}} with creation time greater than ", moment)

  if (delete_archive)
    file.remove(f)
}

#' @title Is the package running in dev mode
#'
#' @return \code{(logical)}
#' @export

is_dev <- function() {
  basename(dirname(getwd())) == "COHHIO"
}

#' @title Load a HUD Export item from disk
#' @description Load the named HUD Export item from the `path` provided
#' @inheritSection hud_filename Export_Items
#' @inheritParams hud_filename
#' @return \code{(POSIXct)} Last modified time
#' @export

hud_load <- function(x, path = "data") {
  p <- system.file(package = "RmData", "data", lib.loc = .libPaths())
  if (basename(path) == "public" && dir.exists(p)) {
    fn <- UU::ext(x, strip = TRUE)
    e <- new.env()
    data(list = fn, envir = e)
    out <- e[[fn]]
  } else {
    .file <- hud_filename(x, path)


    if (!UU::is_legit(.file)) {
      stop(x, ": file not found.")
    } else if (length(.file) > 1) {
      .matches <- names(.file) %in% x
      if (any(.matches))
        .file <- .file[.matches]
      .updated <- hud_last_updated(.file)
      .file <- names(.updated)[1]
      cli::cli_warn(c("Found multiple files:", paste0(basename(names(.updated)),' ',.updated), "Returning: {.path {.file}} @ {cli::col_br_blue(.updated[1])}"))


    }

    import_fn <- UU::file_fn(.file)
    .args <- list(.file)
    if (UU::ext(.file) == "csv" && UU::ext(basename(.file), strip = TRUE) %in% names(.hud_export))
      .args$col_types <- .hud_export[[x]]$col_types

    out <- do.call(import_fn, .args)
  }
  out
}


hud_rename_strings <- function(x, rm_prefixes) {
    trimws(stringr::str_remove(x, stringr::regex(paste0(paste0("(?:^",rm_prefixes,"\\s)"), collapse = "|")))) |>
    stringr::str_replace_all("(?<!a)[Ii][Dd]$", "ID") |>
    stringr::str_remove("^\\w+(?:\\sCustom)?\\s") |>
    stringr::str_replace_all("[Cc][Oo][Cc]", "CoC") |>
    stringr::str_replace_all("^[Zz][Ii][Pp]$", "ZIP") |>
    stringr::str_replace_all("(?<=rk)p(?=lace)", "P") |>
    stringr::str_replace_all("\\s", "")
}



hud_rename <- function(x, rm_prefixes, nms) {
  if (is.null(x))
    return(NULL)
  if (!missing(rm_prefixes)) {
    out <- x %>%
      dplyr::rename_with(.fn = ~ {
        # All column names are prefixed with the HUD CSV Export BETA report name from Looker - with spaces between capitalized words. This is removed
        out <-  .x |>
          hud_rename_strings(rm_prefixes)

        if (all(is.na(out)))
          out <- .x
        out
      })
  } else {
    out <- setNames(x, nms)
  }
  if (!missing(nms)) {
    out <- dplyr::select(out, tidyselect::any_of(nms))
  }
  out
}

#' @title Write object to the *data* directory as feather file
#' @description Writes a \code{tibble/data.frame} as a feather file to the path
#' @param .data \code{(tibble/data.frame)} The object to write to feather
#' @param path \code{(character vector)} A character vector of the directory path to be passed to \link[base]{file.path}. If the filename is appended IE "Export.feather", the file will be named as such. Otherwise the name of the `.data` object will be used IE, if an object named `Affiliation` containing the Affiliation data is passed to `.data`, the resulting file will be [path]/Affiliation.feather.
#' @return A success message at the console
#' @export

hud_feather <- function(.data, path = "data", nm) {
  if (missing(nm))
    nm <- deparse(rlang::enexpr(.data))
  fn <-
    rlang::exec(file.path,
                !!!purrr::when(
                  stringr::str_detect(path, "feather$"),
                  isTRUE(.) ~ path,
                  list(path, paste0(nm, ".feather"))
                ))
  if (!dir.exists(dirname(fn)))
    UU::mkpath(dirname(fn))
  arrow::write_feather(.data, fn)
  cli::cli_alert_success(paste0(fn, " saved"))
}

#' @title Filter out specific Clients
#' @description Often used to filter test/training demo clients
#' @param x \code{(data.frame)} With PersonalID or UniqueID column
#' @param clients \code{(character)} of PersonalIDs to filter with names corresponding their UniqueIDs (Clarity only)
#' @family Client functions
#' @return \code{(data.frame)} without `clients_to_filter`
#' @export

Client_filter <- function(x, clients = getOption("HMIS")$clients_to_filter) {

  if (is.data.frame(x) && UU::is_legit(clients)) {
    nms <- na.omit(stringr::str_extract(colnames(x), UU::regex_or(c("PersonalID", "UniqueID"))))
    if (UU::is_legit(nms))
      for (nm in nms) {
        x <- dplyr::filter(x, !(!!rlang::sym(nm)) %in% !!purrr::when(nm, . == "PersonalID" ~ clients, ~ names(clients)))
      }
  }

  x
}

#' @title Filter for specific clients
#'
#' @param x \code{(data.frame)} with either UniqueID or PersonalID
#' @param clients \code{(character)} of either UniqueID's or PersonalID's. The corresponding column must be present in the data to filter
#'
#' @return \code{(data.frame)} filtered for clients
#' @export

find_clients <- function(x, clients) {
  if (nchar(clients[1]) == 9 && "UniqueID" %in% names(x))
    x <- dplyr::filter(x, UniqueID %in% clients)
  else if ("PersonalID" %in% names(x))
    x <- dplyr::filter(x, PersonalID %in% clients)
  x
}

is_link <- function(.col) {
  any(stringr::str_detect(.col, "^\\<a"), na.rm = TRUE) || inherits(.col[[1]], "shiny.tag")
}

#' @title Make a Clarity link using the `PersonalID` and `UniqueID/EnrollmentID`
#' @description If used in a \link[DT]{datatable}, set `escape = FALSE`
#' @param PersonalID \code{(character)} The `PersonalID` column
#' @param ID \code{(character)} The `UniqueID/EnrollmentID` column
#' @param chr \code{(logical)} Whether to output a character or a `shiny.tag` if `FALSE`. **Default** TRUE
#'
#' @return \code{(character/shiny.tag)} If `PersonalID` is a character vector (IE nested in a mutate), and `chr = TRUE` a character vector, if `chr = FALSE` a `shiny.tag`. The `ID` column will be replaced with the link.
#' @export
#'
#' @examples
#' data.frame(a = letters, b = seq_along(letters)) |>  dplyr::mutate(a = make_link(a, b))


make_link <- function(PersonalID, ID, chr = TRUE) {
  href <- getOption("HMIS")$Clarity_URL %||% "https://cohhio.clarityhs.com"
  .type <- ifelse(any(stringr::str_detect(ID, "[A-F]"), na.rm = TRUE), "profile", "enroll")
  sf_args <- switch(.type,
                    profile = list("<a href=\"%s/client/%s/profile\" target=\"_blank\">%s</a>", href, PersonalID, ID),
                    enroll = list("<a href=\"%s/clients/%s/program/%s/enroll\" target=\"_blank\">%s</a>", href, PersonalID, ID, ID))
  if (chr) {
    out <- do.call(sprintf, sf_args)
  } else {
    href <- httr::parse_url(href)
    if (!identical(length(PersonalID), length(ID))) {
      l <- list(PersonalID = PersonalID, ID = ID)
      big <- which.max(purrr::map_int(l, length))
      i <- seq_along(l)
      small <- subset(i, subset = i != big)
      assign(names(l)[small], rep(l[[small]], length(l[[big]])))
    }
    out <- purrr::map2(PersonalID, ID, ~{
      href$path <- switch(.type,
                          profile = c("client",.x, "profile"),
                          enroll = c("clients",.x, "program", .y, "enroll"))
      htmltools::tags$a(href = httr::build_url(href), .y, target = "_blank")
    })
  }
  out
}

#' @title Make UniqueID or EnrollmentID into a Clarity hyperlink
#' @param .data \code{(data.frame)} The following columns are required for the specified link type:
#' \itemize{
#'   \item{\code{PersonalID & UniqueID}}{ for Profile link}
#'   \item{\code{PersonalID & EnrollmentID}}{ for Enrollment link}
#' }
#' @param ID \code{(name)} unquoted of the column to unlink.
#' @param unlink \code{(logical)} Whether to turn the link back into the respective columns from which it was made.
#' @param new_ID \code{(name)} unquoted of the column to be created with the data from the linked column. (`PersonalID` will be recreated automatically if it doesn't exist).
#' @inheritParams make_link
#' @return \code{(data.frame)} With `UniqueID` or `EnrollmentID` as a link
#' data.frame(a = letters, b = seq_along(letters)) |>  dplyr::mutate(a = make_link(a, b)) |> make_linked_df(a, unlink = TRUE)
#' @export
make_linked_df <- function(.data, ID, unlink = FALSE, new_ID, chr = TRUE) {
  out <- .data
  .data_nm <- rlang::expr_deparse(rlang::call_args(match.call())$.data)
  ID <- rlang::enexpr(ID)
  .col <- .data[[ID]]
  if (is.null(.col))
    rlang::abort(glue::glue("{as.character(ID)} not found in `.data`"), trace = rlang::trace_back())

  .type <- ifelse(any(stringr::str_detect(.col, ifelse(unlink, "profile", "[A-F0-9]{9}")), na.rm = TRUE), "profile", "enroll")

  if (unlink) {
    # TODO handle shiny.tag
    if (!is_link(.col))
      rlang::abort(glue::glue("{.data_nm}: `{as.character(ID)}` is not a link"))
    if (!"PersonalID" %in% names(.data))
      out$PersonalID <- stringr::str_extract(.col, "(?<=client\\/)\\d+")
    if (!missing(new_ID))
      ID <- rlang::enexpr(new_ID)
    out[[ID]] <- stringr::str_extract(.col, switch(.type,
                                                     profile = "(?<=\\>)[:alnum:]+(?=\\<)",
                                                     enroll = "\\d+(?=\\/enroll)"))
  } else {
    if (is_link(.col)) {

      rlang::inform(glue::glue("{.data_nm}: `{as.character(ID)}` is already a link."))
    } else {
      out <- .data |>
        dplyr::mutate(!!ID := make_link(PersonalID, !!ID, chr = chr))
    }


  }

  out
}
