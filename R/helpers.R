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
    if (UU::ext(.file) == "csv" && UU::ext(basename(.file), strip = TRUE) %in% names(.hud_export)) {
      .args$col_types <- .hud_export[[x]]$col_types
      .args$lazy = FALSE
    } else if (UU::ext(.file) == "feather")
      .args$mmap <- FALSE


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
  any(stringr::str_detect(.col, "^\\<a"), na.rm = TRUE) || isTRUE(try(inherits(.col[[1]], "shiny.tag"), silent = TRUE))
}

#' @title Make a Clarity link using an `ID` column
#' @description If used in a \link[DT]{datatable}, set `escape = FALSE`
#' @param ID \code{(character)} The `ID` column. See the list below for the appropriate matching `ID`. If `link_text` is:
#' \itemize{
#'   \item{\code{UniqueID}}{ \code{PersonalID}}
#'   \item{\code{EnrollmentID}}{ \code{PersonalID}}
#'   \item{\code{ProjectName}}{ \code{ProjectID}}
#'   \item{\code{AgencyName}}{ \code{AgencyID}}
#' }
#' @param link_text \code{(character)} The link text. Typically `UniqueID/EnrollmentID/AgencyName/ProjectName` column
#' @param chr \code{(logical)} Whether to output a character or a `shiny.tag` if `FALSE`. **Default** TRUE
#' @param type \code{(character)} the type of link to create based on `link_text`. One of:
#' \itemize{
#'   \item{\code{"profile"}}{ When \code{link_text} is \code{UniqueID}}
#'   \item{\code{"enrollment"}}{ When \code{link_text} is \code{EnrollmentID}}
#'   \item{\code{"agency_switch"}}{ When \code{link_text} is \code{AgencyName}}
#'   \item{\code{"program_edit"}}{ When \code{link_text} is \code{ProgramName}}
#' }
#' @return \code{(character/shiny.tag)} If `chr = TRUE` a character vector, if `chr = FALSE` a `shiny.tag`.
#' @export
#'
#' @examples
#' data.frame(a = letters, b = seq_along(letters)) |>  dplyr::mutate(a = make_link(a, b))


make_link <- function(ID, link_text, type = NULL, chr = TRUE) {
  href <- getOption("HMIS")$Clarity_URL %||% "https://cohhio.clarityhs.com"
  .type = link_type(type, link_text, rlang::expr_deparse(link_text))
  sf_args <- switch(
    .type,
    profile = list(
      "<a href=\"%s/client/%s/profile\" target=\"_blank\">%s</a>",
      href,
      ID,
      link_text
    ),
    enrollment = list(
      "<a href=\"%s/clients/%s/program/%s/enroll\" target=\"_blank\">%s</a>",
      href,
      ID,
      link_text,
      link_text
    ),
    agency_switch = list(
      "<a href=\"%s/manage/agency/switch/%s\" target=\"_blank\">%s</a>",
    href,
    ID,
    link_text
    ),
    program_edit = list("<a href=\"%s/manage/program/edit/%s\" target=\"_blank\">%s</a>", href, ID, link_text)
  )

  if (chr) {
    out <- do.call(sprintf, sf_args)
  } else {
    href <- httr::parse_url(href)
    if (!identical(length(ID), length(link_text))) {
      l <- list(PersonalID = ID, ID = link_text)
      big <- which.max(purrr::map_int(l, length))
      i <- seq_along(l)
      small <- subset(i, subset = i != big)
      assign(names(l)[small], rep(l[[small]], length(l[[big]])))
    }
    out <- purrr::map2(ID, link_text, ~{
      href$path <- switch(.type,
                          profile = c("client",.x, "profile"),
                          enrollment = c("clients",.x, "program", .y, "enroll"),
                          agency_switch = c("manage","agency", "switch", .x),
                          program_edit = c("manage","program", "edit", .x)
                          )
      htmltools::tags$a(href = httr::build_url(href), .y, target = "_blank")
    })
  }
  out
}

link_type <- function(x, link_text, link_chr, new_ID) {
  .type <- x %||% switch(link_chr,
                         UniqueID = "profile",
                         EnrollmentID = "enrollment",
                         ProjectName = "program_edit",
                         ProgramName = "program_edit",
                         AgencyName = "agency_switch") %||% switch(rlang::expr_deparse(new_ID),
                                                                   UniqueID = "profile",
                                                                   EnrollmentID = "enrollment",
                                                                   ProjectName = "program_edit",
                                                                   ProgramName = "program_edit",
                                                                   AgencyName = "agency_switch") %||%
    ifelse(any(stringr::str_detect(link_text, "[A-F]"), na.rm = TRUE), "profile", "enrollment")
  UU::match_letters(.type, "profile", "enrollment", "program_edit", "agency_switch", n = 4)
}

#' @title Make UniqueID or EnrollmentID into a Clarity hyperlink
#' @param .data \code{(data.frame)} The following columns are required for the specified link type:
#' \itemize{
#'   \item{\code{PersonalID & UniqueID}}{ for Profile link}
#'   \item{\code{PersonalID & EnrollmentID}}{ for Enrollment link}
#'   \item{\code{PersonalID & EnrollmentID}}{ for Enrollment link}
#' }
#' @param link_text \code{(name)} unquoted of the column to unlink.
#' @param unlink \code{(logical)} Whether to turn the link back into the respective columns from which it was made.
#' @param new_ID \code{(name)} unquoted of the column to be created with the data from the linked column. (`PersonalID` will be recreated automatically if it doesn't exist).
#' @inheritParams make_link
#' @return \code{(data.frame)} With `UniqueID` or `EnrollmentID` as a link
#' data.frame(a = letters, b = seq_along(letters)) |>  dplyr::mutate(a = make_link(a, b)) |> make_linked_df(a, unlink = TRUE)
#' @export
make_linked_df <- function(.data, link_text, unlink = FALSE, new_ID, type = NULL, chr = TRUE) {
  stopifnot(!is.null(names(.data)))
  if (nrow(.data) == 0)
    return(.data)
  out <- .data
  .data_nm <- rlang::expr_deparse(rlang::call_args(match.call())$.data)
  link_text <- rlang::enexpr(link_text)
  has_new_ID <- !missing(new_ID)
  if (has_new_ID)
    new_ID <- rlang::enexpr(new_ID)
  link_chr <- rlang::expr_deparse(link_text)

  .type <- link_type(type, link_text, rlang::expr_deparse(link_text), new_ID)
  ID <- switch(link_chr,
         UniqueID = "PersonalID",
         EnrollmentID = "PersonalID",
         ProjectName = "ProjectID",
         ProgramName = "ProgramID",
         AgencyName = "AgencyID") %||% switch(rlang::expr_deparse(new_ID),
                                              UniqueID = "PersonalID",
                                              EnrollmentID = "PersonalID",
                                              ProjectName = "ProjectID",
                                              ProgramName = "ProgramID",
                                              AgencyName = "AgencyID")
  .col <- .data[[link_text]]
  if (is.null(.col))
    rlang::abort(glue::glue("{link_chr} not found in `.data`"), trace = rlang::trace_back())

  if (unlink) {
    # TODO handle shiny.tag
    if (is_link(.col)) {
      if (!ID %in% names(.data))
        out[[ID]] <- stringr::str_extract(.col, switch(.type,
                                                       enrollment = ,
                                                       profile = "(?<=clients?\\/)\\d+",
                                                       program_edit = "(?<=edit\\/)\\d+",
                                                       agency_switch = "(?<=switch\\/)\\d+"))

      if (has_new_ID)
        link_text <- new_ID
      out[[link_text]] <- stringr::str_extract(.col, switch(.type,
                                                            agency_switch = ,
                                                            program_edit = ,
                                                     profile = "(?<=\\>)[:alnum:]+(?=\\<)",
                                                     enrollment = "\\d+(?=\\/enroll)"))
    } else
      rlang::warn(glue::glue("{.data_nm}: `{link_chr}` is not a link"))

  } else {
    if (is_link(.col)) {
      rlang::inform(glue::glue("{.data_nm}: `{link_chr}` is already a link."))
    } else {
      ID <- rlang::sym(ID)
      out <- .data |>
        dplyr::mutate(!!link_text := make_link(!!ID, !!link_text, chr = chr, type = .type))
    }
  }

  out
}
