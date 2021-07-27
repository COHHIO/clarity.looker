setup_looker <- function(ini_filepath = "Looker.ini", base_url = "https://looker.clarityhs.com:19999", client_id, client_secret) {
  write(paste0(
    "[Looker]\n",
    "# API version is required\n",
    "api_version=3.1\n",
    "# Base URL for API. Do not include /api/* in the url\n",
    "base_url=",base_url,"\n",
    "# API 3 client id\n",
    "client_id=",client_id,"\n",
    "# API 3 client secret\n",
    "client_secret=", client_secret,"\n",
    "# Optional embed secret for SSO embedding\n",
    "embed_secret=\n",
    "# Optional user_id to impersonate\n",
    "user_id=\n",
    "  # Set to false if testing locally against self-signed certs. Otherwise leave True\n",
    "verify_ssl=True"
  ), purrr::when(ini_filepath,
                 stringr::str_detect(., "ini$") ~ ini_filepath,
                 file.path(ini_filepath, "Looker.ini")))
}

#' @title Extract Looks and their names from the folder browse table in Looker
#' @description Takes file with the "outerHTML" from chrome > inspect of the table of all the looks in Looker folder browse view and parse it to extract the look id numbers and their names provided that the format of the look name is HUD_Export.[ExportItemName]
#' @param htm \code{(character)} Path to html file with outerHTML copy/pasted into it
#' @return \code{(numeric)} vector of look ids and their respective names

parse_look_table <- function(htm) {
  .htm <- xml2::read_html(htm)
  setNames(
    .htm %>%
      rvest::html_nodes(xpath = "//div[@ng-bind = 'item.title']/ancestor::a") %>%
      rvest::html_attr("href") %>%
      stringr::str_extract("[\\d]+$") %>%
      as.numeric(),
    .htm %>%
      rvest::html_nodes(xpath = "//div[@ng-bind = 'item.title']") %>%
      rvest::html_attr("title") %>%
      stringr::str_extract("[A-Za-z]+$")
  )
}


#' @title Update the `.hud_export` spec given a `look_table`
#' @description Using the `look_table` returned from `parse_look_table`, update the `.hud_export` spec
#' @param look_table \code{(numeric)} output from `parse_look_table`
#' @param look_type \code{(character)} which Look type to update (or add)
#' @return Console output from dput with the new spec to copy/paste into hud_export.R

hud_export_look_update <- function(look_table, look_type) {
  purrr::imap(.hud_export, ~{
    if (.y %in% names(look_table))
      .x[[look_type]] <- look_table[[.y]]
    .x
  }) %>% dput
}
