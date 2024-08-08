#' @title Setup the Looker ini file
#' @description Connecting to the Looker API via \href{https://github.com/looker-open-source/lookr}{`lookr`} requires an ini file with credentials to access the API. This function creates that ini file in a directory of your choosing.
#' @param client_id \code{(character)} The Client ID provided by Clarity
#' @param client_secret \code{(character)} The Client Secret provided by Clarity
#' @param ini_filepath \code{(character)} The directory and name of the ini file. **Default** creates *Looker.ini* in the working directory. A path can also be provided, and the file will be named *Looker.ini* in that directory.
#' @param base_url \code{(character)} The base URL to the Clarity Looker instance. This is typically the same for all Clarity users.
#' @export

setup_looker <- function(client_id, client_secret, ini_filepath = "Looker.ini", base_url = "https://looker.clarityhs.com:19999") {
  fp <- purrr::when(ini_filepath,
              stringr::str_detect(., "ini$") ~ ini_filepath,
              file.path(ini_filepath, "Looker.ini"))
  if (!dir.exists(dirname(fp)))
    UU::mkpath(dirname(fp))
  write(paste0(
    "[Looker]\n",
    "# API version is required\n",
    "api_version=4.0\n",
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
  ), fp)

  purrr::walk(c(".gitignore", ".Rbuildignore"), ~{
    if (file.exists(.x)) {
      write(fp, .x, append = TRUE)
      cli::cli_alert_info(paste0(fp, " added to ", .x))
    }
  })

}

#' @title Extract Looks and their names from the folder browse table in Looker
#' @description Takes file with the "outerHTML" from chrome > inspect of the table of all the looks in Looker folder browse view and parse it to extract the look id numbers and their names provided that the format of the look name is HUD_Export.(`ExportItemName`)
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
      stringr::str_extract("[A-Za-z\\_]+$")
  )
}


#' @title Update the `.hud_export` spec given a `look_table`
#' @description Using the `look_table` returned from `parse_look_table`, update the `.hud_export` spec
#' @param look_table \code{(list)} The existing `.hud_export` spec, usually a list where each element corresponds to a look type.
#' @param look_html \code{(character)} HTML content or path to the HTML file used to parse new look table entries.
#' @param look_type \code{(character)} The Look type to update or add. This determines which look type to modify in the `look_table`.
#' @param add_new \code{(logical)} Whether to add new look types not already present in `look_table`. **Default** is `FALSE`.
#' @return Console output from `dput` with the updated spec. This output can be copied and pasted into `hud_export.R` to update the spec.
#' @export
#' @examples
#' Assuming parse_look_table and a look_table already exist
#' hud_look_table_update(look_table = my_look_table, look_html = "path/to/look.html", look_type = "new_look_type", add_new = TRUE)
hud_look_table_update <- function(look_table, look_html, look_type, add_new = FALSE) {
  .new_look_table <- parse_look_table(look_html)
  for (nm in names(.new_look_table)[names(.new_look_table) %in% look_table]) {
    look_table[[nm]]$look <- setNames(c(look_table[[nm]]$look, .new_look_table[[.y]]), c(names(look_table[[nm]]$look), look_type))
  }

  if (add_new) {
    .to_add <- !names(.new_look_table) %in% names(look_table)
    look_table <- append(look_table, purrr::imap(setNames(.new_look_table[.to_add], names(.new_look_table)[.to_add]), ~list(look = setNames(c(.x), look_type))))
  }
  dput(look_table)
}
