setup_looker <- function(ini_filepath, base_url = "https://looker.clarityhs.com:19999", client_id, client_secret) {
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
    "  # Set to false if testing locally against self-signed certs. Otherwise leave True",
    "  verify_ssl=True"
  ), purrr::when(ini_file,
                 stringr::str_detect(., "ini$") ~ ini_file,
                 file.path(ini_filepath, "Looker.ini")))
}
