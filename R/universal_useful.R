
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
