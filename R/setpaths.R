#' Build a list of paths to your local OneDrive SharePoint folders
#'
#' Reads the `SHAREPOINT_PATH` environment variable (set in `.Renviron`,
#' either at the project level or in your `HOME` directory) and returns
#' a named list whose first element is the SharePoint root and whose
#' remaining elements are the top-level subfolders inside it.
#'
#' @details
#' Subfolder names are cleaned with [janitor::make_clean_names()] so they
#' can be used as syntactically valid, snake_case R names (for example
#' `paths$my_project_2024`). The on-disk folder casing is preserved in
#' the path *values* — only the *names* of the list elements are cleaned.
#'
#' The function fails fast with an informative message when:
#' - `SHAREPOINT_PATH` is unset or empty in `.Renviron`, or
#' - `SHAREPOINT_PATH` points to a directory that does not exist on disk.
#'
#' @return A named list. The first element, `sharepoint_path`, is the
#'   SharePoint root as a single character string. Each remaining element
#'   is the full path to one immediate subdirectory of the root, named
#'   after a snake_case version of the folder name.
#'
#' @examples
#' \dontrun{
#' paths <- set_paths()
#' paths$sharepoint_path
#' paths$my_project
#' }
set_paths <- function() {
  sharepoint_path <- Sys.getenv("SHAREPOINT_PATH", unset = NA_character_)

  if (is.na(sharepoint_path) || !nzchar(sharepoint_path)) {
    cli::cli_abort(c(
      "{.envvar SHAREPOINT_PATH} is not set in your {.file .Renviron}.",
      "i" = "Open it with {.run usethis::edit_r_environ()}, add \\
             {.code SHAREPOINT_PATH=\"/path/to/sharepoint\"}, then \\
             restart R so the change is picked up."
    ))
  }

  if (!fs::dir_exists(sharepoint_path)) {
    cli::cli_abort(c(
      "{.envvar SHAREPOINT_PATH} points to a directory that does \\
       not exist.",
      "x" = "Path: {.path {sharepoint_path}}"
    ))
  }

  subfolders <- fs::dir_ls(sharepoint_path, type = "directory")
  names(subfolders) <- janitor::make_clean_names(fs::path_file(subfolders))

  c(
    list(sharepoint_path = sharepoint_path),
    as.list(subfolders)
  )
}
