#' Download a register RSF file
#'
#' @description
#' Either downloads the Register Serialisation Format (RSF) file of a register,
#'   constructing the URL from the name of the register and the given phase, or
#'   reads it from disk.  Optionally saves it to disk.
#'
#' @param name Character, the name of the register.
#' @param phase Character, one of `"beta"` or `"alpha"`
#' @param file Character, file path or URL, passed on to [readr::read_lines()]
#'   if `name` is not provided.
#' @param write Logical, whether to write the RSF file to disk.  If `TRUE`,
#' either `name` or `dest_path` must be provided.
#' @param dest_path Character, path and file name to write the RSF to.
#'
#' @export
#' @examples
#' \dontrun{
#'   rr_rsf("country")
#' }
rr_rsf <- function(name = NULL, phase = c("beta", "alpha"), file = NULL,
                   write = FALSE, dest_path = NULL) {
  phase <- match.arg(phase)
  if (write) {
    if (is.null(dest_path)) {
      if (is.null(name)) {
        stop("`write` is TRUE but neither `name` nor `dest_path` has been provided.")
      }
      dest_dir <- phase
      dest_path <- fs::path(dest_dir, paste0(name, ".rsf"))
    }
  }
  if (is.null(name)) {
    out <- readr::read_lines(file)
  } else {
    phase <- match.arg(phase)
    register_url <-
      switch(phase,
             beta = "https://{name}.register.gov.uk/download-rsf",
             alpha = "https://{name}.{phase}.openregister.org/download-rsf")
    register_url <- glue::glue(register_url)
    message("Downloading register '", file,
            "' from the '", phase, "' phase ...\n")
    register_path <- tempfile()
    on.exit(unlink(register_path))
    download.file(register_url, register_path)
    out <- readr::read_lines(register_path)
  }
  if (write) {
    fs::dir_create(fs::path_dir(dest_path))
    readr::write_lines(out, dest_path)
  }
  return(out)
}
