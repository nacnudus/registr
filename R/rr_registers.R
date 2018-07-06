#' Download all registers from an environment
#'
#' @description Downloads all the registers listed in the register register of
#'    an environment ('alpha', 'beta', or 'discovery'), meaning 'ready to use',
#'    'open for feedback', or 'in progress').
#'
#'    Unlike [rr_register()] this function only downloads register, and does not
#'    read them from disk.
#'
#' @param dir Character, optional name of a directory of RSF files of registers.
#'   If `NULL` (default), the registers will be downloaded.
#' @param dest_dir Character, name of a directory to save the RSF file of each
#'   register to.  The file names will be the names of the registers.
#' @inheritParams rr_register
#'
#' @return An S3 object of class `register`
#'
#' @examples
#' path <- fs::path(tempdir(), "beta")
#' names(rr_registers("beta", write = TRUE, dest_dir = path))
#' names(rr_registers(dir = path))
#' @export
rr_registers <- function(phase = c("beta", "alpha", "discovery"),
                         dir = NULL,
                         parse_datetimes = FALSE,
                         write = FALSE,
                         dest_dir = phase,
                         quiet = TRUE,
                         api_key = "") {
  phase <- match.arg(phase)
  if (is.null(dir)) {
    register_names <-
      rr_register("register", phase, quiet = quiet, api_key = api_key) %>%
      rr_snapshot() %>%
      purrr::pluck("data") %>%
      dplyr::pull(register)
    if (write) {
      dest_path <- fs::path(dest_dir, paste0(register_names, ".rsf"))
    } else {
      dest_path <- list(NULL)
    }
    registers <-
      purrr::map2(register_names, dest_path,
                  ~ rr_register(.x, phase = phase, write = write,
                                dest_path = .y, quiet = quiet))
    names(registers) <- register_names
  } else {
    paths <- fs::dir_ls(dir)
    register_names <- stringr::str_replace(basename(paths), "\\.rsf$", "")
    dest_paths <- fs::path(dest_dir, paste0(register_names, ".rsf"))
    registers <- purrr::map2(paths, dest_paths,
                             ~ rr_register(file = .x, phase = phase,
                                           parse_datetimes = parse_datetimes,
                                           write = write, dest_path = .y,
                                           quiet = quiet))
    names(registers) <- register_names
  }
  registers
}
