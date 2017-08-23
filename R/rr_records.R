#' @title Download All Register Records
#' @description Download all records from a register, automatically handling
#'              pages of up to 5000 records each and displaying a progress bar.
#'
#'              A 'record' is the latest entry per key, so the record count is
#'              the number of unique keys in a register.
#'
#'              The progress bar can be disabled by setting
#'              `options("dplyr.show_progress" = FALSE)` and running the
#'              function inside `suppressMessages()` (see the examples).
#' @param register character, name of the register, e.g. "school-eng"
#' @param phase character, one of "beta", "alpha", "discovery", default: "beta"
#' @param page_size numeric, number of records per page of download, default: 5000
#' @return A tibble of all the records in a register
#' @details Does *not* include all entries, only the latest entry per record.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_all_records("country", "beta")
#'
#'  # To suppress the progress bar
#'  options("dplyr.show_progress" = FALSE)
#'  suppressMessages(rr_records("country", "beta"))
#'  }
#' }
#' @rdname rr_records
#' @export
rr_records <- function(register, phase = "beta", page_size = 5000) {
  record_count <- rr_record_count(register, phase)
  page_count <- ceiling(record_count / page_size)
  base_url <-
    dplyr::if_else(phase == "beta",
            "https://{register}.register.gov.uk/records.tsv?page-size={page_size}&page-index={page_index}",
            "https://{register}.{phase}.openregister.org/records.tsv?page-size={page_size}&page-index={page_index}")
  p <- dplyr::progress_estimated(page_count)
  message("Downloading register '", register, "' from the '", phase, "' phase ...\n")
  out <-
    purrr::map_df(seq_len(page_count),
                  function(page_index) {
                    out <-
                      base_url %>%
                      glue::glue() %>%
                      readr::read_tsv(col_types = readr::cols(.default = "c")) # until all pages downloaded
                    p$tick()$print()
                    out
                  }) %>%
    dplyr::mutate_all(readr::parse_guess) %>%
    dplyr::mutate(register = register, phase = phase) %>%
    dplyr::select(register, phase, dplyr::everything())
  message("\n\n")
  out
}
