#' @title Download All Register Entries
#' @description Download all entries from a register.
#'
#'              An 'entry' is a sort-of version of a 'record', in that a
#'              'record' is the latest entry per key.  There may be multiple
#'              entries per key.
#'
#'              The registers API separates each entry into an 'entry' and an
#'              'item', where the entry records the key, as well as various
#'              numbers and timestamps, and the item records the field values.
#'              Th `rr_entries()` function combines each 'entry' and 'item' back
#'              together.
#'
#'              Technically, an entry can have multiple related items, which
#'              means there can be multiple sets of field values against a
#'              single key, but no registers use that possibility ('indexes' do,
#'              which is another story) so this function assumes one item per
#'              entry.
#'
#'              This function doesn't have to handle paged downloads, because it
#'              downloads the whole register as a zip file.
#'
#' @param register character, name of the register, e.g. "school-eng"
#' @param phase character, one of "beta", "alpha", default: "beta"
#' @return A tibble of all the entries in a register
#' @details *Does* include all entries, not merely the latest entry per record.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  rr_entries("country")
#'  }
#' }
#' @rdname rr_entries
#' @export
rr_entries <- function(register, phase = "beta") {
  zip_url <-
    dplyr::if_else(phase == "beta",
                   "https://{register}.register.gov.uk/download-register",
                   "https://{register}.{phase}.openregister.org/download-register") %>%
  glue::glue()
  zip_file <- tempfile()
  unzip_dir <- tempdir()
  download.file(zip_url, zip_file, mode = "wb")
  unzip(zip_file, exdir = unzip_dir)
  item_files <- list.files(file.path(unzip_dir, "item"), full.names = TRUE)
  entry_files <- list.files(file.path(unzip_dir, "entry"), full.names = TRUE)
  items <-
    purrr::map2_dfr(item_files,
                    stringr::str_sub(basename(item_files), end = -6),
                    ~ jsonlite::read_json(.x) %>%
                      tibble::as_data_frame() %>%
                      dplyr::mutate(item_hash = paste0("sha-256:", .y)) %>% # nest() can't handle even backticked `item-hash`
                      tidyr::nest(-item_hash)) %>%
    dplyr::rename(`item-hash` = item_hash)
  entries <-
    purrr::map(entry_files, jsonlite::read_json) %>%
    purrr::map_df(tibble::as_data_frame) %>%
    tidyr::unnest(`item-hash` ) %>%
    dplyr::left_join(items, by = "item-hash") %>%
    tidyr::unnest(data)
  file.remove(zip_file)
  unlink(unzip_dir, recursive = TRUE)
  entries
}
