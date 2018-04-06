#' Download a register
#'
#' @description Downloads a whole register (or reads it from a file) and
#'   constructs an object that can be interrogated for its records, entries,
#'   items, schema, links to other registers, etc.
#'
#'   You should probably run [rr_snapshot()] on the output before using it.
#'
#' @param register character, name of the register, e.g. "school-eng"
#' @param phase character, one of "beta", "alpha", default: "beta"
#' @param path_type Character, one of `"url"` or `"file"` to decide what to do
#'   with `register`.
#'
#' @return An S3 object of class `register`
#'
#' @examples
#' \dontrun{
#'   rr_register("country")
#'   rr_register("country", "beta")
#' }
#'
#'
#' @export
rr_register <- function(register, phase = c("beta", "alpha"),
                        path_type = c("url", "file"), parse_datetimes = FALSE) {
  rsf <- register_lines(register, phase, path_type)
  root_hash <- parse_root_hash(rsf)
  entries <- parse_entries(rsf)
  items <- parse_items(rsf)
  entry_data <- resolve_entry_items(entries, items)
  system_entries <- dplyr::filter(entry_data, type == "system")
  names <-
    dplyr::filter(system_entries, key == "name") %>%
    dplyr::select(-json) %>%
    tidyr::unnest()
  custodians <-
    dplyr::filter(system_entries, key == "custodian") %>%
    dplyr::select(-json) %>%
    tidyr::unnest()
  fields <-
    dplyr::filter(system_entries, stringr::str_detect(key, "^field:")) %>%
    dplyr::select(-json) %>%
    tidyr::unnest()
  user_entries <-
    dplyr::filter(entry_data, type == "user") %>%
    dplyr::select(-json) %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(blank_tibble(unique(fields$field))) %>%
    dplyr::select(`entry-number`, type, key, timestamp, hash,
                  unique(fields$field))
  converters <-
    purrr::map2(rlang::syms(fields$field),
                fields$datatype,
                ~ rlang::expr(apply_datatype(!! .x,
                                             !! .y,
                                             apply_iso_8601 = parse_datetimes)))
  names(converters) <- fields$field
  user_entries <- dplyr::mutate(user_entries, !!! converters)
  structure(list(root_hash = root_hash,
                 entries = entries,
                 items = items,
                 schema = list(names = names,
                               custodians = custodians,
                               fields = fields),
                 data = user_entries),
            class = "register")
}

parse_root_hash <- function(rsf) {
  root_hash_line <- rsf[stringr::str_detect(rsf, "^assert-root-hash\\t")]
  stringr::str_extract(root_hash_line, "(?<=^assert-root-hash\\t).*$")
}

parse_entries <- function(rsf) {
  rsf[stringr::str_detect(rsf, "^append-entry\\t")] %>%
    paste0(collapse = "\n") %>%
    paste0("\n") %>%
    readr::read_tsv(col_types = c("_ccTc"),
                    col_names = c("type", "key", "timestamp", "hash-list"),
                    na = character()) %>%
    dplyr::mutate(`hash-list` = purrr::map(`hash-list`, parse_hash_list),
                  `entry-number` = seq_len(n())) %>%
    dplyr::select(`entry-number`, dplyr::everything())
}

parse_items <- function(rsf) {
  rsf[stringr::str_detect(rsf, "^add-item\\t")] %>%
    stringr::str_extract("(?<=^add-item\\t).*$") %>%
    tibble::tibble(json = .) %>%
    dplyr::mutate(hash = purrr::map_chr(json,
                                        digest::digest,
                                        algo = "sha256",
                                        serialize = FALSE),
                  json = purrr::map(json, jsonlite::fromJSON),
                  data = purrr::map(json, tibble::as_tibble))
}

parse_hash_list <- function(x) {
  stringr::str_extract(stringr::str_split(x, ";")[[1]], "(?<=^sha-256:).*$")
}

resolve_entry_items <- function(entries, items) {
  entries %>%
    tidyr::unnest(`hash-list`) %>%
    dplyr::rename(hash = `hash-list`) %>%
    dplyr::left_join(items, by = "hash")
}
