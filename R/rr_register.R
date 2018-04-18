#' Download a register
#'
#' @description Downloads a whole register (or reads it from a file) and
#'   constructs an object that can be interrogated for its records, entries,
#'   items, schema, links to other registers, etc.
#'
#'   You should probably run [rr_snapshot()] on the output before using it.
#'
#' @param parse_datetimes Logical, whether to parse ISO8601 strings as datetimes
#'   with [parsedate::parse_iso_8601()], otherwise leave as a string.  Partial
#'   datetimes are parsed as the earliest possible datetime, e.g. `"2018"`
#'   becomes `"2018-01-01 UTC"`.
#' @inheritParams rr_rsf
#'
#' @return An S3 object of class `register`
#'
#' @examples
#' rr_register("country")
#' rr_register("country", "beta")
#' path <- tempfile()
#' download.file("https://country.register.gov.uk/download-rsf", path)
#' rr_register(file = path)
#' unlink(path)
#' @export
rr_register <- function(name = NULL, phase = c("beta", "alpha"), file = NULL,
                        write = FALSE, dest_path = NULL,
                        parse_datetimes = FALSE, quiet = TRUE) {
  rsf <- rr_rsf(name, phase, file, write, dest_path, quiet = quiet)
  root_hash <- parse_root_hash(rsf)
  entries <- parse_entries(rsf)
  items <- parse_items(rsf)
  entry_data <- resolve_entry_items(entries, items)
  system_entries <- dplyr::filter(entry_data, type == "system")
  names <-
    system_entries %>%
    dplyr::filter(key == "name") %>%
    flatten_entries()
  custodians <-
    system_entries %>%
    dplyr::filter(key == "custodian") %>%
    flatten_entries()
  fields <-
    system_entries %>%
    dplyr::filter(stringr::str_detect(key, "^field:")) %>%
    flatten_entries() %>%
    dplyr::select(`entry-number`, type, key, timestamp, hash,
                  field, datatype, phase, register, cardinality, text)
  cardinality_one_fields <-
    fields %>%
    dplyr::filter(cardinality == "1") %>%
    dplyr::pull(field) %>%
    unique()
  user_entries <-
    entry_data %>%
    dplyr::filter(type == "user") %>%
    dplyr::select(-json) %>%
    tidyr::unnest() %>%
    dplyr::bind_rows(blank_tibble(unique(fields$field))) %>%
    dplyr::mutate_if(is.list,
                     ~ purrr::map(.x, ~ if (is.null(.x)) NA else .x)) %>%
    dplyr::mutate_at(cardinality_one_fields, purrr::flatten_chr) %>%
    dplyr::select(`entry-number`, type, key, timestamp, hash,
                  unique(fields$field))
  converters <-
    purrr::pmap(list(rlang::syms(fields$field),
                     fields$datatype,
                     fields$cardinality),
                ~ rlang::expr(rr_apply_datatype(!! ..1, !! ..2, !! ..3,
                                                !! parse_datetimes)))
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
                  json = purrr::modify_depth(json, 2, list),
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

# Flatten list-columns as long as they aren't cardinality='n'.
# It's complicated because by this stage some list-elements can be NULL
flatten_entries <- function(x, fields_to_flatten = NULL) {
  x <-
    x %>%
    dplyr::select(-json) %>%
    tidyr::unnest()
  if (is.null(fields_to_flatten))
    fields_to_flatten <- colnames(x)[purrr::map_lgl(x, is.list)]
  x %>%
    dplyr::mutate_if(is.list,
                     ~ purrr::map(.x, ~ if (is.null(.x)) NA else .x)) %>%
    dplyr::mutate_at(fields_to_flatten, purrr::flatten_chr)
}
