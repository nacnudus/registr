#' A snapshot of a whole register
#'
#' @description Returns a register as it was at a given time, with no history
#'   and no future.
#'
#'   Use [rr_records()] for snapshots of specifically a register's schema or its
#'   data, but without adjusting the data to the schema of the same time.
#'
#' @param register An object of class `register`, returned by [rr_register()].
#' @param sequence One of `"entry-number"` (default) or `"timestamp"`.  The
#'   snapshot is taken at the `maximum` value of this field of
#'   `register$entries`.
#' @param maximum An `integer` if `sequence` is `"entry-number" or `POSIXct` if
#'   `sequence` is `"timestamp"`, giving the time at which to take the snapshot.
#'   Only the latest entry up to this value will be kept, per `key`.  By default
#'   it is the maximum of all entries, to return the most recent state of the
#'   register.
#' @param include_maximum Logical, whether to include entries whose
#' `entry-number` or `timestamp` equals `maximum`.
#' @param parse_datetimes Logical, whether to parse ISO8601 strings as datetimes
#'   with [parsedate::parse_iso_8601()], otherwise leave as a string.  Partial
#'   datetimes are parsed as the earliest possible datetime, e.g. `"2018"`
#'   becomes `"2018-01-01 UTC"`.
#'
#' @return A tibble of records (latest entry per key).
#'
#' @details
#' A record is the latest entry for a given key.  'Latest' can be either the
#' maximum `entry-number` (recommended), or the maximum `timestamp`.  An entry
#' `timestamp` may be `NA`, and may not be unique.  If the `timestamp` of any
#' entry of a given key is `NA`, then no record for that key will be returned.
#' If the `timestamp` is not unique, then the entry with the maximum
#' `entry-number` will be chosen.
#'
#' @examples
#' country <- rr_register("country")
#' snapshot <- rr_snapshot(country)
#'
#' nrow(country$data)
#' nrow(snapshot$data)
#'
#' country$schema$custodian
#' snapshot$schema$custodian
#' @export
rr_snapshot <- function(register, sequence = c("entry-number", "timestamp"),
                        maximum = NULL, include_maximum = TRUE,
                        parse_datetimes = FALSE) {
  UseMethod("rr_snapshot")
}

#' @export
rr_snapshot.register <- function(register,
                                 sequence = c("entry-number", "timestamp"),
                                 maximum = NULL, include_maximum = TRUE,
                                 parse_datetimes = FALSE) {
  entries <- rr_records(register$entries, sequence, maximum, include_maximum)
  entry_data <- resolve_entry_items(entries, register$items)
  system_entries <- dplyr::filter(entry_data, type == "system")
  register_id <-
    system_entries %>%
    dplyr::filter(key == "name") %>%
    flatten_entries()
  register_name <-
    system_entries %>%
    dplyr::filter(key == "register-name") %>%
    flatten_entries()
  custodian <-
    system_entries %>%
    dplyr::filter(key == "custodian") %>%
    flatten_entries()
  fields <-
    system_entries %>%
    dplyr::filter(stringr::str_detect(key, "^field:")) %>%
    flatten_entries()
  cardinality_one_fields <-
    fields %>%
    dplyr::filter(cardinality == "1") %>%
    dplyr::pull(field)
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
  structure(list(root_hash = register$root_hash,
                 entries = register$entries,
                 items = register$items,
                 schema = list(ids = register_id,
                               names = register_name,
                               custodians = custodian,
                               fields = fields),
                 data = user_entries),
            class = "register")
}
