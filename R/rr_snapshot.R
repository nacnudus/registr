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
#'   snapshot is taken at the `maximum` value of this column of
#'   `register$entries`.
#' @param maximum An `integer` if `sequence` is `"entry-number" or `POSIXct` if
#'   `sequence` is `"timestamp"`, giving the time at which to take the snapshot.
#'   Only the latest entry up to this value will be kept, per `key`.  By default
#'   it is the maximum of all entries, to return the most recent state of the
#'   register.
#' @param include_maximum Logical, whether to include entries whose
#' `entry-number` or `timestamp` equals `maximum`.
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
#' \dontrun{
#' if(interactive()){
#' country <- rr_register("country")
#'
#' country$data$entries %>%
#'   filter(key == "CZ") %>%
#'   select(`entry-number`, timestamp, name, `official-name`)
#'
#' country$data$entries %>%
#'   rr_records(maximum = 64) %>%
#'   filter(key == "CZ") %>%
#'   select(`entry-number`, timestamp, name, `official-name`)
#'
#' country$data$entries %>%
#'   rr_records(maximum = 217, include_maximum = FALSE) %>%
#'   filter(key == "CZ") %>%
#'   select(`entry-number`, timestamp, name, `official-name`)
#'
#' country$data$entries %>%
#'   rr_records(sequence = "timestamp",
#'              maximum = as.POSIXct("2016-04-05 13:23:05", tz = "UTC")) %>%
#'   filter(key == "CZ") %>%
#'   select(`entry-number`, timestamp, name, `official-name`)
#'
#' entries <- country$data$entries
#' entries$timestamp[entries$`entry-number` == 64] <- NA
#' rr_records(entries, sequence = "timestamp") %>%
#'   filter(key == "CZ")
#'
#' # Not all entries are data, some are schema
#' country$schema$custodians %>%
#'   rr_records(maximum = 11) %>%
#'   select(`entry-number`, timestamp, custodian)
#'  }
#' }
#'
#' @export
rr_snapshot <- function(register, sequence = c("entry-number", "timestamp"),
                        maximum = NULL, include_maximum = TRUE) {
  UseMethod("rr_snapshot")
}

#' @export
rr_snapshot.register <- function(register,
                                 sequence = c("entry-number", "timestamp"),
                                 maximum = NULL, include_maximum = TRUE) {
  entries <- rr_records(register$entries, sequence, maximum, include_maximum)
  entry_data <- resolve_entry_items(entries, register$items)
  system_entries <- dplyr::filter(entry_data, type == "system")
  name <-
    dplyr::filter(system_entries, key == "name") %>%
    dplyr::select(-json) %>%
    tidyr::unnest()
  custodian <-
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
    map2(rlang::syms(fields$field),
         fields$datatype,
         ~ rlang::expr(apply_datatype(!! .x, !! .y)))
  names(converters) <- fields$field
  user_entries <- dplyr::mutate(user_entries, !!! converters)
  list(name = name, custodian = custodian, fields = fields, data = user_entries)
  structure(list(root_hash = register$root_hash,
                 entries = register$entries,
                 items = register$items,
                 schema = list(names = name,
                               custodians = custodian,
                               fields = fields),
                 data = user_entries),
            class = "register")
}
