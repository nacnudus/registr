#' A snapshot of records from given entries
#'
#' @description Filters for entries up to a given `entry-number` or `timestamp`,
#'   then returns the latest entry per `key`.
#'
#'   Use [rr_snapshot()] for a snapshot of a whole register (both schema and
#'   data).
#'
#' @param entries A data frame or tibble with at least the fields `key` and
#' either `entry-number` or `timestamp`, depending on the value of `sequence`.
#' @param sequence One of `"entry-number"` (default) or `"timestamp"`.  The
#'   snapshot is taken at the `maximum` value of this field of `entries`.
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
#' country <- rr_register("country")
#'
#' # There can be more than one entry per key
#' country$data %>%
#'   dplyr::filter(key == "CZ") %>%
#'   dplyr::select(`entry-number`, timestamp, name, `official-name`)
#'
#' # Snapshot by entry number
#' country$data %>%
#'   rr_records(maximum = 64) %>%
#'   dplyr::filter(key == "CZ") %>%
#'   dplyr::select(`entry-number`, timestamp, name, `official-name`)
#'
#' country$data %>%
#'   rr_records(maximum = 217, include_maximum = FALSE) %>%
#'   dplyr::filter(key == "CZ") %>%
#'   dplyr::select(`entry-number`, timestamp, name, `official-name`)
#'
#' # Snapshot by entry timestamp
#' country$data %>%
#'   rr_records(sequence = "timestamp",
#'              maximum = as.POSIXct("2016-04-05 13:23:05", tz = "UTC")) %>%
#'   dplyr::filter(key == "CZ") %>%
#'   dplyr::select(`entry-number`, timestamp, name, `official-name`)
#'
#' # When the timestamp of any entry of a key is missing, no entries are
#' # returned.
#' entries <- country$data
#' entries$timestamp[entries$`entry-number` == 64] <- NA
#' rr_records(entries, sequence = "timestamp") %>%
#'   dplyr::filter(key == "CZ")
#'
#' # Not all entries are data, some are schema
#' country$schema$custodians %>%
#'   rr_records(maximum = 11) %>%
#'   dplyr::select(`entry-number`, timestamp, custodian)
#' @rdname rr_records
#' @export
rr_records <- function(entries, sequence = c("entry-number", "timestamp"),
                       maximum = NULL, include_maximum = TRUE) {
  seq_string <- match.arg(sequence)
  seq_sym <- rlang::sym(seq_string)
  missing_fields <- missing_col_names(entries, key, !! seq_sym)
  if (length(missing_fields) > 0L) {
    stop("`entries` is missing the fields:",
         paste0("\n    - `", missing_fields, "`"))
  }
  if (is.null(maximum)) {
    maximum <- switch(seq_string,
                      `entry-number` = max(entries$`entry-number`),
                      timestamp = max(entries$`timestamp`))
  }
  if (include_maximum) `%</=%` <- `<=` else `%</=%` <- `<`
  entries %>%
    dplyr::filter(!! seq_sym %</=% maximum) %>%
    dplyr::group_by(type, key) %>%
    dplyr::arrange(desc(!! seq_sym), desc(`entry-number`)) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!! seq_sym)
}
