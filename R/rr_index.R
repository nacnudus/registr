#' Index a register by CURIEs
#'
#' @description
#' You can refer to records in a register by CURIEs of the form
#' `"prefix:reference"`, where the `prefix` is the name of the register, and
#' the `reference` is a value in a field of the register.  [rr_index()]
#' indexes records by these CURIES.
#'
#' @param register An object of class `"register"`.
#' @param key Character, the name of the field to use as theh `reference` part
#' of the CURIE.  If ommitted (default) then the 'key' field of the register is
#' used (the field with the same name as the register, that is guaranteed to
#' have unique values)
#'
#' @export
#' @examples
#' country <- rr_register("country")
#' rr_index(country, "start-date")
#' rr_index(country, "end-date")
#' rr_index(country)
#' rr_index(rr_register("local-authority-eng"), "local-authority-type")
rr_index <- function(register, key = NULL) {
  snapshot <- rr_snapshot(register)
  .register_name <- snapshot$schema$ids$name
  if (is.null(key)) key <- .register_name
  .key <- rlang::sym(key)
  cardinality <-
    snapshot$schema$fields %>%
    dplyr::filter(field == !! key) %>%
    purrr::pluck("cardinality")
  if (cardinality == "n") {
    stop("Key fields of cardinality \"n\" are not supported by rr_index()")
  }
  register$data %>%
    dplyr::mutate(!! .key := dplyr::if_else(is.na(!! .key), "", !! .key)) %>%
    dplyr::mutate(.curie := paste0(.register_name, ":", !! .key)) %>%
    tidyr::nest(- .curie, .key = ".data")
}
