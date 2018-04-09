#' List registers linked to by CURIEs
#'
#' @description
#' Values in fields of datatype `"curie"` link to other registers.  These
#' functions list which registers are linked to by a field or all the fields in
#' a register.
#'
#' [rr_curie_fields()] names which fields in a register are of datatype
#' `"curie"`.
#'
#' [rr_curie_links()] names which registers are linked to by a field of datatype
#' `"curie"`, or gives a data frame of all fields in a register that are of
#' datatype `"curie"`, and the names of the registers they link to.
#'
#' @param x Object of class `"register"`, or a character vector that is a field
#' of a register object (`register$data$foo`) of datatype `"curie"`.
#'
#' @name rr_curie
#' @examples
#' register <- rr_register("statistical-geography")
#' rr_curie_fields(register)
#' rr_curie_links(register$data$area)
#' rr_curie_links(register$data$organisation)
#' rr_curie_links(register)
NULL

#' @rdname rr_curie
#' @export
rr_curie_fields <- function(x) {
  UseMethod("rr_curie_fields")
}

#' @rdname rr_curie
#' @export
rr_curie_fields.register <- function(x) {
  x$schema$fields %>%
  dplyr::filter(datatype == "curie") %>%
  dplyr::pull(field)
}

#' @rdname rr_curie
#' @export
rr_curie_links <- function(x) {
  UseMethod("rr_curie_links")
}

#' @rdname rr_curie
#' @export
rr_curie_links.default <- function(x) {
  purrr::map(x,
             ~ if (length(.x) > 1) {
               purrr::map_chr(.x, ~ rr_parse_curie(.x)$prefix)
             } else {
               rr_parse_curie(.x)$prefix
             }) %>%
    purrr::flatten() %>%
    purrr::discard(is.na) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    sort()
}

#' @rdname rr_curie
#' @export
rr_curie_links.register <- function(x) {
  curie_fields <- rr_curie_fields(x)
  x$data %>%
  dplyr::select(curie_fields) %>%
  purrr::map(rr_curie_links) %>%
  purrr::map2_dfr(curie_fields, ~ tibble::tibble(field = .y, register = .x)) %>%
  dplyr::distinct() %>%
  dplyr::arrange() %>%
  dplyr::bind_rows(tibble::tibble(field = character(), register = character()))
}
