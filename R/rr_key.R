#' List registers linked to by key fields
#'
#' @description
#' Values in fields with the `"register"` proporty link to other registers.
#' These functions list which registers are linked to by a field or all the
#' fields in a register.
#'
#' [rr_key_fields()] names which fields in a register have the `"register"`
#' properety.
#'
#' [rr_key_links()] names which registers are linked to by a register that has
#' fields with the `"register"` property.
#'
#' @param x Object of class `"register"`, or a character vector that is a field
#' of a register object (`register$data$foo`) with the `"register"` property.
#'
#' @return A character vector of names of registers that are linked to.
#' @name rr_key
#' @examples
#' register <- rr_register("allergen")
#' rr_key_fields(register)
#' rr_key_links(register)
NULL

#' @rdname rr_key
#' @export
rr_key_fields <- function(x) {
  UseMethod("rr_key_fields")
}

#' @rdname rr_key
#' @export
rr_key_fields.register <- function(x) {
  x$schema$fields %>%
  dplyr::filter(!is.na(register), !(register == field)) %>%
  dplyr::pull(field)
}

#' @rdname rr_key
#' @export
rr_key_links <- function(x) {
  UseMethod("rr_key_links")
}

#' @rdname rr_key
#' @export
rr_key_links.register <- function(x) {
  x$schema$fields %>%
  dplyr::filter(!is.na(register), !(register == field)) %>%
  dplyr::pull(register) %>%
  unique()
}
