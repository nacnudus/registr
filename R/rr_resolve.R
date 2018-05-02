#' Resolve links in registers
#'
#' @description
#' Registers can link to each other either by a field with the `"register"`
#' property, or via CURIEs.  See [rr_key_links()] and [rr_curie_links()] for
#' more about that.  These functions resolve those links.
#'
#' When a link is resolve, the whole record is returned in a data frame.  That's
#' because the link doesn't specify a particular field of a record.  For fields
#' with `cardinality = "n"`, several records might be returned.  If the field is
#' also `datatype = "curie"`, then a list of single-row data frames will be
#' returned, because each record could be from a different register with a
#' different set of fields.
#'
#' @param field_name Character, the name of a field to resolve.
#' @param register An object of class `"register"`.
#' @param registers A list of objects of class `"register"`.  Links will be
#'   sought among these registers.
#'
#' @param
#' targets An optional data frame of indexed register records.  This is
#' constructed internally to resolve CURIE fields, but is expensive, so you can
#' create your own as follows, and use it more than once.
#'
#'   targets <-
#'     registers %>%
#'     purrr::map(rr_snapshot) %>%
#'     purrr::map_dfr(rr_index)
#'
#' @name rr_resolve
#' @examples
#' registers <- rr_registers()
#'
#' targets <-
#'   registers %>%
#'   purrr::map(rr_snapshot) %>%
#'   purrr::map_dfr(rr_index)
#'
#' sg <- registers$`statistical-geography`
#' rr_links(sg)
#'
#' rr_resolve_key_field("register", sg, registers)$data
#' rr_resolve_curie_field("organisation", sg, registers)$data
#' rr_resolve_curie_field("organisation", sg, targets = targets)$data
#' rr_resolve_links(sg, registers)$data
#' rr_resolve_links(sg, registers, targets = targets)$data
NULL

#' Resolve a key field
#'
#' @rdname rr_resolve
#' @export
rr_resolve_key_field <- function(field_name, register, registers) {
  key_link <-
    register %>%
    rr_key_links() %>%
    dplyr::filter(field == field_name)
  lookup_data <- purrr::pluck(registers, key_link$register, "data")
  field_sym <- rlang::sym(key_link$field)
  register_sym <- rlang::sym(key_link$register)
  key <-
    dplyr::select(register$data, !! field_sym) %>%
    dplyr::rename(!! register_sym := !! field_sym)
  target <-
    dplyr::left_join(key, lookup_data, by = key_link$register) %>%
    dplyr::select(`entry-number`, type, key, timestamp, hash,
                  dplyr::everything())
  target_list <- purrr::map(seq_len(nrow(target)), ~ target[.x, ])
  register$data <- dplyr::mutate(register$data,
                                 !! rlang::sym(field_name) := target_list)
  register
}

#' Resolve a CURIE field
#'
#' @rdname rr_resolve
#' @export
rr_resolve_curie_field <- function(field_name, register,
                                            registers = NULL, targets = NULL) {
  if (is.null(registers)) {
    if (is.null(targets)) {
      stop("At least one of `registers` and `targets` must be provided")
    }
  } else {
    targets <-
      registers %>%
      purrr::map(rr_snapshot) %>%
      purrr::map_dfr(rr_index)
  }
  field_sym <- rlang::sym(field_name)
  cardinality <-
    register$schema$fields %>%
    dplyr::filter(field == field_name) %>%
    purrr::pluck("cardinality")
  if (cardinality == "n") {
    register$data <-
      dplyr::mutate(register$data,
                    !! field_sym := purrr::map(!! field_sym, resolve_curies,
                                               targets))
  } else {
    register$data <-
      dplyr::mutate(register$data,
                    !! field_sym := resolve_curies(!! field_sym, targets))
  }
  register
}

#' Resolve all links in a register, both key and CURIE
#'
#' @rdname rr_resolve
#' @export
rr_resolve_links <- function(register, registers, targets = NULL) {
  UseMethod("rr_resolve_links")
}

#' @rdname rr_resolve
#' @export
rr_resolve_links.register <- function(register, registers, targets = NULL) {
  snapshot <- rr_snapshot(register)
  links <-
    register %>%
    rr_links() %>%
    dplyr::distinct(type, field)
  key_fields <- dplyr::filter(links, type == "key")
  curie_fields <- dplyr::filter(links, type == "curie")
  for (field in key_fields$field) {
    register <- rr_resolve_key_field(field, register, registers)
  }
  if (nrow(curie_fields) > 0 && is.null(targets)) {
    targets <-
      registers %>%
      purrr::map(rr_snapshot) %>%
      purrr::map_dfr(rr_index)
  }
  for (field in curie_fields$field) {
    register <- rr_resolve_curie_field(field, register, targets = targets)
  }
  register
}

resolve_curies <- function(curies, targets) {
  tibble::tibble(.curie = curies) %>%
    dplyr::left_join(targets, by = ".curie") %>%
    purrr::pluck(".data")
}
