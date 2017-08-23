#' @title Get the number of records in a register
#' @description Queries the API for the number of records in a register, without
#'              having to download the records themselves.
#'
#'              A 'record' is the latest entry per key, so the record count is
#'              the number of unique keys in a register.
#' @param register character, name of the register, e.g. "school-eng"
#' @param phase character, one of "beta", "alpha", "discovery", default: "beta"
#' @return a numeric vector of length 1
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rr_record_count
#' @export
rr_record_count <- function(register, phase) {
  dplyr::if_else(phase == "beta",
          "https://{register}.register.gov.uk/register",
          "https://{register}.{phase}.openregister.org/register") %>%
  glue::glue() %>%
  jsonlite::read_json() %>%
  purrr::pluck("total-records")
}

