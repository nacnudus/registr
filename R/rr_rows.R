#' @title Get the number of records in a register
#' @description Queries the API for the number of records in a register, without
#'     having to download the records themselves.
#' @param register character, name of the register, e.g. "school-eng"
#' @param phase character, one of "beta", "alpha", "discovery", default: "beta"
#' @return a numeric vector of length 1
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rr_rows
#' @export
rr_rows <- function(register, phase) {
  dplyr::if_else(phase == "beta",
          "https://{register}.register.gov.uk/register",
          "https://{register}.{phase}.openregister.org/register") %>%
  glue::glue() %>%
  jsonlite::read_json() %>%
  purrr::pluck("total-records")
}

