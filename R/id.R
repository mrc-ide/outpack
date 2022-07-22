##' Generate a new outpack id
##'
##' @title Generate outpack id
##'
##' @return A new outpack id (a string)
##'
##' @export
outpack_id <- function() {
  time <- Sys.time()
  sprintf("%s-%s%s",
          iso_time_str(time),
          val_to_bytes(as.numeric(time), 2),
          paste(as.character(openssl::rand_bytes(2)), collapse = ""))
}


validate_outpack_id <- function(id) {
  assert_scalar_character(id)
  if (!grepl(re_id, id)) {
    stop(sprintf("Malformed id '%s'", id), call. = FALSE)
  }
}
