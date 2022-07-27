validate_parameters <- function(parameters) {
  if (is.null(parameters) || length(parameters) == 0) {
    return()
  }
  assert_is(parameters, "list")
  assert_named(parameters, unique = TRUE)
  ## NOTE: technically this allows raw and complex through, which is a
  ## bit undesirable but unlikely
  ok <- vlapply(parameters, function(x) {
    length(x) == 1 && is.atomic(x) && !is.na(x)
  })
  if (!all(ok)) {
    stop(sprintf("All parameters must be scalar atomics: error for %s",
                 paste(squote(names(parameters)[!ok]), collapse = ", ")))
  }
}
