##' Set configuration options. Not all can currently be set; this will
##' be expanded over time. See Details.
##'
##' Options are set in the order that they are provided.  Currently,
##' if setting one option fails, no further options will be processed
##' but previous ones will be (do not rely on this behaviour, it
##' may change).
##'
##' Currently you can set:
##'
##' * `core.require_complete_tree`
##'
##' See [outpack::outpack_init] for description of these options.
##'
##' @title Set configuration options
##'
##' @param ... Named options to set (e.g., pass the argument
##'   `core.require_complete_tree = TRUE`)
##'
##' @param options As an alternative to `...`, you can pass a list of
##'   named options here (e.g., `list(core.require_complete_tree =
##'   TRUE)`).  This interface is typically easier to program against.
##'
##' @inheritParams outpack_location_list
##'
##' @return Nothing
##' @export
outpack_config_set <- function(..., options = list(...), root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  if (!missing(options) && ...length() > 0) {
    stop("If 'options' is given, no dot arguments are allowed")
  }
  if (length(options) == 0) {
    return(invisible())
  }

  assert_is(options, "list")
  assert_named(options)

  setters <- list(
    "core.require_complete_tree" = config_set_require_complete_tree)

  unknown <- setdiff(names(options), names(setters))
  if (length(unknown)) {
    stop("Can't set configuration option: ",
         paste(squote(unknown), collapse = ", "))
  }

  for (nm in names(options)) {
    root <- setters[[nm]](options[[nm]], root)
  }

  invisible()
}


config_set_require_complete_tree <- function(value, root) {
  config <- root$config

  if (config$core$require_complete_tree == value) {
    message("'core.require_complete_tree' was unchanged")
    return()
  }

  if (value) {
    id <- root$index()$unpacked$packet
    outpack_location_pull_packet(id, recursive = TRUE, root = root)
  }

  config$core$require_complete_tree <- value
  config_write(config, root$path)
  root$config <- config
}


config_new <- function(path_archive, use_file_store, require_complete_tree) {
  if (!is.null(path_archive)) {
    assert_scalar_character(path_archive)
  }
  assert_scalar_logical(use_file_store)
  if (is.null(path_archive) && !use_file_store) {
    stop("if 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  }

  assert_scalar_logical(require_complete_tree)

  ## TODO: There's a good reason here to wonder if this _should_ be
  ## configurable.  I'll keep it here within the configuration even
  ## though it can't be changed really.
  hash_algorithm <- "sha256"

  list(
    schemaVersion = outpack_schema_version(),
    core = list(
      path_archive = path_archive,
      use_file_store = use_file_store,
      require_complete_tree = require_complete_tree,
      hash_algorithm = hash_algorithm),
    location = new_location_entry(local, 0, "local", NULL))
}


config_serialise <- function(config, path) {
  config$schemaVersion <- scalar(config$schemaVersion) # nolint
  config$core <- lapply(config$core, scalar)

  prepare_location <- function(loc) {
    c(lapply(loc[setdiff(names(loc), "args")], scalar),
      list(args = lapply(loc$args[[1]], scalar)))
  }
  config$location <- lapply(seq_len(nrow(config$location)), function(i) {
    prepare_location(config$location[i, ])
  })

  to_json(config, "config")
}


config_write <- function(config, root_path) {
  writeLines(config_serialise(config),
             file.path(root_path, ".outpack", "config.json"))
}


config_read <- function(root_path) {
  config <- jsonlite::read_json(file.path(root_path, ".outpack/config.json"))
  ## NOTE: make sure that this matches the order in new_location_entry
  config$location <- data_frame(
    name = vcapply(config$location, "[[", "name"),
    id = vcapply(config$location, "[[", "id"),
    priority = vnapply(config$location, "[[", "priority"),
    type = vcapply(config$location, "[[", "type"),
    args = I(lapply(config$location, "[[", "args")))
  config
}
