##' Start a packet build (`outpack_packet_start`), end one
##' (`outpack_packet_cancel`, `outpack_packet_end`) and interact with
##' the current packet (`outpack_packet_use_dependency`,
##' `outpack_packet_run`)
##'
##' @title Start, interact with, and end a packet build
##'
##' @param path Path to the build / output directory.
##'
##' @param name The name of the packet
##'
##' @param parameters Optionally, a named list of parameters.  The
##'   names must be unique, and the values must all be non-NA scalar
##'   atomics (logical, integer, numeric, character)
##'
##' @param id Optionally, an outpack id via [outpack::outpack_id]. If
##'   not given a new id will be generated.
##'
##' @param root The outpack root. Will be searched for from the
##'   current directory if not given.
##'
##' @return Invisibly, a copy of the packet data
##' @rdname outpack_packet
##' @export
outpack_packet_start <- function(path, name, parameters = NULL, id = NULL,
                                 root = NULL) {
  root <- outpack_root_open(root, locate = TRUE)
  if (!is.null(current$packet)) {
    ## * Could make this root specific?
    ## * Could make this path specific?
    ## * Could use a stack?
    ## * Could automatically exit?
    ## * Could be fine if name/id match
    stop("Already a current packet - call outpack_packet_cancel()")
  }

  assert_scalar_character(name)
  assert_directory(path)
  validate_parameters(parameters)

  if (is.null(id)) {
    id <- outpack_id()
  } else {
    validate_outpack_id(id)
  }

  time <- list(start = Sys.time())

  ## LOGGING: name / id / path, start time, parameters
  ##
  ## We log these all in orderly and that's super useful
  current$packet <- list(
    name = name,
    id = id,
    path = path,
    parameters = parameters,
    files = list(),
    time = time,
    root = root)

  invisible(current$packet)
}


##' @export
##' @rdname outpack_packet
outpack_packet_cancel <- function() {
  p <- outpack_packet_current()
  outpack_packet_clear()
}


##' @export
##' @rdname outpack_packet
outpack_packet_current <- function() {
  if (is.null(current$packet)) {
    stop("No current packet")
  }
  current$packet
}


##' @export
##' @rdname outpack_packet
outpack_packet_end <- function() {
  p <- outpack_packet_current()
  p$time$end <- Sys.time()
  hash_algorithm <- p$root$config$core$hash_algorithm
  json <- outpack_metadata_create(p$path, p$name, p$id, p$time,
                                  files = NULL,
                                  depends = p$depends,
                                  parameters = p$parameters,
                                  script = p$script,
                                  custom = p$custom,
                                  session = NULL,
                                  file_hash = p$files$immutable,
                                  file_ignore = p$files$ignored,
                                  hash_algorithm = hash_algorithm)
  outpack_insert_packet(p$path, json, p$root)
  outpack_packet_clear()
}


##' @export
##' @rdname outpack_packet
##'
##' @param script Path to the script within the packet directory (a
##'   relative path).  This function can be safely called multiple
##'   times within a single packet run (or zero times!) as needed.
##'
##' @param envir Environment in which to run the script
outpack_packet_run <- function(script, envir = .GlobalEnv) {
  p <- outpack_packet_current()
  assert_relative_path(script, no_dots = TRUE)
  assert_file_exists(script, p$path, "Script")

  ## TODO: Logging

  ## TODO: Control over echo (as in orderly)

  ## TODO: Control over running in separate process (if we do that,
  ## the process should return session, too)

  ## TODO: What should we do/store on error?

  with_dir(p$path, sys.source(script, envir = envir))

  p$script <- c(p$script, script)
  current$packet <- p

  ## What is a good return value here?
  invisible()
}


##' @export
##' @rdname outpack_packet
##'
##' @param id The id of an existing packet to use files from
##'
##' @param files A named character vector of files; the name
##'   corresponds to the name within the current packet, while the
##'   value corresponds to the name within the upstream packet
outpack_packet_use_dependency <- function(id, files) {
  p <- outpack_packet_current()
  root <- p$root

  ## TODO: currently no capacity here for *querying* to find the id
  ## (e.g., latest(name) or something more complex).  Any progress on
  ## this will depend on the query interface.  It's probable that we
  ## might want to record the query here alongside the id, if one was
  ## used?  Or should we allow a query here?

  ## TODO: allow pattern to be used in files (but then how do we
  ## translate to destination?)
  assert_named(files, unique = TRUE)
  assert_relative_path(names(files), no_dots = TRUE)

  src <- unname(files)
  dst <- file.path(p$path, names(files))

  ## Ensures that we can actually find all the files within this
  ## dependency (and that it exists)
  validate_packet_has_file(root, id, src)

  ## Actually copy the files into the requested directory
  file_export(root, id, src, dst)

  ## Only update packet information after success, to reflect new
  ## metadata

  depends <- list(id = id,
                  files = data_frame(here = names(files), there = src))
  current$packet$depends <- c(p$depends, list(depends))

  invisible()
}


##' @section Custom metadata:
##'
##' The `outpack_packet_add_custom` function adds arbitrary
##'   additional metadata into a packet. It is primarily designed for
##'   use with applications that build on outpack to provide
##'   additional information beyond the minimal set provided by
##'   outpack.
##'
##' For example, orderly tracks "artefacts" which collect groups of
##'   file outputs into logical bundles.  To support this it needs to
##'   register additional data for each artefact with:
##'
##' * the description of the artefect (a short phrase)
##' * the format of the artefact (a string describing the data type)
##' * the contents of the artefact (an array of filenames)
##'
##' JSON for this might look like:
##'
##' ```json
##' {
##'   "artefacts": [
##'     {
##'       "description": "Data for onward use",
##'       "format": "data",
##'       "contents": ["results.rds", "summary.rds"]
##'     },
##'     {
##'       "description": "Diagnostic figures",
##'       "format": "staticgraph",
##'       "contents": ["fits.png", "inputs.png"]
##'     }
##'   ]
##' }
##' ```
##'
##' Here, we describe two artefacts, together collecting four files.
##'
##' We need to store these in outpack's final metadata, and we want to
##'   do this in a way that allows easy querying later on while
##'   scoping the data to your application.  To allow for this we
##'   group all data your application adds under an application key
##'   (e.g., `orderly`).  You can then store whatever data you want
##'   beneath this key.
##'
##' **NOTE1**: A limitation here is that the filenames above cannot be
##'   checked against the outpack list of files because outpack does
##'   not know that `contents` here refers to filenames.
##'
##' **NOTE2**: To allow for predictable serialisation to JSON, you
##'   must serialise your own data before passing through to
##'   `outpack_packet_add_custom`.
##'
##' @export
##' @rdname outpack_packet
##'
##' @param application The name of the application (used to organise
##'   the data and query it later, see Details)
##'
##' @param data Additional metadata to add to the packet. This must be
##'   a string representing already-serialised json data.
##'
##' @param schema Optionally, but recommended, a schema to validate
##'   `data` against.  Validation will only happen if the option
##'   `outpack.schema_validate` is `TRUE`, as for the main schema
##'   validation.  Will be passed to [jsonvalidate::json_schema], so
##'   can be a string containing the schema or a path to the schema.
outpack_packet_add_custom <- function(application, data, schema = NULL) {
  p <- outpack_packet_current()

  assert_scalar_character(application)
  assert_scalar_character(data)
  if (!is.null(schema)) {
    assert_scalar_character(data)
  }

  tryCatch(
    jsonlite::parse_json(data),
    error = function(e) {
      stop("Syntax error in custom metadata: ", e$message, call. = FALSE)
    })

  if (should_validate_schema(schema)) {
    tryCatch(
      custom_schema(schema)$validate(data, error = TRUE),
      error = function(e) {
        stop("Validating custom metadata failed: ", e$message, call. = FALSE)
      })
  }

  if (application %in% vcapply(p$custom, "[[", "application")) {
    stop(sprintf("metadata for '%s' has already been added for this packet",
                 application))
  }

  custom <- list(application = application, data = data)
  current$packet$custom <- c(p$custom, list(custom))
  invisible()
}


##' Mark file within an in-progress packet. This will store the hash
##' of the file within the internal outpack structures and force an
##' error if the file is changed or deleted later.  The function
##' [outpack::outpack_packet_file_list()] will report on which files
##' are marked (or unmarked) within the directory.
##'
##' @title Mark files during packet run
##'
##' @param files A character vector of relative paths
##'
##' @param status A status to mark the file with. Must be "immutable"
##'   or "ignored"
##'
##' @return Depending on function
##'
##' * `outpack_packet_file_mark` returns nothing
##' * `outpack_packet_file_list` returns a [data.frame] with columns
##'   `path` and `status` (`immutable`, `ignored` or `unknown`)
##'
##' @rdname outpack_packet_file
##'
##' @export
outpack_packet_file_mark <- function(files, status) {
  status <- match_value(status, c("immutable", "ignored"))
  p <- outpack_packet_current()

  assert_relative_path(files, no_dots = TRUE)
  assert_file_exists(files, p$path)

  ## TODO: these are exclusive categories because we later return a
  ## 1:1 mapping of file to status
  if (status == "immutable") {
    hash_algorithm <- p$root$config$core$hash_algorithm
    value <- with_dir(p$path, hash_files(files, hash_algorithm, named = TRUE))

    if (any(files %in% p$files$ignored)) {
      stop(sprintf("Cannot mark ignored files as immutable: %s",
                   paste(squote(intersect(files, p$files$ignored)),
                         collapse = ", ")))
    }
    if (any(files %in% names(p$files$immutable))) {
      validate_hashes(value, p$files$immutable)
      value <- value[!(names(value) %in% names(p$files))]
    }

    current$packet$files$immutable <- c(p$files$immutable, value)
  } else if (status == "ignored") {
    if (any(files %in% names(p$files$immutable))) {
      stop(sprintf("Cannot mark immutable files as ignored: %s",
                   paste(squote(intersect(files, names(p$files$immutable))),
                         collapse = ", ")))
    }

    current$packet$files$ignored <- union(p$files$ignored, files)
  }
  invisible()
}


##' @export
##' @rdname outpack_packet_file
outpack_packet_file_list <- function() {
  p <- outpack_packet_current()
  files <- with_dir(p$path,
                    dir(all.files = TRUE, recursive = TRUE, no.. = TRUE))
  status <- rep("unknown", length(files))
  status[files %in% names(p$files$immutable)] <- "immutable"
  status[files %in% p$files$ignored] <- "ignored"
  data_frame(path = files, status = status)
}


outpack_packet_clear <- function() {
  current$packet <- NULL
}


current <- new.env(parent = emptyenv())
