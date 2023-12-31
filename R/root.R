##' Create a new outpack root.
##'
##' @title Create outpack root
##'
##' @param root Path to use.  This path may exist, but it is an error
##'   to call this on a path that has already been initialised.
##'
##' @param path_archive Path to the archive directory, used to store
##'   human-readable copies of packets.  If `NULL`, no such copy is
##'   made, and `file_store` must be `TRUE`
##'
##' @param use_file_store Logical, indicating if we should use a
##'   content-addressable file-store as the source of truth for
##'   packets.  If `archive` is non-`NULL`, the file-store will be
##'   used as the source of truth and the duplicated files in archive
##'   exist only for convenience.
##'
##' @param require_complete_tree Logical, indicating if we require a
##'   complete tree of packets.  This currently affects
##'   [outpack::outpack_location_pull_packet], by requiring that it
##'   always operates in recursive mode.  This is `FALSE` by default,
##'   but set to `TRUE` if you want your archive to behave well as a
##'   location; if `TRUE` you will always have all the packets that
##'   you hold metadata about.
##'
##' @return Invisibly, an `outpack_root` object; these will change in
##'   future verisons!
##' @export
outpack_init <- function(root, path_archive = "archive",
                         use_file_store = FALSE,
                         require_complete_tree = FALSE) {
  ## Logging: print information about what we're doing here.
  path_outpack <- file.path(root, ".outpack")
  if (file.exists(path_outpack)) {
    stop(sprintf("outpack already initialised at '%s'", path_outpack))
  }

  config <- config_new(path_archive, use_file_store, require_complete_tree)

  fs::dir_create(path_outpack)
  fs::dir_create(file.path(path_outpack, "metadata"))
  fs::dir_create(file.path(path_outpack, "location"))
  config_write(config, root)

  invisible(outpack_root$new(root))
}


## TODO: I am torn here on design - we could make most of the things
## that use the root be methods, but that risks a god class.  Getting
## access to the index does require mutability so that must be a
## method, but it's possible that moving to free functions everywhere
## would be best.
outpack_root <- R6::R6Class(
  "outpack_root",
  cloneable = FALSE,

  private = list(
    index_data = NULL,
    metadata_read = function(id) {
      path_metadata <- file.path(self$path, ".outpack", "metadata", id)
      if (!file.exists(path_metadata)) {
        stop(sprintf("id '%s' not found in index", id))
      }
      outpack_metadata_load(path_metadata)
    },

    metadata_load = function(id) {
      ## TODO: this contains more logic than ideal but attempts to
      ## avoid updating the index if needed.  The other thing to do
      ## would _always_ be to update the index but that feels wasteful
      ## really.
      ##
      ## We could probably be much more efficient if we cached all
      ## roots within a session, though doing that safely would
      ## practically mean putting a key file in each root so that we
      ## can detect directory moves.
      meta <- private$index_data$metadata[[id]] %||%
        self$index()$metadata[[id]]
      if (is.null(meta)) {
        stop(sprintf("id '%s' not found in index", id))
      }
      meta
    }
  ),

  public = list(
    path = NULL,
    config = NULL,
    files = NULL,

    initialize = function(path) {
      assert_file_exists(path)
      assert_file_exists(file.path(path, ".outpack"))
      self$path <- path
      self$config <- config_read(path)
      if (self$config$core$use_file_store) {
        self$files <- file_store$new(file.path(path, ".outpack", "files"))
      }
      lockBinding("path", self)
      lockBinding("files", self)
    },

    metadata = function(id, full = FALSE) {
      if (full) {
        private$metadata_read(id)
      } else {
        private$metadata_load(id)
      }
    },

    index = function(refresh = FALSE) {
      prev <- if (refresh) list() else private$index_data
      private$index_data <- index_update(self, prev)
      private$index_data
    }
  ))


##' Open an existing outpack root. This returns a "root" object, which
##' can be passed through to various outpack functions. The root
##' object is the same as that returned by
##' [outpack::outpack_init] and will be documented once the
##' interface stabilises.
##'
##' @title Open outpack root
##'
##' @param path The path to look for the root; must be an existing
##'   directory. Use `.` or `getwd()` for the current directory.
##'
##' @param locate Logical, indicating if we should look in parent
##'   directories until the root is found (similar behaviour to how
##'   `git` can find its root directory from any directory below the
##'   root).
##'
##' @return An `outpack_root` object; treat this as an opaque object
##'   for now.
##'
##' @export
outpack_root_open <- function(path, locate = TRUE) {
  if (inherits(path, "outpack_root")) {
    return(path)
  }
  assert_scalar_character(path)
  assert_directory(path)
  if (locate) {
    root_found <- find_file_descend(".outpack", path)
    if (is.null(root_found)) {
      stop(sprintf("Did not find existing outpack root from directory '%s'",
                   path))
    }
    path <- root_found
  } else {
    if (!file.exists(file.path(path, ".outpack"))) {
      stop(sprintf("'%s' does not look like an outpack root", path))
    }
  }
  outpack_root$new(path)
}


read_location <- function(location_id, root_path, prev) {
  ## TODO: If we're more relaxed here about format, then this will
  ## need changing.  This regex will end up moving somewhere central
  ## in the package in that case.
  re <- "^([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8})$"
  path <- file.path(root_path, ".outpack", "location", location_id)
  packets <- dir(path, re)
  is_new <- !(packets %in% prev$packet[prev$location == location_id])
  if (!any(is_new)) {
    return(NULL)
  }

  dat <- lapply(file.path(path, packets[is_new]), jsonlite::read_json)
  data_frame(packet = vcapply(dat, "[[", "packet"),
             time = num_to_time(vnapply(dat, "[[", "time")),
             hash = vcapply(dat, "[[", "hash"),
             location = location_id)
}


read_locations <- function(root, prev) {
  location_id <- root$config$location$id
  if (is.null(prev)) {
    prev <- data_frame(packet = character(),
                       time = empty_time(),
                       hash = character(),
                       location = character())
  }
  new <- do.call(rbind, lapply(location_id, read_location, root$path, prev))
  ret <- rbind(prev, new)
  ## Always sort by location (highest priority first) then id
  ret <- ret[order(match(ret$location, location_id), ret$packet), ]
  ## Avoids weird computed rownames - always uses 1:n
  rownames(ret) <- NULL
  ret
}


read_metadata <- function(root, prev) {
  path <- file.path(root$path, ".outpack", "metadata")
  id_new <- setdiff(dir(path), names(prev))

  if (length(id_new) == 0) {
    return(prev)
  }

  files <- file.path(path, id_new)
  new <- lapply(files, outpack_metadata_index_read)
  names(new) <- id_new
  ret <- c(prev, new)
  ret[order(names(ret))]
  ret
}


read_unpacked <- function(root, prev) {
  if (is.null(prev)) {
    prev <- data_frame(packet = character(),
                       time = empty_time(),
                       location = character())
  }

  path <- file.path(root$path, ".outpack", "unpacked")
  id_new <- setdiff(dir(path), prev$packet)

  if (length(id_new) == 0) {
    return(prev)
  }

  dat <- lapply(file.path(path, id_new), jsonlite::read_json)
  new <- data_frame(packet = vcapply(dat, "[[", "packet"),
                    time = num_to_time(vnapply(dat, "[[", "time")),
                    location = vcapply(dat, "[[", "location"))
  ret <- rbind(prev, new)

  rownames(ret) <- NULL
  ret
}


## The index consists of a few bits:
## $location - data.frame of id, location and date
## $metadata - named list of full metadata
##
## Later on we'll want to have some sort of index over this (e.g.,
## name/id/parameters) to support the query interface, but that can
## wait.
index_update <- function(root, prev) {
  root_path <- root$path
  path_index <- file.path(root_path, ".outpack", "index", "outpack.rds")

  if (is.null(prev)) {
    data <- if (file.exists(path_index)) readRDS(path_index) else list()
  } else {
    data <- prev
  }

  ## TODO: Add some logging through here.

  data$location <- read_locations(root, data$location)
  data$metadata <- read_metadata(root, data$metadata)
  data$unpacked <- read_unpacked(root, data$unpacked)

  if (!identical(data, prev)) {
    fs::dir_create(dirname(path_index))
    saveRDS(data, path_index)
  }

  data
}


## Not just for the file store, but this is how we can interact with
## the files safely:
file_export <- function(root, id, path, dest) {
  ## This validation *always* occurs; does the packet even claim to
  ## have this path?
  validate_packet_has_file(root, id, path)
  ## TODO: log file copy information, including hashes.  Because copy
  ## can be slow for large files, we might want to do this file by
  ## file?

  ## TODO: The copy should ideally all succeed or all fail wherever
  ## possible

  ## TODO: check that no dependency destination exists, or offer solution
  ## to overwrite (requires argument here, flowing back to the interface)

  ## TODO: Additional work required to support directory based
  ## dependencies

  fs::dir_create(dirname(dest))

  meta <- root$metadata(id)
  hash <- meta$files$hash[match(path, meta$files$path)]

  if (root$config$core$use_file_store) {
    for (i in seq_along(dest)) {
      root$files$get(hash[[i]], dest[[i]])
    }
  } else {
    src <- file.path(root$path, root$config$core$path_archive,
                     meta$name, meta$id, path)
    assert_file_exists(src)
    ## TODO: Ideally we would have an argument/option support a faster
    ## possibility here if requested (e.g., no validation validate just
    ## size, validate hash); this only applies to this non-file-store
    ## using branch, so typically would affect users running "draft"
    ## type analyses
    for (i in seq_along(dest)) {
      hash_validate(src[[i]], hash[[i]])
    }
    fs::file_copy(src, dest)
  }
}


file_import_store <- function(root, path, file_path, file_hash) {
  if (root$config$core$use_file_store) {
    for (i in seq_along(file_path)) {
      root$files$put(file.path(path, file_path[[i]]), file_hash[[i]])
    }
  }
}


file_import_archive <- function(root, path, file_path, name, id) {
  if (!is.null(root$config$core$path_archive)) {
    dest <- file.path(root$path, root$config$core$path_archive, name, id)

    ## TODO: These should not ever happen, so just asserting here.  If
    ## it does happen it requires that the user has provided an id,
    ## and also copied files around?  Not sure how we'd recover here
    ## either.
    stopifnot(path != dest,
              !file.exists(dest))

    ## TODO: open question as to if we should filter this down to just
    ## the required files (as we do here); this means that if the user
    ## has provided "files" to the metadata function we'd be leaving
    ## some files behind.  This does match the behaviour of the file
    ## store version, but not of orderly.
    file_path_dest <- file.path(dest, file_path)
    fs::dir_create(dirname(file_path_dest))
    fs::file_copy(file.path(path, file_path), file_path_dest)
  }
}


find_file_by_hash <- function(root, hash) {
  index <- root$index()

  path_archive <- file.path(root$path, root$config$core$path_archive)
  algorithm <- hash_parse(hash)$algorithm

  ## TODO: allow short circuiting validation (e.g., check only the
  ## size matches, or check nothing)
  for (id in index$unpacked$packet) {
    meta <- index$metadata[[id]]
    for (i in which(meta$files$hash == hash)) {
      path <- file.path(path_archive, meta$name, id, meta$files$path[[i]])
      if (file.exists(path) && hash_file(path, algorithm) == hash) {
        return(path)
      }
      ## TODO: incorporate this into logging later:
      message(sprintf("Rejecting file '%s' in '%s/%s'",
                      meta$files$path[[i]], meta$name, id))
    }
  }

  NULL
}


## This might move elsewhere
validate_packet_has_file <- function(root, id, path) {
  ## TODO: wrap this in tryCatch/withCallingHandlers or similar to get
  ## better error, or make this part of the metadata call (a 'reason'
  ## arg?).  This issue will appear elsewhere too.
  meta <- root$metadata(id)
  err <- setdiff(path, meta$files$path)
  if (length(err) > 0) {
    ## TODO: this might also want wrapping so that we report back
    ## better errors.  One possibility here is that we should report
    ## "near misses" (Did you mean: X), though that will be best to
    ## think about fairly broadly as it will likely affect other parts
    ## of the packge.
    stop(sprintf("Packet '%s' does not contain path %s",
                 id, paste(squote(err), collapse = ", ")))
  }
}
