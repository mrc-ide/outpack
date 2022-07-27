test_that("Can run a basic packet", {
  on.exit(outpack_packet_clear(), add = TRUE)
  ## TODO: We'll probably want to move some of these into test
  ## fixtures (or a demo directory like in orderly) as these will get
  ## boring to copy around. At the same time, it's pretty hard to
  ## remember which files will play well with each other, so keeping
  ## things explicit for now.

  ## A simple example where we run something.
  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  p <- outpack_packet_start(path_src, "example", root = root)

  outpack_packet_run("script.R")
  expect_true(file.exists(file.path(path_src, "myplot.png")))
  expect_equal(outpack_packet_current()$script, "script.R")

  outpack_packet_end()

  index <- root$index()
  expect_length(index$metadata, 1)
  id <- p$id

  path_metadata <- file.path(path, ".outpack", "metadata", id)
  expect_true(file.exists(path_metadata))
  outpack_schema("metadata")$validate(path_metadata)

  location_id <- root$config$location$id

  path_location <- file.path(path, ".outpack", "location", location_id, id)
  expect_true(file.exists(path_location))
  outpack_schema("location")$validate(path_location)

  meta <- outpack_metadata_load(path_metadata)

  ## The index metadata is a subset of the full set:
  expect_mapequal(index$metadata[[id]],
                  meta[c("name", "id", "parameters", "files", "depends")])

  expect_setequal(
    names(meta),
    c("schemaVersion", "name", "id", "time", "parameters", "files",
      "depends", "script", "session", "custom", "hash"))

  expect_equal(meta$schemaVersion, outpack_schema_version())
  expect_equal(meta$name, "example")
  expect_equal(meta$id, id)
  expect_null(meta$parameters)
  expect_equal(meta$depends, data_frame(id = character(), files = I(list())))
  expect_setequal(meta$files$path, c("data.csv", "myplot.png", "script.R"))
  expect_equal(meta$files$size,
               file.size(file.path(path_src, meta$files$path)))
  expect_equal(meta$files$hash,
               hash_files(file.path(path_src, meta$files$path)))
  expect_null(meta$custom)

  ## Copy of the files in human readable archive:
  expect_true(all(file.exists(
    file.path(path, "archive", "example", id, meta$files$path))))
  expect_equal(
    hash_files(file.path(path, "archive", "example", id, meta$files$path)),
    meta$files$hash)

  ## Copy of the files in the file store:
  expect_setequal(root$files$list(), meta$files$hash)

  expect_setequal(names(index$unpacked), c("packet", "time", "location"))
  expect_equal(index$unpacked$packet, id)
  expect_equal(index$unpacked$location, location_id)
  expect_s3_class(index$unpacked$time, "POSIXt")

  ## Easily retrieve metadata from root:
  expect_equal(root$metadata(id), index$metadata[[id]])
})


test_that("Can handle dependencies", {
  on.exit(outpack_packet_clear(), add = TRUE)
  ## A simple example where we run something.
  path_src1 <- tempfile()
  fs::dir_create(path_src1)
  on.exit(unlink(path_src1, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- tempfile()
  fs::dir_create(path_src2)
  on.exit(unlink(path_src2, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run("script.R")
  outpack_packet_end()

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  outpack_packet_use_dependency(id1, c("incoming.csv" = "data.csv"))
  outpack_packet_run("script.R")
  outpack_packet_end()

  meta <- outpack_root_open(path)$metadata(id2)
  expect_equal(
    meta$depends,
    data_frame(
      id = id1,
      files = I(list(data_frame(here = "incoming.csv", there = "data.csv")))))
})


test_that("Can't start a packet twice", {
  on.exit(outpack_packet_clear(), add = TRUE)

  ## A simple example where we run something.
  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  p1 <- outpack_packet_start(path_src, "example", root = root)
  ## TODO: We might expand this to indicate where the packet is
  ## running (and if it's the same one that we're trying to restart)
  expect_error(
    outpack_packet_start(path_src, "example", root = root),
    "Already a current packet - call outpack_packet_cancel()",
    fixed = TRUE)
  outpack_packet_cancel()
  p2 <- outpack_packet_start(path_src, "example", root = root)
  expect_true(p2$id != p1$id)
})


test_that("Can't use a nonexistant running packet", {
  outpack_packet_clear()
  expect_error(
    outpack_packet_current(),
    "No current packet")
})


test_that("Can't add a packet twice", {
  on.exit(outpack_packet_clear())

  ## A simple example where we run something.
  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_end()

  id <- p$id
  json <- read_string(file.path(path, ".outpack", "metadata", id))
  class(json) <- "json"
  expect_error(
    outpack_insert_packet(path_src, json, root),
    "'.+' has already been added for 'local'")
})


test_that("Can't use nonexistant id as dependency", {
  on.exit(outpack_packet_clear(), add = TRUE)

  ## A simple example where we run something.
  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  p1 <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_end()

  outpack_packet_start(path_src, "example", root = root)
  expect_error(
    outpack_packet_use_dependency(p1$id, c("a" = "b")),
    sprintf("Packet '%s' does not contain path 'b'", p1$id))
  outpack_packet_cancel()
})


test_that("Can't use file that does not exist from dependency", {
  on.exit(outpack_packet_clear(), add = TRUE)
  path_src1 <- tempfile()
  fs::dir_create(path_src1)
  on.exit(unlink(path_src1, recursive = TRUE), add = TRUE)

  path_src2 <- tempfile()
  fs::dir_create(path_src2)
  on.exit(unlink(path_src2, recursive = TRUE), add = TRUE)

  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  outpack_packet_end()

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  expect_error(
    outpack_packet_use_dependency(p1$id, c("incoming.csv" = "data.csv")),
    "Packet '.+' does not contain path 'data.csv'")
})


test_that("Can use dependency from outpack without file store", {
  on.exit(outpack_packet_clear(), add = TRUE)
  ## A simple example where we run something.
  path_src1 <- tempfile()
  fs::dir_create(path_src1)
  on.exit(unlink(path_src1, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- tempfile()
  fs::dir_create(path_src2)
  on.exit(unlink(path_src2, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  root <- outpack_init(path, path_archive = "archive", use_file_store = FALSE)

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run("script.R")
  outpack_packet_end()

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  outpack_packet_use_dependency(id1, c("incoming.csv" = "data.csv"))
  outpack_packet_run("script.R")
  outpack_packet_end()

  meta <- outpack_root_open(path)$metadata(id2)
  expect_equal(
    meta$depends,
    data_frame(
      id = id1,
      files = I(list(data_frame(here = "incoming.csv", there = "data.csv")))))
})


test_that("validate dependencies from archive", {
  on.exit(outpack_packet_clear(), add = TRUE)
  ## A simple example where we run something.
  path_src1 <- tempfile()
  fs::dir_create(path_src1)
  on.exit(unlink(path_src1, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src1, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src1, "data.csv"),
            row.names = FALSE)

  path_src2 <- tempfile()
  fs::dir_create(path_src2)
  on.exit(unlink(path_src2, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('incoming.csv')",
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src2, "script.R"))

  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  root <- outpack_init(path, path_archive = "archive", use_file_store = FALSE)

  p1 <- outpack_packet_start(path_src1, "a", root = root)
  id1 <- p1$id
  outpack_packet_run("script.R")
  outpack_packet_end()

  ## Change the value here:
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(root$path, "archive", "a", id1, "data.csv"),
            row.names = FALSE)

  p2 <- outpack_packet_start(path_src2, "b", root = root)
  id2 <- p2$id
  expect_error(
    outpack_packet_use_dependency(id1, c("incoming.csv" = "data.csv")),
    "Hash of '.+' does not match")
})


test_that("Can add additional data", {
  on.exit(outpack_packet_clear(), add = TRUE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path_root <- file.path(tmp, "root")
  root <- outpack_init(path_root)

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, "example", root = root)$id
  custom <- '{"a": 1, "b": 2}'
  outpack_packet_add_custom("potato", custom)
  outpack_packet_end()

  ## See mrc-3091 - this should be made easier
  path_metadata <- file.path(path_root, ".outpack", "metadata", id)
  meta <- outpack_metadata_load(path_metadata)
  expect_equal(meta$custom, list(potato = list(a = 1, b = 2)))
})


test_that("Can add multiple copies of extra data", {
  on.exit(outpack_packet_clear(), add = TRUE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path_root <- file.path(tmp, "root")
  root <- outpack_init(path_root)

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, "example", root = root)$id
  outpack_packet_add_custom("app1", '{"a": 1, "b": 2}')
  outpack_packet_add_custom("app2", '{"c": [1, 2, 3]}')
  outpack_packet_end()

  path_metadata <- file.path(path_root, ".outpack", "metadata", id)
  meta <- outpack_metadata_load(path_metadata)
  expect_equal(meta$custom,
               list(app1 = list(a = 1, b = 2),
                    app2 = list(c = list(1, 2, 3))))
})


test_that("Can't add custom data for same app twice", {
  on.exit(outpack_packet_clear(), add = TRUE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path_root <- file.path(tmp, "root")
  root <- outpack_init(path_root)

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, "example", root = root)$id
  outpack_packet_add_custom("app1", '{"a": 1, "b": 2}')
  outpack_packet_add_custom("app2", '{"a": 1, "b": 2}')
  expect_error(
    outpack_packet_add_custom("app1", '{"c": [1, 2, 3]}'),
    "metadata for 'app1' has already been added for this packet")
  expect_error(
    outpack_packet_add_custom("app2", '{"c": [1, 2, 3]}'),
    "metadata for 'app2' has already been added for this packet")
})


test_that("Can validate custom metadata against schema", {
  on.exit(outpack_packet_clear(), add = TRUE)
  schema <- '{
    "type": "object",
    "properties": {"a": { "type": "string" }, "b": { "type": "number" }}}'

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path_root <- file.path(tmp, "root")
  root <- outpack_init(path_root)

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, "example", root = root)$id
  expect_error(
    outpack_packet_add_custom("app1", '{"a": 1, "b": 2}', schema),
    "Validating custom metadata failed:")
  ## No error
  outpack_packet_add_custom("app1", '{"a": "str", "b": 2}', schema)
  outpack_packet_end()

  path_metadata <- file.path(path_root, ".outpack", "metadata", id)
  meta <- outpack_metadata_load(path_metadata)
  expect_equal(meta$custom,
               list(app1 = list(a = "str", b = 2)))
})


test_that("Can report nicely about json syntax errors", {
  on.exit(outpack_packet_clear(), add = TRUE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path_root <- file.path(tmp, "root")
  root <- outpack_init(path_root)

  src <- fs::dir_create(file.path(tmp, "src"))
  saveRDS(runif(10), file.path(src, "data.rds"))
  id <- outpack_packet_start(src, "example", root = root)$id
  expect_error(
    outpack_packet_add_custom("app1", '{"a": 1, "b": 2'),
    "Syntax error in custom metadata:")
})


test_that("pre-prepared id can be used to start packet", {
  on.exit(outpack_packet_clear(), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  id <- outpack_id()
  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)

  p <- outpack_packet_start(path_src, "example", id = id, root = root)
  expect_equal(p$id, id)

  outpack_packet_end()
  index <- root$index()
  expect_equal(names(index$metadata), id)
})


test_that("Can hash files on startup", {
  on.exit(outpack_packet_clear(), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)

  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('zzz.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)

  inputs <- c("data.csv", "script.R")

  p <- outpack_packet_start(path_src, "example", root = root)
  expect_equal(outpack_packet_file_list(),
               data_frame(path = inputs, status = "unknown"))
  outpack_packet_file_mark(inputs, "immutable")
  expect_equal(outpack_packet_file_list(),
               data_frame(path = inputs, status = "immutable"))
  outpack_packet_run("script.R")
  expect_equal(outpack_packet_file_list(),
               data_frame(path = c(inputs, "zzz.png"),
                          status = c("immutable", "immutable", "unknown")))
  outpack_packet_file_mark("zzz.png", "immutable")
  expect_equal(outpack_packet_file_list(),
               data_frame(path = c(inputs, "zzz.png"), status = "immutable"))
  outpack_packet_end()
})


test_that("Can detect changes to hashed files", {
  on.exit(outpack_packet_clear(), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)

  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "file.create('data.csv')", # truncates file
    "png('myplot.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path_src, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path_src, "data.csv"),
            row.names = FALSE)
  inputs <- c("script.R", "data.csv")
  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark(inputs, "immutable")
  outpack_packet_run("script.R")
  expect_error(
    outpack_packet_end(),
    "File was changed after being added: 'data.csv'")
})


test_that("Re-adding files triggers hash", {
  on.exit(outpack_packet_clear(), add = TRUE)

  path <- tempfile()
  root <- outpack_init(path, path_archive = "archive", use_file_store = TRUE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  path_src <- tempfile()
  fs::dir_create(path_src)
  on.exit(unlink(path_src, recursive = TRUE), add = TRUE)
  write.csv(mtcars, file.path(path_src, "data.csv"))

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark("data.csv", "immutable")
  expect_silent(outpack_packet_file_mark("data.csv", "immutable"))
  expect_length(outpack_packet_current()$files, 1)
  file.create(file.path(path_src, "data.csv"))
  expect_error(outpack_packet_file_mark("data.csv", "immutable"),
               "File was changed after being added: 'data.csv'")
})


test_that("Can ignore files from the final packet", {
  on.exit(outpack_packet_clear(), add = TRUE)
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  inputs <- c("data.csv", "script.R")

  p <- outpack_packet_start(path_src, "example", root = root)
  expect_equal(outpack_packet_file_list(),
               data_frame(path = inputs, status = "unknown"))
  outpack_packet_file_mark("data.csv", "ignored")
  expect_equal(outpack_packet_file_list(),
               data_frame(path = inputs, status = c("ignored", "unknown")))
  outpack_packet_run("script.R")
  expect_equal(outpack_packet_file_list(),
               data_frame(path = c(inputs, "zzz.png"),
                          status = c("ignored", "unknown", "unknown")))
  outpack_packet_end()

  meta <- root$metadata(p$id)
  expect_equal(meta$files$path, c("script.R", "zzz.png"))
  expect_length(root$files$list(), 2)
  expect_setequal(dir(file.path(root$path, "archive", "example", p$id)),
                  c("script.R", "zzz.png"))
  expect_setequal(dir(path_src),
                  c("data.csv", "script.R", "zzz.png"))
})


test_that("Files cannot be immutable and ignored", {
  on.exit(outpack_packet_clear(), add = TRUE)
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path_src <- create_temporary_simple_src()

  p <- outpack_packet_start(path_src, "example", root = root)
  outpack_packet_file_mark("data.csv", "ignored")
  outpack_packet_file_mark("script.R", "immutable")

  expect_error(
    outpack_packet_file_mark("data.csv", "immutable"),
    "Cannot mark ignored files as immutable: 'data.csv'")
  expect_error(
    outpack_packet_file_mark("script.R", "ignored"),
    "Cannot mark immutable files as ignored: 'script.R'")
})
