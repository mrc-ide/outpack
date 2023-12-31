test_that("can create new root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path)
  expect_s3_class(r, "outpack_root")

  expect_true(file.exists(file.path(path, ".outpack", "metadata")))
  expect_true(file.exists(file.path(path, ".outpack", "location")))
  expect_mapequal(r$config$core,
                  list(path_archive = "archive",
                       use_file_store = FALSE,
                       require_complete_tree = FALSE,
                       hash_algorithm = "sha256"))
  expect_false(file.exists(file.path(path, ".outpack", "files")))
  expect_equal(outpack_location_list(r), "local")
})


test_that("Re-initialising root errors", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  expect_silent(outpack_init(path))
  expect_error(r <- outpack_init(path),
                 "outpack already initialised at")
})


test_that("Can control root config on initialisation", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path, path_archive = NULL, use_file_store = TRUE,
                    require_complete_tree = TRUE)
  expect_mapequal(r$config$core,
                  list(path_archive = NULL,
                       use_file_store = TRUE,
                       require_complete_tree = TRUE,
                       hash_algorithm = "sha256"))
  expect_true(file.exists(file.path(path, ".outpack", "files")))
})


test_that("Must include some packet storage", {
  path <- tempfile()
  expect_error(
    outpack_init(path, path_archive = NULL, use_file_store = FALSE),
    "if 'path_archive' is NULL, then 'use_file_store' must be TRUE")
  expect_false(file.exists(path))
})


test_that("Can locate an outpack root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  p <- file.path(path, "a", "b", "c")
  fs::dir_create(p)
  expect_equal(
    outpack_root_open(p)$path,
    outpack_root_open(path)$path)
  expect_equal(
    with_dir(p, outpack_root_open(".")$path),
    outpack_root_open(path)$path)
  expect_identical(
    outpack_root_open(r), r)
})


test_that("outpack_root_open errors if it reaches toplevel", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  fs::dir_create(path)
  expect_error(
    outpack_root_open(path),
    "Did not find existing outpack root from directory '.+'")
})


test_that("outpack_root_open does not recurse if locate = FALSE", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  expect_identical(outpack_root_open(r, locate = FALSE), r)
  expect_equal(outpack_root_open(path, locate = FALSE)$path, path)

  p <- file.path(path, "a", "b", "c")
  fs::dir_create(p)
  expect_error(
    outpack_root_open(p, locate = FALSE),
    "'.+/a/b/c' does not look like an outpack root")
})


test_that("root configuration matches schema", {
  skip_if_not_installed("jsonvalidate")
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  path_config <- file.path(path, ".outpack", "config.json")
  expect_true(outpack_schema("config")$validate(path_config))
})


test_that("Can't get nonexistant metadata", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path, path_archive = NULL, use_file_store = TRUE)
  id <- outpack_id()
  expect_error(
    r$metadata(id),
    sprintf("id '%s' not found in index", id))
  expect_error(
    r$metadata(id, full = TRUE),
    sprintf("id '%s' not found in index", id))
})


test_that("empty root has nothing unpacked", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))

  r <- outpack_init(path)
  index <- r$index()
  expect_equal(index$unpacked,
               data_frame(packet = character(),
                          time = empty_time(),
                          location = character()))
})


test_that("Can read full metadata via root", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  r <- outpack_init(path)
  id1 <- create_random_packet(path)
  id2 <- create_random_packet(path)

  d1 <- r$metadata(id1, TRUE)
  d2 <- r$metadata(id1, FALSE)

  expect_identical(d1[names(d2)], d2)
  extra <- setdiff(names(d1), names(d2))
  expect_equal(d1$script, list())
  expect_equal(d1$schemaVersion, outpack_schema_version())
})
