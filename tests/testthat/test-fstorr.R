context("fstorr")

test_that("fstorr behaviour when empty", {
  path <- tempfile()
  obj <- fstorr(path, charToRaw)
  expect_equal(obj$list(), character(0))
  expect_false(obj$exists("a"))
  expect_error(obj$get("a", FALSE),
               "Did not find key: 'a'")
  expect_error(obj$get(c("a", "b"), FALSE),
               "Did not find keys: 'a', 'b'")

  expect_equal(obj$hash("a"), NA_character_)
  expect_equal(obj$hash(character(0)), character(0))
  expect_equal(obj$hash(letters), rep(NA_character_, 26))

  expect_equal(obj$path("a"), NA_character_)
  expect_equal(obj$path(character(0)), character(0))
  expect_equal(obj$path(letters), rep(NA_character_, 26))
})


test_that("basic operation", {
  path <- tempfile()
  obj <- fstorr(path, charToRaw)
  res <- withVisible(obj$insert("a"))
  expect_true(obj$exists("a"))

  p <- obj$path("a")
  expect_equal(res, list(value = p, visible = FALSE))

  expect_true(file.exists(p))
  expect_equal(obj$list(), "a")
  expect_equal(
    basename(p),
    "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
  expect_equal(p, file.path(path, "data", basename(p)))
  expect_equal(readLines(p, warn = FALSE), "a")
})


test_that("partial insert", {
  path <- tempfile()
  obj <- fstorr(path, charToRaw)
  obj$insert(c("a", "c", "e"))

  expect_equal(obj$list(), c("a", "c", "e"))
  res <- obj$get(c("a", "b", "c", "d", "e"))
  expect_equal(length(res), 5)
  expect_true(all(!is.na(res)))
  expect_true(all(file.exists(res)))
  expect_equal(
    lapply(res, readLines, warn = FALSE),
    as.list(letters[1:5]))
})
