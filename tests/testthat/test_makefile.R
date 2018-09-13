library(farspackage)
context("Creating proper filename")

test_that("make_filename creates proper filename",{
  throws_error( make_filename(2))
  expect_equal(make_filename(2), "accident_2.csv.bz2")

})
