context("test cast label")

test_that("casting label works", {
  expect_equal(castLabel(c(1,1,2,2), 2), c(1,1,2,2))
  expect_equal(castLabel(c(1,1,2,2), -1), c(-1,-1,1,1))
  expect_equal(castLabel(c(2,2,1,1), -1), c(1,1,-1,-1))
  expect_equal(castLabel(c(2,2,1,1), 2), c(2,2,1,1))
})
