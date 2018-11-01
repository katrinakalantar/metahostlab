context("inject label noise")

initial_classes <- c(rep(1,100), rep(2,100))
initial_fd <- rep(-1, length(initial_classes))

test_that("flipping all lables works", {
  noised_classes <- inject_label_noiseR(initial_classes, 1, 1)
  expect_equal(noised_classes$yz, rev(initial_classes))
  expect_equal(noised_classes$fd, initial_fd*-1)
})

test_that("flipping only one class works", {
  noised_classes <- inject_label_noiseR(initial_classes, 0, 1)
  expect_equal(noised_classes$yz, rep(1, length(initial_classes)))
  expect_equal(noised_classes$fd, c(rep(-1, length(initial_classes)/2),rep(1, length(initial_classes)/2)))
})

