context("test cast label")

test_that("casting label works for input vectors of {-1,1}", {
  
  expect_equal(castLabel(c(-1,-1,1,1), -1), c(-1,-1,1,1))  
  expect_equal(castLabel(c(-1,-1,1,1), 0), c(0,0,1,1))
  expect_equal(castLabel(c(-1,-1,1,1), 2), c(1,1,2,2))

})

test_that("casting label works for input vectors of {0,1}",{
  
  expect_equal(castLabel(c(0,0,1,1), -1), c(-1,-1,1,1))
  expect_equal(castLabel(c(0,0,1,1), 0), c(0,0,1,1))
  expect_equal(castLabel(c(0,0,1,1), 2), c(1,1,2,2))
  
})

test_that("casting label works for input vectors of {1,2}",{

  expect_equal(castLabel(c(1,1,2,2), -1), c(-1,-1,1,1))
  expect_equal(castLabel(c(1,1,2,2), 0), c(0,0,1,1))
  #expect_equal(castLabel(c(1,1,2,2), 2), c(1,1,2,2))

})

test_that("casting label works for input vectors in reverse order",{
  
  expect_equal(castLabel(c(2,2,1,1), -1), c(1,1,-1,-1))
  expect_equal(castLabel(c(2,2,1,1), 2), c(2,2,1,1))
  expect_equal(castLabel(c(2,2,1,1), 1), c(2,2,1,1))
  
})