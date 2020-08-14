context("test the k-medoids algorithm")

data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
data2 <- NULL

test_that("k-medoids finds the correct clusters for k = 2", {
  expect_equal(6, sum(attributes(k_medoids(data, 2))[[2]]))
})

test_that("k-medoids finds the correct clusters for k = 1", {
  expect_equal(4, sum(attributes(k_medoids(data, 1))[[2]]))
})

test_that("k-medoids finds the correct clusters for k = 4", {
  expect_equal(10, sum(attributes(k_medoids(data, 4))[[2]]))
})

test_that("wrong input results in error for k-medoids", {
  expect_error(k_medoids(data2, 2))
})
