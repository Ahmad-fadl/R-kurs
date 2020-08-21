context("test the Hierarchical clustering algorithm")

data <- distance(USArrests)
data_1 <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
data2 <- NULL
data_clust <- hierclust(data, method="single")
data_clust_complete <- hierclust(data, method="complete")

test_that("hierclust finds the total number of correct clusters for single linkage", {
  expect_equal(50, sum(lengths(data_clust$label)))
})

test_that("The height of the final cluster tree for single linkage method", {
  expect_equal(774.3925, sum(data_clust$height))
})

test_that("wrong input results in error for hierclust", {
  expect_error(hierclust(data2, method="single"))
})

test_that("wrong input results in error for distance", {
  expect_error(distance(data2))
})

test_that("wrong input results in error for hclust_order", {
  expect_error(hclust_order(data2))
  })

test_that("Measuring Euclidean distance diagonals using the distance method", {
  x <- sum(diag(distance(data_1)))
  expect_equal(0, x)
})

test_that("Testing the sum of dissimilarities", {
expect_equal(0.2828427, sum(distance(data_1)))
})
