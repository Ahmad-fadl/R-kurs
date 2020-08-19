context("test the Hierarchical clustering algorithm")

data <- distance(USArrests)
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
