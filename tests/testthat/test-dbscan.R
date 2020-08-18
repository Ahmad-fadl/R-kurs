set.seed(188)
n <- 50
x <- cbind(
  x = runif(5, 0, 2) + rnorm(n, sd = 0.05),
  y = runif(5, 0, 2) + rnorm(n, sd = 0.05)
)
x <- t(x)
d <- c(x, .6, .75, .8, 1.3, 0, 1.6, .6, 1, .9, .9, .5, 1.5, 1, .4, .1, .6);
x <- matrix(d, nrow=2);

clustered_data <- DBSCAN(x, .1, 7);
cluster_attr <- attr(clustered_data, "cluster");



test_that("check if the correct number of clusters was found.", {
  # 4 clusters + noise = 5
  expect_equal(5, length(unique(cluster_attr)));
})

test_that("correct error detection",{
  expect_error(DBSCAN(c(), .1, 7));
  expect_error(DBSCAN(1:9, .1, 7));
  expect_error(DBSCAN(x, -1, 7));
  expect_error(DBSCAN(x, 0, 7));
  expect_error(DBSCAN(x, .1, 0));
  expect_error(DBSCAN(x, .1, -2));
})
