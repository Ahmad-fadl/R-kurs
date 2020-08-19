set.seed(188)
n <- 50
x <- cbind(
  x = runif(5, 0, 2) + rnorm(n, sd = 0.05),
  y = runif(5, 0, 2) + rnorm(n, sd = 0.05)
)
x <- t(x)
d <- c(x, .6, .75, .8, 1.3, 0, 1.6, .6, 1, .9, .9, .5, 1.5, 1, .4, .1, .6);
x <- matrix(d, nrow=2);


n <- ncol(x);
optics_data <- OPTICS(x, .1, 7);
rd <- attr(optics_data, "reachability-distance");
cd <- attr(optics_data, "core-distance");
order <- attr(optics_data, "ordering");

extracted_DBSCAN <- extract_DBSCAN_clustering(optics_data);
optics_cluster <- attr(extracted_DBSCAN, "cluster");

dbscan_data <- DBSCAN(x, .1, 7);
dbscan_cluster <- attr(dbscan_data, "cluster");

test_that("correct error detection",{
  expect_error(OPTICS(c(), .1, 7));
  expect_error(OPTICS(1:9, .1, 7));
  expect_error(OPTICS(x, -1, 7));
  expect_error(OPTICS(x, 0, 7));
  expect_error(OPTICS(x, .1, 0));
  expect_error(OPTICS(x, .1, -2));
})

test_that("correct output dimensions",{
  expect_equal(n, ncol(optics_data))
  expect_equal(n, length(rd))
  expect_equal(n, length(cd))
  expect_equal(n, length(order))
  expect_equal(1, length(attr(optics_data, "eps")))
  expect_equal(1, length(attr(optics_data, "minPts")))
  expect_equal(n, length(optics_cluster))
})

test_that("first element in ordering always has undefined reachability", {
  expect_identical(Inf, rd[order[1]])
})

test_that("extraced DBSCAN equal to actual DBSCAN", {
  expect_identical(dbscan_cluster, optics_cluster);
})
