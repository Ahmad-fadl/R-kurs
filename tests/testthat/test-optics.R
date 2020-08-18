set.seed(188)
n <- 50
x <- cbind(
  x = runif(5, 0, 2) + rnorm(n, sd = 0.05),
  y = runif(5, 0, 2) + rnorm(n, sd = 0.05)
)
x <- t(x)
d <- c(x, .6, .75, .8, 1.3, 0, 1.6, .6, 1, .9, .9, .5, 1.5, 1, .4, .1, .6);
x <- matrix(d, nrow=2);

clustered_data <- OPTICS(x, .1, 7);
cluster_attr <- attr(clustered_data, "cluster");

test_that("correct error detection",{
  expect_error(OPTICS(c(), .1, 7));
  expect_error(OPTICS(1:9, .1, 7));
  expect_error(OPTICS(x, -1, 7));
  expect_error(OPTICS(x, 0, 7));
  expect_error(OPTICS(x, .1, 0));
  expect_error(OPTICS(x, .1, -2));
})
