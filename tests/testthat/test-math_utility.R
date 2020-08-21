
x <- c(1,2,3)
y <- c(2,3,4)

a <- rnorm(10, 0, 2)
b <- rnorm(10, 0, 2)

eps <- .Machine$double.eps

test_that("euclidean functions produce correct output", {
  expect_equal(euclidean_norm_squared(x), 14)
  expect_equal(euclidean_norm(x), sqrt(14))
  expect_true(euclidean_distance(x, y) - 1.732051 < eps)
  expect_true(euclidean_distance_squared(x, y) - 1.732051**2 < eps)
})

test_that("gaussian_kernel produces correct results", {
  expect_true(gaussian_kernel(x, y) - 1.691898e-10 < eps)
  expect_true(gaussian_kernel(x, y, gamma = 7.5) - 1.691898e-10 < eps)

  expect_lt(gaussian_kernel(x, y, gamma = 7.5), gaussian_kernel(x, y, gamma = 5))
  expect_lt(gaussian_kernel(x, y, gamma = 15), gaussian_kernel(x, y, gamma = 7.5))
})

test_that("gaussian_kernel handles erroneous input correctly", {
  expect_error(gaussian_kernel(x, y, gamma = -1))
  expect_error(gaussian_kernel(1:3, 1:4))
})

test_that("infinity_norm and one_norm produce correct results", {
  expect_equal(infinity_norm(x), 3)
  expect_equal(infinity_norm(y), 4)

  expect_equal(one_norm(x), 6)
  expect_equal(one_norm(y), 9)

  expect_true(infinity_norm(a) >= 0)
  expect_true(one_norm(a) >= 0)
  expect_true(infinity_norm(a) <= one_norm(a))
})

test_that("p_norm produces correct results", {
  expect_equal(euclidean_norm(a), p_norm(a, 2))
  expect_equal(one_norm(a), p_norm(a, 1))
})

test_that("p_norm_factory produces correct functions", {
  norm1 <- p_norm_factory(1)
  norm2 <- p_norm_factory(2)
  norm80 <- p_norm_factory(80)

  expect_true(is.function(norm1))
  expect_true(is.function(norm2))

  expect_equal(euclidean_norm(a), norm2(a))
  expect_equal(one_norm(a), norm1(a))
  expect_equal(p_norm(a, 80), norm80(a))
})

test_that("p_norm_factory handles faulty input correctly", {
  expect_error(p_norm_factory("ZAHL"))
  expect_error(p_norm_factory(-1))
})

test_that("induced_metric_factory produces correct functions", {
  eucl_distance <- induced_metric_factory(euclidean_norm)
  eucl_distance_squared <- induced_metric_factory(euclidean_norm_squared)


  expect_equal(euclidean_distance(x, y), eucl_distance(x, y))
  expect_equal(euclidean_distance(a, b), eucl_distance(a, b))

  expect_equal(euclidean_distance_squared(x, y), eucl_distance_squared(x, y))
  expect_equal(euclidean_distance_squared(a, b), eucl_distance_squared(a, b))
})


test_that("induced_metric_factory handles faulty input correctly", {
  expect_error(induced_metric_factory("ZAHL"))
})
