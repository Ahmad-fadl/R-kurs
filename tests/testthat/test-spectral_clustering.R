# produce test data
set.seed(1)
X <- clustering::generate_nested_2d_training_data(200)
projections <- clustering::spectral_clustering(X, k=2)
clustered_data <- clustering::spectral_clustering(X, k=2, mercer_kernel=gaussian_kernel_with_fixed_gamma(30), clustering=k_medoids, 2);
cluster_attr <- attr(clustered_data, "cluster")

test_that("number of projected vectors equals number of input vectors", {
  expect_equal(ncol(X), ncol(projections));
})

test_that("find exactly two clusters in the projected data", {
  expect_equal(2, length(unique(cluster_attr)));
})

test_that("invalid projection dimensions", {
  expect_error(clustering::spectral_clustering(X, k=3));
  expect_error(clustering::spectral_clustering(X, k=-1));
})

test_that("invalid input data", {
  expect_error(clustering::spectral_clustering(1:10, k=3));
})
