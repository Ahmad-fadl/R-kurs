### Test generate_nested_2d_training_data ###

test_that("generate_nested_2d_training_data returns the requested number of vectors", {
  expect_length(generate_nested_2d_training_data(0), 0)
  expect_length(generate_nested_2d_training_data(20), 20 * 2)
})

test_that("generate_nested_2d_training_data handles erroneous input correctly", {
  expect_error(generate_nested_2d_training_data("Zahl"))
  expect_error(generate_nested_2d_training_data(-5))
})


### Test generate_2d_cluster ###

test_that("generate_2d_cluster returns the requested number of vectors", {
  expect_length(generate_2d_cluster(0), 0)
  expect_length(generate_2d_cluster(20), 20 * 2)
})

test_that("generate_2d_cluster handles erroneous input correctly", {
  expect_error(generate_2d_cluster("Zahl"))
  expect_error(generate_2d_cluster(-5))
})

names(dev.cur()) == "RStudioGD"

### Test plot_clustered_2d_data ###


test_that("plot_clustered_2d_data runs without issue when given valid input", {
  m <- matrix(c(1,1,2,1), nrow=2)
  attr(m, "cluster") <- c(1,2)

  # expects the function to run without error
  expect_error(plot_clustered_2d_data(m), NA)

  # this does not work when checked by devtools::check() since it does not plot to RStudio
  # expect_true(names(dev.cur()) == "RStudioGD")
})

test_that("plot_clustered_2d_data handles erroneous input correctly", {
  m <- matrix(c(1,1,2,1), nrow=2)

  expect_error(plot_clustered_2d_data(m))

  # m is valid now
  attr(m, "cluster") <- c(1,2)

  expect_error(plot_clustered_2d_data(m, point_size="NUMBER"))
  expect_error(plot_clustered_2d_data(m, point_size=-1))
  expect_error(plot_clustered_2d_data(m, point_size=0))
})
