# Dummy dataset generator for testing
generate_test_data <- function(n = 200, p = 5) {
  as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
}

test_that("targeted_profile_clusterer works with valid input and default settings", {
  test_datasets <- list(
    dataset1 = generate_test_data(500),
    dataset2 = generate_test_data(1200)
  )

  result <- suppressWarnings(targeted_profile_clusterer(test_datasets))  # suppress benign convergence warnings

  expect_type(result, "list")
  expect_equal(length(result), length(test_datasets))

  for (i in seq_along(result)) {
    expect_s3_class(result[[i]], "data.frame")
    expect_true("Clustering_file" %in% colnames(result[[i]]))
  }
})

test_that("targeted_profile_clusterer works when allow_further_itteration = TRUE", {
  test_datasets <- list(
    dataset1 = generate_test_data(1000)
  )

  result <- suppressWarnings(targeted_profile_clusterer(test_datasets, allow_further_itteration = TRUE))

  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_s3_class(result[[1]], "data.frame")
  expect_true("Clustering_file" %in% colnames(result[[1]]))
})

test_that("targeted_profile_clusterer handles empty or invalid input gracefully", {
  empty_result <- targeted_profile_clusterer(list())
  expect_type(empty_result[[1]], "character")
  expect_match(empty_result[[1]], "no profiles")

  invalid_result <- targeted_profile_clusterer("not_a_list")
  expect_type(invalid_result[[1]], "character")
  expect_match(invalid_result[[1]], "dataset is not a list")
})

#############

test_that("combine_clusters works with valid and invalid dataset names", {
  # Create sample datasets
  test1 <- data.frame(a = 1:5)
  test2 <- data.frame(b = 6:10)
  assign("test1", test1, envir = .GlobalEnv)
  assign("test2", test2, envir = .GlobalEnv)

  # Combine datasets
  result <- combine_clusters(c("test1", "test2"))

  # Expect both datasets returned
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(result[[1]], test1)
  expect_equal(result[[2]], test2)

  # Test with one missing dataset
  result_partial <- combine_clusters(c("test1", "not_existing"))
  expect_equal(length(result_partial), 1)
  expect_equal(result_partial[[1]], test1)

  # Test with all missing
  result_none <- combine_clusters(c("fake1", "fake2"))
  expect_equal(length(result_none), 0)
})

