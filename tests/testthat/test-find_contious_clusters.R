test_that("get_outlier_features identifies outliers correctly in large dataset", {
  set.seed(123)
  # Create a large matrix of normally distributed values
  test_matrix <- matrix(rnorm(1000 * 50, mean = 10, sd = 2), nrow = 1000, ncol = 50)

  # Introduce artificial outliers in a few random locations
  test_matrix[1:5, 1:5] <- test_matrix[1:5, 1:5] + 50  # Inject extreme values

  result <- get_outlier_features(as.data.frame(test_matrix))

  # Expectations
  expect_type(result, "logical")
  expect_equal(dim(result), dim(test_matrix))
  expect_true(any(result))  # Should detect the injected outliers
})

test_that("make_outlier_cluster returns expected output structure", {
  profile_data <- data.frame(
    a = c(1, 2, 3, 4, 100),
    b = c(2, 2, 2, 2, 200)
  )
  filtereddata <- data.frame(CellID = 1:5)
  out <- make_outlier_cluster(profile_data, "test", filtereddata)
  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), 2)
  expect_true(all(c("test outliers", "Clustering_file") %in% colnames(out)))
})


