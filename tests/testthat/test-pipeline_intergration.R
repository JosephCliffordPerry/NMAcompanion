#testsuite scripts#

test_that("get regions of interest works as expected with the toy dataset", {
  a <- get_regions_of_interest(NMAcompanion::data)
  expect_equal(length(a), 10, info = "Error in ROI detection")
  rm(data)
  expect_true(is.list(a), "a should be a list")
  b <- Cluster_ROI_list(a)
  expect_true(is.list(b), "b should be a list")
  expect_equal(length(b), 10, info = "Unexpected number of clusters after initial clustering")
  expect_equal(length(a), length(b), info = "Lists a and b must have the same length")

  # Check that every dataframe in b is one row longer than the corresponding dataframe in a
  for (i in 1:length(a)) {
    expect_true(ncol(b[[i]]) == ncol(a[[i]]) + 1,
                info = paste("Dataframe", i, "in b should have one row more than in a"))
  }
  #c <- find_contious_clusters(b)

  #b <- append(b, c)

  #d <- Amalgamate_morphological_features(data, b)
  #expect_equal(length(c), 5, info = "Error in hamming amalgamation")

  # Assuming this function creates a graph or visualization, and doesn't return a value to test
  #expect_error_free(Graph_clustered_ROIs(b, rawdata = rawdata))
})
