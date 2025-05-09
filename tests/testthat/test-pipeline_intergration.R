#testsuite scripts#

test_that("entire pipeline runs input to output", {
  a <- get_regions_of_interest(NMAcompanion::NMA_toy_dataset)
  expect_equal(length(a), 10, info = "Error in ROI detection")
  expect_true(is.list(a), "a should be a list")
  b <- Cluster_ROI_list(a,allow_further_itteration = TRUE)
  expect_true(is.list(b), "b should be a list")
  expect_equal(length(b), 10, info = "Unexpected number of clusters after initial clustering")
  expect_equal(length(a), length(b), info = "Lists a and b must have the same length")

  # Check that every dataframe in b is one row longer than the corresponding dataframe in a
  for (i in 1:length(a)) {
    expect_true(ncol(b[[i]]) == ncol(a[[i]]) + 1,
                info = paste("Dataframe", i, "in b should have one row more than in a"))
  }
  c <- find_contious_clusters(NMAcompanion::NMA_toy_dataset)
  expect_equal(length(c), 3, info = "Unexpected number of clusters after initial clustering")
  b <- append(b, c)

  d <- Amalgamate_morphological_features(NMAcompanion::NMA_toy_dataset, b)
  expect_equal(length(d), 5, info = "Error in hamming amalgamation")
  expect_type( d, "list")
  expect_true("graph1" %in% names( d[[1]]))
  # Assuming this function creates a graph or visualization, and doesn't return a value to test
  e<-Graph_clustered_ROIs(b, NMAcompanion::NMA_toy_dataset)
  test_that("Graph_clustered_ROIs returns correctly structured list", {
    # Check that e is a list of length 13
    expect_type(e, "list")
    expect_length(e, 13)

    # Check each element is a list of length 3
    lapply(e, function(sublist) {
      expect_type(sublist, "list")
      expect_length(sublist, 3)
    })

    # Check the first two lists contain three ggplot objects
    for (i in 1:2) {
      for (j in 1:3) {
        expect_s3_class(e[[i]][[j]], "gg")
      }
    }

    # Check third list: first two elements are ggplot, third is character vector
    expect_type(e[[3]][[1]], "character")
    expect_s3_class(e[[3]][[2]], "gg")
    expect_s3_class(e[[3]][[3]], "gg")
  })
  })


