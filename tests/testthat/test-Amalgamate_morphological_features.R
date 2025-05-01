###
#first step is test the rand index calculations

# Sample mock data for testing
set.seed(1215)

mock_data <- data.frame(CellID = 1:10)

mock_clusters <- list(
  list(Clustering_file = sample(1:2, 10, replace = TRUE)),
  list(Clustering_file = sample(1:3, 10, replace = TRUE)),
  list(Clustering_file = sample(1:2, 10, replace = TRUE))
)

# Use the function to generate rand_data
rand_data <- make_randindex_data(mock_data, mock_clusters)
test_that("make_randindex_data returns expected data frame", {
  expect_s3_class(rand_data, "data.frame")
  expect_equal(nrow(rand_data), nrow(mock_data))
  expect_equal(ncol(rand_data), length(mock_clusters) + 1)  # +1 for CellID
  expect_equal(colnames(rand_data)[1], "clusties")
  expect_true(all(grepl("^Clustering_", colnames(rand_data)[-1])))
})

test_that("make_randindex_data handles empty cluster list", {
  empty_clusters <- list()
 expect_error(result <- make_randindex_data(mock_data, empty_clusters))
})



test_that("calculate_rand_indexes returns matrix with correct dimensions", {
  result_matrix <- calculate_rand_indexes(rand_data)
  expected_size <- ncol(rand_data)
  expect_true(is.matrix(result_matrix))
  expect_equal(dim(result_matrix), c(expected_size - 1, expected_size - 1))
})

test_that("Rand Index values are between 0 and 1", {
  result_matrix <- calculate_rand_indexes(rand_data)
  expect_true(all(result_matrix >= 0, na.rm = TRUE))
  expect_true(all(result_matrix <= 1, na.rm = TRUE))
})

test_that("calculate_rand_indexes handles small input gracefully", {
  minimal_data <- data.frame(
    CellID = 1:5,
    Clustering_1 = c(1, 1, 2, 2, 2),
    Clustering_2 = c(1, 2, 2, 1, 2)
  )
  matrix_result <- calculate_rand_indexes(minimal_data)
  expect_true(is.matrix(matrix_result))
  expect_equal(dim(matrix_result), c(2, 2))
  expect_true(matrix_result[1, 2] >= 0 && matrix_result[1, 2] <= 1)
})
################

test_that("give_featureidentities returns correct structure", {
  test_matrix <- matrix(c(
    NA, 0.6, 0.8,
    0.6, NA, 0.7,
    0.8, 0.7, NA
  ), nrow = 3, byrow = TRUE)

  groupings <- give_featureidentities(test_matrix)

  expect_type(groupings, "list")
  expect_named(groupings, c("low_confidence_grouping", "medium_confidence_grouping", "high_confidence_grouping"))

  expect_true(is.matrix(groupings$low_confidence_grouping))
  expect_true(is.matrix(groupings$medium_confidence_grouping))
  expect_true(is.matrix(groupings$high_confidence_grouping))
})

test_that("give_featureidentities groups by correct thresholds", {
  mat <- matrix(c(
    NA, 0.4, 0.5, 0.8,
    0.4, NA, 0.65, 0.75,
    0.5, 0.65, NA, 0.7,
    0.8, 0.75, 0.7, NA
  ), nrow = 4)

  groups <- give_featureidentities(mat)

  # Check that values meet threshold requirements
  expect_true(all(mat[groups$low_confidence_grouping] >= 0.5, na.rm = TRUE))
  expect_true(all(mat[groups$medium_confidence_grouping] > 0.6, na.rm = TRUE))
  expect_true(all(mat[groups$high_confidence_grouping] > 0.7, na.rm = TRUE))
})

test_that("give_featureidentities handles matrix with all NA", {
  mat <- matrix(NA, nrow = 3, ncol = 3)
  groups <- give_featureidentities(mat)

  expect_equal(nrow(groups$low_confidence_grouping), 0)
  expect_equal(nrow(groups$medium_confidence_grouping), 0)
  expect_equal(nrow(groups$high_confidence_grouping), 0)
})

test_that("give_featureidentities handles empty matrix", {
  mat <- matrix(numeric(0), nrow = 0, ncol = 0)
  groups <- give_featureidentities(mat)

  expect_equal(nrow(groups$low_confidence_grouping), 0)
  expect_equal(nrow(groups$medium_confidence_grouping), 0)
  expect_equal(nrow(groups$high_confidence_grouping), 0)
})

test_that("hamming_distance_calc returns correct distance", {
  v1 <- "1111"
  v2 <- "1101"
  expect_equal(hamming_distance_calc(v1, v2), 1)
})

test_that("ID_creation handles empty dataframe", {
  df <- data.frame()
  result <- ID_creation(df)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("ID_creation merges overlapping IDs", {
  df <- data.frame(V1 = c(1, 2, 5), V2 = c(2, 3, 6))
  result <- ID_creation(df)
  expect_true(any(lengths(result) >= 3))  # 1,2,3 in same group
})

test_that("Make_cluster_id_df returns valid dataframe", {
  data <- data.frame(CellID = c("a", "b"), dummy = 1:2)
  ids <- c("111", "110")
  df <- Make_cluster_id_df(data, ids)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("UUID", "Cluster_characterising_ids"))
})

test_that("Do_hamming_amalgamation assigns closest vectors", {
  df <- data.frame(UUID = c("a", "b"), Cluster_characterising_ids = c("111", "110"))
  CharVectors <- c("111", "101")
  output <- Do_hamming_amalgamation(df, CharVectors)

  expect_s3_class(output, "data.frame")
  expect_true(all(c("UUID", "Cluster_characterising_ids", "idtitles") %in% colnames(output)))
  expect_type(output$Cluster_characterising_ids, "integer")
})
test_that("cluster_characterising returns consistent ID table", {
  data <- data.frame(CellID = c("a", "b"), dummy = 1:2)
  ids <- c("111", "110")
  result <- cluster_characterising(data = data, ids = ids)
  expect_s3_class(result, "data.frame")
  expect_true("Cluster_characterising_ids" %in% colnames(result))
})

test_that("hamming_amalgamate_Clustering runs end-to-end", {
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE))
  outlinedata <- data.frame()  # mocked input
  data <- data.frame(CellID = c("A", "B", "C"))
  rand_data <- data.frame(Clustering_1 = c(1, 1, 2), Clustering_2 = c(1, 1, 2))
  ID_list <- list(c(1, 2))

  result <- hamming_amalgamate_Clustering(data = data, ID_list = ID_list, rand_data = rand_data, outlinedata = outlinedata)
  expect_type(result, "list")
  expect_true("graph1" %in% names(result[[1]]))
})




