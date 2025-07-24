test_that("MakeNMAConsensus returns a ggplot object without groups", {
  fake_data <- data.frame(
    Outline_OrientedCoordinates_X_1 = c(1, 2, 3),
    Outline_OrientedCoordinates_X_2 = c(4, 5, 6),
    Outline_OrientedCoordinates_Y_1 = c(7, 8, 9),
    Outline_OrientedCoordinates_Y_2 = c(10, 11, 12)
  )

  result <- MakeNMAConsensus(fake_data)

  expect_s3_class(result, "ggplot")
})

test_that("MakeNMAConsensus returns a ggplot object with valid groups", {


  result <- MakeNMAConsensus(NMA_toy_dataset, groups = NMA_toy_dataset$Dataset)

  expect_s3_class(result, "ggplot")
})

test_that("MakeNMAConsensus errors when group length does not match outlines", {
  fake_data <- data.frame(
    Outline_OrientedCoordinates_X_1 = c(1, 2, 3),
    Outline_OrientedCoordinates_X_2 = c(4, 5, 6),
    Outline_OrientedCoordinates_Y_1 = c(7, 8, 9),
    Outline_OrientedCoordinates_Y_2 = c(10, 11, 12)
  )

  bad_groups <- c("A")

  expect_error(
    MakeNMAConsensus(fake_data, groups = bad_groups),
    "Length of groups must match the number of outlines"
  )
})

test_that("MakeNMAConsensus produces different plots for different groupings", {
  fake_data <- data.frame(
    Outline_OrientedCoordinates_X_1 = c(1, 2, 3),
    Outline_OrientedCoordinates_X_2 = c(4, 5, 6),
    Outline_OrientedCoordinates_Y_1 = c(7, 8, 9),
    Outline_OrientedCoordinates_Y_2 = c(10, 11, 12)
  )

  result1 <- MakeNMAConsensus(fake_data, groups = c("A", "A","B"))
  result2 <- MakeNMAConsensus(fake_data, groups = c("A", "B","A"))

  expect_false(identical(result1$data, result2$data))
})

###

test_that("make_NMA_profile_graphs works without clusters (faceted)", {
  p <- make_NMA_profile_graphs(data = NMA_toy_dataset)
  expect_s3_class(p, "ggplot")
  expect_true("type" %in% names(p$data))
  expect_equal(length(unique(p$data$type)), 3) # All three profile types
})

test_that("make_NMA_profile_graphs works for a single profile type", {
  p <- make_NMA_profile_graphs(data = NMA_toy_dataset, profile_type = "Angle")
  expect_s3_class(p, "ggplot")
  expect_equal(unique(p$data$type), "Angle")
})

test_that("make_NMA_profile_graphs works with clusters", {
  p <- make_NMA_profile_graphs(data = NMA_toy_dataset, groups = NMA_toy_dataset$Dataset)
  expect_s3_class(p, "ggplot")
  expect_true("group" %in% names(p$data))
  expect_true(length(unique(p$data$group)) <=6)
})

test_that("make_NMA_profile_graphs works with clusters and one profile type", {
  p <- make_NMA_profile_graphs(data = NMA_toy_dataset, groups =  NMA_toy_dataset$Dataset, profile_type = "Radius")
  expect_s3_class(p, "ggplot")
  expect_equal(unique(p$data$type), "Radius")
})

test_that("make_NMA_profile_graphs errors if group length mismatch", {
  bad_clusters <- rep(1, 10) # Too short
  expect_error(make_NMA_profile_graphs(data = NMA_toy_dataset, groups = bad_clusters),
               "Length of groups must match")
})

test_that("make_NMA_profile_graphs errors with invalid profile_type", {
  expect_error(make_NMA_profile_graphs(data = NMA_toy_dataset, profile_type = "Invalid"),
               "must be one of")
})

###
# tests/testthat/test-umapnmadata.R

test_that("UmapNMAdata runs without groups and returns a ggplot", {
  data <- NMA_toy_dataset %>%
    dplyr::select(starts_with("Angle_profile_")) %>%
    as.matrix()

  plot <- UmapNMAdata(data)

  expect_s3_class(plot, "ggplot")
})

test_that("UmapNMAdata runs with groups and returns patchworked ggplot", {
  data <- NMA_toy_dataset %>%
    dplyr::select(starts_with("Angle_profile_")) %>%
    as.matrix()

  groups <- rep(1:2, length.out = nrow(data))

  plot <- UmapNMAdata(data, groups = groups)

  # patchwork plots are gg objects with class "patchwork"
  expect_s3_class(plot, "gg")
})

test_that("UmapNMAdata throws error for mismatched group length", {
  data <- NMA_toy_dataset %>%
    dplyr::select(starts_with("Angle_profile_")) %>%
    as.matrix()

  groups <- rep(1, nrow(data) - 1)

  expect_error(
    UmapNMAdata(data, groups = groups),
    "Length of 'groups' must match number of rows"
  )
})

test_that("UmapNMAdata throws error for non-numeric input", {
  bad_data <- list(A = letters[1:10], B = letters[11:20])

  expect_error(
    UmapNMAdata(bad_data),
    "'data' must be a data frame or matrix"
  )
})

