#test that extreme angle detector detects extreme angles
devtools::load_all()

test_that("dataframe with known error numbers work",{
  expect_equal(sum(Extreme_angle_detector(data = data.frame(
      Angle_profile_of_imagination = 1:300,
      Words = rep("glorp", 300)))$suspected_detection_error),320)
  expect_equal(sum(Extreme_angle_detector(data = data.frame(
    Angle_profile_of_imagination = 1:280,
    Words = rep("glorp", 280)))$suspected_detection_error),280)
  expect_equal(sum(Extreme_angle_detector(data = data.frame(
    Angle_plorp_of_imagination = 1:300,
    Words = rep("glorp", 300)))$suspected_detection_error),300)
})
