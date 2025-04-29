#extreme angle detector tested separately so first tests are to the hartigans
#dip stats stuff



#multi 3 should be below threshold
df <- data.frame(
  multi1 = c(rnorm(50, -3), rnorm(50, 3)),
  multi2 = c(rnorm(40, 0, 0.5), rnorm(60, 5, 1.5)),
  uni1   = rnorm(100),
  uni2   = rlnorm(100, 0, 0.5),
  multi3 = c(rnorm(30, -4), rnorm(35, 0), rnorm(35, 4)),
  uni3   = rnorm(100),
  uni4   = rnorm(100),
  uni5   = rnorm(100),
  uni6   = rnorm(100)
)
df2<-cbind(df,df,df,df,df,df)
test_that("test diptest regions detect the regions with reasonable non-unimodality",{
  expect_equal( length(get.dip.test.regions(data = df2)),6)
})

test_that("test the single diptests work",{
  expect_equal( length(monohartigansdipper(data = df)),2)
})
