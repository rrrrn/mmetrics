test_that("binary accuracy function", {
  # global
  pred = c(1,0)
  target = c(1,1)
  expect_equal(binary_acc(pred, target), 0.5)

  # global, customize threshold
  pred = c(0.2, 0.35)
  target = c(0, 0)
  expect_equal(binary_acc(pred, target, 0.4), 1)

  # samplewise
  pred = matrix(c(1,0,1,0),2,2, T)
  target = t(pred)
  expect_equal(binary_acc(pred, target, multidim_average = "samplewise"), c(0.5,0.5))
})


