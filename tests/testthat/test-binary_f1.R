test_that("binary f1", {
  preds = c(1,1)
  target = c(1,0)
  result = binary_f1(preds, target)
  expect_equal(result, 2/3)

  preds = c(0.2,0.8)
  target = c(1,1)
  result = binary_f1(preds, target, threshold=0.7)
  expect_equal(result, 2/3)

  pred = matrix(c(1,0,1,0),2,2, T)
  target = t(pred)
  result = binary_f1(pred, target, multidim_average = "samplewise")
  expect_equal(result, c(2/3,NaN))

  pred = (c(1,0,1,0))
  target = (c(0,1,0,1))
  result = binary_f1(pred, target)
  expect_equal(result, NaN)

  pred = matrix(c(1,0,1,0),2,2)
  target = t(pred)
  result = binary_f1(pred, target, multidim_average = "samplewise")
  expect_equal(result, c(2/3,NaN))
})
