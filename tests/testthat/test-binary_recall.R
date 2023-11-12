test_that("binary recall", {
  preds = c(1,1)
  target = c(1,0)
  result = binary_recall(preds, target)
  expect_equal(result, 1)

  preds = c(0.2,0.8)
  target = c(1,1)
  result = binary_recall(preds, target, threshold=0.7)
  expect_equal(result, 1/2)

  pred = matrix(c(1,0,1,0),2,2, T)
  target = t(pred)
  result = binary_recall(pred, target, multidim_average = "samplewise")
  expect_equal(result, c(1/2, NaN))

  pred = matrix(c(1,0,1,0),2,2)
  target = t(pred)
  result = binary_recall(pred, target, multidim_average = "samplewise")
  expect_equal(result, c(1, 0))
})
