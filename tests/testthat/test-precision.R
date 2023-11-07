test_that("binary precision", {
  preds = c(1,1,1,1)
  target = c(1,0,1,1)
  result = binary_precision(preds, target)
  expect_equal(result, 3/4)

  preds = c(0,0,0,0.8)
  target = c(1,0,1,1)
  result = binary_precision(preds, target, threshold=0.7)
  expect_equal(result, 1)

  preds = matrix(c(1,1,0,1,0,1),2,3)
  target = matrix(c(1,1,0, 0,0,1),2,3)
  result = binary_precision(preds, target, multidim_average = "samplewise")
  expect_equal(result, c(1,2/3))
})

test_that("multiclass_precision precision", {
  preds = seq(1, 5)
  target = rep(2, 5)
  result1 = multiclass_precision(preds, target)
  expect_equal(result1, .2)

  preds = c(1,1,2,1,5)
  target = c(1,1,2,2,5)
  result1 = multiclass_precision(preds, target)
  result2 = multiclass_precision(preds, target, average = "macro")
  expect_equal(result1, .8)
  expect_equal(result2, 8/9)

  preds = matrix(c(.99, .1, .23, .5),2,2)
  target = c(1,1)
  result = multiclass_precision(preds, target)
  expect_equal(result, .5)

  preds = matrix(c(1,3,3,1,1,3),2,3)
  target = matrix(c(1,1,3,3,1,3),2,3)
  result = multiclass_precision(preds, target, multidim_average = "samplewise", average="micro")
  result2 = multiclass_precision(preds, target, multidim_average = "samplewise", average="macro")
  expect_equal(result, c(1, 1/3))
  expect_equal(result2, c(1, 1/4))

  preds = rep(1,4)
  target = preds
  result = multiclass_precision(preds, target)
  expect_equal(result, 1)
})
