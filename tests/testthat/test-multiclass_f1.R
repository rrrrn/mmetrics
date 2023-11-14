test_that("multiclass_f1", {
  preds = seq(1, 5)
  target = rep(2, 5)
  result1 = multiclass_f1(preds, target)
  expect_equal(result1, .2)

  preds = factor(c(1,1,2,1,5), levels = c(1,2,5))
  target = factor(c(1,1,2,2,5), levels = c(1,2,5))
  result1 = multiclass_f1(preds, target)
  result2 = multiclass_f1(preds, target, average = "macro")
  expect_equal(result1, .8)
  expect_equal(result2, 37/45)

  preds = matrix(c(.99, .1, .23, .5),2,2)
  target = c(1,1)
  result = multiclass_f1(preds, target)
  expect_equal(result, .5)

  preds = (matrix(c(1,3,3,1,1,3),2,3))
  target = (matrix(c(1,1,3,3,1,3),2,3))
  result = multiclass_f1(preds, target, multidim_average = "samplewise", average="micro")
  result2 = multiclass_f1(preds, target, multidim_average = "samplewise", average="macro")
  expect_equal(result, c(1, 1/3))
  expect_equal(result2, c(1, NaN))

  preds = rep(1,4)
  target = preds
  result = multiclass_f1(preds, target)
  expect_equal(result, 1)
  result = multiclass_f1(preds, target, multidim_average = "None")
  expect_equal(result, NA)
})
