test_that("multiclass_acc function", {
  preds = seq(1, 5)
  target = rep(2, 5)
  result1 = multiclass_acc(preds, target)
  expect_equal(result1, .2)

  preds = c(1,1,2,1,5)
  target = c(1,1,2,2,5)
  result1 = multiclass_acc(preds, target)
  result2 = multiclass_acc(preds, target, average = "macro")
  expect_equal(result1, .8)
  expect_equal(result2, 5/6)

  preds = matrix(c(.99, .1, .23, .5),2,2)
  target = c(1,1)
  result = multiclass_acc(preds, target)
  expect_equal(result, 0.5)

  preds = matrix(c(1,2,3,1,1,3),2,3)
  target = matrix(c(1,1,3,3,1,1),2,3)
  result = multiclass_acc(preds, target, multidim_average = "samplewise")
  expect_equal(result, c(1, 0))
})
