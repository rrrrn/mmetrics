test_that("confusion_scores", {
  preds = rep(1, 4)
  target = rep(1, 4)
  result = confusion_scores(preds, target)
  expect_equal((result$tp==4)&(result$fn==0)&(result$fp==0)&(result$tn==0), TRUE)

  preds = rep(TRUE, 4)
  target = rep(FALSE, 4)
  result = confusion_scores(preds, target)
  expect_equal((result$tp==0)&((result$fn==0)&(result$fp==4)|(result$fn==4)&(result$fp==0))&(result$tn==0), TRUE)

  preds = rep("A", 4)
  target = rep("B", 4)
  result = confusion_scores(preds, target)
  expect_equal((result$tp==0)&((result$fn==0)&(result$fp==4)|(result$fn==4)&(result$fp==0))&(result$tn==0), TRUE)

  preds = matrix(rep(c(1,0, 0), 3), 3, 3)
  target = t(preds)
  result = confusion_scores(preds, target, multidim_average = "samplewise")
  expect_equal(result$tp, c(1, 0, 0))
  result = confusion_scores(preds, target, multidim_average = "None")
  expect_equal(result, NA)
})
