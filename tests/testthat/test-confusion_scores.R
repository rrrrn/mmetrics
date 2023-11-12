test_that("confusion_scores", {
  preds = rep(1, 4)
  target = rep(1, 4)
  result = confusion_scores(preds, target)
  expect_equal((result$tp==4)&(result$fn==0)&(result$fp==0)&(result$tn==0), TRUE)

  preds = rep(1, 4)
  target = rep(0, 4)
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
})

test_that("multiclass confusion_scores", {
  preds = rep(1, 4)
  target = rep(7, 4)
  result = multiclass_confusion_scores(preds, target)
  expect_equal(all(diag(result)==0), TRUE)

  preds = factor(rep("A", 4))
  target = factor(rep("B", 4))
  result = multiclass_confusion_scores(preds, target)
  expect_equal(all(diag(result)==0), TRUE)

  preds = rep("A", 4)
  target = rep(c("B", "C"), 2)
  result = multiclass_confusion_scores(preds, target, class="A")
  expect_equal((result$fn==0&result$fp==4)|(result$fn==4&result$fp==0), TRUE)

  preds = matrix(rep(c(1,0, 3, 1), 4), 4, 4)
  target = t(preds)
  result = multiclass_confusion_scores(preds, target, multidim_average = "samplewise", class=1)
  expect_equal(result$tp, c(2, 0, 0, 2))

  preds = rep(1,4)
  target = preds
  result = multiclass_confusion_scores(preds, target)
  expect_equal(result[[1]], 4)
  result = multiclass_confusion_scores(preds, target, multidim_average="samplewise")
  expect_equal(result, NaN)
})
