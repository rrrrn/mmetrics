## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
rm(list = ls())
library(mmetrics)
library(NHANES)
library(randomForest)

## -----------------------------------------------------------------------------
# filter out records with NA value
df <- NHANES[,c("Depressed", "SleepHrsNight", "BMI", "PhysActive", "Poverty", "Age", "Gender")]
df <- df[apply(!is.na(df), 1, all),]

# split train and test set
set.seed(345)
n <- nrow(df)
train_id <- sample(1:n, floor(0.8*n))
test_id <- seq(1, n)[!(seq(1,n) %in% train_id)]

train_df <- df[train_id, ]
test_df <- df[test_id, 2:ncol(df)]
target <- (df[test_id, ]$Depressed)

## -----------------------------------------------------------------------------
set.seed(345)
model1 <- randomForest(Depressed~., train_df, ntree=10, replace=FALSE)
preds_prob1 <- (predict(model1, test_df, type = "prob"))
preds1 <- (predict(model1, test_df))

## -----------------------------------------------------------------------------
set.seed(345)
model2 <- randomForest(Depressed~., train_df, ntree=200,replace=FALSE)
preds_prob2 <- (predict(model2, test_df, type = "prob"))
preds2 <- (predict(model2, test_df))

## -----------------------------------------------------------------------------
e_s1 = binary_acc(preds1=="Most", target=="Most"); e_r1 = binary_acc(preds2=="Most", target=="Most")
e_s2 = binary_acc(preds1=="Most", target=="Several"); e_r2 = binary_acc(preds2=="Most", target=="Several")
e_s3 = binary_acc(preds1=="Most", target=="None"); e_r3 = binary_acc(preds2=="Most", target=="None")
data.frame(model1 = c(e_s1, e_s2, e_s3), model2 = c(e_r1, e_r2, e_r3), row.names = c("Most", "Several", "None"))

## -----------------------------------------------------------------------------
table(target)

mic_e_s = multiclass_acc(preds1, target)
mac_e_s = multiclass_acc(preds1, target, average = "macro")

mic_e_r = multiclass_acc(preds2, target)
mac_e_r = multiclass_acc(preds2, target, average = "macro")

data.frame(model1 = c(mic_e_s, mac_e_s), model2 = c(mic_e_r, mac_e_r), row.names = c("Micro Average", "Macro Average"))

## -----------------------------------------------------------------------------
## class specific
mtx1 = confusion_scores(preds1=="Most", target=="Most")$matrix
mtx2 = multiclass_confusion_scores(preds1, target, classtype = "Most")$matrix
stopifnot(mtx1==mtx2); mtx1

## -----------------------------------------------------------------------------
multiclass_confusion_scores(preds1, target)

## -----------------------------------------------------------------------------
binary_precision(preds_prob1[,"None"], target=="None", threshold=0.5)

## -----------------------------------------------------------------------------
mic_p_s = multiclass_precision(preds1, target, average = "micro")
mac_p_s = multiclass_precision(preds1, target, average = "macro")

mic_p_r = multiclass_precision(preds2, target, average = "micro")
mac_p_r = multiclass_precision(preds2, target, average = "macro")

data.frame(model1 = c(mic_p_s, mac_p_s), model2 = c(mic_p_r, mac_p_r), row.names = c("Micro Average", "Macro Average"))

## -----------------------------------------------------------------------------
binary_recall(preds_prob1[,"None"], target=="None", threshold=0.5)

## -----------------------------------------------------------------------------
mic_r_s = multiclass_recall(preds1, target, average = "micro")
mac_r_s = multiclass_recall(preds1, target, average = "macro")

mic_r_r = multiclass_recall(preds2, target, average = "micro")
mac_r_r = multiclass_recall(preds2, target, average = "macro")

data.frame(model1 = c(mic_r_s, mac_r_s), model2 = c(mic_r_r, mac_r_r), row.names = c("Micro Average", "Macro Average"))

## -----------------------------------------------------------------------------
set.seed(345)
preds_samplewise = matrix(sample(0:1, 100, replace = T), 10, 10)
target_samplewise = t(preds_samplewise)
binary_acc(preds_samplewise, target_samplewise, multidim_average = "samplewise")

## -----------------------------------------------------------------------------
library(caret)
library(bench)
set.seed(345)
preds_sim = sample(0:1, 100000, replace = T)
preds_sim_fac = factor(preds_sim, levels = c(1,0))
set.seed(123)
target_sim = sample(0:1, 100000, replace = T)
target_sim_fac = factor(target_sim, levels = c(1,0))

cfs = confusionMatrix(preds_sim_fac, target_sim_fac)

## accuracy
all.equal(cfs[["overall"]][["Accuracy"]], binary_acc(preds_sim, target_sim))
test.cfs = function(){
  for(i in 1:100){
    confusionMatrix(preds_sim_fac, target_sim_fac)[["overall"]][["Accuracy"]]
  }
}
test.acc = function(){
  for(i in 1:100){
    binary_acc(preds_sim, target_sim)
  }
}
mark(test.cfs(), test.acc())

## -----------------------------------------------------------------------------
## precision
all.equal(cfs[["byClass"]][["Pos Pred Value"]], binary_precision(preds_sim, target_sim))
test.cfs = function(){
  for(i in 1:100){
    confusionMatrix(preds_sim_fac, target_sim_fac)[["byClass"]][["Pos Pred Value"]]
  }
}
test.precision = function(){
  for(i in 1:100){
    binary_precision(preds_sim, target_sim)
  }
}
mark(test.cfs(), test.precision())

## -----------------------------------------------------------------------------
all.equal(cfs[["byClass"]][["Sensitivity"]], binary_recall(preds_sim, target_sim))
test.cfs = function(){
  for(i in 1:100){
    confusionMatrix(preds_sim_fac, target_sim_fac)[["byClass"]][["Sensitivity"]]
  }
}
test.recall = function(){
  for(i in 1:100){
    binary_recall(preds_sim, target_sim)
  }
}
mark(test.cfs(), test.recall())

