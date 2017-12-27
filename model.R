#model.R
#generate SVM, RF, and GTB models based on finaldata

modeldata<-finalset[,-c(1, 2, 3:4, 13:14, 18)] #drop columns with non-variables

library(dplyr)

modeldata$admit <- as.factor(make.names(modeldata$admit))
modeldata$ed <- as.factor(make.names(modeldata$ed))
modeldata$adm_ed <- as.factor(make.names(modeldata$adm_ed))
modeldata$attending <- as.factor(modeldata$attending)
modeldata$Patient.Primary.Postal.Code <- as.factor(modeldata$Patient.Primary.Postal.Code)

temp <- select(modeldata, -admit, -ed, -adm_ed) #no outcomes

#make a duplicate data frame for just cancer-related details as a comparison
#8 + 169 + 58 + 2 + 51 + 88 + 111 + 59 = 546 columns
canceronly <- select(temp, attending:tsi, `coursedx_Abdominal and pelvic pain`:
                       `coursesub_Systemic Connective Tissue Disorders`, concurrent:`Tubulin Modulators.y`)

#create training and validation sets
#start by binary dummy variables
oh <- model.matrix(~.+0,data = temp)
canceronly_oh <- model.matrix(~.+0, data = canceronly)
rm(temp)

#then process
library(caret)
temp <- as.data.frame(oh)
oh_processed <- select(temp, -nearZeroVar(temp))

#then split the set both as processed and unprocessed
set.seed(220)
trainIndex <- createDataPartition(modeldata$adm_ed, p = 0.75, list=FALSE, times= 1) #note this is endpoint dependent
oh_tr <- temp[trainIndex,]
oh_ts <- temp[-trainIndex,]
rm(temp)
oh_tr_processed <- oh_processed[trainIndex,]
oh_ts_processed <- oh_processed[-trainIndex,]

#also split the cancer data only set as well
canceronly_oh_tr <- canceronly_oh[trainIndex,]
canceronly_oh_ts <- canceronly_oh[-trainIndex,]

#set the output vector
output_tr<-modeldata$adm_ed[trainIndex]
output_ts<-modeldata$adm_ed[-trainIndex]
nmin <- sum(output_tr == "X1") #also depends on the outcome

###Random forest
library(randomForest)
set.seed(220)
rf1 <- randomForest(oh_tr, y = output_tr, ntree = 1000, do.trace = TRUE, importance = TRUE) # ~30 s/tree

varImpPlot(rf1, n.var = 10, type = 1) #type 1 for permuting predictors and prediction accuracy, type 2 for Gini
varImpPlot(rf1, n.var = 10, type = 2) #type 1 for permuting predictors and prediction accuracy, type 2 for Gini
temp <- importance(rf1, type = 1)

#i like pROC for roc curves
library(pROC)

roc1 <- roc(factor(1 * (rf1$y=="X1")), rf1$votes[,2])

par(mfrow=c(1,1))
plot(roc1, main="Random forest training ROC curve", print.auc=TRUE) #black

#what's the AUC?
auc(roc1)

#plot for test set
test<-NULL
test$value <- output_ts

test$prediction <- predict(rf1, oh_ts, type = "vote")
test <- as.data.frame(test)
par(mfrow=c(1,1))
rocrftest <- roc(factor(1 * (test$value=="X1")), test$prediction.X1)
plot(rocrftest, main="Random forest test ROC curve", print.auc=TRUE)
plot(rocrftest, main="Random forest test ROC curve", print.auc=TRUE,
     print.thres = "best", print.thres.best.method="topleft") #plot best
plot(rocrftest, main="Random forest test ROC curve", print.auc=TRUE,
     print.thres = "best", print.thres.best.method="youden") #plot best

auc(rocrftest, min = 0, max = 1)


####svm
library(kernlab)

####what's the endpoint?
oh_tr_processed_svm <- oh_tr_processed
oh_tr_processed_svm <- cbind(oh_tr_processed_svm, adm_ed = output_tr) #set the output vector (depends on outcome)

set.seed(313)
svmFit <- ksvm(adm_ed~., data=oh_tr_processed_svm, method = "svmRadial", preProc = c("center", "scale"),
               class.weights=c("X0"=0.1,"X1"=0.9), #class weight
               prob.model = TRUE) #probability model has 3-fold CV
svmFit #output

#plot the ROC for the training set and then the test set
library(pROC)
#training set
svmtrain<-NULL
svmtrain$value <- output_tr

svmtrain$prediction <- predict(svmFit, oh_tr_processed_svm, type = "probabilities")
par(mfrow=c(1,1))
rocsvmtrain <- roc(factor(1 * (svmtrain$value=="X1")), svmtrain$prediction[,2])
plot(rocsvmtrain, main="Support vector machine train ROC curve", print.auc=TRUE) #black

#test set
svmtest<-NULL
svmtest$value <- output_ts

svmtest$prediction <- predict(svmFit, oh_ts_processed, type = "probabilities")
par(mfrow=c(1,1))
rocsvmtest <- roc(factor(1 * (svmtest$value=="X1")), svmtest$prediction[,2])
plot(rocsvmtest, main="Support vector machine test ROC curve", print.auc=TRUE) #black
#calc out some cut-offs
plot(rocsvmtest, main="Support vector machine test ROC curve", print.auc=TRUE,
     print.thres = "best", print.thres.best.method="topleft") #plot best
plot(rocsvmtest, main="Support vector machine test ROC curve", print.auc=TRUE,
     print.thres = "best", print.thres.best.method="youden") #plot best
#what's the AUC?
auc(rocsvmtest, min = 0, max = 1)



####xgboost
library(xgboost)
library(caret)

#set outputs to numeric
xgboutput <- as.numeric(output_tr)-1
xgbtestoutput <- as.numeric(output_ts)-1

#convert to xgb matrix *******these need to be reproduced everytime you load saved data (pointer to NULL address)
xgbtrain <- xgb.DMatrix(data = as.matrix(oh_tr), label = xgboutput)
xgbtest <- xgb.DMatrix(data = as.matrix(oh_ts), label = xgbtestoutput)
# xgbtrainproc <- xgb.DMatrix(data = as.matrix(oh_tr_processed), label = xgboutput) #proc to debug code
# xgbtestproc <- xgb.DMatrix(data = as.matrix(oh_ts_processed), label = xgbtestoutput)

xgparams <- list(booster = "gbtree", objective = "binary:logistic", eta=0.01, gamma=0, 
               max_depth=6, min_child_weight=1, eval_metric = "auc")

#hyperparameter search
#make sure to save before running this
searchGridSubCol <- expand.grid(subsample = c(0.25, 0.5, 0.75, 1),
                                colsample_bytree = c(0.4, 0.6, 0.8, 1))
ntrees <- 500
rm(aucHyperparameters, xgboostModelCV)
set.seed(313)

aucHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  
  xgboostModelCV <- xgb.cv(data =  xgbtrain,
                           nrounds = ntrees, 
                           nfold = 5, 
                           prediction = TRUE, 
                           showsd = TRUE, 
                           statified = TRUE,
                           print_every_n = 1,
                           early_stop_round = 10,
                           
                           gamma = 0,
                           min_child_weight = 1,
                           booster = "gbtree",
                           
                           metrics = "auc", 
                           verbose = TRUE, 
                           "eval_metric" = "auc",
                           "objective" = "binary:logistic", 
                           "max.depth" = 6,
                           "eta" = 0.01,
                           "subsample" = currentSubsampleRate, 
                           "colsample_bytree" = currentColsampleRate)
  
  #Save auc of the last iteration
  auc <- tail(xgboostModelCV$evaluation_log$test_auc_mean, 1)
  
  return(c(auc, currentSubsampleRate, currentColsampleRate))
  
})

library(ggplot2)
# Basic scatter plot
ggplot(as.data.frame(t(aucHyperparameters)), aes(x=paste(as.character(V2), as.character(V3)), y=V1, "")) + geom_point()

#0.75 subsample and 1 were best performers so we can use that to make a final model
tunedsub = 0.75
tunedcolsamp = 1

set.seed(220)
# fit the model with the tuned parameters
xgb_tuned <- xgboost(data = xgbtrain,
                     params = xgparams,
                     nrounds = 1000, # max number of trees to build
                     verbose = TRUE,                                         
                     print_every_n = 1,
                     early_stop_round = 10, # stop if no improvement within 10 trees
                     subsample=tunedsub, colsample_bytree=tunedcolsamp #tuned
)

# cross-validate xgboost to assess more accurate error
set.seed(220)
xgb_cv_tuned <- xgb.cv(params = xgparams,
                  data = xgbtrain,
                  nrounds = 1000,
                  nfold = 5, # number of folds in K-fold
                  prediction = TRUE, # return the prediction using the final model
                  showsd = TRUE,  # standard deviation of loss across folds
                  stratified = TRUE, # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print_every_n = 1,
                  early_stop_round = 10,
                  subsample=tunedsub, colsample_bytree=tunedcolsamp #these are parameters that usually need tuning
)

# plot the learning curve AUC in CV
library(tidyr)
xgb_cv_tuned$evaluation_log %>%
  select(-contains("std")) %>%
  gather(TestOrTrain, AUC, -iter) %>%
  ggplot(aes(x = iter, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(oh_tr),model = xgb_tuned)
xgb.plot.importance (importance_matrix = mat[1:10])

#let's make the labels nice for publication; check this with changes
prettymat <- mat
prettymat$Feature[prettymat$Feature == "numtx"] <- "Number of radiation treatments"
prettymat$Feature[prettymat$Feature == "lasted"] <- "Time since most recent ED visit"
prettymat$Feature[prettymat$Feature == "weightloss"] <- "Weight loss"
prettymat$Feature[prettymat$Feature == "age"] <- "Age"
prettymat$Feature[prettymat$Feature == "ALB"] <- "Abnormal albumin"
prettymat$Feature[prettymat$Feature == "`coursedx_Malignant neoplasm of breast`"] <- "Malignant neoplasm of breast"
prettymat$Feature[prettymat$Feature == "maxpain"] <- "Pain score 4+"
prettymat$Feature[prettymat$Feature == "lastadmit"] <- "Time since last admission"
prettymat$Feature[prettymat$Feature == "daysyearadmit"] <- "Number of days admitted"
prettymat$Feature[prettymat$Feature == "concurrent"] <- "Concurrent systemic therapy"
xgb.plot.importance (importance_matrix = prettymat[1:10], xlab = "Relative importance (predictive gain)") #built in plot

prettymat <- prettymat[1:10]
prettymat$Feature <- as.factor(prettymat$Feature)
prettymat$Feature <- factor(prettymat$Feature, levels = prettymat$Feature[(order(prettymat$Gain))])
prettymat$`Data type` <- c("Treatment/disease", "Pre-treatment encounters", "Pre-treatment vitals", 
                      "Patient characteristics", "Pre-treatment labs", "Treatment/disease", "Pre-treatment vitals", 
                      "Pre-treatment encounters", "Pre-treatment encounters", "Treatment/disease")
library(ggplot2)
impplot <- ggplot(prettymat, aes(Feature, Gain, fill = `Data type`)) + geom_col() + coord_flip()
impplot + scale_fill_manual(values = c("firebrick", "forestgreen", "darkorchid3", "goldenrod", "dodgerblue4")) +
  theme(legend.position = c(0.7, 0.2)) + ylab("Relative importance (predictive gain)") + xlab("Variable")

#plot the ROC for the training set and then the test set
library(pROC)
#training set
xgbtestcheck<-NULL
xgbtestcheck$value <- output_tr

xgbtestcheck$prediction <- predict(xgb_tuned, xgbtrain, type = "probabilities")
par(mfrow=c(1,1))
rocxgbtrain <- roc(factor(1 * (xgbtestcheck$value=="X1")), xgbtestcheck$prediction)
plot(rocxgbtrain, main="Gradient tree boosting train ROC curve", print.auc=TRUE) #black

#now test set
xgbcheck<-NULL
xgbcheck$value <- output_ts

xgbcheck$prediction <- predict(xgb_tuned, xgbtest, type = "probabilities")
par(mfrow=c(1,1))
library(pROC)
rocgtbtest <- roc(factor(1 * (xgbcheck$value=="X1")), xgbcheck$prediction)
plot(rocgtbtest, main="Gradient tree boosting test ROC curve", print.auc=TRUE) #black
#calc out some cut-offs
plot(rocgtbtest, main="Gradient tree boosting test ROC curve", print.auc=TRUE,
     print.thres = "best", print.thres.best.method="topleft") #plot best
plot(rocgtbtest, main="Gradient tree boosting test ROC curve", print.auc=TRUE,
     print.thres = "best", print.thres.best.method="youden") #plot best
#what's the AUC?
auc(rocgtbtest, min = 0, max = 1)


#compute the confusion matrix for the test set using cancer xgbcheck
#calculate the prediction based on the cut-off
#Youden 0.070
xgbcheck$predictclass[xgbcheck$prediction > 0.070] <- "X1"
xgbcheck$predictclass[xgbcheck$prediction <+ 0.070] <- "X0"
confusionMatrix(xgbcheck$predictclass, xgbcheck$value, positive = "X1")


################
####run the "control" group with cancer only data
#convert to xgb matrix
cancerxgbtrain <- xgb.DMatrix(data = as.matrix(canceronly_oh_tr), label = xgboutput)
cancerxgbtest <- xgb.DMatrix(data = as.matrix(canceronly_oh_ts), label = xgbtestoutput)

set.seed(220)
# fit the model with the tuned parameters
cancerxgb_tuned <- xgboost(data = cancerxgbtrain,
                     params = xgparams,
                     nrounds = 1000, # max number of trees to build
                     verbose = TRUE,                                         
                     print_every_n = 1,
                     early_stop_round = 10, # stop if no improvement within 10 trees
                     subsample=tunedsub, colsample_bytree=tunedcolsamp #tuned
)

#view variable importance plot
cancermat <- xgb.importance (feature_names = colnames(canceronly_oh_tr),model = cancerxgb_tuned)
xgb.plot.importance (importance_matrix = cancermat[1:10])


#plot the ROC for the training set and then the test set
library(pROC)
#training set
cancerxgbtestcheck<-NULL
cancerxgbtestcheck$value <- output_tr

cancerxgbtestcheck$prediction <- predict(cancerxgb_tuned, cancerxgbtrain, type = "probabilities")
par(mfrow=c(1,1))
roccancerxgbtrain <- roc(factor(1 * (cancerxgbtestcheck$value=="X1")), cancerxgbtestcheck$prediction)
plot(roccancerxgbtrain, main="Cancer data only gradient tree boosting train ROC curve", print.auc=TRUE) #black


#now test set
cancerxgbcheck<-NULL
cancerxgbcheck$value <- output_ts

cancerxgbcheck$prediction <- predict(cancerxgb_tuned, cancerxgbtest, type = "probabilities")
par(mfrow=c(1,1))
library(pROC)
cancerrocgtbtest <- roc(factor(1 * (cancerxgbcheck$value=="X1")), cancerxgbcheck$prediction)
plot(cancerrocgtbtest, main="Cancer data only gradient tree boosting test ROC curve", print.auc=TRUE) #black

#######plot all of the ROC curves
par(mfrow=c(1,1))
plot(rocgtbtest, main="Validation ROC curves", col = "dodgerblue2", lwd = 1) #blue
plot(rocrftest, col="chartreuse4", lwd = 1, add=T) #green
plot(rocsvmtest, col="firebrick", lwd = 1, add=T) #red
plot(cancerrocgtbtest, col="black", lwd = 1, add=T) #black

legend("bottomright", legend = c("GTB", "RF", "SVM", "GTB disease/treatment only"), 
       col = c("dodgerblue2", "chartreuse4", "firebrick", "black"),lwd = 2)