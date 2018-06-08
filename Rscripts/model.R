rm(list=ls())
cat('\014')

###############################################################################
#                           Load External Libraries                           #
###############################################################################
library(caret)
library(data.table)
library(parallel)
library(doParallel)
library(pROC)


dat=readRDS('data/dat_for_model.RDS')
dat[,CIF:=NULL]

###############################################################################
#                          Preprocessing                                      #
###############################################################################

#########         Center n Scale all Continuous Variables        ##############
Y=make.names(dat[,target])

dat[,target:=NULL]

preprocessor = preProcess(dat,method=c('center','scale','knnImpute'))
dat=predict(preprocessor,dat)

########-----------Converting Bounce Ratios to factors----------------#########

  # For customers with cash repayment, Bounce Ratio does not exist. Imputing 
  # those missing obs will not be meaningful. 

# dat[,BR6m:=as.factor(round(BR6m,2))]
# dat[,BR3m:=as.factor(round(BR3m,2))]

########                 One Hot Encoding of Factors               ############
id = dat[,AggrNo]
dat[,AggrNo:=NULL]
ohe =dummyVars("~.",data=dat,sep = "_",fullRank =F)
X=data.table(predict(ohe,newdata =dat))
rem = c(
  "Stateother",
  "LoanPurposeother",
  "BorrowerDescother",
  "BorrowerTypeOthers",
  "ManufacturerDescOthers",
  "VehicleCategoryother",
  "AssetLevel_",
  "BounceFlag_FALSE",
  "ever60Flag_FALSE",
  "ever30Flag_FALSE",
  "everXFlag_FALSE",
  "AgriFlag_FALSE",
  "BorrowerCategoryother",
  "CashPayFlag_FALSE",
  "DPDDays_0" 
)
X[,(rem):=NULL]
fwrite(cbind(target=Y,X),file='data/OneHotEncoded.csv')
#Y=as.factor(paste('x',as.integer(Y),sep=''))
########                  Train-Test Split                        #############
set.seed(6335)
index = createDataPartition(Y,p=.75,list=F)
train_X = X[index]
test_X = X[-index]

train_Y = Y[index]
test_Y = as.factor(Y[-index])



###############################################################################
#                                   Models                                    #
###############################################################################


#####-------------------Set Up Parallel Processing-------------------------#####
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)


#####--------------------------Decision Tree------------------------------#####
set.seed(6335)
modelControls=trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs=TRUE,
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE,
                           verboseIter = TRUE)

model_dt = train(train_X,train_Y, method = 'rpart',
                 trControl = modelControls, tuneLength = 15, metric = 'accuracy')
cm_dt = confusionMatrix(data=predict(model_dt,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_dt = predict(model_dt,newdata = test_X,type = 'prob')
ROC_dt = roc(test_Y,pred_dt$X0)
perf_dt = data.table(thres = ROC_dt$thresholds,Spec = ROC_dt$specificities, Sens = ROC_dt$sensitivities)
perf_dt[,temp:=Spec+Sens]
View(perf_dt)
fit = as.factor(ifelse(pred_dt$x1>perf_dt[order(-temp)][1,1],'X1','X0'))
cm_dt_modified = confusionMatrix(data=fit,reference = test_Y)


#####-------------------------Random Forest------------------------------######

modelControls=trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = 'random',
                           classProbs=TRUE,
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)
Sys.time()
model_rf = train(train_X,train_Y, method = 'rf',
                    trControl = modelControls,tuneLength = 12, metric = 'Spec')
cm_rf = confusionMatrix(data=predict(model_rf,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_rf = predict(model_rf,newdata = test_X,type = 'prob')
ROC_rf = roc(test_Y,pred_rf$X0)
perf_rf = data.table(thres = ROC_rf$thresholds,Spec = ROC_rf$specificities, Sens = ROC_rf$sensitivities)
perf_rf[,temp:=Spec+Sens]
View(perf_rf)
fit = as.factor(ifelse(pred_rf$x1>perf_rf[order(-temp)][1,1],'X1','X0'))
cm_rf_modified = confusionMatrix(data=fit,reference = test_Y)


#####-------------------------GBM------------------------------######

modelControls=trainControl(method = "repeatedcv",
                           number = 6,
                           repeats = 3,
                           classProbs=TRUE,
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE,
                           verboseIter = TRUE)
Sys.time()
model_gbm = train(train_X,train_Y, method = 'gbm',
                 trControl = modelControls,tuneLength = 5, metric = 'ROC')
cm_gbm = confusionMatrix(data=predict(model_gbm,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_gbm = predict(model_gbm,newdata = test_X,type = 'prob')
ROC_gbm = roc(test_Y,pred_gbm$X0)
perf_gbm = data.table(thres = ROC_gbm$thresholds,Spec = ROC_gbm$specificities, Sens = ROC_gbm$sensitivities)
perf_gbm[,temp:=Spec+Sens]
View(perf_gbm)
fit = as.factor(ifelse(pred_gbm$x1>perf_gbm[order(-temp)][1,1],'X1','X0'))
cm_gbm_modified = confusionMatrix(data=fit,reference = test_Y)



#####----------------------------------XGBOOST----------------------------#####

modelControls=trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs=TRUE,
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE,
                           verboseIter = TRUE)
Sys.time()
model_xgb = train(train_X,train_Y, method = 'xgbTree',
                  trControl = modelControls,tuneLength = 40, metric = 'ROC')
cm_xgb = confusionMatrix(data=predict(model_xgb,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_xgb = predict(model_xgb,newdata = test_X,type = 'prob')
ROC_xgb = roc(test_Y,pred_xgb$X0)
perf_xgb = data.table(thres = ROC_xgb$thresholds,Spec = ROC_xgb$specificities, Sens = ROC_xgb$sensitivities)
perf_xgb[,temp:=Spec+Sens]
View(perf_xgb)
fit = as.factor(ifelse(pred_xgb$x1>perf_xgb[order(-temp)][1,1],'X1','X0'))
cm_xgb_modified = confusionMatrix(data=fit,reference = test_Y)



