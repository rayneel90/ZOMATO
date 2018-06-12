rm(list = ls())
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
outsample=readRDS('data/outsample_data.RDS')

###############################################################################
#                          Preprocessing                                      #
###############################################################################

dat[,CIF:=NULL]
outsample[,CIF:=NULL]

Y=make.names(dat[,target])
Y_outsample = make.names(outsample[,target])

dat[,target:=NULL]
outsample[,target:=NULL]

#########         Center n Scale all Continuous Variables        ##############
preprocessor = preProcess(dat,method=c('center','scale','knnImpute'))
dat=predict(preprocessor,dat)
outsample = predict(preprocessor,outsample)

########-----------Converting Bounce Ratios to factors----------------#########

# For customers with cash repayment, Bounce Ratio does not exist. Imputing 
# those missing obs will not be meaningful. 

# dat[,BR6m:=as.factor(round(BR6m,2))]
# dat[,BR3m:=as.factor(round(BR3m,2))]

########                 One Hot Encoding of Factors               ############
id = dat[,AggrNo]
id_outsample = outsample[,AggrNo]
dat[,AggrNo:=NULL]
outsample[,AggrNo:=NULL]

ohe =dummyVars("~.",data=dat,sep = "_",fullRank =F)
X=data.table(predict(ohe,newdata =dat))
outsample[DPDDays==11,DPDDays:='7']
X_outsample = data.table(predict(ohe,newdata =outsample))

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
X_outsample[,(rem):=NULL]
X_outsample[is.na(X_outsample)]=0
fwrite(cbind(Y,X),file = 'data/model_data_one_hot_encoded.csv')
fwrite(cbind(Y_outsample,X_outsample),file = 'data/outsample_one_hot_encoded.csv')
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
model_dt=readRDS('model/model_dt.rds')
model_rf=readRDS('model/model_rf.rds')
model_gbm=readRDS('model/model_gbm.rds')
model_xgb=readRDS('model/model_xgb.rds')


df = list()
#####--------------------------Decision Tree------------------------------#####
cm_dt = confusionMatrix(data=predict(model_dt,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_dt_test = predict(model_dt,newdata = test_X,type = 'prob')
pred_dt_train = predict(model_dt,newdata = train_X,type = 'prob')

ROC_dt = roc(train_Y,pred_dt_train$X1)
perf_dt = data.table(thres = ROC_dt$thresholds,Spec = ROC_dt$specificities, Sens = ROC_dt$sensitivities)
perf_dt[,temp:=Spec+Sens]
#View(perf_dt)
cutoff_dt_train = perf_dt[order(-temp)]$thres[1]

ROC_dt = roc(test_Y,pred_dt_test$X1)
perf_dt = data.table(thres = ROC_dt$thresholds,Spec = ROC_dt$specificities, Sens = ROC_dt$sensitivities)
perf_dt[,temp:=Spec+Sens]
#View(perf_dt)
cutoff_dt_test = perf_dt[order(-temp)]$thres[1]
fit = as.factor(ifelse(pred_dt_test$X1>cutoff_dt_train,'X1','X0'))
cm_dt_train_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                      positive = 'X1')

fit = as.factor(ifelse(pred_dt_test$X1>cutoff_dt_test,'X1','X0'))
cm_dt_test_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                     positive = 'X1')

pred_dt_outsample = predict(model_dt,newdata = X_outsample,type = 'prob')
fit = as.factor(ifelse(pred_dt_outsample$X1>cutoff_dt_train,'X1','X0'))
cm_dt_outsample_trc = confusionMatrix(data=fit,reference = as.factor(Y_outsample),
                                   positive = 'X1')

fit = as.factor(ifelse(pred_dt_outsample$X1>cutoff_dt_test,'X1','X0'))
cm_dt_outsample_tsc = confusionMatrix(data=fit,
                                       reference = as.factor(Y_outsample),
                                       positive = 'X1')

tmp = as.data.frame.list(cm_dt_train_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = cbind(
  data.frame('model'='dt','detail'='test data train cutoff'),
  tmp,
  as.data.frame.list(cm_dt_train_cutoff$byClass)
)
tmp = as.data.frame.list(cm_dt_test_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='dt','detail'='test data test cutoff'),
  tmp,
  as.data.frame.list(cm_dt_test_cutoff$byClass)
))
tmp = as.data.frame.list(cm_dt_outsample_trc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='dt','detail'='outsample data train cutoff'),
  tmp,
  as.data.frame.list(cm_dt_outsample_trc$byClass)
))
tmp = as.data.frame.list(cm_dt_outsample_tsc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='dt','detail'='outsample data test cutoff'),
  tmp,
  as.data.frame.list(cm_dt_outsample_tsc$byClass)
))
#####-------------------------Random Forest------------------------------######

cm_rf = confusionMatrix(data=predict(model_rf,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_rf_test = predict(model_rf,newdata = test_X,type = 'prob')
pred_rf_train = predict(model_rf,newdata = train_X,type = 'prob')

ROC_rf = roc(train_Y,pred_rf_train$X1)
perf_rf = data.table(thres = ROC_rf$thresholds,Spec = ROC_rf$specificities, Sens = ROC_rf$sensitivities)
perf_rf[,temp:=Spec+Sens]
#View(perf_rf)
cutoff_rf_train = perf_rf[order(-temp)]$thres[1]

ROC_rf = roc(test_Y,pred_rf_test$X1)
perf_rf = data.table(thres = ROC_rf$thresholds,Spec = ROC_rf$specificities, Sens = ROC_rf$sensitivities)
perf_rf[,temp:=Spec+Sens]
#View(perf_rf)
cutoff_rf_test = perf_rf[order(-temp)]$thres[1]

fit = as.factor(ifelse(pred_rf_test$X1>cutoff_rf_train,'X1','X0'))
cm_rf_train_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                      positive = 'X1')

fit = as.factor(ifelse(pred_rf_test$X1>cutoff_rf_test,'X1','X0'))
cm_rf_test_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                     positive = 'X1')

pred_rf_outsample = predict(model_rf,newdata = X_outsample,type = 'prob')
fit = as.factor(ifelse(pred_rf_outsample$X1>cutoff_rf_train,'X1','X0'))
cm_rf_outsample_trc = confusionMatrix(data=fit,reference = as.factor(Y_outsample),
                                   positive = 'X1')

fit = as.factor(ifelse(pred_rf_outsample$X1>cutoff_rf_test,'X1','X0'))
cm_rf_outsample_tsc = confusionMatrix(data=fit,
                                       reference = as.factor(Y_outsample),
                                       positive = 'X1')
tmp = as.data.frame.list(cm_rf_train_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='rf','detail'='test data train cutoff'),
  tmp,
  as.data.frame.list(cm_rf_train_cutoff$byClass)
))
tmp = as.data.frame.list(cm_rf_test_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='rf','detail'='test data test cutoff'),
  tmp,
  as.data.frame.list(cm_rf_test_cutoff$byClass)
))
tmp = as.data.frame.list(cm_rf_outsample_trc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='rf','detail'='outsample data train cutoff'),
  tmp,
  as.data.frame.list(cm_rf_outsample_trc$byClass)
))
tmp = as.data.frame.list(cm_rf_outsample_tsc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='rf','detail'='outsample data test cutoff'),
  tmp,
  as.data.frame.list(cm_rf_outsample_tsc$byClass)
))

#####-------------------------GBM------------------------------######

cm_gbm = confusionMatrix(data=predict(model_gbm,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_gbm_test = predict(model_gbm,newdata = test_X,type = 'prob')
pred_gbm_train = predict(model_gbm,newdata = train_X,type = 'prob')

ROC_gbm = roc(train_Y,pred_gbm_train$X1)
perf_gbm = data.table(thres = ROC_gbm$thresholds,Spec = ROC_gbm$specificities, Sens = ROC_gbm$sensitivities)
perf_gbm[,temp:=Spec+Sens]
#View(pegbm_gbm)
cutoff_gbm_train = perf_gbm[order(-temp)]$thres[1]

ROC_gbm = roc(test_Y,pred_gbm_test$X1)
perf_gbm = data.table(thres = ROC_gbm$thresholds,Spec = ROC_gbm$specificities, Sens = ROC_gbm$sensitivities)
perf_gbm[,temp:=Spec+Sens]
#View(pegbm_gbm)
cutoff_gbm_test = perf_gbm[order(-temp)]$thres[1]

fit = as.factor(ifelse(pred_gbm_test$X1>cutoff_gbm_train,'X1','X0'))
cm_gbm_train_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                      positive = 'X1')

fit = as.factor(ifelse(pred_gbm_test$X1>cutoff_gbm_test,'X1','X0'))
cm_gbm_test_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                     positive = 'X1')

pred_gbm_outsample = predict(model_gbm,newdata = X_outsample,type = 'prob')
fit = as.factor(ifelse(pred_gbm_outsample$X1>cutoff_gbm_train,'X1','X0'))
cm_gbm_outsample_trc = confusionMatrix(data=fit,reference = as.factor(Y_outsample),
                                   positive = 'X1')

fit = as.factor(ifelse(pred_gbm_outsample$X1>cutoff_gbm_test,'X1','X0'))
cm_gbm_outsample_tsc = confusionMatrix(data=fit,
                                       reference = as.factor(Y_outsample),
                                       positive = 'X1')

tmp = as.data.frame.list(cm_gbm_train_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='gbm','detail'='test data train cutoff'),
  tmp,
  as.data.frame.list(cm_gbm_train_cutoff$byClass)
))
tmp = as.data.frame.list(cm_gbm_test_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='gbm','detail'='test data test cutoff'),
  tmp,
  as.data.frame.list(cm_gbm_test_cutoff$byClass)
))
tmp = as.data.frame.list(cm_gbm_outsample_trc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='gbm','detail'='outsample data train cutoff'),
  tmp,
  as.data.frame.list(cm_gbm_outsample_trc$byClass)
))
tmp = as.data.frame.list(cm_gbm_outsample_tsc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='gbm','detail'='outsample data test cutoff'),
  tmp,
  as.data.frame.list(cm_gbm_outsample_tsc$byClass)
))
#####----------------------------------XGBOOST----------------------------#####

cm_xgb = confusionMatrix(data=predict(model_xgb,newdata = test_X),reference = as.factor(test_Y),positive = 'X1')
pred_xgb_test = predict(model_xgb,newdata = test_X,type = 'prob')
pred_xgb_train = predict(model_xgb,newdata = train_X,type = 'prob')

ROC_xgb = roc(train_Y,pred_xgb_train$X1)
perf_xgb = data.table(thres = ROC_xgb$thresholds,Spec = ROC_xgb$specificities, Sens = ROC_xgb$sensitivities)
perf_xgb[,temp:=Spec+Sens]
#View(pexgb_xgb)
cutoff_xgb_train = perf_xgb[order(-temp)]$thres[1]

ROC_xgb = roc(test_Y,pred_xgb_test$X1)
perf_xgb = data.table(thres = ROC_xgb$thresholds,Spec = ROC_xgb$specificities, Sens = ROC_xgb$sensitivities)
perf_xgb[,temp:=Spec+Sens]
#View(perf_xgb)
cutoff_xgb_test = perf_xgb[order(-temp)]$thres[1]

fit = as.factor(ifelse(pred_xgb_test$X1>cutoff_xgb_train,'X1','X0'))
cm_xgb_train_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                  positive = 'X1')

fit = as.factor(ifelse(pred_xgb_test$X1>cutoff_xgb_test,'X1','X0'))
cm_xgb_test_cutoff = confusionMatrix(data=fit,reference = test_Y,
                                      positive = 'X1')

pred_xgb_outsample = predict(model_xgb,newdata = X_outsample,type = 'prob')
fit = as.factor(ifelse(pred_xgb_outsample$X1>cutoff_xgb_train,'X1','X0'))
cm_xgb_outsample_trc = confusionMatrix(data=fit,
                                      reference = as.factor(Y_outsample),
                                      positive = 'X1')

fit = as.factor(ifelse(pred_xgb_outsample$X1>cutoff_xgb_test,'X1','X0'))
cm_xgb_outsample_tsc = confusionMatrix(data=fit,
                                      reference = as.factor(Y_outsample),
                                      positive = 'X1')

tmp = as.data.frame.list(cm_xgb_train_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='xgb','detail'='test data train cutoff'),
  tmp,
  as.data.frame.list(cm_xgb_train_cutoff$byClass)
))
tmp = as.data.frame.list(cm_xgb_test_cutoff$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='xgb','detail'='test data test cutoff'),
  tmp,
  as.data.frame.list(cm_xgb_test_cutoff$byClass)
))
tmp = as.data.frame.list(cm_xgb_outsample_trc$table)
colnames(tmp) = c
df = rbind(df,cbind(
  data.frame('model'='xgb','detail'='outsample data train cutoff'),
  tmp,
  as.data.frame.list(cm_xgb_outsample_trc$byClass)
))
tmp = as.data.frame.list(cm_xgb_outsample_tsc$table)
colnames(tmp) = c('a0p0','a0p1','a1po','a1p1')
df = rbind(df,cbind(
  data.frame('model'='xgb','detail'='outsample data test cutoff'),
  tmp,
  as.data.frame.list(cm_xgb_outsample_tsc$byClass)
))


write.csv(df,file = 'output/model_performance_comparison.csv')


###############################################################################
#                         Cut_OFF Distribution                                #
###############################################################################
get_CM=function(x)
{
  lst = list(0) 
  for( i in 2:5)
  {
    temp= as.data.frame.list(confusionMatrix(as.factor(ifelse(outsample_data_prediction[,i,with=FALSE]>x,'X1','X0')),as.factor(outsample_data_prediction[[1]]),positive = 'X1')$table)
    names(temp) = paste(colnames(outsample_data_prediction)[i],c('a0p0','a0p1','a1po','a1p1'),sep='_') 
    lst[[i-1]] = temp
  }
  row = do.call(cbind,lst)
}

test_data_prediction = data.table(
  obs = test_Y,
  dt = predict(model_dt,newdata = test_X,type = 'prob')$X1,
  rf = predict(model_rf,newdata = test_X,type = 'prob')$X1,
  gbm = predict(model_gbm,newdata = test_X,type = 'prob')$X1,
  xgb = predict(model_xgb,newdata = test_X,type = 'prob')$X1
)
fwrite(test_data_prediction,file='output/test_data_predictions.csv')
outsample_data_prediction = data.table(
  obs = Y_outsample,
  dt = predict(model_dt,newdata = X_outsample,type = 'prob')$X1,
  rf = predict(model_rf,newdata = X_outsample,type = 'prob')$X1,
  gbm = predict(model_gbm,newdata = X_outsample,type = 'prob')$X1,
  xgb = predict(model_xgb,newdata = X_outsample,type = 'prob')$X1
)
fwrite(test_data_prediction,file='output/outsample_data_predictions.csv')

outsample_data_cutoff_distribution = sapply(c(0:100)/100, get_CM)
outsample_data_cutoff_distribution = data.table(cutoff = c(0:100)/100,t(outsample_data_cutoff_distribution))
fwrite(outsample_data_cutoff_distribution,file='output/outsample_cutoff_distribution.csv')
outsample_data_cutoff_distribution[,c('dt_accuracy','dt_precision','dt_recall'):=
                                     .((dt_a0p0+dt_a1p1)/(dt_a0p0+dt_a0p1+dt_a1p0+dt_a1p1),
                                       dt_a1p1/(dt_a0p1+dt_a1p1),
                                       dt_a1p1/(dt_a1p0+dt_a1p1))]
