rm(list=ls())
cat('\014')
library(data.table)
library(Information)
library(caret)

dat_for_model = readRDS('data/raw_train.RDS')
outsample_dat = readRDS('data/raw_outsample.RDS')
###############################################################################
#                  Check Importance of levels and variables                   #
###############################################################################
dat_for_model[,target:=as.integer(target)]
outsample_dat[,target:=as.integer(target)]
index=createDataPartition(dat_for_model$target,p=0.8,list=F)
train = dat_for_model[index,]
test = dat_for_model[-index]
info_total = create_infotables(data=dat_for_model,y='target')
infotrain = create_infotables(data = train,y='target')
infovalidate = create_infotables(data = train,valid = test,y='target')
summary_tot = setDT(info_total$Summary)
summary_train = setDT(infotrain$Summary)
summary_validate = setDT(infovalidate$Summary)
iv_table=summary_tot[summary_train,
                 .(Variable,info_tot=x.IV,info_train=i.IV),
                 on='Variable'][summary_validate,
                                .(Variable,info_tot,info_train,info_test=i.IV),
                                on='Variable']

#####--------------------------Location-----------------------------------#####
# loc_info = setDT(info_total$Tables$Location)
# fwrite(loc_info[dat_for_model[,.(perc_pos=mean(target)),by=Location],on='Location'],
#        file='data/variable_transforms/Location.csv')
loc_info = fread('data/variable_transforms/Location.csv')
dat_for_model[,Location:=
           loc_info$Location_alt[match(dat_for_model$Location,
                                       loc_info$Location)]]
outsample_dat[,Location:=
                loc_info$Location_alt[match(outsample_dat$Location,
                                            loc_info$Location)]]

# train = dat_for_model[index,]
# test = dat_for_model[-index]
# summary_tot = setDT(create_infotables(data=dat_for_model,y='target')$Summary)
# summary_train = setDT(create_infotables(data = train,y='target')$Summary)
# summary_validate = setDT(create_infotables(data = train,valid = test,y='target')$Summary)
# View(summary_tot[summary_train,
#             .(Variable,info_tot=x.IV,info_train=i.IV),
#             on='Variable'][summary_validate,
#                            .(Variable,info_tot,info_train,info_test=i.IV),
#                            on='Variable'])
#####-----------------------------State-----------------------------------#####

#state_info = setDT(infotable$Tables$State)
# fwrite(state_info[dat_for_model[,.(perc_pos=mean(target)),by=State],on='State'],
#         file='data/variable_transforms/State.csv')
state_info = fread('data/variable_transforms/State.csv')
dat_for_model[,State:=
           state_info$State_alt[match(dat_for_model$State,
                                      state_info$State)]]
outsample_dat[,State:=
                state_info$State_alt[match(outsample_dat$State,
                                           state_info$State)]]

# train = dat_for_model[index,]
# test = dat_for_model[-index]
# summary_tot = setDT(create_infotables(data=dat_for_model,y='target')$Summary)
# summary_train = setDT(create_infotables(data = train,y='target')$Summary)
# summary_validate = setDT(create_infotables(data = train,valid = test,y='target')$Summary)
# View(summary_tot[summary_train,
#                  .(Variable,info_tot=x.IV,info_train=i.IV),
#                  on='Variable'][summary_validate,
#                                 .(Variable,info_tot,info_train,info_test=i.IV),
#                                 on='Variable'])
# cat('\014')
#####--------------------------LoanPurpose--------------------------------#####
#state_info = setDT(infotable$Tables$LoanPurpose)
# fwrite(state_info[dat_for_model[,.(perc_pos=mean(target)),by=LoanPurpose],on='LoanPurpose'],
#         file='data/variable_transforms/LoanPurpose.csv')
lp_info = fread('data/variable_transforms/LoanPurpose.csv')
dat_for_model[,LoanPurpose:=
                lp_info$LoanPurpose_alt[match(dat_for_model$LoanPurpose,
                                              lp_info$LoanPurpose)]]
outsample_dat[,LoanPurpose:=
                lp_info$LoanPurpose_alt[match(outsample_dat$LoanPurpose,
                                              lp_info$LoanPurpose)]]
# train = dat_for_model[index,]
# test = dat_for_model[-index]
# summary_tot = setDT(create_infotables(data=dat_for_model,y='target')$Summary)
# summary_train = setDT(create_infotables(data = train,y='target')$Summary)
# summary_validate = setDT(create_infotables(data = train,valid = test,y='target')$Summary)
# View(summary_tot[summary_train,
#                  .(Variable,info_tot=x.IV,info_train=i.IV),
#                  on='Variable'][summary_validate,
#                                 .(Variable,info_tot,info_train,info_test=i.IV),
#                                 on='Variable'])
# cat('\014')
# 
#####--------------------------BorrowerDesc-------------------------------#####

bd_info = fread('data/variable_transforms/BorrowerDesc.csv')
dat_for_model[,BorrowerDesc:=
                bd_info$BorrowerDesc_alt[match(dat_for_model$BorrowerDesc,
                                               bd_info$BorrowerDesc)]]
outsample_dat[,BorrowerDesc:=
                bd_info$BorrowerDesc_alt[match(outsample_dat$BorrowerDesc,
                                               bd_info$BorrowerDesc)]]
# train = dat_for_model[index,]
# test = dat_for_model[-index]
# summary_tot = setDT(create_infotables(data=dat_for_model,y='target')$Summary)
# summary_train = setDT(create_infotables(data = train,y='target')$Summary)
# summary_validate = setDT(create_infotables(data = train,valid = test,y='target')$Summary)
# View(summary_tot[summary_train,
#                  .(Variable,info_tot=x.IV,info_train=i.IV),
#                  on='Variable'][summary_validate,
#                                 .(Variable,info_tot,info_train,info_test=i.IV),
#                                 on='Variable'])
# cat('\014')
# 
#####--------------------------BorrowerType-------------------------------#####
# typ_info = setDT(infotable$Tables$BorrowerType)
# fwrite(typ_info[dat_for_model[,.(perc_pos=mean(target)),BorrowerType],
#                     on='BorrowerType'],
#        file='data/variable_transforms/BorrowerType.csv')
typ_info = fread('data/variable_transforms/BorrowerType.csv')
dat_for_model[,BorrowerType:=
           typ_info$BorrowerType_alt[match(dat_for_model$BorrowerType,
                                           typ_info$BorrowerType)]]
outsample_dat[,BorrowerType:=
                typ_info$BorrowerType_alt[match(outsample_dat$BorrowerType,
                                                typ_info$BorrowerType)]]

#####--------------------------ManufacturerDesc---------------------------#####
manu_table = fread('data/variable_transforms/ManufacturerDesc.csv')

dat_for_model[,ManufacturerDesc:=
           manu_table$Bins[match(dat_for_model$ManufacturerDesc,
                                 manu_table$ManufacturerDesc)]]
outsample_dat[,ManufacturerDesc:=
                manu_table$Bins[match(outsample_dat$ManufacturerDesc,
                                      manu_table$ManufacturerDesc)]]

#####--------------------------VehicleCategory----------------------------#####
# veh_info = setDT(infotable$Tables$VehicleCategory)
# fwrite(veh_info[dat_for_model[,.(perc_pos=mean(target)),VehicleCategory],
#                     on='VehicleCategory'],
#        file='data/variable_transforms/VehicleCategory.csv')
typ_info = fread('data/variable_transforms/VehicleCategory.csv')
dat_for_model[,VehicleCategory:=typ_info$VehicleCategory_alt[
  match(dat_for_model$VehicleCategory,typ_info$VehicleCategory)]]
outsample_dat[,VehicleCategory:=typ_info$VehicleCategory_alt[
  match(outsample_dat$VehicleCategory,typ_info$VehicleCategory)]]

#####--------------------------AssetLevel---------------------------------#####

#####--------------------------BorrowerCategory---------------------------#####

# bcat_info = setDT(infotable$Tables$BorrowerCategory)
# fwrite(bcat_info[dat_for_model[,.(perc_pos=mean(target)),BorrowerCategory],
#                     on='BorrowerCategory'],
#        file='data/variable_transforms/BorrowerCategory.csv')
typ_info = fread('data/variable_transforms/BorrowerCategory.csv')
dat_for_model[,BorrowerCategory:=typ_info$BorrowerCategory_alt[
  match(dat_for_model$BorrowerCategory,typ_info$BorrowerCategory)]]

outsample_dat[,BorrowerCategory:=typ_info$BorrowerCategory_alt[
  match(outsample_dat$BorrowerCategory,typ_info$BorrowerCategory)]]


dat_for_model[,AssetCost:=scale(AssetCost)]
dat_for_model[,POS:=predict(BoxCoxTrans(POS),POS)]
saveRDS(dat_for_model,file='data/dat_for_model.RDS')



outsample_dat[,AssetCost:=scale(AssetCost)]
outsample_dat[,POS:=predict(BoxCoxTrans(POS),POS)]
saveRDS(outsample_dat,file='data/outsample_data.RDS')
infotable = create_infotables(dat_for_model,y='target')