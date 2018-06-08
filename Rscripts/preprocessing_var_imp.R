rm(list=ls())
cat('\014')
library(data.table)
library(Information)
library(caret)

finaldat = readRDS('data/finaldatajan17.RDS')
###############################################################################
#                  Check Importance of levels and variables                   #
###############################################################################
finaldat[,target:=as.integer(target)]
index=createDataPartition(finaldat$target,p=0.8,list=F)
train = finaldat[index,]
test = finaldat[-index]
info_total = create_infotables(data=finaldat,y='target')
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
# fwrite(loc_info[finaldat[,.(perc_pos=mean(target)),by=Location],on='Location'],
#        file='data/variable_transforms/Location.csv')
loc_info = fread('data/variable_transforms/Location.csv')
finaldat[,Location:=
           loc_info$Location_alt[match(finaldat$Location,
                                       loc_info$Location)]]
# train = finaldat[index,]
# test = finaldat[-index]
# summary_tot = setDT(create_infotables(data=finaldat,y='target')$Summary)
# summary_train = setDT(create_infotables(data = train,y='target')$Summary)
# summary_validate = setDT(create_infotables(data = train,valid = test,y='target')$Summary)
# View(summary_tot[summary_train,
#             .(Variable,info_tot=x.IV,info_train=i.IV),
#             on='Variable'][summary_validate,
#                            .(Variable,info_tot,info_train,info_test=i.IV),
#                            on='Variable'])
#####-----------------------------State-----------------------------------#####

#state_info = setDT(infotable$Tables$State)
# fwrite(state_info[finaldat[,.(perc_pos=mean(target)),by=State],on='State'],
#         file='data/variable_transforms/State.csv')
state_info = fread('data/variable_transforms/State.csv')
finaldat[,State:=
           state_info$State_alt[match(finaldat$State,
                                      state_info$State)]]
# train = finaldat[index,]
# test = finaldat[-index]
# summary_tot = setDT(create_infotables(data=finaldat,y='target')$Summary)
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
# fwrite(state_info[finaldat[,.(perc_pos=mean(target)),by=LoanPurpose],on='LoanPurpose'],
#         file='data/variable_transforms/LoanPurpose.csv')
lp_info = fread('data/variable_transforms/LoanPurpose.csv')
finaldat[,LoanPurpose:=
           lp_info$LoanPurpose_alt[match(finaldat$LoanPurpose,
                                      lp_info$LoanPurpose)]]
# train = finaldat[index,]
# test = finaldat[-index]
# summary_tot = setDT(create_infotables(data=finaldat,y='target')$Summary)
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
finaldat[,BorrowerDesc:=
           bd_info$BorrowerDesc_alt[match(finaldat$BorrowerDesc,
                                         bd_info$BorrowerDesc)]]
# train = finaldat[index,]
# test = finaldat[-index]
# summary_tot = setDT(create_infotables(data=finaldat,y='target')$Summary)
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
# fwrite(typ_info[finaldat[,.(perc_pos=mean(target)),BorrowerType],
#                     on='BorrowerType'],
#        file='data/variable_transforms/BorrowerType.csv')
typ_info = fread('data/variable_transforms/BorrowerType.csv')
finaldat[,BorrowerType:=
           typ_info$BorrowerType_alt[match(finaldat$BorrowerType,
                                           typ_info$BorrowerType)]]

#####--------------------------ManufacturerDesc---------------------------#####
manu_table = fread('data/variable_transforms/ManufacturerDesc.csv')

finaldat[,ManufacturerDesc:=
           manu_table$Bins[match(finaldat$ManufacturerDesc,
                                 manu_table$ManufacturerDesc)]]

#####--------------------------VehicleCategory----------------------------#####
# veh_info = setDT(infotable$Tables$VehicleCategory)
# fwrite(veh_info[finaldat[,.(perc_pos=mean(target)),VehicleCategory],
#                     on='VehicleCategory'],
#        file='data/variable_transforms/VehicleCategory.csv')
typ_info = fread('data/variable_transforms/VehicleCategory.csv')
finaldat[,VehicleCategory:=typ_info$VehicleCategory_alt[
  match(finaldat$VehicleCategory,typ_info$VehicleCategory)]]
#####--------------------------AssetLevel---------------------------------#####

#####--------------------------BorrowerCategory---------------------------#####

# bcat_info = setDT(infotable$Tables$BorrowerCategory)
# fwrite(bcat_info[finaldat[,.(perc_pos=mean(target)),BorrowerCategory],
#                     on='BorrowerCategory'],
#        file='data/variable_transforms/BorrowerCategory.csv')
typ_info = fread('data/variable_transforms/BorrowerCategory.csv')
finaldat[,BorrowerCategory:=typ_info$BorrowerCategory_alt[
  match(finaldat$BorrowerCategory,typ_info$BorrowerCategory)]]


finaldat[,AssetCost:=scale(AssetCost)]
finaldat[,POS:=predict(BoxCoxTrans(POS),POS)]
saveRDS(finaldat,file='data/finaldatajan17.RDS')





infotable = create_infotables(finaldat,y='target')