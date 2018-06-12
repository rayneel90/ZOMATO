rm(list=ls())
cat('\014')
library(lubridate)
library(data.table)
library(Information)
library(caret)
master = readRDS('data/cleaned_master_data.RDS')  # Read the master data table
colnames(master)=c('ApplID','AggrNo','CIF','Location','State','OldNewFlag',
                   'Product','LoanPurpose','BorrowerDesc','BorrowerType',
                   'DisbDt','DueDate',"DisbAmt",'ROI','ManufacturerDesc',
                   'VehicleCategory','AssetCost','AssetLevel','WriteOffFlag',
                   'POS','DPDDays','AssetClassification','NPASinceDate',
                   'AsOf','BounceFlag','ClosedFlag','RestructureFlag',
                   'DqBucket','ever60Flag','ever30Flag','everXFlag',
                   'BounceCount','QuickMortalityFlag','PresentationFlag')
###############################################################################
#                      Set the time Period                                    #
###############################################################################

#TARGETDT=as.Date('2017-01-01')   #train
TARGETDT=as.Date('2017-07-01')   # outsample test
snapshotjan17 =  master[AsOf==TARGETDT]
###############################################################################
#                                 Filters                                     #
###############################################################################

# 1. Not Written off
# 2. Asset is not classified as stressed or worse
# 3. Account is not closed
# 4. Principal OutStanding is more than 0
# 5. Had at least 3 presentations 

finaldat = snapshotjan17[!WriteOffFlag &
                              AssetClassification=="" &
                              !ClosedFlag & 
                              POS>0 & 
                              DisbDt< TARGETDT-months(3)]

# filter on accounts which were delinquincy level 0 or 1 at the beginning of window  

finaldat = finaldat[DqBucket<=1]


###############################################################################
#                           Feature Engineering                               #
###############################################################################

finaldat[,AgriFlag:=Product=='COMMERCIAL VEHICLE - AIB']  # Agri/Retail flag added

promotiondesc = fread('data/variable_transforms/promotion_map.csv')
finaldat[,BorrowerCategory:=promotiondesc$Category[match(
  finaldat$BorrowerDesc, promotiondesc$PROMOTIONDESC)]]
finaldat[,BorrowerDesc:=promotiondesc$New_Value[match(
  finaldat$BorrowerDesc,promotiondesc$PROMOTIONDESC)]]  # Convert PROMOTIONDESC and add category using the promotiondesc lookup table



# Create cash/cheque flag
finaldat[,CashPayFlag:=is.na(DueDate)]
# add column Month since disbursal
finaldat[,MOB:=12*(year(AsOf)-year(DisbDt))+(month(AsOf)-month(DisbDt))]

# Calculate no of bounces in last 6 and 3 month and lifetime
bounce6m=master[TARGETDT-months(6)<=AsOf & AsOf<=TARGETDT,.(bounce6m=max(BounceCount)-min(BounceCount)),ApplID]
present6m=master[TARGETDT-months(6)< AsOf & AsOf <=TARGETDT,.(present6m=sum(PresentationFlag)),ApplID]
bounce3m=master[TARGETDT-months(3)<=AsOf & AsOf <=TARGETDT,.(bounce3m=max(BounceCount)-min(BounceCount)),ApplID]
present3m=master[TARGETDT-months(3)<AsOf & AsOf <=TARGETDT,.(present3m=sum(PresentationFlag)),ApplID]
br6m=bounce6m[present6m,.(ApplID,BR6m=bounce6m/present6m),
               on='ApplID']
br3m=bounce3m[present3m,.(ApplID,BR3m=bounce3m/present3m),
               on='ApplID']
finaldat = merge(finaldat,br6m,on='ApplID',all.x = TRUE)
finaldat = merge(finaldat,br3m,on='ApplID',all.x = TRUE)
finaldat[,BRLife:=BounceCount/MOB]

# Add columns for delinquincy of next 9 months
dqcolnames=c()
for (i in 1:9)
{
  dt = TARGETDT + months(i)
  temp = master[AsOf == dt] 
  set(finaldat,j=paste0('dq',strftime(dt,format = '%b%Y')),
      value = temp$DqBucket[match(finaldat$ApplID,temp$ApplID)])
  dqcolnames = c(dqcolnames,paste0('dq',strftime(dt,format = '%b%Y')))
}

# Create target variable: 1 if in last 6 month the acc has reached dq 3 or 
# more, 0 ow. 
finaldat[,temp:=do.call(pmax,c(.SD,list(na.rm=TRUE))),.SDcols = dqcolnames,
          by=ApplID]
finaldat[temp=='',temp:='0']
finaldat[,target:=as.numeric(temp)>=3]
finaldat[,temp:=NULL]
finaldat[,(dqcolnames):=NULL]
finaldat[,.N,target]
prop.table(table(finaldat[,target]))
prop.table(table(finaldat[DqBucket==0,target]))



###############################################################################
#                       Remove Unnecessary Columns                            #
###############################################################################

finaldat[ ,c('ApplID','WriteOffFlag','Product','AssetClassification','ClosedFlag',
             'OldNewFlag','NPASinceDate','RestructureFlag','QuickMortalityFlag',
             'NonstarterFlag','DisbDt','AsOf','PresentationFlag','DueDate','BounceCount'):=NULL]





###############################################################################
#                       Convert Characters to Factor                          #
###############################################################################

factor_cols = c('Location', 'State', 'LoanPurpose', 'BorrowerDesc', 
                'BorrowerType', 'ManufacturerDesc', 'VehicleCategory',
                'AssetLevel', 'DPDDays', 'BounceFlag', 'DqBucket', 'ever60Flag',
                'ever30Flag', 'everXFlag', 'AgriFlag', 'BorrowerCategory','CashPayFlag')
finaldat[,(factor_cols):=lapply(.SD,as.factor),.SDcols=factor_cols]

#saveRDS(finaldat,file='data/raw_train.RDS')
saveRDS(finaldat,file='data/raw_outsample.RDS')

###############################################################################
#                      CIBIL data processing (Not Yet Complete)               #
###############################################################################

# cifmap = fread('data/CIFJan17.csv',colClasses = 'character')
# colnames(cifmap)=gsub(' ','',colnames(cifmap))
# cibiljan = fread('data/cibil jan 17.csv',colClasses = 'character')
# colnames(cibiljan)=gsub(' ','',colnames(cibiljan))
# cibiljan = cifmap[cibiljan,on="AGREEMENTNO==MemberReference"]
# temp=cibiljan[finaldat,.(AGREEMENTNO,finalcifno,CONSTITUTION,DISBDT
#                            ,EnquiryControlNumber),on="CIFID==finalcifno"]
# 
# changeCols = colnames(cibiljan)[grep('maxdq',colnames(cibiljan))]
# cibiljan[,(changeCols):=lapply(.SD,as.numeric),.SDcols=changeCols]
# cibiljan[,maxdqdecreasing:=apply(cibiljan[,grep('maxdq',colnames(cibiljan)),with=FALSE],1,function(x){return(all(diff(x)<0))})]
# 
