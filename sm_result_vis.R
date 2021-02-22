## script to extract, interpret, and visualise results from county housing stock model
library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Yale Courses/Research/Final Paper/HSM_github/")
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
# load in county level results
load("HSM_results/County_Scenario_SM_Results.RData") # load in stock model results

smop_base<-as.data.frame(smop_base)
smop_base$PopGrowthRel<-1
smop_base$PopGrowthAbs<-0
smop_base$SGrowthRel<-1
smop_base$SGrowthAbs<-0
smop_base$VFGrowthRel<-1
smop_base$VFGrowthAbs<-0
smop_base$MFGrowthAbs<-0
smop_base$DemTot<-0
# calculate relative and absolute changes in population, stock, vacancies, and demolition by county
for (r in 1:3142) {
  smop_base$PopGrowthRel[r]<-smop_base[[3]][[r]]$Population[41]/smop_base[[3]][[r]]$Population[1]
  smop_base$PopGrowthAbs[r]<-smop_base[[3]][[r]]$Population[41]-smop_base[[3]][[r]]$Population[1]
  smop_base$SGrowthRel[r]<-smop_base[[3]][[r]]$Tot_Hous_Units[41]/smop_base[[3]][[r]]$Tot_Hous_Units[1]
  smop_base$SGrowthAbs[r]<-smop_base[[3]][[r]]$Tot_Hous_Units[41]-smop_base[[3]][[r]]$Tot_Hous_Units[1]
  smop_base$VFGrowthRel[r]<-smop_base[[3]][[r]]$Vacancy_Ratio[41]/smop_base[[3]][[r]]$Vacancy_Ratio[1]
  smop_base$VFGrowthAbs[r]<-smop_base[[3]][[r]]$Vacancy_Ratio[41]-smop_base[[3]][[r]]$Vacancy_Ratio[1]
  smop_base$MFGrowthAbs[r]<-smop_base[[3]][[r]]$Tot_HU_MF[41]-smop_base[[3]][[r]]$Tot_HU_MF[1]
  smop_base$DemTot[r]<-sum(smop_base[[3]][[r]][,26:28])
}
summ_base<-smop_base[,-3]    # summary of stock changes, base scenario
# identify counties with strong and sustained population decline
sbs<-summ_base[order(summ_base$PopGrowthRel),][1:35,] # summ base sort
# remove rows with del population absolute value less than 10,000
sbs<-sbs[-c(which(sbs$PopGrowthAbs>-10000)),] # San Juan NM, and McDowell County WV stand out.
pop_dec<-sbs

# identify counties with strong population growth
sbs<-summ_base[order(-summ_base$PopGrowthRel),][1:35,] # summ base sort
# remove rows with del population absolute value less than 50,000
sbs<-sbs[-c(which(sbs$PopGrowthAbs<50000)),] # several ND counties (McKenzie, Williams), TX counties (Hays(b.w. Austin and San Antonio), Fort Bend (suburb of Houston)), and some counties in Utah stand out
pop_inc<-sbs

# identify counties with moderate population decline
sbs<-summ_base[summ_base$PopGrowthRel>0.93&summ_base$PopGrowthRel<0.95,]
# remove rows with del population absolute value less than 2,000
sbs<-sbs[-c(which(sbs$PopGrowthAbs>-2000)),]
modpop_dec<-sbs

# identify counties with moderate population growth
sbs<-summ_base[summ_base$PopGrowthRel>1.05&summ_base$PopGrowthRel<1.07,]
# remove rows with del population absolute value less than 5,000
sbs<-sbs[-c(which(sbs$PopGrowthAbs<5000)),]
modpop_inc<-sbs

# now some rankings to compare between scenarios
hiSGrowth_base<-summ_base[order(-summ_base$SGrowthAbs),][1:35,]
# where is reduction in vacancy rate most apparent/needed?
hiSGrowth_base<-hiSGrowth_base[order(-hiSGrowth_base$VFGrowthAbs),] # Austin TX (Travis County),Atlanta GA suburbs, DC, Minneapolis, FWD TX suburbs, San Antonio, Dallas, Houston TX, Charlotte NC, Bronx, Boston,  Denver,  Oakland (SF suburbs), Seattle, San Jose CA

# where is increase in vacancy rate most acute? This will pick out declining counties
hiVFGrowth_base<-summ_base[order(-summ_base$VFGrowthAbs),][1:35,]
# remove rows with del population absolute value less than 5,000
hiVFGrowth_base<-hiVFGrowth_base[-c(which(hiVFGrowth_base$PopGrowthAbs>-5000)),] # San Juan NM, Moffat CO, Aroostook Maine, Cambria PA

# Where is growth in MF most? ALL of these see their vacancy rate reduced. Especially Bronx, Denver, Seattle
hiMFGrowth_base<-summ_base[order(-summ_base$MFGrowthAbs),][1:35,] # Houston TX, Seattle, Manhattan, LA, Phoenix, Brooklyn, Dallas, Austin, Bronx, SF, Miami, DC, Denver

# where is demolition greatest? will pick out places which currently have larege (and old) housing stocks, and more likely to be in the large Southern counties which have higher loss rates
hiDem_base<-summ_base[order(-summ_base$DemTot),][1:35,] # big cities: LA, Houston, Chicago, Phoenix, Miami, Dallas, SD, Fort Lauderdale, NY city, Fort Worth

smop_hiDR<-as.data.frame(smop_hiDR)
smop_hiDR$PopGrowthRel<-1
smop_hiDR$PopGrowthAbs<-0
smop_hiDR$SGrowthRel<-1
smop_hiDR$SGrowthAbs<-0
smop_hiDR$VFGrowthRel<-1
smop_hiDR$VFGrowthAbs<-0
smop_hiDR$MFGrowthAbs<-0
smop_hiDR$DemTot<-0

for (r in 1:3142) {
  smop_hiDR$PopGrowthRel[r]<-smop_hiDR[[3]][[r]]$Population[41]/smop_hiDR[[3]][[r]]$Population[1]
  smop_hiDR$PopGrowthAbs[r]<-smop_hiDR[[3]][[r]]$Population[41]-smop_hiDR[[3]][[r]]$Population[1]
  smop_hiDR$SGrowthRel[r]<-smop_hiDR[[3]][[r]]$Tot_Hous_Units[41]/smop_hiDR[[3]][[r]]$Tot_Hous_Units[1]
  smop_hiDR$SGrowthAbs[r]<-smop_hiDR[[3]][[r]]$Tot_Hous_Units[41]-smop_hiDR[[3]][[r]]$Tot_Hous_Units[1]
  smop_hiDR$VFGrowthRel[r]<-smop_hiDR[[3]][[r]]$Vacancy_Ratio[41]/smop_hiDR[[3]][[r]]$Vacancy_Ratio[1]
  smop_hiDR$VFGrowthAbs[r]<-smop_hiDR[[3]][[r]]$Vacancy_Ratio[41]-smop_hiDR[[3]][[r]]$Vacancy_Ratio[1]
  smop_hiDR$MFGrowthAbs[r]<-smop_hiDR[[3]][[r]]$Tot_HU_MF[41]-smop_hiDR[[3]][[r]]$Tot_HU_MF[1]
  smop_hiDR$DemTot[r]<-sum(smop_hiDR[[3]][[r]][,26:28])
}
summ_hiDR<-smop_hiDR[,-3]    # summary of stock changes, hiDR scenario

# # now some rankings to compare between scenarios, can be ommitted other than Baseline
# hiSGrowth_hiDR<-summ_hiDR[order(-summ_hiDR$SGrowthAbs),][1:35,]
# # where is reduction in vacancy rate most apparent/needed?
# hiSGrowth_hiDR<-hiSGrowth_hiDR[order(-hiSGrowth_hiDR$VFGrowthAbs),] # Austin TX (Travis County),Atlanta GA suburbs, DC, Minneapolis, FWD TX suburbs, San Antonio, Dallas, Houston TX, Charlotte NC, Bronx, Boston,  Denver,  Oakland (SF suburbs), Seattle, San Jose CA
# 
# # where is increase in vacancy rate most acute? This will pick out declining counties
# hiVFGrowth_hiDR<-summ_hiDR[order(-summ_hiDR$VFGrowthAbs),][1:35,]
# # remove rows with del population absolute value less than 5,000
# hiVFGrowth_hiDR<-hiVFGrowth_hiDR[-c(which(hiVFGrowth_hiDR$PopGrowthAbs>-5000)),] # San Juan NM, Moffat CO, Aroostook Maine, Cambria PA
# 
# # Where is growth in MF most? ALL of these see their vacancy rate reduced. Especially Bronx, Denver, Seattle
# hiMFGrowth_hiDR<-summ_hiDR[order(-summ_hiDR$MFGrowthAbs),][1:35,] # Houston TX, Seattle, Manhattan, LA, Phoenix, Brooklyn, Dallas, Austin, Bronx, SF, Miami, DC, Denver
# 
# # where is demolition greatest? will pick out places which currently have larege (and old) housing stocks.
# hiDem_hiDR<-summ_hiDR[order(-summ_hiDR$DemTot),][1:35,] # big cities: LA, Houston, Chicago, Phoenix, Miami, Dallas, SD, Fort Lauderdale, NY city, Fort Worth

smop_hiMF<-as.data.frame(smop_hiMF)
smop_hiMF$PopGrowthRel<-1
smop_hiMF$PopGrowthAbs<-0
smop_hiMF$SGrowthRel<-1
smop_hiMF$SGrowthAbs<-0
smop_hiMF$VFGrowthRel<-1
smop_hiMF$VFGrowthAbs<-0
smop_hiMF$MFGrowthAbs<-0
smop_hiMF$DemTot<-0

for (r in 1:3142) {
  smop_hiMF$PopGrowthRel[r]<-smop_hiMF[[3]][[r]]$Population[41]/smop_hiMF[[3]][[r]]$Population[1]
  smop_hiMF$PopGrowthAbs[r]<-smop_hiMF[[3]][[r]]$Population[41]-smop_hiMF[[3]][[r]]$Population[1]
  smop_hiMF$SGrowthRel[r]<-smop_hiMF[[3]][[r]]$Tot_Hous_Units[41]/smop_hiMF[[3]][[r]]$Tot_Hous_Units[1]
  smop_hiMF$SGrowthAbs[r]<-smop_hiMF[[3]][[r]]$Tot_Hous_Units[41]-smop_hiMF[[3]][[r]]$Tot_Hous_Units[1]
  smop_hiMF$VFGrowthRel[r]<-smop_hiMF[[3]][[r]]$Vacancy_Ratio[41]/smop_hiMF[[3]][[r]]$Vacancy_Ratio[1]
  smop_hiMF$VFGrowthAbs[r]<-smop_hiMF[[3]][[r]]$Vacancy_Ratio[41]-smop_hiMF[[3]][[r]]$Vacancy_Ratio[1]
  smop_hiMF$MFGrowthAbs[r]<-smop_hiMF[[3]][[r]]$Tot_HU_MF[41]-smop_hiMF[[3]][[r]]$Tot_HU_MF[1]
  smop_hiMF$DemTot[r]<-sum(smop_hiMF[[3]][[r]][,26:28])
}
summ_hiMF<-smop_hiMF[,-3]    # summary of stock changes, hiMF scenario

# # now some rankings to compare between scenarios, can be ommitted other than Baseline
# hiSGrowth_hiMF<-summ_hiMF[order(-summ_hiMF$SGrowthAbs),][1:35,]
# # where is reduction in vacancy rate most apparent/needed?
# hiSGrowth_hiMF<-hiSGrowth_hiMF[order(-hiSGrowth_hiMF$VFGrowthAbs),] # Austin TX (Travis County),Atlanta GA suburbs, DC, Minneapolis, FWD TX suburbs, San Antonio, Dallas, Houston TX, Charlotte NC, Bronx, Boston,  Denver,  Oakland (SF suburbs), Seattle, San Jose CA
# 
# # where is increase in vacancy rate most acute? This will pick out declining counties
# hiVFGrowth_hiMF<-summ_hiMF[order(-summ_hiMF$VFGrowthAbs),][1:35,]
# # remove rows with del population absolute value less than 5,000
# hiVFGrowth_hiMF<-hiVFGrowth_hiMF[-c(which(hiVFGrowth_hiMF$PopGrowthAbs>-5000)),] # San Juan NM, Moffat CO, Aroostook Maine, Cambria PA
# 
# # Where is growth in MF most? ALL of these see their vacancy rate reduced. Especially Bronx, Denver, Seattle
# hiMFGrowth_hiMF<-summ_hiMF[order(-summ_hiMF$MFGrowthAbs),][1:35,] # Houston TX, Seattle, Manhattan, LA, Phoenix, Brooklyn, Dallas, Austin, Bronx, SF, Miami, DC, Denver
# 
# # where is demolition greatest? will pick out places which currently have larege (and old) housing stocks.
# hiDem_hiMF<-summ_hiMF[order(-summ_hiMF$DemTot),][1:35,] # big cities: LA, Houston, Chicago, Phoenix, Miami, Dallas, SD, Fort Lauderdale, NY city, Fort Worth

smop_hiDRMF<-as.data.frame(smop_hiDRMF)
smop_hiDRMF$PopGrowthRel<-1
smop_hiDRMF$PopGrowthAbs<-0
smop_hiDRMF$SGrowthRel<-1
smop_hiDRMF$SGrowthAbs<-0
smop_hiDRMF$VFGrowthRel<-1
smop_hiDRMF$VFGrowthAbs<-0
smop_hiDRMF$MFGrowthAbs<-0
smop_hiDRMF$DemTot<-0

for (r in 1:3142) {
  smop_hiDRMF$PopGrowthRel[r]<-smop_hiDRMF[[3]][[r]]$Population[41]/smop_hiDRMF[[3]][[r]]$Population[1]
  smop_hiDRMF$PopGrowthAbs[r]<-smop_hiDRMF[[3]][[r]]$Population[41]-smop_hiDRMF[[3]][[r]]$Population[1]
  smop_hiDRMF$SGrowthRel[r]<-smop_hiDRMF[[3]][[r]]$Tot_Hous_Units[41]/smop_hiDRMF[[3]][[r]]$Tot_Hous_Units[1]
  smop_hiDRMF$SGrowthAbs[r]<-smop_hiDRMF[[3]][[r]]$Tot_Hous_Units[41]-smop_hiDRMF[[3]][[r]]$Tot_Hous_Units[1]
  smop_hiDRMF$VFGrowthRel[r]<-smop_hiDRMF[[3]][[r]]$Vacancy_Ratio[41]/smop_hiDRMF[[3]][[r]]$Vacancy_Ratio[1]
  smop_hiDRMF$VFGrowthAbs[r]<-smop_hiDRMF[[3]][[r]]$Vacancy_Ratio[41]-smop_hiDRMF[[3]][[r]]$Vacancy_Ratio[1]
  smop_hiDRMF$MFGrowthAbs[r]<-smop_hiDRMF[[3]][[r]]$Tot_HU_MF[41]-smop_hiDRMF[[3]][[r]]$Tot_HU_MF[1]
  smop_hiDRMF$DemTot[r]<-sum(smop_hiDRMF[[3]][[r]][,26:28])
}
summ_hiDRMF<-smop_hiDRMF[,-3]    # summary of stock changes, hiDRMF scenario

# # now some rankings to compare between scenarios, can be ommitted other than Baseline
# hiSGrowth_hiDRMF<-summ_hiDRMF[order(-summ_hiDRMF$SGrowthAbs),][1:35,]
# # where is reduction in vacancy rate most apparent/needed?
# hiSGrowth_hiDRMF<-hiSGrowth_hiDRMF[order(-hiSGrowth_hiDRMF$VFGrowthAbs),] # Austin TX (Travis County),Atlanta GA suburbs, DC, Minneapolis, FWD TX suburbs, San Antonio, Dallas, Houston TX, Charlotte NC, Bronx, Boston,  Denver,  Oakland (SF suburbs), Seattle, San Jose CA
# 
# # where is increase in vacancy rate most acute? This will pick out declining counties
# hiVFGrowth_hiDRMF<-summ_hiDRMF[order(-summ_hiDRMF$VFGrowthAbs),][1:35,]
# # remove rows with del population absolute value less than 5,000
# hiVFGrowth_hiDRMF<-hiVFGrowth_hiDRMF[-c(which(hiVFGrowth_hiDRMF$PopGrowthAbs>-5000)),] # San Juan NM, Moffat CO, Aroostook Maine, Cambria PA
# 
# # Where is growth in MF most? ALL of these see their vacancy rate reduced. Especially Bronx, Denver, Seattle
# hiMFGrowth_hiDRMF<-summ_hiDRMF[order(-summ_hiDRMF$MFGrowthAbs),][1:35,] # Houston TX, Seattle, Manhattan, LA, Phoenix, Brooklyn, Dallas, Austin, Bronx, SF, Miami, DC, Denver
# 
# # where is demolition greatest? will pick out places which currently have larege (and old) housing stocks.
# hiDem_hiDRMF<-summ_hiDRMF[order(-summ_hiDRMF$DemTot),][1:35,] # big cities: LA, Houston, Chicago, Phoenix, Miami, Dallas, SD, Fort Lauderdale, NY city, Fort Worth


# df<-as.data.frame(smop_base[[3]][[1]][,c(1:31,110:169)]) # example data frame for first county. not needed?
## load in intial housing stock data for 2020
load("Intermediate_results/InitStock20.RData")
st_reg<-read.csv('Data/state-geocodes-v2017.csv')
# sr<-st_reg[,c(1,3)] # get just state and region codes
h20pc$StateCode<-as.numeric(substr(h20pc$GeoID,1,nchar(h20pc$GeoID)-3))
h20pc<-merge(h20pc,st_reg,by.x = "StateCode",by.y = "State_FIPS")
codes<-h20pc[,c(2,3,1,77,78)]
codes<-rbind(codes,c(0,"USA",0,0,0))
codes$CensusRegion<-"USA"
codes[codes$Region==1,]$CensusRegion<-"Northeast Cen Region"
codes[codes$Region==2,]$CensusRegion<-"Midwest Cen Region"
codes[codes$Region==3,]$CensusRegion<-"South Cen Region"
codes[codes$Region==4,]$CensusRegion<-"West Cen Region"

stcd<-read.delim('Data/statecodes.txt', header = TRUE, sep = "|")
stcd<-stcd[,1:3] # keep only state abbreviation and state name
colnames(stcd)<-c("STATE_ID","STUSAB","STATE_NAME")
st<-merge(st_reg,stcd,by.x = "State_FIPS",by.y = "STATE_ID")

smop_base$County.StateAbb<-smop_base$County.State
codes$County.StateAbb<-codes$County.State
for (j in 1:51) {
  # smop_base$County.StateAbb<-gsub(st$Name[j],st$STUSAB[j],smop_base$County.StateAbb) # this was commented out, not sure if necessary, circle back at the end
  codes$County.StateAbb<-gsub(st$Name[j],st$STUSAB[j],codes$County.StateAbb)
}
# this following line may then also be unneccesary
smop_hiDR$County.StateAbb<-smop_hiMF$County.StateAbb<-smop_hiDRMF$County.StateAbb<-smop_base$County.StateAbb

# extract template for making US summary results, for each scenario ##########
us_base<-as.data.frame(smop_base[[3]][[1]][,c(1:31,110:169)])
add<-c(3,7:9,14:17,22:91) # columns to add from the initial base dataframe, should correspond to all absolute terms which can be added, population, housing units, construction and demolition
add_smop<-c(3,7:9,14:17,22:31,110:169) # columns to add from the stock model output (smop) dataframes, should be the exact same columns as 'add'
identical(names(us_base[,add]),names(smop_base[[3]][[1]][,add_smop])) # this must be true
# turn all variables except year to 0
us_base[,c(1,3:91)]<-0
nc_base<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3142) {
  us_base[,add]<-us_base[,add]+smop_base[[3]][[i]][,add_smop]
}
us_base$Tot_Con<-us_base$Con_SF+us_base$Con_MF+us_base$Con_MH # calculate total construction at the national level
for (l in 0:7) {
  rows<-(5*l)+1:5
  nc_base[l+1]<-sum(us_base$Tot_Con[rows]) # fill in values for new construction every 5 years
  }
# calculate shares of population by housing type
us_base[,4:6]<-us_base[,7:9]/us_base$Population
# calculation total occ hous_baseing units (currenty affacted by Infinity issue) - not anymore?
us_base[,14]<-rowSums(us_base[,15:17])
# calculate HHS
us_base[,10:12]<-us_base[,7:9]/us_base[,15:17] ## HHS by house type
us_base$HH_Size<-us_base$Population/us_base$Tot_Hous_Units # national average HHS
# calculate vacancy ratios (TU/OU)
us_base[,18:21]<-us_base[,22:25]/us_base[,14:17]
# calculate addition (con) and loss (dem) rates
us_base[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_base[,c("Con_SF","Con_MF","Con_MH")]/us_base[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
us_base[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_base[,c("Dem_SF","Dem_MF","Dem_MH")]/us_base[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]

# extract template for making US summary results, for each scenario, high DR
us_hiDR<-as.data.frame(smop_hiDR[[3]][[1]][,c(1:31,110:169)])
add<-c(3,7:9,14:17,22:91) # columns to add from the initial base dataframe
add_smop<-c(3,7:9,14:17,22:31,110:169)
identical(names(us_hiDR[,add]),names(smop_hiDR[[3]][[1]][,add_smop])) # this must be true
# turn all variables except year to 0
us_hiDR[,c(1,3:91)]<-0
nc_hiDR<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3142) {
  us_hiDR[,add]<-us_hiDR[,add]+smop_hiDR[[3]][[i]][,add_smop]
}
us_hiDR$Tot_Con<-us_hiDR$Con_SF+us_hiDR$Con_MF+us_hiDR$Con_MH
for (l in 0:7) {
  rows<-(5*l)+1:5
  nc_hiDR[l+1]<-sum(us_hiDR$Tot_Con[rows]) # fill in values for new construction every 5 years
}
# calculate shares of population by housing type
us_hiDR[,4:6]<-us_hiDR[,7:9]/us_hiDR$Population
# calculation total occ housing units (currenty affacted by Infinity issue) - not anymore?
us_hiDR[,14]<-rowSums(us_hiDR[,15:17])
# calculate HHS
us_hiDR[,10:12]<-us_hiDR[,7:9]/us_hiDR[,15:17]
us_hiDR$HH_Size<-us_hiDR$Population/us_hiDR$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDR[,18:21]<-us_hiDR[,22:25]/us_hiDR[,14:17]
# calculate addition (con) and loss (dem) rates
us_hiDR[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_hiDR[,c("Con_SF","Con_MF","Con_MH")]/us_hiDR[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
us_hiDR[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_hiDR[,c("Dem_SF","Dem_MF","Dem_MH")]/us_hiDR[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]

# extract template for making US summary results, for each scenario, high MF
us_hiMF<-as.data.frame(smop_hiMF[[3]][[1]][,c(1:31,110:169)])
add<-c(3,7:9,14:17,22:91) # columns to add from the initial base dataframe
add_smop<-c(3,7:9,14:17,22:31,110:169)
identical(names(us_hiMF[,add]),names(smop_hiMF[[3]][[1]][,add_smop])) # this must be true
# turn all variables except year to 0
us_hiMF[,c(1,3:91)]<-0
nc_hiMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3142) {
  us_hiMF[,add]<-us_hiMF[,add]+smop_hiMF[[3]][[i]][,add_smop]
}
us_hiMF$Tot_Con<-us_hiMF$Con_SF+us_hiMF$Con_MF+us_hiMF$Con_MH
for (l in 0:7) {
  rows<-(5*l)+1:5
  nc_hiMF[l+1]<-sum(us_hiMF$Tot_Con[rows]) # fill in values for new construction every 5 years
}
# calculate shares of population by housing type
us_hiMF[,4:6]<-us_hiMF[,7:9]/us_hiMF$Population
# calculation total occ housing units (currenty affacted by Infinity issue) - not anymore?
us_hiMF[,14]<-rowSums(us_hiMF[,15:17])
# calculate HHS
us_hiMF[,10:12]<-us_hiMF[,7:9]/us_hiMF[,15:17]
us_hiMF$HH_Size<-us_hiMF$Population/us_hiMF$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiMF[,18:21]<-us_hiMF[,22:25]/us_hiMF[,14:17]
# calculate addition (con) and loss (dem) rates
us_hiMF[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_hiMF[,c("Con_SF","Con_MF","Con_MH")]/us_hiMF[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
us_hiMF[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_hiMF[,c("Dem_SF","Dem_MF","Dem_MH")]/us_hiMF[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]

# extract template for making US summary results, for each scenario, high DRMF
us_hiDRMF<-as.data.frame(smop_hiDRMF[[3]][[1]][,c(1:31,110:169)])
add<-c(3,7:9,14:17,22:91) # columns to add from the initial base dataframe
add_smop<-c(3,7:9,14:17,22:31,110:169)
identical(names(us_hiDRMF[,add]),names(smop_hiDRMF[[3]][[1]][,add_smop])) # this must be true
# turn all variables except year to 0
us_hiDRMF[,c(1,3:91)]<-0
nc_hiDRMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3142) {
  us_hiDRMF[,add]<-us_hiDRMF[,add]+smop_hiDRMF[[3]][[i]][,add_smop]
}
us_hiDRMF$Tot_Con<-us_hiDRMF$Con_SF+us_hiDRMF$Con_MF+us_hiDRMF$Con_MH
for (l in 0:7) {
  rows<-(5*l)+1:5
  nc_hiDRMF[l+1]<-sum(us_hiDRMF$Tot_Con[rows]) # fill in values for new construction every 5 years
}
# calculate shares of population by housing type
us_hiDRMF[,4:6]<-us_hiDRMF[,7:9]/us_hiDRMF$Population
# calculation total occ housing units (currenty affacted by Infinity issue) - not anymore?
us_hiDRMF[,14]<-rowSums(us_hiDRMF[,15:17])
# calculate HHS
us_hiDRMF[,10:12]<-us_hiDRMF[,7:9]/us_hiDRMF[,15:17]
us_hiDRMF$HH_Size<-us_hiDRMF$Population/us_hiDRMF$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDRMF[,18:21]<-us_hiDRMF[,22:25]/us_hiDRMF[,14:17]
# calculate addition (con) and loss (dem) rates
us_hiDRMF[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_hiDRMF[,c("Con_SF","Con_MF","Con_MH")]/us_hiDRMF[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
us_hiDRMF[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_hiDRMF[,c("Dem_SF","Dem_MF","Dem_MH")]/us_hiDRMF[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
save(us_base,us_hiDR,us_hiMF,us_hiDRMF,file="HSM_results/US_smop_scenarios.RData")
# load("HSM_results/US_smop_scenarios.RData")
# figure out how many occupied new constructions exist every 5 years for each scenario ##############
new_OU_base<-us_base[c(seq(1,41,5)),c(2,38:41,58:61,78:81)] # extract columns of Year, and Tot occupied units by type and new cohort
new_OU_base$Tot_New_OU<-0
new_OU_base$Tot_New_OU[2]<-sum(new_OU_base$Tot_HU_SF_Occ_2020_29[2]+new_OU_base$Tot_HU_MF_Occ_2020_29[2]+new_OU_base$Tot_HU_MH_Occ_2020_29[2]) # tot new units 2025 (built 2020-2025)
new_OU_base$Tot_New_OU[3]<-sum(new_OU_base$Tot_HU_SF_Occ_2020_29[3]+new_OU_base$Tot_HU_MF_Occ_2020_29[3]+new_OU_base$Tot_HU_MH_Occ_2020_29[3])-new_OU_base$Tot_New_OU[2] # tot new units 2030

new_OU_base$Tot_New_OU[4]<-sum(new_OU_base$Tot_HU_SF_Occ_2030_39[4]+new_OU_base$Tot_HU_MF_Occ_2030_39[4]+new_OU_base$Tot_HU_MH_Occ_2030_39[4]) # tot new units 2025 (built 2020-2025)
new_OU_base$Tot_New_OU[5]<-sum(new_OU_base$Tot_HU_SF_Occ_2030_39[5]+new_OU_base$Tot_HU_MF_Occ_2030_39[5]+new_OU_base$Tot_HU_MH_Occ_2030_39[5])-new_OU_base$Tot_New_OU[4] # tot new units 2030

new_OU_base$Tot_New_OU[6]<-sum(new_OU_base$Tot_HU_SF_Occ_2040_49[6]+new_OU_base$Tot_HU_MF_Occ_2040_49[6]+new_OU_base$Tot_HU_MH_Occ_2040_49[6]) # tot new units 2025 (built 2020-2025)
new_OU_base$Tot_New_OU[7]<-sum(new_OU_base$Tot_HU_SF_Occ_2040_49[7]+new_OU_base$Tot_HU_MF_Occ_2040_49[7]+new_OU_base$Tot_HU_MH_Occ_2040_49[7])-new_OU_base$Tot_New_OU[6] # tot new units 2030

new_OU_base$Tot_New_OU[8]<-sum(new_OU_base$Tot_HU_SF_Occ_2050_60[8]+new_OU_base$Tot_HU_MF_Occ_2050_60[8]+new_OU_base$Tot_HU_MH_Occ_2050_60[8]) # tot new units 2025 (built 2020-2025)
new_OU_base$Tot_New_OU[9]<-sum(new_OU_base$Tot_HU_SF_Occ_2050_60[9]+new_OU_base$Tot_HU_MF_Occ_2050_60[9]+new_OU_base$Tot_HU_MH_Occ_2050_60[9])-new_OU_base$Tot_New_OU[8] # tot new units 2030
# s2 hi DR
new_OU_hiDR<-us_hiDR[c(seq(1,41,5)),c(2,38:41,58:61,78:81)]
new_OU_hiDR$Tot_New_OU<-0
new_OU_hiDR$Tot_New_OU[2]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2020_29[2]+new_OU_hiDR$Tot_HU_MF_Occ_2020_29[2]+new_OU_hiDR$Tot_HU_MH_Occ_2020_29[2]) # tot new units 2025 (built 2020-2025)
new_OU_hiDR$Tot_New_OU[3]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2020_29[3]+new_OU_hiDR$Tot_HU_MF_Occ_2020_29[3]+new_OU_hiDR$Tot_HU_MH_Occ_2020_29[3])-new_OU_hiDR$Tot_New_OU[2] # tot new units 2030

new_OU_hiDR$Tot_New_OU[4]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2030_39[4]+new_OU_hiDR$Tot_HU_MF_Occ_2030_39[4]+new_OU_hiDR$Tot_HU_MH_Occ_2030_39[4]) # tot new units 2025 (built 2020-2025)
new_OU_hiDR$Tot_New_OU[5]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2030_39[5]+new_OU_hiDR$Tot_HU_MF_Occ_2030_39[5]+new_OU_hiDR$Tot_HU_MH_Occ_2030_39[5])-new_OU_hiDR$Tot_New_OU[4] # tot new units 2030

new_OU_hiDR$Tot_New_OU[6]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2040_49[6]+new_OU_hiDR$Tot_HU_MF_Occ_2040_49[6]+new_OU_hiDR$Tot_HU_MH_Occ_2040_49[6]) # tot new units 2025 (built 2020-2025)
new_OU_hiDR$Tot_New_OU[7]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2040_49[7]+new_OU_hiDR$Tot_HU_MF_Occ_2040_49[7]+new_OU_hiDR$Tot_HU_MH_Occ_2040_49[7])-new_OU_hiDR$Tot_New_OU[6] # tot new units 2030

new_OU_hiDR$Tot_New_OU[8]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2050_60[8]+new_OU_hiDR$Tot_HU_MF_Occ_2050_60[8]+new_OU_hiDR$Tot_HU_MH_Occ_2050_60[8]) # tot new units 2025 (built 2020-2025)
new_OU_hiDR$Tot_New_OU[9]<-sum(new_OU_hiDR$Tot_HU_SF_Occ_2050_60[9]+new_OU_hiDR$Tot_HU_MF_Occ_2050_60[9]+new_OU_hiDR$Tot_HU_MH_Occ_2050_60[9])-new_OU_hiDR$Tot_New_OU[8] # tot new units 2030

# s3 hi MF
new_OU_hiMF<-us_hiMF[c(seq(1,41,5)),c(2,38:41,58:61,78:81)]
new_OU_hiMF$Tot_New_OU<-0
new_OU_hiMF$Tot_New_OU[2]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2020_29[2]+new_OU_hiMF$Tot_HU_MF_Occ_2020_29[2]+new_OU_hiMF$Tot_HU_MH_Occ_2020_29[2]) # tot new units 2025 (built 2020-2025)
new_OU_hiMF$Tot_New_OU[3]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2020_29[3]+new_OU_hiMF$Tot_HU_MF_Occ_2020_29[3]+new_OU_hiMF$Tot_HU_MH_Occ_2020_29[3])-new_OU_hiMF$Tot_New_OU[2] # tot new units 2030

new_OU_hiMF$Tot_New_OU[4]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2030_39[4]+new_OU_hiMF$Tot_HU_MF_Occ_2030_39[4]+new_OU_hiMF$Tot_HU_MH_Occ_2030_39[4]) # tot new units 2025 (built 2020-2025)
new_OU_hiMF$Tot_New_OU[5]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2030_39[5]+new_OU_hiMF$Tot_HU_MF_Occ_2030_39[5]+new_OU_hiMF$Tot_HU_MH_Occ_2030_39[5])-new_OU_hiMF$Tot_New_OU[4] # tot new units 2030

new_OU_hiMF$Tot_New_OU[6]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2040_49[6]+new_OU_hiMF$Tot_HU_MF_Occ_2040_49[6]+new_OU_hiMF$Tot_HU_MH_Occ_2040_49[6]) # tot new units 2025 (built 2020-2025)
new_OU_hiMF$Tot_New_OU[7]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2040_49[7]+new_OU_hiMF$Tot_HU_MF_Occ_2040_49[7]+new_OU_hiMF$Tot_HU_MH_Occ_2040_49[7])-new_OU_hiMF$Tot_New_OU[6] # tot new units 2030

new_OU_hiMF$Tot_New_OU[8]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2050_60[8]+new_OU_hiMF$Tot_HU_MF_Occ_2050_60[8]+new_OU_hiMF$Tot_HU_MH_Occ_2050_60[8]) # tot new units 2025 (built 2020-2025)
new_OU_hiMF$Tot_New_OU[9]<-sum(new_OU_hiMF$Tot_HU_SF_Occ_2050_60[9]+new_OU_hiMF$Tot_HU_MF_Occ_2050_60[9]+new_OU_hiMF$Tot_HU_MH_Occ_2050_60[9])-new_OU_hiMF$Tot_New_OU[8] # tot new units 2030

# s4 hi DRMF
new_OU_hiDRMF<-us_hiDRMF[c(seq(1,41,5)),c(2,38:41,58:61,78:81)]
new_OU_hiDRMF$Tot_New_OU<-0
new_OU_hiDRMF$Tot_New_OU[2]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2020_29[2]+new_OU_hiDRMF$Tot_HU_MF_Occ_2020_29[2]+new_OU_hiDRMF$Tot_HU_MH_Occ_2020_29[2]) # tot new units 2025 (built 2020-2025)
new_OU_hiDRMF$Tot_New_OU[3]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2020_29[3]+new_OU_hiDRMF$Tot_HU_MF_Occ_2020_29[3]+new_OU_hiDRMF$Tot_HU_MH_Occ_2020_29[3])-new_OU_hiDRMF$Tot_New_OU[2] # tot new units 2030

new_OU_hiDRMF$Tot_New_OU[4]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2030_39[4]+new_OU_hiDRMF$Tot_HU_MF_Occ_2030_39[4]+new_OU_hiDRMF$Tot_HU_MH_Occ_2030_39[4]) # tot new units 2035
new_OU_hiDRMF$Tot_New_OU[5]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2030_39[5]+new_OU_hiDRMF$Tot_HU_MF_Occ_2030_39[5]+new_OU_hiDRMF$Tot_HU_MH_Occ_2030_39[5])-new_OU_hiDRMF$Tot_New_OU[4] # tot new units 2040

new_OU_hiDRMF$Tot_New_OU[6]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2040_49[6]+new_OU_hiDRMF$Tot_HU_MF_Occ_2040_49[6]+new_OU_hiDRMF$Tot_HU_MH_Occ_2040_49[6]) # tot new units 2045 
new_OU_hiDRMF$Tot_New_OU[7]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2040_49[7]+new_OU_hiDRMF$Tot_HU_MF_Occ_2040_49[7]+new_OU_hiDRMF$Tot_HU_MH_Occ_2040_49[7])-new_OU_hiDRMF$Tot_New_OU[6] # tot new units 2050

new_OU_hiDRMF$Tot_New_OU[8]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2050_60[8]+new_OU_hiDRMF$Tot_HU_MF_Occ_2050_60[8]+new_OU_hiDRMF$Tot_HU_MH_Occ_2050_60[8]) # tot new units 2055 
new_OU_hiDRMF$Tot_New_OU[9]<-sum(new_OU_hiDRMF$Tot_HU_SF_Occ_2050_60[9]+new_OU_hiDRMF$Tot_HU_MF_Occ_2050_60[9]+new_OU_hiDRMF$Tot_HU_MH_Occ_2050_60[9])-new_OU_hiDRMF$Tot_New_OU[8] # tot new units 2060
# new construction per scenario and year
newcon_sy<-matrix(c(new_OU_base$Tot_New_OU,new_OU_hiDR$Tot_New_OU,new_OU_hiMF$Tot_New_OU,new_OU_hiDRMF$Tot_New_OU),9,4)
write.csv(newcon_sy,file="HSM_results/NewConEstimates.csv")
# visualize us summary scenario results ##############
# scenarios<-c("Baseline","High Demolition","High MF Share","High Demolition & MF Share")
scenarios<-c("1. Baseline","2. High Turnover","3. High Multifamily Growth","4. High Turnover & Multifamily Growth")
scen_name<-c("base","hiDR","hiMF","hiDRMF")
location<-"USA"
graphics.off()
# Figures/ directory must first be created
fol<-paste("Figures/",as.character(Sys.Date()),sep = "")
dir.create(fol)
for (scen in 1:4) {
df<-get(paste("us",scen_name[scen],sep = "_"))
names(df)[18]<-"VR_Tot"
# calculate annual population growth
df$Pop_Growth<-0
for (i in 2:41) {df$Pop_Growth[i]<-df$Population[i]-df$Population[i-1]}
# calculate housing stocks by cohorts for each type, add together occupied and vacant stock
# SF
df[,paste(substr(names(df[,32:41]),8,10),substr(names(df[,32:41]),15,nchar(names(df[,32:41]))),sep = "")]<-
  df[,32:41]+df[,42:51]
# MF
df[,paste(substr(names(df[,52:61]),8,10),substr(names(df[,52:61]),15,nchar(names(df[,52:61]))),sep = "")]<-
  df[,52:61]+df[,62:71]
# MH
df[,paste(substr(names(df[,72:81]),8,10),substr(names(df[,72:81]),15,nchar(names(df[,72:81]))),sep = "")]<-
  df[,72:81]+df[,82:91]

pop<-melt(df[,c(1,2,7:9)],id = c("GeoID","Year"))
names(pop)[3:4]<-c("Type","Population")
pop$Type<-substr(pop$Type,5,6)

# windows()
g<-ggplot(pop,aes(x=Year,y=1e-6*Population,group=Type))+geom_point(aes(color=Type)) + scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Population by house type, 2020-2060,",location),subtitle = scenarios[scen],y= "Population (Millions)") + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
ggsave(paste("Pop_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)

# pop share
if (scen %% 2 == 1) {
pop_sh<-melt(df[,c(1,2,4:6)],id = c("GeoID","Year"))
names(pop_sh)[3:4]<-c("Type","Population Share")
pop_sh$Type<-substr(pop_sh$Type,11,12)
# windows()
g<-ggplot(pop_sh,aes(x=Year,y=`Population Share`,group=Type))+geom_point(aes(color=Type)) +scale_y_continuous(labels=scales::percent) +
  labs(title = paste("Population share by house type, 2020-2060,",location ),subtitle = scenarios[scen]) + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
ggsave(paste("PopShare_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)
}

# Occupied housing units total
ohu<-melt(df[,c(1,2,15:17)],id = c("GeoID","Year"))
names(ohu)[3:4]<-c("Type","Occupied Units")
location<-codes[codes$GeoID==as.numeric(ohu$GeoID[1]),]$County.State
ohu$Type<-substr(ohu$Type,8,9)
# windows()
g<-ggplot(ohu,aes(x=Year,y=0.001*`Occupied Units`,group=Type))+geom_point(aes(color=Type)) + scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Occupied housing units by type, 2020-2060,",location),subtitle = scenarios[scen], y = "Occupied Units (1,000 Units)") + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
ggsave(paste("OHU_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)

# Occupied housing units share
OHU<-df[,c(1,2,15:17)]
OHU[,3:5]<-OHU[,3:5]/rowSums(OHU[,3:5])
ohus<-melt(OHU,id = c("GeoID","Year"))
names(ohus)[3:4]<-c("Type","Occupied Units")
ohus$Type<-substr(ohus$Type,8,9)
# windows()
g<-ggplot(ohus,aes(x=Year,y=`Occupied Units`,group=Type))+geom_line(aes(color=Type))+geom_point(aes(color=Type)) + scale_y_continuous(labels = scales::percent) +
  labs(title = paste("Occupied housing units by type, 2020-2060,",location ),subtitle = scenarios[scen]) + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
#ggsave(paste("OHUShare_US_",  scen_name[scen],  ".jpeg",sep=""),g,path = fol)

# construction and demolition flows
cd<-melt(df[,c(1,2,26:31)],id = c("GeoID","Year"))
cd$Flow<-substr(cd$variable,1,3)
cd[cd$Flow=="Dem",]$Flow<-"Loss"
cd[cd$Flow=="Con",]$Flow<-"Add"
cd$Type<-substr(cd$variable,5,6)
names(cd)[3]<-c("Flow_Type")
cd<-cd[cd$Year<2060,]
# windows() # +geom_line(aes(color=Type)) removed the lines
g<-ggplot(cd,aes(x=Year,y=0.001*value,group=Flow_Type))+geom_point(aes(color=Type, shape=Flow),size=2) + scale_y_continuous(limits = c(0,1900), labels = scales::comma) +
  labs(title = paste("Stock additions and losses, 2020-2060,",location ),subtitle = scenarios[scen], y = "Stock additions and losses (1,000 Units/yr)") + theme_bw() + scale_shape_manual(values = c(16, 2)) + 
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
assign(paste("condem_",scen_name[scen],sep = ""),g)
# g
# ggsave(paste("ConDem_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol,width = 7,height = 5.5)
ggsave(paste("ConDem_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol,width = 5.7,height = 5.1)

# vacancy rates
vr<-melt(df[,c(1,2,18:21)],id = c("GeoID","Year"))
names(vr)[3:4]<-c("Type","Vacancy Ratio")
vr$Type<-substr(vr$Type,4,nchar(as.character(vr$Type)))
vr<-vr[!vr$Type=="Tot",]
# windows()
g<-ggplot(vr,aes(x=Year,y=`Vacancy Ratio`,group=Type))+geom_point(aes(color=Type),size=2) + 
  labs(title = paste("Vacancy Factor by house type, 2020-2060,",location),subtitle = scenarios[scen], y = "Vacancy Factor (Tot. Units/Occ. Units)") + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
# g
ggsave(paste("VacRat_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)

# construction and demolition rates
cdr<-melt(df[,c(1,2,93:98)],id = c("GeoID","Year"))
cdr$Flow<-substr(cdr$variable,1,3)
cdr[cdr$Flow=="Dem",]$Flow<-"Loss"
cdr[cdr$Flow=="Con",]$Flow<-"Add"
cdr$Type<-substr(cdr$variable,10,11)
names(cdr)[3]<-c("Flow_Type")
cdr<-cdr[cdr$Year<2060,]
# windows()
g<-ggplot(cdr,aes(x=Year,y=value,group=Flow_Type))+geom_line(aes(color=Type, linetype=Flow))+geom_point(aes(color=Type, shape=Flow),size=2) + scale_y_continuous(labels = scales::percent,limits = c(0,0.055)) +
  labs(title = paste("Stock addition and loss rates, 2020-2060,",location ),subtitle = scenarios[scen], y = "Annual addition and loss rates (%)") + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
assign(paste("condemRate_",scen_name[scen],sep = ""),g)
ggsave(paste("ConDemRat_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol,width = 7,height = 5)

# housing stock by cohort (area chart) for each house type
tc<-melt(df[,c(1,2,100:129)],id = c("GeoID","Year")) # for the us version
names(tc)[3:4]<-c("Type-Cohort","Stock")
tc$Type<-substr(tc$`Type-Cohort`,1,2)
tc$Cohort<-substr(tc$`Type-Cohort`,4,nchar(as.character(tc$`Type-Cohort`)))
tc[tc$Cohort=="p1940",]$Cohort<-"<1940"

tc_sf<-tc[tc$Type=="SF",]
tc_sf$order<-rep(rev(c(1:10)),each=41)
# windows()
g<-ggplot(tc_sf,aes(x=Year,y=0.001*Stock,fill=reorder(Cohort,order)))+geom_area() + scale_y_continuous(labels = scales::comma,limits = c(0,119000)) +
  labs(title = paste("Total Single-Family Units by Cohort,",location ),subtitle = scenarios[scen], y = "Total Stock (1,000 Units)",fill="Cohort") + theme_bw() + scale_fill_brewer(palette="Paired")+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
# g
assign(paste("SF_Coh_",scen_name[scen],sep = ""),g)
ggsave(paste("SF_Coh_US_",  scen_name[scen],  ".jpeg",sep=""),g,path = fol,width = 5.7,height = 5.1)

tc_mf<-tc[tc$Type=="MF",]
tc_mf$order<-rep(rev(c(1:10)),each=41)
# windows()
g<-ggplot(tc_mf,aes(x=Year,y=0.001*Stock,fill=reorder(Cohort,order)))+geom_area() + scale_y_continuous(labels = scales::comma,limits = c(0,65000)) +
  labs(title = paste("Total Multifamily Units by Cohort,",location ),subtitle = scenarios[scen], y = "Total Stock (1,000 Units)",fill="Cohort") + theme_bw() + scale_fill_brewer(palette="Paired")+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
assign(paste("MF_Coh_",scen_name[scen],sep = ""),g)
ggsave(paste("MF_Coh_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol,width = 5.7,height = 5.1)

tc_mh<-tc[tc$Type=="MH",]
tc_mh$order<-rep(rev(c(1:10)),each=41)
# windows()
g<-ggplot(tc_mh,aes(x=Year,y=0.001*Stock,fill=reorder(Cohort,order)))+geom_area() + scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Total Manuf. Housing Units by Cohort,",location ),subtitle = scenarios[scen], y = "Total Stock (1,000 Units)",fill="Cohort") + theme_bw() + scale_fill_brewer(palette="Paired")+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
assign(paste("MH_Coh_",scen_name[scen],sep = ""),g)
ggsave(paste("MH_Coh_US_", scen_name[scen],  ".jpeg",sep=""),g,path = fol,width = 5.7,height = 5.1)

}
# make individual plots to copy into manuscript ##########
# plots for manuscript Fig 3
# adjust window to approx dev.size("in") = 5.1 x 5.1
windows(width = 5.2, height = 5.1)
condem_base+theme(legend.position = "none")
condem_hiDR+theme(legend.position = "none")
condem_hiMF+theme(legend.position = "none")
condem_hiDRMF+theme(legend.position = "none")

# plots for manuscript Fig 4
windows(width = 6.415, height = 5.415)
SF_Coh_base
SF_Coh_hiDRMF

MF_Coh_base
MF_Coh_hiDRMF

# now make graphs for individual counties ###############
smop_scenarios<-c("smop_base","smop_hiDR","smop_hiMF","smop_hiDRMF")
scenarios<-c("1. Baseline","2. High Turnover","3. High Multifamily Growth","4. High Turnover & MF Growth")
# scenarios<-c("1. Baseline","2. High Turnover","3. High Multifamily","4. High TO & MF")
scen_name<-c("base","hiDR","hiMF","hiDRMF")
graphics.off()
fol<-paste("Figures/Counties/",as.character(Sys.Date()),sep = "") # need to create the Counties subdirectory first
dir.create(fol) 
# San Juan NM, Harrix TX, Marquette MI, Prov RI, are chosen to represent counties with high population decline/growth, and moderate population decine/growth
rnm2<-c(1820,2624,1283,2315) 
# loop stars here
for (i in 1:length(rnm2)) { # toggle between rnm and rnm2
  rn<-rnm2[i] # toggle between rnm and rnm2
  cty<-codes$GeoID[rn]
  
 for (scen in 1:4) {
smop<-get(smop_scenarios[scen])

cty_df<-as.data.frame(smop[[3]][[rn]])
codes$County.State[rn]
df<-smop[[3]][[rn]][,c(1:31,308:313)]
names(df)[18]<-"VR_Tot"
# calculate annual population growth
df$Pop_Growth<-0
for (i in 2:41) {df$Pop_Growth[i]<-df$Population[i]-df$Population[i-1]}
# calculate housing stocks by cohorts for each type
# SF
df[,paste(substr(names(cty_df[,110:119]),8,10),substr(names(cty_df[,110:119]),15,nchar(names(cty_df[,110:119]))),sep = "")]<-
  cty_df[,110:119]+cty_df[,120:129]
# MF
df[,paste(substr(names(cty_df[,130:139]),8,10),substr(names(cty_df[,130:139]),15,nchar(names(cty_df[,130:139]))),sep = "")]<-
  cty_df[,130:139]+cty_df[,140:149]
# MH
df[,paste(substr(names(cty_df[,150:159]),8,10),substr(names(cty_df[,150:159]),15,nchar(names(cty_df[,150:159]))),sep = "")]<-
  cty_df[,150:159]+cty_df[,160:169]

# make some  plots
# pop tot 
pop<-melt(df[,c(1,2,7:9)],id = c("GeoID","Year"))
names(pop)[3:4]<-c("Type","Population")
location<-codes[codes$GeoID==pop$GeoID[1],]$County.StateAbb
pop$Type<-substr(pop$Type,5,6)
# windows()
if (scen %% 2 == 1) {
g<-ggplot(pop,aes(x=Year,y=Population,group=Type))+geom_point(aes(color=Type)) + scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Population by house type, 2020-2060,",location ),subtitle = scenarios[scen]) + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
assign(paste("Pop", as.character(cty), "_", scen_name[scen],sep=""),g)

# pop share
# pop_sh<-melt(df[,c(1,2,4:6)],id = c("GeoID","Year"))
# names(pop_sh)[3:4]<-c("Type","Population Share")
# pop_sh$Type<-substr(pop_sh$Type,11,12)
# # windows()
# g<-ggplot(pop_sh,aes(x=Year,y=`Population Share`,group=Type))+geom_point(aes(color=Type)) +scale_y_continuous(labels=scales::percent) +
#   labs(title = paste("Population share by house type, 2020-2060,",location ),subtitle = scenarios[scen]) + theme_bw() +
#   theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# g
# ggsave(paste("PopShare", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)
}
# construction and demolition flows
cd<-melt(df[,c(1,2,26:31)],id = c("GeoID","Year"))
cd$Flow<-substr(cd$variable,1,3)
cd$Type<-substr(cd$variable,5,6)
names(cd)[3]<-c("Flow_Type")
cd<-cd[cd$Year<2060,]
# windows()
g<-ggplot(cd,aes(x=Year,y=value,group=Flow_Type))+geom_point(aes(color=Type, shape=Flow),size=2) + scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Construction and demolition, 2020-2060,",location ),subtitle = scenarios[scen], y = "Annual construction and demolition") + theme_bw() + scale_shape_manual(values = c(16, 2)) + 
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
ggsave(paste("ConDem", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)

# vacancy rates
vr<-melt(df[,c(1,2,18:21)],id = c("GeoID","Year"))
names(vr)[3:4]<-c("Type","Vacancy Ratio")
vr$Type<-substr(vr$Type,4,nchar(as.character(vr$Type)))
vr<-vr[!vr$Type=="Tot",]
g<-ggplot(vr,aes(x=Year,y=(`Vacancy Ratio`-1)/`Vacancy Ratio`,group=Type))+geom_point(aes(color=Type),size=2) + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = paste("Vacancy Rate by house type, 2020-2060,",location),subtitle = scenarios[scen], y = "Vacancy Rate") + theme_bw() +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
ggsave(paste("VacRate", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)
assign(paste("VacRate", as.character(cty), "_", scen_name[scen],sep=""),g)

# construction and demolition rates
cdr<-melt(df[,c(1,2,32:37)],id = c("GeoID","Year"))
cdr$Flow<-substr(cdr$variable,1,3)
cdr[cdr$Flow=="Con",]$Flow<-"Add"
cdr[cdr$Flow=="Dem",]$Flow<-"Loss"
cdr$Type<-substr(cdr$variable,10,11)
names(cdr)[3]<-c("Flow_Type")
cdr<-cdr[cdr$Year<2060,]
# windows()
g<-ggplot(cdr,aes(x=Year,y=value,group=Flow_Type))+geom_point(aes(color=Type, shape=Flow),size=2)  + scale_y_continuous(labels = scales::percent) +
  labs(title = paste("Stock Addition and Loss Rates,",location ),subtitle = scenarios[scen], y = "Add, Loss Rate") + theme_bw() + scale_shape_manual(values = c(16, 2)) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
ggsave(paste("ConDemRate", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)
assign(paste("ConDemRate", as.character(cty), "_", scen_name[scen],sep=""),g)

# housing stock by cohort (area chart) for each house type
tc<-melt(df[,c(1,2,39:68)],id = c("GeoID","Year"))
names(tc)[3:4]<-c("Type-Cohort","Stock")
tc$Type<-substr(tc$`Type-Cohort`,1,2)
tc$Cohort<-substr(tc$`Type-Cohort`,4,nchar(as.character(tc$`Type-Cohort`)))
tc[tc$Cohort=="p1940",]$Cohort<-"<1940"

tc_sf<-tc[tc$Type=="SF",]
tc_sf$order<-rep(rev(c(1:10)),each=41)
# windows()
g<-ggplot(tc_sf,aes(x=Year,y=0.001*Stock,fill=reorder(Cohort,order)))+geom_area() + scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  labs(title = paste("Total Single-Family Units by Cohort,",location ),subtitle = scenarios[scen], y = "Total SF Stock (1,000 Units)",fill="Cohort") + theme_bw() + scale_fill_brewer(palette="Paired")+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
ggsave(paste("SF_Coh", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)

tc_mf<-tc[tc$Type=="MF",]
tc_mf$order<-rep(rev(c(1:10)),each=41)
# windows()
g<-ggplot(tc_mf,aes(x=Year,y=0.001*Stock,fill=reorder(Cohort,order)))+geom_area() + scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  labs(title = paste("Total Multifamily Units by Cohort,",location ),subtitle = scenarios[scen],y = "Total MF Stock (1,000 Units)",fill="Cohort") + theme_bw() + scale_fill_brewer(palette="Paired")+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
ggsave(paste("MF_Coh", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)
assign(paste("MF_Coh", as.character(cty), "_", scen_name[scen],sep=""),g)

tc_mh<-tc[tc$Type=="MH",]
tc_mh$order<-rep(rev(c(1:10)),each=41)
# windows()
g<-ggplot(tc_mh,aes(x=Year,y=0.001*Stock,fill=reorder(Cohort,order)))+geom_area() + scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  labs(title = paste("Total Manuf. Housing Units by Cohort,",location ),subtitle = scenarios[scen],  y = "Total MH Stock (1,000 Units)",fill="Cohort") + theme_bw() + scale_fill_brewer(palette="Paired")+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold")) 
ggsave(paste("MH_Coh", as.character(cty), "_", scen_name[scen],  ".jpeg",sep=""),g,path = fol)

# graph dem rates by age group, by house type and census region
# dr<-melt(cty_df[1,c(1,2,170:187)],id = c("GeoID","Year"))
# dr$Type<-substr(dr$variable,10,11)
# dr$AgeRange<-substr(dr$variable,13,nchar(as.character(dr$variable))-3)
# dr$Vacancy<-substr(dr$variable,nchar(as.character(dr$variable))-2,nchar(as.character(dr$variable)))
# names(dr)[3:4]<-c("Type-Age-Vacancy","Dem_Rate")
# location<-codes[codes$GeoID==as.numeric(dr$GeoID[1]),]$CensusRegion

# jitter <- position_jitter(width = 0.115, height = 0.0)
# windows()
# ggplot(dr,aes(x=AgeRange,y=Dem_Rate)) + geom_point(position=jitter,aes(shape=Vacancy,color=Type),size=3) + scale_y_continuous(labels = scales::percent) +
#   labs(title = paste("Loss Rates by Type, Cohort & Vacancy,",location), y = "Loss Rate (%)") + theme_bw() +
#   theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

}
}
# now make some multiplots ##########
# vacancy factors
windows()
multiplot(VacRate48201_base+ theme(axis.title.x=element_blank(),legend.position = "none")+labs(y="Vac Rate")+scale_y_continuous(limits = c(0,0.23), labels = scales::percent_format(accuracy = 1)),
          VacRate35045_base+ theme(legend.position = "none")+labs(y="Vac Rate")+scale_y_continuous(limits = c(0,0.6), labels = scales::percent_format(accuracy = 1)),
          VacRate44007_base+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none")+scale_y_continuous(limits = c(0,0.23), labels = scales::percent_format(accuracy = 1)),
          VacRate26103_base+ theme(axis.title.y=element_blank(),legend.position = "none")+scale_y_continuous(limits = c(0,0.6), labels = scales::percent_format(accuracy = 1)),cols=2)

# just base, four counties population, one plot
windows()
multiplot(Pop48201_base+ theme(axis.title.x=element_blank(),legend.position = "none"),
          Pop35045_base+ theme(legend.position = "none"),
          Pop44007_base+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none"),
          Pop26103_base+ theme(axis.title.y=element_blank(),legend.position = "none"),cols=2)

# just base, four counties, one plot
windows()
multiplot(ConDemRate48201_base+ theme(legend.position = "none",axis.title.x=element_blank())+labs(y="Add, Loss Rates")+scale_y_continuous(limits = c(0,0.065), labels = scales::percent),
          ConDemRate35045_base+ theme(legend.position = "none")+labs(y="Add, Loss Rates")+scale_y_continuous(limits = c(0,0.046), labels = scales::percent),
          ConDemRate44007_base+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none")+labs(y="Add, Loss Rates")+scale_y_continuous(limits = c(0,0.046), labels = scales::percent),
          ConDemRate26103_base+theme(axis.title.y=element_blank(),legend.position = "none")+labs(y="Add, Loss Rates")+scale_y_continuous(limits = c(0,0.046), labels = scales::percent),cols=2)

# just base, four counties, one plot, MF Cohorts. This looks better if individual plots are pasted together in the word doc
windows()
multiplot(MF_Coh48201_base+ theme(legend.position = "none",axis.title.x=element_blank()),MF_Coh35045_base+ theme(legend.position = "none"),
          MF_Coh44007_base+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none"),MF_Coh26103_base+theme(axis.title.y=element_blank(),legend.position = "none"),cols=2)

# check all stock model outputs for all scenarios for a given county, defined by rowname of smop data frames
rn<-1820
cty_df1<-as.data.frame(smop_base[[3]][[rn]])
cty_df2<-as.data.frame(smop_hiDR[[3]][[rn]])
cty_df3<-as.data.frame(smop_hiMF[[3]][[rn]])
cty_df4<-as.data.frame(smop_hiDRMF[[3]][[rn]])
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
