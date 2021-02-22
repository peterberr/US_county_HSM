# script to run housing stock model projections for all US counties, 2020-2060
# Last updated Feb 17 2021
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
setwd("~/Yale Courses/Research/Final Paper/HSM_github/")
library(tidyr)
library(reshape2)
source('demrate_fn.R')
source('vacrate_fn.R')
source('run_sm_cty.R')
mean0<-function(df) { # function to calculate average values, excluding negative numbers and values above 0.5. Used to estimate how much of stock additions come from sources other than new construction
  df<-df[!df<0]
  df<-df[!df>0.5]
  m<-mean(df,na.rm = TRUE)
  m
}

create_sm_df<- function (county,summary,projection,scenNum) { # function to create stock model data frame
  sum_cn<-names(summary) # column names of county dataframe
  cty_cn<-names(county)
  # keep years, population (tot and by house type), and HH Size. Keep pop split same for now
  stockModel<-projection
  stockModel<-projection[,c(12,1:11)] # move GeoID to beginning
  
  # add columns for average HHS, occupied units, and average VR
  stockModel$Vacancy_Ratio<-stockModel$Occ_HU_MH<-stockModel$Occ_HU_MF<-stockModel$Occ_HU_SF<-stockModel$Occ_Hous_Units<-stockModel$HH_Size<-0 
  # add values for year 1 for vacancy rates
  stockModel[1,19:21]<-county[68:70]
  # add values for year 1 for tot units
  stockModel[1,22:25]<-county[4:7]
  # add columns for tot demolition and construction by type
  stockModel[,sum_cn[25:30]]<-0
  # add columns for construction scenarios (whether occupied stock growth was negative "A" or positive "B), initially set to "O"
  stockModel$Con_Scenario_MH<-stockModel$Con_Scenario_MF<-stockModel$Con_Scenario_SF<-"O" 
  # Add columns for vacancy-rate adjusted occupied stock growth, a first estimate for total stock growth.
  stockModel$VOSG_MH<-stockModel$VOSG_MF<-stockModel$VOSG_SF<-0 
  # Add columns for occupied and total stock growth, H (TSG/VOSG), and change in vacancy rate by type. These parameters are used to estimate construction. Set to zero
  # Actually only OSG is used. TSG is in the end used to identify non-standard demolition scenarios. dVR is renamed to Vn, to keep track of the natural vacancy rates.
  stockModel[,sum_cn[210:221]]<-0
  names(stockModel)[47:49]<-c("Vn_SF","Vn_MF","Vn_MH") # rename the unused dVRs to Vns to store the natural rates
  
  # Add breakdown of total stock by age cohort and occ status within each type group, for starting timestep only
  stockModel[1,cty_cn[8:67]]<-county[1,8:67]
  # Add columns for the total (not percentage) of housing units by type, occ, and age cohort
  stockModel[,substr(cty_cn[8:67],3,nchar(cty_cn[8:67]))]<-0
  # Add demolition rates by type, occ/vac, and age range, depending on census Region, The model will convert to rates by age cohort
  # In two of the scenarios assume the demolition rates are constant throughout the model projection period
  # In two of the scenarios assume accelerated demolition rates. 
  DRmult<-1 # default demolition rate multiplier == 1
  if (scenNum%%2==0) {DRmult<-1.5} # change demolition rate to 1.5*historical rates in the high DR scenarios
  if (county$Region==1) {
    stockModel[,names(dem_rates_NE)]<-dem_rates_NE*DRmult
    stockModel[,c("Vn_SF","Vn_MF","Vn_MH")]<-matrix(rep(c(Vn_SF_NE,Vn_MF_NE,Vn_MH_NE),each=41),41,3)
  }
  if (county$Region==2) {
    stockModel[,names(dem_rates_MW)]<-dem_rates_MW*DRmult
    stockModel[,c("Vn_SF","Vn_MF","Vn_MH")]<-matrix(rep(c(Vn_SF_MW,Vn_MF_MW,Vn_MH_MW),each=41),41,3)
  }
  if (county$Region==3) {
    stockModel[,names(dem_rates_S)]<-dem_rates_S*DRmult
    stockModel[,c("Vn_SF","Vn_MF","Vn_MH")]<-matrix(rep(c(Vn_SF_S,Vn_MF_S,Vn_MH_S),each=41),41,3)
  }
  if (county$Region==4) {
    stockModel[,names(dem_rates_W)]<-dem_rates_W*DRmult
    stockModel[,c("Vn_SF","Vn_MF","Vn_MH")]<-matrix(rep(c(Vn_SF_W,Vn_MF_W,Vn_MH_W),each=41),41,3)
  }
  # Add columns for demolition rates and absolute values by type, occ/vac, and age cohort, initially set to zero
  stockModel[,paste("Dem_Rate_",substr(cty_cn[8:67],10,nchar(cty_cn[8:67])),sep = "")]<-0
  stockModel[,paste("Dem_",substr(cty_cn[8:67],10,nchar(cty_cn[8:67])),sep = "")]<-0
  
  # Add columns for construction rate and demolition rate by aggregate types
  stockModel[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH","Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-0
  ## calculate housing stocks by age cohort for year 1
  stockModel[is.na(stockModel)]<-0
  stockModel[1,110:129]<-stockModel$Tot_HU_SF[1]*stockModel[1,50:69] # for SF homes
  stockModel[1,130:149]<-stockModel$Tot_HU_MF[1]*stockModel[1,70:89] # for MF homes
  stockModel[1,150:169]<-stockModel$Tot_HU_MH[1]*stockModel[1,90:109] # for MH homes
  # calculate occupied housing units by sum type for year 1
  stockModel$Occ_HU_SF[1]<-sum(stockModel[1,110:119]) # this is one way of calculating this number, another is with stockModel$Pop_SF[i]/stockModel$HHS_SF[i]
  stockModel$Occ_HU_MF[1]<-sum(stockModel[1,130:139])
  stockModel$Occ_HU_MH[1]<-sum(stockModel[1,150:159])
  # calculate total occupied units and total vacancy ratio for year 1
  stockModel$Occ_Hous_Units[1]<-sum(stockModel[1,c("Occ_HU_SF","Occ_HU_MF","Occ_HU_MH")])
  stockModel$Vacancy_Ratio[1]<-stockModel$Tot_Hous_Units[1]/stockModel$Occ_Hous_Units[1]
  
  if (scenNum>2) { # high MF share scenarios
    # does population grow to 2040 by at least 5%?
    # if so, increase MF share by 0.25 percentage points each year to 2040
    if (stockModel$Population[21]>=1.05*stockModel$Population[1]&stockModel$Pop_Share_MF[1]<0.9) { # don't implement any change if population share is already 90% or higher
      stockModel$Pop_Share_MF[2:21]<-stockModel$Pop_Share_MF[1]+seq(0.0025,0.05,0.0025)
      stockModel$Pop_Share_SF[2:21]<-stockModel$Pop_Share_SF[1]-seq(0.0025,0.05,0.0025)
      # in case no change from 2041-2060, set shares to 2040 values
      stockModel$Pop_Share_MF[22:41]<-stockModel$Pop_Share_MF[21]
      stockModel$Pop_Share_SF[22:41]<-stockModel$Pop_Share_SF[21]
      # set HHS in SF and MF to make no change, as the HHS reduction will be facilitated by increases in MF share.
      stockModel$HHS_SF[2:21]<-stockModel$HHS_SF[1]
      stockModel$HHS_MF[2:21]<-stockModel$HHS_MF[1]
      # in case no change from 2041-2060, set HHS to decline from their 2040 values in lines with national rates (preserved here by MH)
      stockModel$HHS_SF[22:41]<-stockModel$HHS_SF[21]*stockModel$HHS_MH[22:41]/stockModel$HHS_MH[21]
      stockModel$HHS_MF[22:41]<-stockModel$HHS_MF[21]*stockModel$HHS_MH[22:41]/stockModel$HHS_MH[21]
    }
    # does population grow 2040 to 2060 by at least 5%? if so, increase MF share by 0.25 percentage points each year to 2040
    if (stockModel$Population[41]>=1.05*stockModel$Population[21]&stockModel$Pop_Share_MF[21]<0.9) { # don't implement any change if population share is already 90% or higher
      stockModel$Pop_Share_MF[22:41]<-stockModel$Pop_Share_MF[21]+seq(0.0025,0.05,0.0025)
      stockModel$Pop_Share_SF[22:41]<-stockModel$Pop_Share_SF[21]-seq(0.0025,0.05,0.0025)
      # set HHS in SF and MF to make no change, as the HHS reduction will be facilitated by increases in MF share.
      stockModel$HHS_SF[22:41]<-stockModel$HHS_SF[21]
      stockModel$HHS_MF[22:41]<-stockModel$HHS_MF[21]
    }
    # if none of these statements are true, leave pop share by type as is, even in high MF scenarios
  }
  stockModel[,c("Pop_SF","Pop_MF","Pop_MH")]<-stockModel$Population*stockModel[,c("Pop_Share_SF","Pop_Share_MF","Pop_Share_MH")]
  # calculate occupied housing units by sum type for all other model years
  stockModel$Occ_HU_SF[2:41]<-stockModel$Pop_SF[2:41]/stockModel$HHS_SF[2:41]
  stockModel$Occ_HU_MF[2:41]<-stockModel$Pop_MF[2:41]/stockModel$HHS_MF[2:41]
  stockModel$Occ_HU_MH[2:41]<-stockModel$Pop_MH[2:41]/stockModel$HHS_MH[2:41]
  
  stockModel$Occ_Hous_Units<-rowSums(stockModel[,c("Occ_HU_SF","Occ_HU_MF","Occ_HU_MH")])
  stockModel$HH_Size<-stockModel$Population/stockModel$Occ_Hous_Units
  
  stockModel$Region<-county$Region
  stockModel$HS_Scenario<-scenNum
  
  stockModel_df<-stockModel # could also remove the unused columns here?
  
  stockModel_df
}

## load in intial housing stock data for 2020
load("Intermediate_results/InitStock20.RData") # this is created by the pop_housing.R script
## load in projections of population and household size by county
load("Intermediate_results/CountyProjections.RData") # this is created by the pop_housing.R script
## load in linear models of construction and construction ratios
load("Intermediate_results/ConLinModels.RData") # this is created by the AHS_HSM_dev.R script
## load in summaries of stock evolution for US and four census regions, 1985-2017
load("Intermediate_results/summaries.RData") # this is created by the AHS_HSM_dev.R script

# define adjustments to vacancy rates by age group for each housing type, based on calculations in AHS_HSM_development
dem_adj_SF<-c(0,-0.005,0,0.005) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups. Name is misleading, should be renamed vac_adj
dem_adj_SF2<-c(0,-0.0015,0,0.0015) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups. to be used in cases where dem_adj_SF causes negative occupied stock
dem_adj_MF<-c(0.005,0,-0.005,0) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups
dem_adj_MF2<-c(0.0015,0,-0.0015,0) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups, to be used in cases where dem_adj_MF causes negative occupied stock
dem_adj_MH<-c(-0.017,0,0.017,0) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups

# define vacancy rates, using national averages for now. Adjust down by factor 0.995, because national level model tended to overestimate vacancies slightly
# should produce VR about 1.1 for SF, 1.175 for MF, 1.25 for MH
Vn_SF<-mean(summary_US$VR_SF)*0.995 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-mean(summary_US$VR_MF)*0.995 # estimate 'natural' vacancy rate of 1.175 for MF housing
Vn_MH<-mean(summary_US$VR_MH)*0.995 # estimate 'natural' vacancy rate of 1.25 for MH housing

Vn_SF_NE<-mean(summary_NE$VR_SF)*0.995
Vn_MF_NE<-mean(summary_NE$VR_MF)*0.995
Vn_MH_NE<-mean(summary_NE$VR_MH)*0.995

Vn_SF_MW<-mean(summary_MW$VR_SF)*0.995
Vn_MF_MW<-mean(summary_MW$VR_MF)*0.995
Vn_MH_MW<-mean(summary_MW$VR_MH)*0.995

Vn_SF_S<-mean(summary_S$VR_SF)*0.995
# Vn_MF_S<-mean(summary_S$VR_MF)*0.995
# reduce VN for MF in the south to be a bit closer to national levels, from 1.267 to 1.23
Vn_MF_S<-1.23
Vn_MH_S<-mean(summary_S$VR_MH)*0.995

Vn_SF_W<-mean(summary_W$VR_SF)*0.995
Vn_MF_W<-mean(summary_W$VR_MF)*0.995
Vn_MH_W<-mean(summary_W$VR_MH)*0.995

# load concordance of states to divisions and regions
st_reg<-read.csv('Data/state-geocodes-v2017.csv')
sr<-st_reg[,c(1,3)] # get just state and region codes
h20pc$StateCode<-as.numeric(substr(h20pc$GeoID,1,nchar(h20pc$GeoID)-3))
h20pc<-merge(h20pc,sr,by.x = "StateCode",by.y = "State_FIPS") # keep the state code in the df, it may be used later for particular counties, e.g. CA
dem_rates_US<-summary_US[1,84:101] # demolition rates by type, age group, and occupancy status, define dimensions
dem_rates_US[,]<-0 # set values to 0
# demolition rates for all types in US
l_US<-summary_US[1:15,84:101]
dem_rates_US[,]<-lapply(l_US,mean,na.rm=TRUE) # set demolition rates to averages by type, age group, and occupancy status from 1985-2015
# do the same for each Region, filling in zeros with national values
dem_rates_NE<-summary_NE[1,84:101]
dem_rates_NE[,]<-0
# demolition rates for all types in NE
l_NE<-summary_NE[1:15,84:101]
dem_rates_NE[,]<-lapply(l_NE,mean,na.rm=TRUE)
dem_rates_NE[which(dem_rates_NE==0)]<-dem_rates_US[which(dem_rates_NE==0)] # replace zero values with national values

dem_rates_MW<-summary_MW[1,84:101]
dem_rates_MW[,]<-0
# demolition rates for all types in MW
l_MW<-summary_MW[1:15,84:101]
dem_rates_MW[,]<-lapply(l_MW,mean,na.rm=TRUE)
dem_rates_MW[which(dem_rates_MW==0)]<-dem_rates_US[which(dem_rates_MW==0)] # replace zero values with national values

dem_rates_W<-summary_W[1,84:101]
dem_rates_W[,]<-0
# demolition rates for all types in W
l_W<-summary_W[1:15,84:101]
dem_rates_W[,]<-lapply(l_W,mean,na.rm=TRUE)
dem_rates_W[which(dem_rates_W==0)]<-dem_rates_US[which(dem_rates_W==0)] # replace zero values with national values

dem_rates_S<-summary_S[1,84:101]
dem_rates_S[,]<-0
# demolition rates for all types in S
l_S<-summary_S[1:15,84:101]
dem_rates_S[,]<-lapply(l_S,mean,na.rm=TRUE)
dem_rates_S[which(dem_rates_S==0)]<-dem_rates_US[which(dem_rates_S==0)] # replace zero values with national values

# Use US avg values for dem rates of MH, because of smaller sample size and volatility associated with MH moving
dem_rates_W[13:18]<-dem_rates_S[13:18]<-dem_rates_MW[13:18]<-dem_rates_NE[13:18]<-dem_rates_US[13:18]

# # to get the dem rates into excel via the clipboard
# drNE<-matrix(dem_rates_NE,3,6,byrow = TRUE)
# write.table(drNE, "clipboard", sep="\t")
# 
# drMW<-matrix(dem_rates_MW,3,6,byrow = TRUE)
# write.table(drMW, "clipboard", sep="\t")
# 
# drS<-matrix(dem_rates_S,3,6,byrow = TRUE)
# write.table(drS, "clipboard", sep="\t")
# 
# drW<-matrix(dem_rates_W,3,6,byrow = TRUE)
# write.table(drW, "clipboard", sep="\t")

# To plot relations between the stock adjustment factor and changes in vacancy rates
# relSF<-summary_S[1:16,c("OSG_SF","TSG_SF","H_SF","dVR_SF","Con_Rate_SF","Tot_HU_SF")]
# rSF<-relSF[relSF$OSG_SF>0&relSF$TSG_SF>0,] # this model can be applied to instances when neither occupied nor total stock declines.
# lmSF<-lm(H_SF~dVR_SF,data=rSF)
# windows()
# plot(H_SF~dVR_SF,data=rSF,pch=16,col="blue",main="SA factor vs change in VR, Single-Family",xlab="dVR (change in Vac Rate)",ylab="SA factor (TSG/VR*OSG)")
# abline(lmSF)
# abline(h=0)
# abline(v=0)

# To plot relations between stock addition rates and stock growth rates 
# relSF$OSG_Rate_SF<-0.5*relSF$OSG_SF/relSF$Tot_HU_SF # annual occupied stock growth divided by total stock
# lmSFc<-lm(Con_Rate_SF~OSG_Rate_SF,data=relSF[1:15,]) # linear model of construction rate based on occupied stock growth rate. rates calculated by dividing by total stock.
# windows()
# plot(Con_Rate_SF~OSG_Rate_SF,data=relSF[1:15,],pch=16,col="blue",main="Addition Rate vs OSG Rate, Single-Family, S",xlab="Occ Stock Growth Rate",ylab="Addition Rate",ylim=c(0,0.035))
# abline(lmSFc)
# abline(h=0)
# abline(v=0)

# estimate roughly how much of stock growth is *not* from new construction for each type, based on mean of reasonable values from the data, i.e. remove negative values, and values >0.5
nn_US<-summary_US[,c("PcNewConsReturnsNotNew_SF","PcNewConsReturnsNotNew_MF","PcNewConsReturnsNotNew_MH")]
SF_growth_returns_pc_US<-mean0(nn_US$PcNewConsReturnsNotNew_SF) # 13% for SF 
MF_growth_returns_pc_US<-mean0(nn_US$PcNewConsReturnsNotNew_MF) # 20% for MF
MH_growth_returns_pc_US<-mean0(nn_US$PcNewConsReturnsNotNew_MH) # 23% for MH

nn_NE<-summary_NE[,c("PcNewConsReturnsNotNew_SF","PcNewConsReturnsNotNew_MF","PcNewConsReturnsNotNew_MH")]
SF_growth_returns_pc_NE<-mean0(nn_NE$PcNewConsReturnsNotNew_SF) # 17% for SF 
MF_growth_returns_pc_NE<-mean0(nn_NE$PcNewConsReturnsNotNew_MF) # 21% for MF
MH_growth_returns_pc_NE<-mean0(nn_NE$PcNewConsReturnsNotNew_MH) # 18% for MH

nn_MW<-summary_MW[,c("PcNewConsReturnsNotNew_SF","PcNewConsReturnsNotNew_MF","PcNewConsReturnsNotNew_MH")]
SF_growth_returns_pc_MW<-mean0(nn_MW$PcNewConsReturnsNotNew_SF) # 15% for SF 
MF_growth_returns_pc_MW<-mean0(nn_MW$PcNewConsReturnsNotNew_MF) # 21% for MF
MH_growth_returns_pc_MW<-mean0(nn_MW$PcNewConsReturnsNotNew_MH) # 21% for MH

nn_S<-summary_S[,c("PcNewConsReturnsNotNew_SF","PcNewConsReturnsNotNew_MF","PcNewConsReturnsNotNew_MH")]
SF_growth_returns_pc_S<-mean0(nn_S$PcNewConsReturnsNotNew_SF) # 14% for SF 
MF_growth_returns_pc_S<-mean0(nn_S$PcNewConsReturnsNotNew_MF) # 19% for MF
MH_growth_returns_pc_S<-mean0(nn_S$PcNewConsReturnsNotNew_MH) # 21% for MH
# these values are much lower in W for SF and MF
nn_W<-summary_W[,c("PcNewConsReturnsNotNew_SF","PcNewConsReturnsNotNew_MF","PcNewConsReturnsNotNew_MH")]
SF_growth_returns_pc_W<-mean0(nn_W$PcNewConsReturnsNotNew_SF) # 10% for SF 
MF_growth_returns_pc_W<-mean0(nn_W$PcNewConsReturnsNotNew_MF) # 13% for MF
MH_growth_returns_pc_W<-mean0(nn_W$PcNewConsReturnsNotNew_MH) # 24% for MH

scenarios<-c("Baseline","Hi_DR","Hi_MFS","Hi_DR_Hi_MFS")

# run loops to create the stock model dataframes (pre model) for each county
for (scen in 1:4) { print(scenarios[scen])
  smdf<-h20pc[,2:3]
for (i in 1:3142) { print(i)
projection<-ss[[2]][[i]]
names(projection)[1]<-"Year"
projection$GeoID<-ss$GEOID[i]

smdf[i,3]<-nest(create_sm_df(h20pc[i,],summary_US,projection,scen),cty_smdf = everything())
}
assign(paste("smdf_",scenarios[scen],sep=""),smdf)
}
save(smdf_Baseline,smdf_Hi_DR,smdf_Hi_MFS,smdf_Hi_DR_Hi_MFS,file = "HSM_Results/County_SMDF_Scenarios.RData")

# create files to hold the stock model output 'smop' for each scenario
smop_base<-h20pc[,2:3]
smop_hiDR<-h20pc[,2:3]
smop_hiMF<-h20pc[,2:3]
smop_hiDRMF<-h20pc[,2:3]
# now call stock model function to fill in data frame for every county
# some low growth counties 'lgc'
# lgc<-c(3,104,205,207,223,311,313,363,611,726,1230,1685,1885,2231,2624,2793) # Barbour AL, Maricopa, LA, Marin CA, San Diego CA, Litchfield CT, NHV, Miami-Dade, Cook IL, Hamilton IN, Franklin NE, Suffolk MA,Warren NY, Malheur OR, Harrix TX, Piute UT
lgc<-c(3,205)
for (k in 1:3142) {print(k)
# for (k in lgc) { print(k)
  df<-as.data.frame(smdf_Baseline[[3]][[k]]) # remove annoying list properties
  smop_base[k,3]<-nest(run_sm_cty(df),cty_sm=everything())
}

for (k in 1:3142) {print(k)
   df<-as.data.frame(smdf_Hi_DR[[3]][[k]]) # remove annoying list properties
   smop_hiDR[k,3]<-nest(run_sm_cty(df),cty_sm=everything())
}

for (k in 1:3142) {print(k)
   df<-as.data.frame(smdf_Hi_MFS[[3]][[k]]) # remove annoying list properties
   smop_hiMF[k,3]<-nest(run_sm_cty(df),cty_sm=everything())
}

for (k in 1:3142) {print(k)
  df<-as.data.frame(smdf_Hi_DR_Hi_MFS[[3]][[k]]) # remove annoying list properties
  smop_hiDRMF[k,3]<-nest(run_sm_cty(df),cty_sm=everything())
}

save(smop_base,smop_hiDR,smop_hiDRMF,smop_hiMF,file="HSM_Results/County_Scenario_SM_Results.RData")
