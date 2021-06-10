# Script to view sas AHS case history files in R, and develop various relations, data objects, and and parameters for use in Housing Stock Model (HSM) projections ##########
# Peter Berrill
# March 2021
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
# library(sas7bdat)
library(dplyr)
library(sjPlot)
library(ggplot2)
source('demrate_fn.R') # function to convert demolition rates
source('vacrate_fn.R') # function to convert vacancy rates
source('run_sm.R') # function to run stock model
# download the two sample case history files from the following link: http://www2.census.gov/programs-surveys/ahs/2017/Sample%20Case%20History.zip
# sch1<-read.sas7bdat("AHSNCaseHistory1985to2013.sas7bdat")
# sch2<-read.sas7bdat("AHSCaseHistory2015to2017.sas7bdat")
# save(sch1,file = "Data/CaseHistory8513.RData")
# save(sch2,file = "Data/CaseHistory1517.RData")
load("Data/CaseHistory8513.RData")
load("Data/CaseHistory1517.RData")
load("Data/AHS_All_1973_2019.RData") # compiled selected variables from AHS microdata 1973-2019

ahs_2015$CenReg<-"1-NE"
ahs_2015[ahs_2015$DIVISION==3|ahs_2015$DIVISION==4,]$CenReg<-"2-MW"
ahs_2015[ahs_2015$DIVISION==5|ahs_2015$DIVISION==6|ahs_2015$DIVISION==7,]$CenReg<-"3-S"
ahs_2015[ahs_2015$DIVISION>7,]$CenReg<-"4-W"

ahs_2015$REGION<-1
ahs_2015[ahs_2015$DIVISION==3|ahs_2015$DIVISION==4,]$REGION<-2
ahs_2015[ahs_2015$DIVISION==5|ahs_2015$DIVISION==6|ahs_2015$DIVISION==7,]$REGION<-3
ahs_2015[ahs_2015$DIVISION>7,]$REGION<-4

ahs_2017$CenReg<-"1-NE"
ahs_2017[ahs_2017$DIVISION==3|ahs_2017$DIVISION==4,]$CenReg<-"2-MW"
ahs_2017[ahs_2017$DIVISION==5|ahs_2017$DIVISION==6|ahs_2017$DIVISION==7,]$CenReg<-"3-S"
ahs_2017[ahs_2017$DIVISION>7,]$CenReg<-"4-W"

ahs_2017$REGION<-1
ahs_2017[ahs_2017$DIVISION==3|ahs_2017$DIVISION==4,]$REGION<-2
ahs_2017[ahs_2017$DIVISION==5|ahs_2017$DIVISION==6|ahs_2017$DIVISION==7,]$REGION<-3
ahs_2017[ahs_2017$DIVISION>7,]$REGION<-4

mhs<-read.csv('Data/MH_annual.csv') # annual MH shipments, 1959-2018, national, from MHS data https://www.census.gov/data/tables/time-series/econ/mhs/shipments.html
mhs2yr<-mhs
mhs2yr$MH2yr<-0
for (i in seq(1,61,2)) {mhs2yr$MH2yr[i]<-mhs2yr$MH[i]+mhs2yr$MH[i+1]} # convert annual shipments into bi-annual shipments

const<-read.csv('Data/AllCompletedRegions.csv') # construction of MF and SF housing, 4 regions, 1979-2019, from NRC data https://www.census.gov/construction/nrc/xls/co_cust.xls
con2yr<-const[const$Year>1984&const$Year<2019,] # convert annual construction into bi-annual construction
con2yr$SF2yr<-0
con2yr$MF2yr<-0
cn<-colnames(sch1)
cn2<-colnames(sch2)

# set up this script to do this job in a loop from 1987-2017, with a gap in 2015 because of a change in sample
# predefine summary df
years=c(seq(1987,2013,2),2017,2019) # survey years for tracking stock inventory change
yo<-years-2
# function to calculate bi-annual construction flows for SF and MF
for (i in 1:16) {for (j in 1:5) {con2yr[which(yo[i]==con2yr$Year)[j],]$SF2yr<-con2yr$SF[which(yo[i]==con2yr$Year)[j]]+con2yr$SF[which(yo[i]==con2yr$Year)[j]+1]; 
                  con2yr[which(yo[i]==con2yr$Year)[j],]$MF2yr<-con2yr$MF[which(yo[i]==con2yr$Year)[j]]+con2yr$MF[which(yo[i]==con2yr$Year)[j]+1]} }
con2yr<-con2yr[which(con2yr$SF2yr!=0),] # remove even years with no bi-annual data
mhs2yr<-mhs2yr[mhs2yr$Year>1984&mhs2yr$Year<2018,] # restrict the MH data to the same window as for SF and MF
mhs2yr<-mhs2yr[mhs2yr$Year!=2013,]
mhs2yr<-mhs2yr[which(mhs2yr$MH2yr!=0),]
# predefine columns of summary data frame. These will change substantially.
summary<-data.frame("Year"=c(years-2,2019),
                    "Population"=rep(0,17),
                    "Pop_Share_SF" = rep(0,17),
                    "Pop_Share_MF" = rep(0,17),
                    "Pop_Share_MH" = rep(0,17),
                    "Pop_SF" = rep(0,17),
                    "Pop_MF" = rep(0,17),
                    "Pop_MH" = rep(0,17),
                    "HH_Size" = rep(0,17),
                    "HH_Size_SF" = rep(0,17),
                    "HH_Size_MF" = rep(0,17),
                    "HH_Size_MH" = rep(0,17),
                    "Occ_Hous_Units" = rep(0,17),
                    "Occ_HU_SF" = rep(0,17),
                    "Occ_HU_MF" = rep(0,17),
                    "Occ_HU_MH" = rep(0,17),
                    "Vacancy_Ratio" = rep(0,17),
                    "VR_SF"  = rep(0,17),
                    "VR_MF"  = rep(0,17),
                    "VR_MH"  = rep(0,17),
                    "Tot_Hous_Units" = rep(0,17),
                    "Tot_HU_SF" = rep(0,17),
                    "Tot_HU_MF" = rep(0,17),
                    "Tot_HU_MH" = rep(0,17),
                    "Dem_SF" = rep(0,17),
                    "Dem_MF" = rep(0,17),
                    "Dem_MH" = rep(0,17),
                    "Con_SF" = rep(0,17),
                    "Con_MF" = rep(0,17),
                    "Con_MH" = rep(0,17),
                    "Dem_Rate_SF" = rep(0,17),
                    "Dem_Rate_MF" = rep(0,17),
                    "Dem_Rate_MH" = rep(0,17),
                    "Con_Rate_SF" = rep(0,17),
                    "Con_Rate_MF" = rep(0,17),
                    "Con_Rate_MH" = rep(0,17),
                    "Con_SF_calc" = rep(0,17),
                    "Con_MF_calc" = rep(0,17),
                    "Con_MH_calc" = rep(0,17)
)
summary_NE<-summary
summary_MW<-summary
summary_S<-summary
summary_W<-summary
summary_US<-summary
# extend years to 2021
years=c(seq(1987,2013,2),2017,2019,2021)
Vn_SF<-1.1 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-1.18 # estimate 'natural' vacancy rate of 1.18 for MF housing
Vn_MH<-1.26 # estimate 'natural' vacancy rate of 1.26 for MH housing
# PUT LARGE CREATE SUMMARY FUNCTION BEFORE MAIN LOOP
# This function creates the summary data frame for each region, given inputs of the basic input population and housing stock data,
# details about the houses exiting and returning to the stock, and the next ahs survey.
create_summary <- function(summary,loss_det,return_det,loss_dem_det,ahsnext) { # begin function
  if (i<16) { 
    UndCon<-return_det %>% filter(get(rsn0)==11) # how many units 'returned' that were under construction in previous survey
    TotRet<-return_det %>% filter(get(stat0)==4) # how many units in total returned from no-int in previous survey
    NotUndCon<-return_det %>% filter(get(stat0)==4 & get(rsn0)!=11) # how many units in total returned from no-int in previous survey, excluding those that were under construction in the previous survey
  }
  # calculate occ and vac stock by type and age cohort. type 1 = SF, 2 = MF, 3 = MH
  stock_sf_occ<- tapply(loss_det$WEIGHT*loss_det$IsOccupied,list(loss_det$type,loss_det$CohortGroup),sum)[1,]
  stock_sf_vac<-tapply(loss_det$WEIGHT*loss_det$IsVacant,list(loss_det$type,loss_det$CohortGroup),sum)[1,]
  stock_mf_occ<- tapply(loss_det$WEIGHT*loss_det$IsOccupied,list(loss_det$type,loss_det$CohortGroup),sum)[2,]
  stock_mf_vac<-tapply(loss_det$WEIGHT*loss_det$IsVacant,list(loss_det$type,loss_det$CohortGroup),sum)[2,]
  stock_mh_occ<- tapply(loss_det$WEIGHT*loss_det$IsOccupied,list(loss_det$type,loss_det$CohortGroup),sum)[3,]
  stock_mh_vac<-tapply(loss_det$WEIGHT*loss_det$IsVacant,list(loss_det$type,loss_det$CohortGroup),sum)[3,]
  # calculate demolition rates by type and occupancy/vacancy status, by 3 aggregate age groups
  if (i<16) {
  dem_rates_sf_occ<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum))[1,,1]
  dem_rates_sf_vac<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum))[1,,2]  # Annualized demolition rates, by cohort for sf homes
  dem_rates_mf_occ<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum))[2,,1] # Annualized demolition rates, by cohort for mf homes
  dem_rates_mf_vac<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum))[2,,2]
  dem_rates_mh_occ<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum))[3,,1] # Annualized demolition rates, by cohort for mf homes
  dem_rates_mh_vac<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroupAgg,loss_det$IsVacant),sum))[3,,2]
  
  dr_coh_sf_occ<-demrate_fn(yr0,stock_sf_occ,dem_rates_sf_occ)
  dr_coh_sf_vac<-demrate_fn(yr0,stock_sf_vac,dem_rates_sf_vac)
  dr_coh_mf_occ<-demrate_fn(yr0,stock_mf_occ,dem_rates_mf_occ)
  dr_coh_mf_vac<-demrate_fn(yr0,stock_mf_vac,dem_rates_mf_vac)
  dr_coh_mh_occ<-demrate_fn(yr0,stock_mh_occ,dem_rates_mh_occ)
  dr_coh_mh_vac<-demrate_fn(yr0,stock_mh_vac,dem_rates_mh_vac)
  }
  
  # summary data frame
  if (i<15) {
    if (min(loss_det$PER)<0) {loss_det[loss_det$PER<0,]$PER<-0}
    summary$Population[i]<-sum(loss_det$PER*loss_det$WEIGHT) # tot pop
    summary[i,c("Pop_SF","Pop_MF","Pop_MH")]<-tapply(loss_det$PER*loss_det$WEIGHT,loss_det$type,sum) # pop by house type
  }
  if (i>14) {
    if (min(loss_det$NUMPEOPLE)<0) {loss_det[loss_det$NUMPEOPLE<0,]$NUMPEOPLE<-0}
    summary$Population[i]<-sum(loss_det$NUMPEOPLE*loss_det$WEIGHT) # tot pop
    summary[i,c("Pop_SF","Pop_MF","Pop_MH")]<-tapply(loss_det$NUMPEOPLE*loss_det$WEIGHT,loss_det$type,sum) # pop by house type
  }
  summary[i,c("Pop_Share_SF","Pop_Share_MF","Pop_Share_MH")]<-summary[i,c("Pop_SF","Pop_MF","Pop_MH")]/summary$Population[i] # pop share by house type
  summary$HH_Size[i]<-summary$Population[i]/sum(loss_det$WEIGHT*loss_det$IsOccupied) # average hh size
  summary[i,c("HH_Size_SF","HH_Size_MF","HH_Size_MH")]<-summary[i,c("Pop_SF","Pop_MF","Pop_MH")]/tapply(loss_det$WEIGHT*loss_det$IsOccupied,loss_det$type,sum) # avg hh size by type
  summary$Occ_Hous_Units[i]<-sum(loss_det$WEIGHT*loss_det$IsOccupied)
  summary[i,c("Occ_HU_SF","Occ_HU_MF","Occ_HU_MH")]<-tapply(loss_det$WEIGHT*loss_det$IsOccupied,loss_det$type,sum) # occupied units by type, total units by occupied marker, by type
  summary$Vacancy_Ratio[i]<-sum(loss_det$WEIGHT)/sum(loss_det$WEIGHT*loss_det$IsOccupied) # vacancy ratio, total units/occupied units
  summary[i,c("VR_SF","VR_MF","VR_MH")]<-tapply(loss_det$WEIGHT,loss_det$type,sum)/tapply(loss_det$WEIGHT*loss_det$IsOccupied,loss_det$type,sum)
  summary$Tot_Hous_Units[i]<-sum(loss_det$WEIGHT)
  summary[i,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]<-tapply(loss_det$WEIGHT,loss_det$type,sum)
  if (i>1){ # define occupied and total stock growth (OSG,TSG), H factors, and changes in vacancy rate for each housing type
    summary[i-1,c("OSG_SF","OSG_MF","OSG_MH")]<-c(summary$Occ_HU_SF[i]-summary$Occ_HU_SF[i-1],summary$Occ_HU_MF[i]-summary$Occ_HU_MF[i-1],summary$Occ_HU_MH[i]-summary$Occ_HU_MH[i-1])
    summary[i-1,c("TSG_SF","TSG_MF","TSG_MH")]<-c(summary$Tot_HU_SF[i]-summary$Tot_HU_SF[i-1],summary$Tot_HU_MF[i]-summary$Tot_HU_MF[i-1],summary$Tot_HU_MH[i]-summary$Tot_HU_MH[i-1])
    summary[i-1,c("H_SF","H_MF","H_MH")]<-c(summary$TSG_SF[i-1]/(Vn_SF*summary$OSG_SF[i-1]),summary$TSG_MF[i-1]/(Vn_MF*summary$OSG_MF[i-1]),summary$TSG_MH[i-1]/(Vn_MH*summary$OSG_MH[i-1]))
    summary[i-1,c("dVR_SF","dVR_MF","dVR_MH")]<-c(summary$VR_SF[i]-summary$VR_SF[i-1],summary$VR_MF[i]-summary$VR_MF[i-1],summary$VR_MH[i]-summary$VR_MH[i-1])
  }
  summary[i,c("pcTot_HU_SF_Occ_p1940","pcTot_HU_SF_Occ_1940_59","pcTot_HU_SF_Occ_1960_79","pcTot_HU_SF_Occ_1980_99","pcTot_HU_SF_Occ_2000_09","pcTot_HU_SF_Occ_2010_19")]<-
    tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$CohortGroup,loss_det$IsVacant),sum)[1,,1]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[1] # pc of all SF units that are occupied, by cohort group 
  summary[i,c("pcTot_HU_SF_Vac_p1940","pcTot_HU_SF_Vac_1940_59","pcTot_HU_SF_Vac_1960_79","pcTot_HU_SF_Vac_1980_99","pcTot_HU_SF_Vac_2000_09","pcTot_HU_SF_Vac_2010_19")]<-
    tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$CohortGroup,loss_det$IsVacant),sum)[1,,2]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[1] # pc of all SF units that are vacant by cohort group 
  
  summary[i,c("pcTot_HU_MF_Occ_p1940","pcTot_HU_MF_Occ_1940_59","pcTot_HU_MF_Occ_1960_79","pcTot_HU_MF_Occ_1980_99","pcTot_HU_MF_Occ_2000_09","pcTot_HU_MF_Occ_2010_19")]<-
    tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$CohortGroup,loss_det$IsVacant),sum)[2,,1]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[2] # pc of all MF units that are occupied, by cohort group 
  summary[i,c("pcTot_HU_MF_Vac_p1940","pcTot_HU_MF_Vac_1940_59","pcTot_HU_MF_Vac_1960_79","pcTot_HU_MF_Vac_1980_99","pcTot_HU_MF_Vac_2000_09","pcTot_HU_MF_Vac_2010_19")]<-
    tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$CohortGroup,loss_det$IsVacant),sum)[2,,2]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[2] # pc of all MF units that are vacant by cohort group 
  
  summary[i,c("pcTot_HU_MH_Occ_p1940","pcTot_HU_MH_Occ_1940_59","pcTot_HU_MH_Occ_1960_79","pcTot_HU_MH_Occ_1980_99","pcTot_HU_MH_Occ_2000_09","pcTot_HU_MH_Occ_2010_19")]<-
    tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$CohortGroup,loss_det$IsVacant),sum)[3,,1]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[3] # pc of all MH units that are occupied, by cohort group 
  summary[i,c("pcTot_HU_MH_Vac_p1940","pcTot_HU_MH_Vac_1940_59","pcTot_HU_MH_Vac_1960_79","pcTot_HU_MH_Vac_1980_99","pcTot_HU_MH_Vac_2000_09","pcTot_HU_MH_Vac_2010_19")]<-
    tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$CohortGroup,loss_det$IsVacant),sum)[3,,2]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[3] # pc of all MH units that are vacant by cohort group 
  
  summary[i,c("pcTot_HU_MHOcc","pcTot_HU_MHVac")]<-tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$IsVacant),sum)[3,]/tapply(loss_det$WEIGHT,list(loss_det$type),sum)[3]
  if (i<16) { # here begins if statement
    summary[i,c("Dem_SF","Dem_MF","Dem_MH")]<-0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,loss_det$type,sum) # Annualized demolition, by type.Refers to total removals
    summary[i,c("Demol_SF","Demol_MF","Demol_MH")]<-0.5*tapply(loss_dem_det$WEIGHT*loss_dem_det$Demolished,loss_dem_det$type,sum) # Demolition by type (annual), refers only to demolition and MH movement
    summary[i,c("Demol_Rate_SF","Demol_Rate_MF","Demol_Rate_MH")]<-summary[i,c("Demol_SF","Demol_MF","Demol_MH")]/summary[i,c("Dem_SF","Dem_MF","Dem_MH")] # what percentage of removals are from pure demolition & MH moving 
    summary[i,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-summary[i,c("Dem_SF","Dem_MF","Dem_MH")]/summary[i,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")] # Annualized demolition rates, by type.
    summary[i,c("Dem_Rate_SF_0-19Occ","Dem_Rate_SF_20-59Occ","Dem_Rate_SF_60+Occ")]<-dem_rates_sf_occ  # Annualized demolition rates, by age range for sf homes
    summary[i,c("Dem_Rate_SF_0-19Vac","Dem_Rate_SF_20-59Vac","Dem_Rate_SF_60+Vac")]<-dem_rates_sf_vac  # Annualized demolition rates, by age range for sf homes
    summary[i,c("Dem_Rate_MF_0-19Occ","Dem_Rate_MF_20-59Occ","Dem_Rate_MF_60+Occ")]<-dem_rates_mf_occ # Annualized demolition rates, by age range for mf homes
    summary[i,c("Dem_Rate_MF_0-19Vac","Dem_Rate_MF_20-59Vac","Dem_Rate_MF_60+Vac")]<-dem_rates_mf_vac # Annualized demolition rates, by age range for mf homes
    summary[i,c("Dem_Rate_MH_0-19Occ","Dem_Rate_MH_20-59Occ","Dem_Rate_MH_60+Occ")]<-dem_rates_mh_occ # Annualized demolition rates, by age range for mh homes
    summary[i,c("Dem_Rate_MH_0-19Vac","Dem_Rate_MH_20-59Vac","Dem_Rate_MH_60+Vac")]<-dem_rates_mh_vac # Annualized demolition rates, by age range for mh homes
    summary[i,c("Dem_Rate_MHOcc","Dem_Rate_MHVac")]<-(0.5*tapply(loss_det$WEIGHT*loss_det$Demolished,list(loss_det$type,loss_det$IsVacant),sum)/tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$IsVacant),sum))[3,] # dem rates by occupancy status for MH
    
    summary[i,c("Dem_Rate_SF_Occ_p1940","Dem_Rate_SF_Occ_1940_59","Dem_Rate_SF_Occ_1960_79","Dem_Rate_SF_Occ_1980_99","Dem_Rate_SF_Occ_2000_09","Dem_Rate_SF_Occ_2010_19")]<-dr_coh_sf_occ  # Annualized demolition rates, by cohort for Occupied SF homes
    summary[i,c("Dem_Rate_SF_Vac_p1940","Dem_Rate_SF_Vac_1940_59","Dem_Rate_SF_Vac_1960_79","Dem_Rate_SF_Vac_1980_99","Dem_Rate_SF_Vac_2000_09","Dem_Rate_SF_Vac_2010_19")]<-dr_coh_sf_vac  # Annualized demolition rates, by cohort for Vacant SF homes
    summary[i,c("Dem_Rate_MF_Occ_p1940","Dem_Rate_MF_Occ_1940_59","Dem_Rate_MF_Occ_1960_79","Dem_Rate_MF_Occ_1980_99","Dem_Rate_MF_Occ_2000_09","Dem_Rate_MF_Occ_2010_19")]<-dr_coh_mf_occ  # Annualized demolition rates, by cohort for Occupied MF homes
    summary[i,c("Dem_Rate_MF_Vac_p1940","Dem_Rate_MF_Vac_1940_59","Dem_Rate_MF_Vac_1960_79","Dem_Rate_MF_Vac_1980_99","Dem_Rate_MF_Vac_2000_09","Dem_Rate_MF_Vac_2010_19")]<-dr_coh_mf_vac  # Annualized demolition rates, by cohort for Vacant MF homes
    summary[i,c("Dem_Rate_MH_Occ_p1940","Dem_Rate_MH_Occ_1940_59","Dem_Rate_MH_Occ_1960_79","Dem_Rate_MH_Occ_1980_99","Dem_Rate_MH_Occ_2000_09","Dem_Rate_MH_Occ_2010_19")]<-dr_coh_mh_occ  # Annualized demolition rates, by cohort for Occupied MH homes
    summary[i,c("Dem_Rate_MH_Vac_p1940","Dem_Rate_MH_Vac_1940_59","Dem_Rate_MH_Vac_1960_79","Dem_Rate_MH_Vac_1980_99","Dem_Rate_MH_Vac_2000_09","Dem_Rate_MH_Vac_2010_19")]<-dr_coh_mh_vac  # Annualized demolition rates, by cohort for Vacant MH homes
    
    summary[i,c("Dem_SF_Occ_p1940","Dem_SF_Occ_1940_59","Dem_SF_Occ_1960_79","Dem_SF_Occ_1980_99","Dem_SF_Occ_2000_09","Dem_SF_Occ_2010_19")]<-summary$Tot_HU_SF[i]*
      summary[i,c("pcTot_HU_SF_Occ_p1940","pcTot_HU_SF_Occ_1940_59","pcTot_HU_SF_Occ_1960_79","pcTot_HU_SF_Occ_1980_99","pcTot_HU_SF_Occ_2000_09","pcTot_HU_SF_Occ_2010_19")]*
      summary[i,c("Dem_Rate_SF_Occ_p1940","Dem_Rate_SF_Occ_1940_59","Dem_Rate_SF_Occ_1960_79","Dem_Rate_SF_Occ_1980_99","Dem_Rate_SF_Occ_2000_09","Dem_Rate_SF_Occ_2010_19")]
    
    summary[i,c("Dem_SF_Vac_p1940","Dem_SF_Vac_1940_59","Dem_SF_Vac_1960_79","Dem_SF_Vac_1980_99","Dem_SF_Vac_2000_09","Dem_SF_Vac_2010_19")]<-summary$Tot_HU_SF[i]*
      summary[i,c("pcTot_HU_SF_Vac_p1940","pcTot_HU_SF_Vac_1940_59","pcTot_HU_SF_Vac_1960_79","pcTot_HU_SF_Vac_1980_99","pcTot_HU_SF_Vac_2000_09","pcTot_HU_SF_Vac_2010_19")]*
      summary[i,c("Dem_Rate_SF_Vac_p1940","Dem_Rate_SF_Vac_1940_59","Dem_Rate_SF_Vac_1960_79","Dem_Rate_SF_Vac_1980_99","Dem_Rate_SF_Vac_2000_09","Dem_Rate_SF_Vac_2010_19")]
    
    summary[i,c("Dem_MF_Occ_p1940","Dem_MF_Occ_1940_59","Dem_MF_Occ_1960_79","Dem_MF_Occ_1980_99","Dem_MF_Occ_2000_09","Dem_MF_Occ_2010_19")]<-summary$Tot_HU_MF[i]*
      summary[i,c("pcTot_HU_MF_Occ_p1940","pcTot_HU_MF_Occ_1940_59","pcTot_HU_MF_Occ_1960_79","pcTot_HU_MF_Occ_1980_99","pcTot_HU_MF_Occ_2000_09","pcTot_HU_MF_Occ_2010_19")]*
      summary[i,c("Dem_Rate_MF_Occ_p1940","Dem_Rate_MF_Occ_1940_59","Dem_Rate_MF_Occ_1960_79","Dem_Rate_MF_Occ_1980_99","Dem_Rate_MF_Occ_2000_09","Dem_Rate_MF_Occ_2010_19")]
    
    summary[i,c("Dem_MF_Vac_p1940","Dem_MF_Vac_1940_59","Dem_MF_Vac_1960_79","Dem_MF_Vac_1980_99","Dem_MF_Vac_2000_09","Dem_MF_Vac_2010_19")]<-summary$Tot_HU_MF[i]*
      summary[i,c("pcTot_HU_MF_Vac_p1940","pcTot_HU_MF_Vac_1940_59","pcTot_HU_MF_Vac_1960_79","pcTot_HU_MF_Vac_1980_99","pcTot_HU_MF_Vac_2000_09","pcTot_HU_MF_Vac_2010_19")]*
      summary[i,c("Dem_Rate_MF_Vac_p1940","Dem_Rate_MF_Vac_1940_59","Dem_Rate_MF_Vac_1960_79","Dem_Rate_MF_Vac_1980_99","Dem_Rate_MF_Vac_2000_09","Dem_Rate_MF_Vac_2010_19")]
    
    summary[i,c("Dem_MH_Occ_p1940","Dem_MH_Occ_1940_59","Dem_MH_Occ_1960_79","Dem_MH_Occ_1980_99","Dem_MH_Occ_2000_09","Dem_MH_Occ_2010_19")]<-summary$Tot_HU_MH[i]*
      summary[i,c("pcTot_HU_MH_Occ_p1940","pcTot_HU_MH_Occ_1940_59","pcTot_HU_MH_Occ_1960_79","pcTot_HU_MH_Occ_1980_99","pcTot_HU_MH_Occ_2000_09","pcTot_HU_MH_Occ_2010_19")]*
      summary[i,c("Dem_Rate_MH_Occ_p1940","Dem_Rate_MH_Occ_1940_59","Dem_Rate_MH_Occ_1960_79","Dem_Rate_MH_Occ_1980_99","Dem_Rate_MH_Occ_2000_09","Dem_Rate_MH_Occ_2010_19")]
    
    summary[i,c("Dem_MH_Vac_p1940","Dem_MH_Vac_1940_59","Dem_MH_Vac_1960_79","Dem_MH_Vac_1980_99","Dem_MH_Vac_2000_09","Dem_MH_Vac_2010_19")]<-summary$Tot_HU_MH[i]*
      summary[i,c("pcTot_HU_MH_Vac_p1940","pcTot_HU_MH_Vac_1940_59","pcTot_HU_MH_Vac_1960_79","pcTot_HU_MH_Vac_1980_99","pcTot_HU_MH_Vac_2000_09","pcTot_HU_MH_Vac_2010_19")]*
      summary[i,c("Dem_Rate_MH_Vac_p1940","Dem_Rate_MH_Vac_1940_59","Dem_Rate_MH_Vac_1960_79","Dem_Rate_MH_Vac_1980_99","Dem_Rate_MH_Vac_2000_09","Dem_Rate_MH_Vac_2010_19")]
    # difference between vacancy rates if vacancy rates were the same for all age groups, vs how they actually are. 
    # positive values mean vacancies turn out lower than if assuming same rates for all ages. Negative values mean vacancies are higher than if assuming the same among all age groups
    summary[i,c("VacDiff_SF_0_10","VacDiff_SF_11_30","VacDiff_SF_31_60","VacDiff_SF_61")]<-(tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroup),sum)[1,]*((summary$VR_SF[i]-1)/summary$VR_SF[i])/summary$Tot_HU_SF[i])-
      (tapply(loss_det$WEIGHT*loss_det$IsVacant,list(loss_det$type,loss_det$ageGroup),sum)[1,]/summary$Tot_HU_SF[i])
    summary[i,c("VacDiff_MF_0_10","VacDiff_MF_11_30","VacDiff_MF_31_60","VacDiff_MF_61")]<-(tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroup),sum)[2,]*((summary$VR_MF[i]-1)/summary$VR_MF[i])/summary$Tot_HU_MF[i])-
      (tapply(loss_det$WEIGHT*loss_det$IsVacant,list(loss_det$type,loss_det$ageGroup),sum)[2,]/summary$Tot_HU_MF[i])
    summary[i,c("VacDiff_MH_0_10","VacDiff_MH_11_30","VacDiff_MH_31_60","VacDiff_MH_61")]<-(tapply(loss_det$WEIGHT,list(loss_det$type,loss_det$ageGroup),sum)[3,]*((summary$VR_MH[i]-1)/summary$VR_MH[i])/summary$Tot_HU_MH[i])-
      (tapply(loss_det$WEIGHT*loss_det$IsVacant,list(loss_det$type,loss_det$ageGroup),sum)[3,]/summary$Tot_HU_MH[i])  

    summary[i,c("Con_SF_calc","Con_MF_calc","Con_MH_calc")]<-0.5*(tapply(ahsnext$WEIGHT,ahsnext$type,sum)-tapply(loss_det$WEIGHT,loss_det$type,sum)+tapply(loss_det$WEIGHT*loss_det$Demolished,loss_det$type,sum)) # calculate apparent annual construction by type, based on the change in total stock plus demolition
    
    summary[i,"Dem_SF_Coh"]<-sum(summary[i,c("Dem_SF_Occ_p1940","Dem_SF_Occ_1940_59","Dem_SF_Occ_1960_79","Dem_SF_Occ_1980_99","Dem_SF_Occ_2000_09","Dem_SF_Occ_2010_19",
                                             "Dem_SF_Vac_p1940","Dem_SF_Vac_1940_59","Dem_SF_Vac_1960_79","Dem_SF_Vac_1980_99","Dem_SF_Vac_2000_09","Dem_SF_Vac_2010_19")],na.rm = TRUE)
    summary[i,"Dem_MF_Coh"]<-sum(summary[i,c("Dem_MF_Occ_p1940","Dem_MF_Occ_1940_59","Dem_MF_Occ_1960_79","Dem_MF_Occ_1980_99","Dem_MF_Occ_2000_09","Dem_MF_Occ_2010_19",
                                             "Dem_MF_Vac_p1940","Dem_MF_Vac_1940_59","Dem_MF_Vac_1960_79","Dem_MF_Vac_1980_99","Dem_MF_Vac_2000_09","Dem_MF_Vac_2010_19")],na.rm = TRUE)
    summary[i,"Dem_MH_Coh"]<-sum(summary[i,c("Dem_MH_Occ_p1940","Dem_MH_Occ_1940_59","Dem_MH_Occ_1960_79","Dem_MH_Occ_1980_99","Dem_MH_Occ_2000_09","Dem_MH_Occ_2010_19",
                                             "Dem_MH_Vac_p1940","Dem_MH_Vac_1940_59","Dem_MH_Vac_1960_79","Dem_MH_Vac_1980_99","Dem_MH_Vac_2000_09","Dem_MH_Vac_2010_19")],na.rm = TRUE)
    if (i<15) {      
      summary[i,c("Con_SF","Con_MF","Con_MH")]<-tapply(ahsnext[ahsnext$BUILT==builtyr,]$WEIGHT,ahsnext[ahsnext$BUILT==builtyr,]$type,sum) # measured new construction by type, based on the next survey
      summary[i,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-summary[i,c("Con_SF_calc","Con_MF_calc","Con_MH_calc")]/summary[i,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
    }
    if (i==15) {      
      summary[i,c("Con_SF","Con_MF","Con_MH")]<-0.5*(tapply(ahsnext[ahsnext$YRBUILT==2010,]$WEIGHT,ahsnext[ahsnext$YRBUILT==2010,]$type,sum)[1:3]-tapply(loss_det[loss_det$YRBUILT==2010,]$WEIGHT,loss_det[loss_det$YRBUILT==2010,]$type,sum)[1:3]) # approximation (as individual years not included in 2015/2017 surveys) for new construction, assumes no 2010 houses were demolished between 2015-2017
      summary[i,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-summary[i,c("Con_SF_calc","Con_MF_calc","Con_MH_calc")]/summary[i,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
    }
    # how much of returns this year are houses that were under construction in previous survey, for SF and MF
    summary[i+1,c("PcReturnsUnderCons_SF","PcReturnsUnderCons_MF")]<-tapply(UndCon$WEIGHT,UndCon$type,sum)[1:2]/tapply(TotRet$WEIGHT,TotRet$type,sum)[1:2]
    # how much of apparent new construction over the next two years is houses that were under construction in previous survey, for SF and MF
    summary[i+1,c("PcNewConsUndCons_SF","PcNewConsUndCons_MF")]<-0.5*tapply(UndCon$WEIGHT,UndCon$type,sum)[1:2]/c(summary$Con_SF_calc[i],summary$Con_MF_calc[i]) # 
    # for each type, excluding those that were under construction in previous survey, what percent of returns fall into each age group?
    summary[i+1,c("pcRet_SF_019","pcRet_SF_2059","pcRet_SF_60")]<-tapply(NotUndCon$WEIGHT,list(NotUndCon$ageGroupAgg,NotUndCon$type),sum)[,1]/ tapply(NotUndCon$WEIGHT,list(NotUndCon$type),sum)[1]
    summary[i+1,c("pcRet_MF_019","pcRet_MF_2059","pcRet_MF_60")]<-tapply(NotUndCon$WEIGHT,list(NotUndCon$ageGroupAgg,NotUndCon$type),sum)[,2]/ tapply(NotUndCon$WEIGHT,list(NotUndCon$type),sum)[2]
    summary[i+1,c("pcRet_MH_019","pcRet_MH_2059","pcRet_MH_60")]<-tapply(NotUndCon$WEIGHT,list(NotUndCon$ageGroupAgg,NotUndCon$type),sum)[,3]/ tapply(NotUndCon$WEIGHT,list(NotUndCon$type),sum)[3]
    # how much of apparent new construction in the next two years is from houses returning to the stock, for any reason (including being under construction in the first survey)?
    summary[i+1,c("pcNew_SF_Ret","pcNew_MF_Ret","pcNew_MH_Ret")]<- 0.5*tapply(TotRet$WEIGHT,TotRet$type,sum)/c(summary$Con_SF_calc[i],summary$Con_MF_calc[i],summary$Con_MH_calc[i])
    # how much of apparent new construction is from returns that are not newly built, for SF and MF
    summary[i+1,c("PcNewConsReturnsNotNew_SF","PcNewConsReturnsNotNew_MF","PcNewConsReturnsNotNew_MH")]<-0.5*tapply(NotUndCon$WEIGHT,NotUndCon$type,sum)/c(summary$Con_SF_calc[i],summary$Con_MF_calc[i],summary$Con_MH_calc[i])
  } # here ends if statement
  if (i==16) {    # new addition to make use of ahs_2019 data to calculate new construction in 2017
    summary[i,c("Con_SF","Con_MF","Con_MH")]<-tapply(ahsnext[ahsnext$YRBUILT==2017,]$WEIGHT,ahsnext[ahsnext$YRBUILT==2017,]$type,sum) 
    summary[i,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-summary[i,c("Con_SF_calc","Con_MF_calc","Con_MH_calc")]/summary[i,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
  }
  
  summary
} # end large function

# start loop to create summary files for US and four census regions, indicating stock 'loss' and 'addition' rates
for (i in 1:17) {print (i)
  yr<-years[i]
  yr0<-yr-2
  yrstr<-as.character(yr)
  if (i<15) {
  statY<-paste("STATUS",substr(as.character(yr),3,4),sep = "") ## status in year first identified as demolished
  stat0<-paste("STATUS",substr(as.character(yr0),3,4),sep = "") ## status in previous year
  }
  if (i==15) {
    statY<-paste("INTSTATUS",substr(as.character(yr),3,4),sep = "") ## status in year first identified as demolished
    stat0<-paste("INTSTATUS",substr(as.character(yr0),3,4),sep = "") ## status in previous year
  }
  rsn<-paste("NOINT",substr(as.character(yr),3,4),sep = "") # reason for no interview 
  rsn0<-paste("NOINT",substr(as.character(yr-2),3,4),sep = "") # reason for no interview two years ago
  # in the next step single out units who are demolished in year Y but still in stock two years previous, this doesn't work when i = 16, years = 2019
  if (i<15) {
  loss<-sch1 %>% filter(get(statY) == "4" & (get(stat0)=="1"|get(stat0)=="2"|get(stat0)=="3") & get(rsn)>6 & get(rsn)<42 ) #  review this, to consider higher loss of MF, reason also equals 31
  lossdem<-sch1 %>% filter(get(statY) == "4" & (get(stat0)=="1"|get(stat0)=="2"|get(stat0)=="3") & (get(rsn)==30|get(rsn)==31))
  loss$CONTROL<-as.numeric(loss$CONTROL) 
  lossdem$CONTROL<-as.numeric(lossdem$CONTROL) 
  
  return<-sch1 %>% filter(get(stat0) == "4" & (get(statY)=="1"|get(statY)=="2"|get(statY)=="3")  & get(rsn0)>6 & get(rsn0)<42)
  return$CONTROL<-as.numeric(return$CONTROL)
  }
  if (i==15) {
    loss<-sch2 %>% filter(get(statY) == "4" & (get(stat0)=="1"|get(stat0)=="2"|get(stat0)=="3") & get(rsn)>6)
    lossdem<-sch2 %>% filter(get(statY) == "4" & (get(stat0)=="1"|get(stat0)=="2"|get(stat0)=="3") & (get(rsn)==30|get(rsn)==31))
    loss$CONTROL<-as.numeric(loss$CONTROL)
    lossdem$CONTROL<-as.numeric(lossdem$CONTROL)
    
    return<-sch2 %>% filter(get(stat0) == "4" & (get(statY)=="1"|get(statY)=="2"|get(statY)=="3")  & get(rsn0)>6 & get(rsn0)<42)
    return$CONTROL<-as.numeric(return$CONTROL)
  }

  ahsname<-paste("ahs_",as.character(yr0),sep = "")
  # need the year y ahs for calculating construction from year 0
  if (i < 16) { # here starts if statement
  ahsnext<-get(paste("ahs_",as.character(yr),sep = ""))
  ahsnext<-ahsnext[ahsnext$type>0&ahsnext$type<4,] # remove unallocated and other types
  # join e.g. ahs_1985 and units which were identified as lost in 1987
  loss_det<-left_join(get(ahsname),loss,by="CONTROL")
  loss_dem_det<-left_join(get(ahsname),lossdem,by="CONTROL")
  
  return_det<-left_join(ahsnext,return,by="CONTROL")
  } # here ends if statement

  if (i==16) {loss_det<-ahs_2017; # solution for year 2017
  ahsnext<-get(paste("ahs_",as.character(yr),sep = ""));
  ahsnext<-ahsnext[ahsnext$type>0&ahsnext$type<4,] # remove unallocated and other types
  } 
  if (i==17) {loss_det<-ahs_2019} # solution for year 2019
  loss_det$count<-1
  loss_det<-loss_det[loss_det$type>0,] # remove the unallocated housing types.
  loss_det<-loss_det[loss_det$type<4,] # remove other from the analysis

  if (i < 16) { # here starts if statement applying only to yr0 < 2017
  loss_dem_det<-loss_dem_det[loss_dem_det$type>0,] # remove the unallocated housing types. this doesn't apply to yr0 = 2017 or 2019
  loss_dem_det<-loss_dem_det[loss_dem_det$type<4,] # remove other from the analysis
    
  loss_det$Demolished<-loss_dem_det$Demolished<-0
  return_det$Returning<-0
  # identify units that were existing in this survey that were demolished before the next survey
  loss_det[which(loss_det[,which(colnames(loss_det)==statY)]==4),"Demolished"]<-1 # there may be a simpler way of doing this, but I couldn't figure that out
  loss_dem_det[which(loss_dem_det[,which(colnames(loss_dem_det)==statY)]==4),"Demolished"]<-1 # there may be a simpler way of doing this, but I couldn't figure that out
  
  return_det[which(return_det[,which(colnames(return_det)==stat0)]==4),"Returning"]<-1
  } # here ends if statement
  
  # create a factor with type names
  loss_det$HType<-"1-SF"
  loss_det[loss_det$type==2,]$HType<-"2-MF"
  loss_det[loss_det$type==3,]$HType<-"3-MH"
  loss_det$HType<-as.factor(loss_det$HType)
  # define regions
  if (i<15) {
  loss_det$CenReg<-"1-NE"
  loss_det[loss_det$REGION==2,]$CenReg<-"2-MW"
  loss_det[loss_det$REGION==3,]$CenReg<-"3-S"
  loss_det[loss_det$REGION==4,]$CenReg<-"4-W"
  loss_dem_det$CenReg<-"1-NE"
  loss_dem_det[loss_dem_det$REGION==2,]$CenReg<-"2-MW"
  loss_dem_det[loss_dem_det$REGION==3,]$CenReg<-"3-S"
  loss_dem_det[loss_dem_det$REGION==4,]$CenReg<-"4-W"
  ahsnext$CenReg<-"1-NE"
  ahsnext[ahsnext$REGION==2,]$CenReg<-"2-MW"
  ahsnext[ahsnext$REGION==3,]$CenReg<-"3-S"
  ahsnext[ahsnext$REGION==4,]$CenReg<-"4-W"
  }
  if (i==15) {
      loss_det$CenReg<-"1-NE"
      loss_det[loss_det$DIVISION==3|loss_det$DIVISION==4,]$CenReg<-"2-MW"
      loss_det[loss_det$DIVISION==5|loss_det$DIVISION==6|loss_det$DIVISION==7,]$CenReg<-"3-S"
      loss_det[loss_det$DIVISION>7,]$CenReg<-"4-W"
      loss_dem_det$CenReg<-"1-NE"
      loss_dem_det[loss_dem_det$DIVISION==3|loss_dem_det$DIVISION==4,]$CenReg<-"2-MW"
      loss_dem_det[loss_dem_det$DIVISION==5|loss_dem_det$DIVISION==6|loss_dem_det$DIVISION==7,]$CenReg<-"3-S"
      loss_dem_det[loss_dem_det$DIVISION>7,]$CenReg<-"4-W"
      ahsnext$CenReg<-"1-NE"
      ahsnext[ahsnext$DIVISION==3|ahsnext$DIVISION==4,]$CenReg<-"2-MW"
      ahsnext[ahsnext$DIVISION==5|ahsnext$DIVISION==6|ahsnext$DIVISION==7,]$CenReg<-"3-S"
      ahsnext[ahsnext$DIVISION>7,]$CenReg<-"4-W"
  }
  
  if (i==16) {
    loss_det$CenReg<-"1-NE"
    loss_det[loss_det$DIVISION==3|loss_det$DIVISION==4,]$CenReg<-"2-MW"
    loss_det[loss_det$DIVISION==5|loss_det$DIVISION==6|loss_det$DIVISION==7,]$CenReg<-"3-S"
    loss_det[loss_det$DIVISION>7,]$CenReg<-"4-W"

    ahsnext$CenReg<-"1-NE"
    ahsnext[ahsnext$DIVISION==3|ahsnext$DIVISION==4,]$CenReg<-"2-MW"
    ahsnext[ahsnext$DIVISION==5|ahsnext$DIVISION==6|ahsnext$DIVISION==7,]$CenReg<-"3-S"
    ahsnext[ahsnext$DIVISION>7,]$CenReg<-"4-W"
  }
  
  if (i==17) {
    loss_det$CenReg<-"1-NE"
    loss_det[loss_det$DIVISION==3|loss_det$DIVISION==4,]$CenReg<-"2-MW"
    loss_det[loss_det$DIVISION==5|loss_det$DIVISION==6|loss_det$DIVISION==7,]$CenReg<-"3-S"
    loss_det[loss_det$DIVISION>7,]$CenReg<-"4-W"
  }
    
  loss_det$CohortGroup<-loss_det$cohort
  loss_det[loss_det$cohort=="<1920" | loss_det$cohort=="1920s" | loss_det$cohort=="1930s",]$CohortGroup<-"1890.1939"
  loss_det[loss_det$cohort=="1940s" | loss_det$cohort=="1950s",]$CohortGroup<-"1940.1959"
  loss_det[loss_det$cohort=="1960s" | loss_det$cohort=="1970s",]$CohortGroup<-"1960.1979"
  loss_det[loss_det$cohort=="1980s" | loss_det$cohort=="1990s",]$CohortGroup<-"1980.1999"
  if (yr0>1999){loss_det[loss_det$cohort=="2000s",]$CohortGroup<-"2000.2009"}
  if (yr0>2009){loss_det[loss_det$cohort=="2010s",]$CohortGroup<-"2010.2019"}
  loss_det$CohortGroup<-as.factor(loss_det$CohortGroup)
  levels(loss_det$CohortGroup) <- c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
  # original are groups of 0-10, 11-30, 31-60, 60+. 
  loss_det$ageGroup<-"0-10"
  loss_det[loss_det$age>10&loss_det$age<31,]$ageGroup<-"11-30"
  loss_det[loss_det$age>30&loss_det$age<61,]$ageGroup<-"31-60"
  loss_det[loss_det$age>60,]$ageGroup<-"60+"
  loss_det$ageGroup<-as.factor(loss_det$ageGroup)
  
  loss_det$ageGroupAgg<-"0-19"
  loss_det[loss_det$age>19&loss_det$age<60,]$ageGroupAgg<-"20-59"
  loss_det[loss_det$age>59,]$ageGroupAgg<-"60+"
  loss_det$ageGroupAgg<-as.factor(loss_det$ageGroupAgg)
  # estimate another age range grouping, more useful for calculating vacancy by age group
  loss_det$ageGroupVac<-"0-5"
  loss_det[loss_det$age>5&loss_det$age<50,]$ageGroupVac<-"6-49"
  loss_det[loss_det$age>49,]$ageGroupVac<-"50+"
  loss_det$ageGroupVac<-as.factor(loss_det$ageGroupVac)
 
  # do same age calcs and groupings for returning houses, again age is already pre-calculated with more detail, so commenting that section out
  if (i<16) {
  # original are groups of 0-10, 11-30, 31-60, 60+
  return_det$ageGroup<-"0-10"
  return_det[return_det$age>10&return_det$age<31,]$ageGroup<-"11-30"
  return_det[return_det$age>30&return_det$age<61,]$ageGroup<-"31-60"
  return_det[return_det$age>60,]$ageGroup<-"60+"
  return_det$ageGroup<-as.factor(return_det$ageGroup)
  # aggregated age groups of 0-19, 20-59, and 60+. These are used for the demrate function, and for checking the age groups of 'returning' houses
  return_det$ageGroupAgg<-"0-19"
  return_det[return_det$age>19&return_det$age<60,]$ageGroupAgg<-"20-59"
  return_det[return_det$age>59,]$ageGroupAgg<-"60+"
  return_det$ageGroupAgg<-as.factor(return_det$ageGroupAgg)
  }
  
  loss_det[is.na(loss_det)]<-0
  if (i==10) { # adjust for change of MH sample between 2003-2005, see https://www.huduser.gov/Datasets/ahs/AHS_%20FAQ_9-9-08.pdf pg. 16
    loss_det[loss_det$type==3 & loss_det$NOINT05==38,]$Demolished<-0
  }
  if (i<16) {lost<-loss_det %>% filter(get(statY)==4) }# here begins/ends if statement
  
  loss_det$IsOccupied<-0
  loss_det[loss_det$VACANCY<0,]$IsOccupied<-1
  loss_det$IsVacant<-1
  loss_det[loss_det$VACANCY<0,]$IsVacant<-0
  
  if (i<6){
  builtyr<-as.numeric(substr(as.character(yr0),3,4))
  }
  if (i>5 & i<16){
    builtyr<-yr0
  }
 
  summary_US<-create_summary(summary_US,loss_det,return_det,loss_dem_det,ahsnext)
  
  loss_det_NE<-loss_det[loss_det$CenReg=="1-NE",]
  return_det_NE<-return_det[return_det$REGION==1,]
  loss_dem_det_NE<-loss_dem_det[loss_dem_det$CenReg=="1-NE",]
  ahsnext_NE<-ahsnext[ahsnext$CenReg=="1-NE",]
  ahsnext_NE$type<-as.factor(ahsnext_NE$type) # necessary?>? yes
  summary_NE<-create_summary(summary_NE,loss_det_NE,return_det_NE,loss_dem_det_NE,ahsnext_NE)
  
  loss_det_MW<-loss_det[loss_det$CenReg=="2-MW",]
  loss_dem_det_MW<-loss_dem_det[loss_dem_det$CenReg=="2-MW",]
  return_det_MW<-return_det[return_det$REGION==2,]
  ahsnext_MW<-ahsnext[ahsnext$CenReg=="2-MW",]
  ahsnext_MW$type<-as.factor(ahsnext_MW$type) 
  summary_MW<-create_summary(summary_MW,loss_det_MW,return_det_MW,loss_dem_det_MW,ahsnext_MW)
  
  loss_det_S<-loss_det[loss_det$CenReg=="3-S",]
  loss_dem_det_S<-loss_dem_det[loss_dem_det$CenReg=="3-S",]
  return_det_S<-return_det[return_det$REGION==3,]
  ahsnext_S<-ahsnext[ahsnext$CenReg=="3-S",]
  ahsnext_S$type<-as.factor(ahsnext_S$type) 
  summary_S<-create_summary(summary_S,loss_det_S,return_det_S,loss_dem_det_S,ahsnext_S)
  
  loss_det_W<-loss_det[loss_det$CenReg=="4-W",]
  loss_dem_det_W<-loss_dem_det[loss_dem_det$CenReg=="4-W",]
  return_det_W<-return_det[return_det$REGION==4,]
  ahsnext_W<-ahsnext[ahsnext$CenReg=="4-W",]
  ahsnext_W$type<-as.factor(ahsnext_W$type) 
  summary_W<-create_summary(summary_W,loss_det_W,return_det_W,loss_dem_det_W,ahsnext_W)
  
  
} #end to main statement, incorporating regions 
save(summary_US,summary_NE,summary_MW,summary_S,summary_W,file='Intermediate_results/summaries.Rdata') # this data object will be called by the hsm_cty script
# load('Intermediate_results/summaries.Rdata')

# define linear models for H (growth adjustment factor) as a function of dVR for positive occupied and total stock growth
# H is a measure for how much Total stock growth exceeds vacancy adjusted occupied stock growth (OSG). H is termed 'GF' in the manuscript and SI files.
# define linear models for construction as a function of OSG for negative values of OSG
# It will be used to estimate H (how much construction is less than what would be expected by VOSG in cases when the vacancy rate declines.) This will help to predict construction quantities when there is "too much" vacancy
relSF<-summary_US[1:16,c("OSG_SF","TSG_SF","H_SF","dVR_SF","Con_Rate_SF","Tot_HU_SF")]
rSF<-relSF[relSF$OSG_SF>0&relSF$TSG_SF>0,] # this model can be applied to instances when neither occupied nor total stock declines.
lmSF<-lm(H_SF~dVR_SF,data=rSF)
windows()
plot(H_SF~dVR_SF,data=rSF,pch=16,col="blue",main="Growth Factor vs change in Vacancy Factor, Single-Family",xlab="dVF (change in Vacancy Factor)",ylab="Growth Factor")
abline(lmSF)
abline(h=0)
abline(v=0)

relSF$OSG_Rate_SF<-0.5*relSF$OSG_SF/relSF$Tot_HU_SF # annual occupied stock growth divided by total stock
lmSFc<-lm(Con_Rate_SF~OSG_Rate_SF,data=relSF[1:15,]) # linear model of construction rate based on occupied stock growth rate. rates calculated by dividing by total stock.
windows()
plot(Con_Rate_SF~OSG_Rate_SF,data=relSF[1:15,],pch=16,col="blue",main="Construction Rate vs OSG Rate, Single-Family",xlab="Occ Stock Growth Rate",ylab="Construction Rate")
abline(lmSFc)
abline(h=0)
abline(v=0)

relMF<-summary_US[1:16,c("OSG_MF","TSG_MF","H_MF","dVR_MF","Con_Rate_MF","Tot_HU_MF")]
rMF<-relMF[relMF$OSG_MF&relMF$TSG_MF>0&relMF$H_MF<2.5,]
lmMF<-lm(H_MF~dVR_MF,data=rMF)
windows()
plot(H_MF~dVR_MF,data=rMF,pch=16,col="blue",main="Growth Factor vs change in Vacancy Factor, Multifamily",xlab="dVF (change in Vacancy Factor)",ylab="Growth Factor")
abline(lmMF)
abline(h=0)
abline(v=0)

relMF$OSG_Rate_MF<-0.5*relMF$OSG_MF/relMF$Tot_HU_MF
lmMFc<-lm(Con_Rate_MF~OSG_Rate_MF,data=relMF[1:15,])
windows()
plot(Con_Rate_MF~OSG_Rate_MF,data=relMF[1:15,],pch=16,col="blue",main="Construction Rate vs OSG Rate, Multifamily",xlab="Occ Stock Growth Rate",ylab="Construction Rate")
abline(lmMFc)
abline(h=0)
abline(v=0)

relMH<-summary_US[1:16,c("OSG_MH","TSG_MH","H_MH","dVR_MH","Con_Rate_MH","Tot_HU_MH")]
rMH<-relMH[relMH$OSG_MH>0 & relMH$TSG_MH>0&relMH$H_MH<2.5,]
lmMH<-lm(H_MH~dVR_MH,data=rMH)
windows()
plot(H_MH~dVR_MH,data=rMH,pch=16,col="blue",main="Growth Factor vs change in Vacancy Factor, Man. Homes",xlab="dVF (change in Vacancy Factor)",ylab="Growth Factor")
abline(lmMH)
abline(h=0)
abline(v=0)

relMH$OSG_Rate_MH<-0.5*relMH$OSG_MH/relMH$Tot_HU_MH
lmMHc<-lm(Con_Rate_MH~OSG_Rate_MH,data=relMH[1:15,])
windows()
plot(Con_Rate_MH~OSG_Rate_MH,data=relMH[1:15,],pch=16,col="blue",main="Construction Rate vs OSG Rate, Man. Homes",xlab="Occ Stock Growth Rate",ylab="Construction Rate")
abline(lmMHc)
abline(h=0)
abline(v=0)
tab_model(lmSF,lmMF,lmMH,digits = 2,show.se = TRUE,show.ci = FALSE)
tab_model(lmSFc,lmMFc,lmMHc,digits = 3,show.se = TRUE,show.ci = FALSE)
# save linear models
save(lmSF,lmSFc,lmMF,lmMFc,lmMH,lmMHc,file = "Intermediate_results/ConLinModels.RData") # these will be called by hsm_cty script

# estimate roughly how much of stock growth is not from new construction for each type, based on reasonable values from the data
# these will be estimated, avoiding outlier values, in the hsm_cty script
SF_growth_returns_pc<-mean(summary_US$PcNewConsReturnsNotNew_SF[5:16]) # around 14% of single-family stock growth does not come from new construction, on average
MF_growth_returns_pc<-mean(summary_US$PcNewConsReturnsNotNew_MF[9:16]) # around 18% for multifamily
MH_growth_returns_pc<-mean(summary_US$PcNewConsReturnsNotNew_MH[2:10]) # around 22% for manufactured homes

# dem/total loss by type region
mean(summary_NE$Demol_Rate_SF[1:15]) # 23% NE SF
mean(summary_MW$Demol_Rate_SF[1:15]) # 37% MW
mean(summary_S$Demol_Rate_SF[1:15]) # 33% S
mean(summary_W$Demol_Rate_SF[1:15]) # 27% W

mean(summary_NE$Demol_Rate_MF[1:15]) # 12% NE MF
mean(summary_MW$Demol_Rate_MF[1:15]) # 21% MW
mean(summary_S$Demol_Rate_MF[1:15]) # 23% S
mean(summary_W$Demol_Rate_MF[1:15]) # 18% W

mean(summary_NE$Demol_Rate_MH[1:15]) # 23% NE MH
mean(summary_MW$Demol_Rate_MH[1:15]) # 37% MW
mean(summary_S$Demol_Rate_MH[1:15]) # 33% S
mean(summary_W$Demol_Rate_MH[1:15]) # 27% W

# estimate how much young and old age groups tend to have higher vacancy rates, by type, these values will also be used in hsm_cty
SF_0_10_vac_surplus<-mean(summary_US$VacDiff_SF_0_10,na.rm = TRUE) # negligible
SF_11_30_vac_surplus<-mean(summary_US$VacDiff_SF_11_30,na.rm = TRUE) # subtract 0.5% of vacancies from here, these are less likely to be vacant
SF_31_60_vac_surplus<-mean(summary_US$VacDiff_SF_31_60,na.rm = TRUE) # quite small, small enough to ignore
SF_61_vac_surplus<-mean(summary_US$VacDiff_SF_61,na.rm = TRUE) # add 0.5% to here, these are more likely to be vacant
dem_adj_SF<-c(0,-0.005,0,0.005) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups

MF_0_10_vac_surplus<-mean(summary_US$VacDiff_MF_0_10,na.rm = TRUE) # add 0.5% of vacancies to here, more likely to be vacant
MF_11_30_vac_surplus<-mean(summary_US$VacDiff_MF_11_30,na.rm = TRUE) # negligible
MF_31_60_vac_surplus<-mean(summary_US$VacDiff_MF_31_60,na.rm = TRUE) # subtract 0.5% of vacancies from here, less likely to be vacant
MF_61_vac_surplus<-mean(summary_US$VacDiff_MF_61,na.rm = TRUE) # negligible
dem_adj_MF<-c(0.005,0,-0.005,0) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups

MH_0_10_vac_surplus<-mean(summary_US$VacDiff_MH_0_10,na.rm = TRUE) # subtract 1.7% of vacancies from here, much less likely to be vacant
MH_11_30_vac_surplus<-mean(summary_US$VacDiff_MH_11_30,na.rm = TRUE) #
MH_31_60_vac_surplus<-mean(summary_US$VacDiff_MH_31_60,na.rm = TRUE) # add 1.7% of vacancies to here, much more
MH_61_vac_surplus<-mean(summary_US$VacDiff_MH_61,na.rm = TRUE) #
dem_adj_MH<-c(-0.017,0,0.017,0) # adjustments to vacancies by 0-10, 11-30, 31-60, and 61+ age-groups

windows()
plot(summary_US$Year,summary_US$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.05,1.15),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for single-family homes, 1985-2017") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2017. natural rate=1.1
text(2010,1.09,paste("mean: ",round(mean(summary_US$VR_SF),3),sep = ""))
windows()
plot(summary_US$Year,summary_US$VR_MF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.1,1.25),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for multifamily homes, 1985-2017") # vacancy rate for MF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2017. natural rate=1.1
text(2010,1.12,paste("mean: ",round(mean(summary_US$VR_MF),3),sep = ""))
windows()
plot(summary_US$Year,summary_US$VR_MH,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.2,1.35),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for manufactured homes, 1985-2017") # vacancy rate for MH hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2017. natural rate=1.1
text(2010,1.2,paste("mean: ",round(mean(summary_US$VR_MH),3),sep = ""))
# vacancy factors US average. Fig S8 in supporting information
windows()
plot(summary_US$Year,summary_US$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.05,1.35),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Factor (Tot Units/Occ. Units)",main = "Vacancy Factor for all home types, US avg, 1985-2019") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2017. natural rate=1.1
lines(summary_US$Year,summary_US$VR_MF,type="b",pch=19,col="blue")
lines(summary_US$Year,summary_US$VR_MH,type="b",pch=19,col="darkgreen")
text(1990,1.06,paste("mean SF: ",round(mean(summary_US$VR_SF),3),sep = ""))
text(2002,1.06,paste("mean MF: ",round(mean(summary_US$VR_MF),3),sep = ""))
text(2014,1.06,paste("mean MH: ",round(mean(summary_US$VR_MH),3),sep = ""))
legend("topleft",legend = c("MH","MF","SF"),col=c("darkgreen","blue","red"),lty=c(1,1,1),cex=0.95)
# vacancy ratios US average. Fig S8 in supporting information
windows()
plot(summary_US$Year,(summary_US$VR_SF-1)/summary_US$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(0.05,0.25),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Vac Units/Tot Units)",main = "Vacancy Ratio for all home types, US avg, 1985-2019") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2017. natural rate=1.1
lines(summary_US$Year,(summary_US$VR_MF-1)/summary_US$VR_MF,type="b",pch=19,col="blue")
lines(summary_US$Year,(summary_US$VR_MH-1)/summary_US$VR_MH,type="b",pch=19,col="darkgreen")
text(2010,0.09,paste("mean SF: ",round(mean((summary_US$VR_SF-1)/summary_US$VR_SF),3),sep = ""))
text(2010,0.075,paste("mean MF: ",round(mean((summary_US$VR_MF-1)/summary_US$VR_MF),3),sep = ""))
text(2010,0.06,paste("mean MH: ",round(mean((summary_US$VR_MH-1)/summary_US$VR_MH),3),sep = ""))
legend("topleft",legend = c("MH","MF","SF"),col=c("darkgreen","blue","red"),lty=c(1,1,1),cex=0.95)

# vacancy rates NE average. vacancy rates for MF and MH much lower in NE than US average
windows(width = 12,height = 12)
par(mfrow=c(2,2))
plot(summary_NE$Year,summary_NE$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.05,1.35),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for all home types, NorthEast, 1985-2019") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2019. natural rate=1.1
lines(summary_NE$Year,summary_NE$VR_MF,type="b",pch=19,col="blue")
lines(summary_NE$Year,summary_NE$VR_MH,type="b",pch=19,col="darkgreen")
text(1990,1.06,paste("mean SF: ",round(mean(summary_NE$VR_SF),3),sep = ""))
text(2002,1.06,paste("mean MF: ",round(mean(summary_NE$VR_MF),3),sep = ""))
text(2014,1.06,paste("mean MH: ",round(mean(summary_NE$VR_MH),3),sep = ""))
legend("topleft",legend = c("MH","MF","SF"),col=c("darkgreen","blue","red"),lty=c(1,1,1),cex=0.95)
# vacancy rates MW average. vacancy rates for all types similar to US average
# windows()
plot(summary_MW$Year,summary_MW$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.05,1.35),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for all home types, MidWest, 1985-2019") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2019. natural rate=1.1
lines(summary_MW$Year,summary_MW$VR_MF,type="b",pch=19,col="blue")
lines(summary_MW$Year,summary_MW$VR_MH,type="b",pch=19,col="darkgreen")
text(1990,1.06,paste("mean SF: ",round(mean(summary_MW$VR_SF),3),sep = ""))
text(2002,1.06,paste("mean MF: ",round(mean(summary_MW$VR_MF),3),sep = ""))
text(2014,1.06,paste("mean MH: ",round(mean(summary_MW$VR_MH),3),sep = ""))
# legend("topleft",legend = c("MH","MF","SF"),col=c("darkgreen","blue","red"),lty=c(1,1,1),cex=0.95)
# vacancy rates w average. vacancy rates for MF and MH much lower in W than US average
# windows()
plot(summary_W$Year,summary_W$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.05,1.35),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for all home types, West, 1985-2019") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2019. natural rate=1.1
lines(summary_W$Year,summary_W$VR_MF,type="b",pch=19,col="blue")
lines(summary_W$Year,summary_W$VR_MH,type="b",pch=19,col="darkgreen")
text(1990,1.06,paste("mean SF: ",round(mean(summary_W$VR_SF),3),sep = ""))
text(2002,1.06,paste("mean MF: ",round(mean(summary_W$VR_MF),3),sep = ""))
text(2014,1.06,paste("mean MH: ",round(mean(summary_W$VR_MH),3),sep = ""))
# legend("topleft",legend = c("MH","MF","SF"),col=c("darkgreen","blue","red"),lty=c(1,1,1),cex=0.95)
# vacancy rates s average. vacancy rates for all types higher than US average, especially for MF
# windows()
plot(summary_S$Year,summary_S$VR_SF,type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.05,1.35),xlim=c(1985,2019),xlab = "Year",ylab = "Vacancy Ratio (Tot Units/Occ. Units)",main = "Vacancy Ratio for all home types, South, 1985-2019") # vacancy rate for SF hovers around 1.1 until 2001, reaches a peak of over 1.13 in 2009, and comes back to 1.12 by 2015 and under 1.11 by 2019. natural rate=1.1
lines(summary_S$Year,summary_S$VR_MF,type="b",pch=19,col="blue")
lines(summary_S$Year,summary_S$VR_MH,type="b",pch=19,col="darkgreen")
text(1990,1.06,paste("mean SF: ",round(mean(summary_S$VR_SF),3),sep = ""))
text(2002,1.06,paste("mean MF: ",round(mean(summary_S$VR_MF),3),sep = ""))
text(2014,1.06,paste("mean MH: ",round(mean(summary_S$VR_MH),3),sep = ""))
# legend("topleft",legend = c("MH","MF","SF"),col=c("darkgreen","blue","red"),lty=c(1,1,1),cex=0.95)

# Now create stock turnover model and apply it at the national level ##########
graphics.off()
create_sm_df<- function (summary) { # function to create the data frame, with the model outputs empty
sum_cn<-names(summary) # column names of summary dataframe
# keep years, population (tot and by house type), and HH Size, and occupied housing units
stockModel<-summary[,1:16] 
# fill in values for year 1 for vacancy rates, and tot units
stockModel[1,sum_cn[17:24]]<-summary[1,17:24] 
# add columns for tot demolition and construction by type
stockModel[,sum_cn[25:30]]<-0
# add columns for construction scenarios (whether occupied stock growth was negative "A" or positive "B), initially set to "O"
stockModel$Con_Scenario_SF<-stockModel$Con_Scenario_MF<-stockModel$Con_Scenario_MH<-"O" 
# Add columns for vacancy-rate adjusted occupied stock growth, a first estimate for total stock growth.
stockModel$VOSG_SF<-stockModel$VOSG_MF<-stockModel$VOSG_MH<-0 
# Add columns for occupied and total stock growth, H (TSG/VOSG), and change in vacancy rate by type. There parameters are used to estimate construction. Set to zero
stockModel[,sum_cn[210:221]]<-0
# Add breakdown of total stock by age cohort and occ status within each type group, for starting timestep only
stockModel[1,sum_cn[40:75]]<-summary[1,40:75]
# Add columns for the total (not percentage) of housing units by type, occ, and age cohort
stockModel[,substr(sum_cn[40:75],3,nchar(sum_cn[40:75]))]<-0
# Add demolition rates by type, occ/vac, and age range. Multiply by 2 so that they refer to construction over 2 years as a percentage of current stock. 
# The model will convert to rates by age cohort
stockModel[,sum_cn[84:101]]<-2*summary[,84:101]
# Add columns for demolition rates and absolute values by type, occ/vac, and age cohort, initially set to zero
stockModel[,sum_cn[104:175]]<-0
# Add columns for construction rate and demolition rate by aggregate types
stockModel[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH","Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-0
## calculate housing stocks by age cohort for year 1
stockModel[is.na(stockModel)]<-0
stockModel[1,85:96]<-stockModel$Tot_HU_SF[1]*stockModel[1,49:60] # for SF homes
stockModel[1,97:108]<-stockModel$Tot_HU_MF[1]*stockModel[1,61:72] # for MF homes
stockModel[1,109:120]<-stockModel$Tot_HU_MH[1]*stockModel[1,73:84] # for MH homes

stockModel_df<-stockModel

stockModel_df
}
stockModel_df_US<-create_sm_df(summary_US)
smod_cn<-names(stockModel_df_US) # column names
# define vacancy rates, based on the US_summary averages above (e.g. mean(summary_US$VR_SF))
Vn_SF<-1.106 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-1.181 # estimate 'natural' vacancy rate of 1.18 for MF housing
Vn_MH<-1.257 # estimate 'natural' vacancy rate of 1.26 for MH housing
# stock model now contained in function 'run_sm.R'
stockModel_US<-run_sm(stockModel_df_US)

# repeat stock model for the census divisions
# redefine vacancy rates more suitable for NE
Vn_SF<-1.109 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-1.124 # estimate 'natural' vacancy rate of 1.18 for MF housing
Vn_MH<-1.187 # estimate 'natural' vacancy rate of 1.26 for MH housing
stockModel_df_NE<-create_sm_df(summary_NE)
stockModel_NE<-run_sm(stockModel_df_NE)

# redefine vacancy rates more suitable for MW
Vn_SF<-1.1 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-1.18 # estimate 'natural' vacancy rate of 1.18 for MF housing
Vn_MH<-1.26 # estimate 'natural' vacancy rate of 1.26 for MH housing
stockModel_df_MW<-create_sm_df(summary_MW)
stockModel_MW<-run_sm(stockModel_df_MW)

# redefine vacancy rates more suitable for W
Vn_SF<-1.1 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-1.14 # estimate 'natural' vacancy rate of 1.18 for MF housing
Vn_MH<-1.23 # estimate 'natural' vacancy rate of 1.26 for MH housing
stockModel_df_W<-create_sm_df(summary_W)
stockModel_W<-run_sm(stockModel_df_W)

# redefine vacancy rates more suitable for S
Vn_SF<-1.11 # estimate 'natural' vacancy rate of 1.1 for SF housing
Vn_MF<-1.28 # estimate 'natural' vacancy rate of 1.18 for MF housing
Vn_MH<-1.28 # estimate 'natural' vacancy rate of 1.26 for MH housing
stockModel_df_S<-create_sm_df(summary_S)
stockModel_S<-run_sm(stockModel_df_S)

# make some plots to compare the years and validate the model, US Avg ############
graphics.off()
windows()
plot(summary_US$Year, summary_US$Tot_HU_SF, type = "b",frame=FALSE, pch=19,col="red",ylim=c(6e7,9.5e7),xlab = "Year",ylab = "Tot Sin-Fam Units",main = "Model validation: Total SF Units")
lines(stockModel_US$Year,stockModel_US$Tot_HU_SF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_US$Year,stockModel_US$Occ_HU_SF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_US$Year[1:15], 2*summary_US$Con_SF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,5e6),xlab = "Year",ylab = "New Sin-Fam Units every 2 years",main = "Model validation: Total SF Construction")
lines(stockModel_US$Year[1:15],stockModel_US$Con_SF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="National",]$Year[1:15],1000*con2yr[con2yr$Region=="National",]$SF2yr[1:15],col="forestgreen",type="b",pch=15,lty=1)
lines(summary_US$Year[1:15],2*summary_US$Con_SF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_US$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(1995.5,8e5,paste("AHS app. mean: ",2*round(mean(summary_US$Con_SF_calc[1:15])),sep = ""),cex=0.8)
text(1995,6e5,paste("Model mean: ",round(mean(stockModel_US$Con_SF[1:15])),sep = ""),cex=0.8)
text(1995,4e5,paste("NRC mean: ",1000*round(mean(con2yr[con2yr$Region=="National",]$SF2yr)),sep = ""),cex=0.8)
text(1995,2e5,paste("AHS nxt-yr mean: ",2*round(mean(summary_US$Con_SF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_US$Year, summary_US$VR_SF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.085,1.135), main = "Model validation: SF Vac Rates")
lines(stockModel_US$Year,stockModel_US$VR_SF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2010,1.1,paste("AHS mean: ",round(mean(summary_US$VR_SF),3),sep = ""))
text(2010,1.095,paste("Model mean: ",round(mean(stockModel_US$VR_SF),3),sep = ""))

windows() 
plot(summary_US$Year[1:15], summary_US$Con_Rate_SF[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,3.1), xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: SF Con Rates")
lines(stockModel_US$Year[1:15],stockModel_US$Con_Rate_SF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="National",]$Year[1:15],50000*con2yr[con2yr$Region=="National",]$SF2yr[1:15]/summary_US$Tot_HU_SF[1:15],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,0.8,paste("AHS mean: ",round(100*mean(summary_US$Con_Rate_SF[1:15]),3),sep = ""))
text(1995,0.65,paste("Model mean: ",round(mean(100*stockModel_US$Con_Rate_SF[1:15]),3),sep = ""))
text(1995,0.5,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="National",]$SF2yr/summary_US$Tot_HU_SF[1:16]),3),sep = ""))

windows()
plot(summary_US$Year[1:15], 2*summary_US$Dem_SF[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim = c(0,1.1e6),xlab = "Year",ylab = "SF Units removed every 2 years",main = "Model validation: Total SF Demolition")
lines(summary_US$Year[1:15],2*summary_US$Demol_SF[1:15], type = "b", pch=19,col="dimgray")
legend("topleft",legend = c("AHS total removals","AHS demolition only"),col=c("red","dimgray"),lty=1,cex=0.75)
text(1990,6e5,paste("Total mean: ",2*round(mean(summary_US$Dem_SF[1:15])),sep = ""))
text(1990,5e5,paste("Demol mean: ",2*round(mean(summary_US$Demol_SF[1:15])),sep = ""))

windows()
plot(summary_US$Year[1:15], 100*summary_US$Dem_Rate_SF[1:15], type = "b",frame=FALSE, ylim=c(0.2,1), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: SF Demolition Rates")
lines(stockModel_US$Year[1:15],100*stockModel_US$Dem_Rate_SF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.40,paste("AHS mean: ",100*round(mean(summary_US$Dem_Rate_SF[1:15]),4),"%",sep = ""))
text(2000,0.35,paste("Model mean: ",100*round(mean(stockModel_US$Dem_Rate_SF[1:15]),4),"%",sep = ""))

windows()
plot(summary_US$Year[1:15],100*summary_US$Demol_Rate_SF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, SF")
text(2000,25,paste("Mean percent: ",round(mean(100*summary_US$Demol_Rate_SF[1:15]),1),"%",sep = ""))

# MF
windows()
plot(summary_US$Year, summary_US$Tot_HU_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(2.4e7,3.8e7),ylab = "Tot Mul-Fam Units",main = "Model validation: Total MF Units")
lines(stockModel_US$Year,stockModel_US$Tot_HU_MF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_US$Year,stockModel_US$Occ_HU_MF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_US$Year[1:15], 2*summary_US$Con_MF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,2.4e6),xlab = "Year",ylab = "New Mul-Fam Units every 2 years",main = "Model validation: Total MF Construction")
lines(stockModel_US$Year[1:15],stockModel_US$Con_MF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="National",]$Year,1000*con2yr[con2yr$Region=="National",]$MF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_US$Year[1:15],2*summary_US$Con_MF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_US$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2000,2.4e6,paste("AHS app. mean: ",2*round(mean(summary_US$Con_MF_calc[1:15])),sep = ""),cex=0.8)
text(2000,2.25e6,paste("Model mean: ",round(mean(stockModel_US$Con_MF[1:15])),sep = ""),cex=0.8)
text(2000,2.1e6,paste("NRC mean: ",round(mean(1000*con2yr[con2yr$Region=="National",]$MF2yr)),sep = ""),cex=0.8)
text(2000,1.95e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_US$Con_MF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_US$Year, summary_US$VR_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.15,1.24), main = "Model validation: MF Vac Rates")
lines(stockModel_US$Year,stockModel_US$VR_MF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.225,paste("AHS mean: ",round(mean(summary_US$VR_MF),3),sep = ""))
text(1990,1.22,paste("Model mean: ",round(mean(stockModel_US$VR_MF),3),sep = ""))

windows() 
plot(summary_US$Year[1:15], summary_US$Con_Rate_MF[1:15]*100, type = "b",frame=FALSE,ylim=c(-0.2,3.5), pch=19,col="red",xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MF Con Rates")
lines(stockModel_US$Year[1:15],stockModel_US$Con_Rate_MF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="National",]$Year[1:15],50000*con2yr[con2yr$Region=="National",]$MF2yr[1:15]/summary_US$Tot_HU_MF[1:15],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,3,paste("AHS mean: ",100*round(mean(summary_US$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,2.7,paste("Model mean: ",100*round(mean(stockModel_US$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,2.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="National",]$MF2yr[1:15]/summary_US$Tot_HU_MF[1:15]),3),sep = ""))

windows()
plot(summary_US$Year[1:15], 2*summary_US$Dem_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim = c(0,8e5),xlab = "Year",ylab = "MF Units removed every 2 years",main = "Model validation: Total MF Demolition")
lines(summary_US$Year[1:15],2*summary_US$Demol_MF[1:15], type = "b", pch=19,col="dimgray")
legend("topleft",legend = c("AHS total removals","AHS demolition only"),col=c("red","dimgray"),lty=1,cex=0.75)
text(1990,3e5,paste("Total mean: ",2*round(mean(summary_US$Dem_MF[1:15])),sep = ""))
text(1990,2.5e5,paste("Demol mean: ",2*round(mean(summary_US$Demol_MF[1:15])),sep = ""))

windows()
plot(summary_US$Year[1:15], 100*summary_US$Dem_Rate_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MF Demolition Rates")
legend("topleft",legend = c("AHS values"),col=c("red"),lty=1,cex=0.75)
text(2000,0.7,paste("AHS mean: ",100*round(mean(summary_US$Dem_Rate_MF),4),"%",sep = ""))

windows()
plot(summary_US$Year[1:15], 100*summary_US$Dem_Rate_MF[1:15], type = "b",frame=FALSE, ylim=c(0.5,1.5), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MF Demolition Rates")
lines(stockModel_US$Year[1:15],100*stockModel_US$Dem_Rate_MF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.68,paste("AHS mean: ",100*round(mean(summary_US$Dem_Rate_MF[1:15]),4),"%",sep = ""))
text(2000,0.60,paste("Model mean: ",100*round(mean(stockModel_US$Dem_Rate_MF[1:15]),4),"%",sep = ""))

windows()
plot(summary_US$Year[1:15],100*summary_US$Demol_Rate_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MF")
text(1995,25,paste("Mean percent: ",round(mean(100*summary_US$Demol_Rate_MF[1:15]),1),"%",sep = ""))

# MH
windows()
plot(summary_US$Year, summary_US$Tot_HU_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(4.5e6,9.5e6),ylab = "Tot Man. Home Units",main = "Model validation: Total MH Units")
lines(stockModel_US$Year,stockModel_US$Tot_HU_MH,col="blue",type = "b",lty=2,pch=18) 
lines(stockModel_US$Year,stockModel_US$Occ_HU_MH,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_US$Year[1:15], 2*summary_US$Con_MH_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.5e5,1.5e6),xlab = "Year",ylab = "New Man. Home Units every 2 years",main = "Model validation: Total MH Construction")
lines(stockModel_US$Year[1:15],stockModel_US$Con_MH[1:15],col="blue",type = "b",lty=2,pch=18)
lines(mhs2yr$Year[1:15],1000*mhs2yr$MH2yr[1:15],col="forestgreen",type="b",pch=15,lty=1)
lines(summary_US$Year[1:15],2*summary_US$Con_MH[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_US$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","MHS data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2007,1.5e6,paste("AHS app. mean: ",2*round(mean(summary_US$Con_MH_calc[1:15])),sep = ""),cex=0.8)
text(2007,1.4e6,paste("Model mean: ",round(mean(stockModel_US$Con_MH[1:15])),sep = ""),cex=0.8)
text(2007,1.3e6,paste("MHS mean: ",round(mean(1000*mhs2yr$MH2yr[1:15])),sep = ""),cex=0.8)
text(2007,1.2e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_US$Con_MH[1:15])),sep = ""),cex=0.8) 

windows()
plot(summary_US$Year, summary_US$VR_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.2,1.35), main = "Model validation: MH Vac Rates")
lines(stockModel_US$Year,stockModel_US$VR_MH,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.23,paste("AHS mean: ",round(mean(summary_US$VR_MH),3),sep = ""))
text(1990,1.22,paste("Model mean: ",round(mean(stockModel_US$VR_MH),3),sep = ""))

windows() 
plot(summary_US$Year[1:15], summary_US$Con_Rate_MH[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,10),xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MH Con Rates")
lines(stockModel_US$Year[1:15],stockModel_US$Con_Rate_MH[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(mhs2yr$Year[1:15],50000*mhs2yr$MH2yr[1:15]/summary_US$Tot_HU_MH[1:15],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","MHS values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,2,paste("AHS mean: ",round(100*mean(summary_US$Con_Rate_MH[1:15]),3),sep = ""))
text(1995,1.5,paste("Model mean: ",round(mean(100*stockModel_US$Con_Rate_MH[1:15]),3),sep = ""))
text(1995,1,paste("MHS mean: ",round(mean(50000*mhs2yr$MH2yr[1:15]/summary_US$Tot_HU_MH[1:15]),3),sep = ""))

windows()
plot(summary_US$Year[1:15], 2*summary_US$Dem_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim = c(0,7e5),xlab = "Year",ylab = "MH Units removed every 2 years",main = "Model validation: Total MH Demolition")
lines(summary_US$Year[1:15],2*summary_US$Demol_MH[1:15], type = "b", pch=19,col="dimgray")
legend("topleft",legend = c("AHS total removals","AHS demolition only"),col=c("red","dimgray"),lty=1,cex=0.75)
text(1990,4e5,paste("Total mean: ",2*round(mean(summary_US$Dem_MH[1:15])),sep = ""))
text(1990,3.5e5,paste("Demol mean: ",2*round(mean(summary_US$Demol_MH[1:15])),sep = ""))

windows()
plot(summary_US$Year[1:15], 100*summary_US$Dem_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MH Demolition Rates")
lines(stockModel_US$Year[1:15],100*stockModel_US$Dem_Rate_MH[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1995,2.2,paste("AHS mean: ",100*round(mean(summary_US$Dem_Rate_MH[1:15]),4),"%",sep = ""))
text(1995,2.0,paste("Model mean: ",100*round(mean(stockModel_US$Dem_Rate_MH[1:15]),4),"%",sep = ""))

windows()
plot(summary_US$Year[1:15],100*summary_US$Demol_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MH")
text(1990,40,paste("Mean percent: ",round(mean(100*summary_US$Demol_Rate_MH[1:15]),1),"%",sep = ""))
# make some plots to compare the years, NE ############
graphics.off()
windows()
plot(summary_NE$Year, summary_NE$Tot_HU_SF, type = "b",frame=FALSE, pch=19,col="red",ylim=c(1e7,1.6e7),xlab = "Year",ylab = "Tot Sin-Fam Units",main = "Model validation: Total SF Units, NE")
lines(stockModel_NE$Year,stockModel_NE$Tot_HU_SF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_NE$Year,stockModel_NE$Occ_HU_SF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_NE$Year[1:15], 2*summary_NE$Con_SF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,1.3e6),xlab = "Year",ylab = "New Sin-Fam Units every 2 years",main = "Model validation: Total SF Construction, NE")
lines(stockModel_NE$Year[1:15],stockModel_NE$Con_SF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Northeast",]$Year,1000*con2yr[con2yr$Region=="Northeast",]$SF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_NE$Year[1:15],2*summary_NE$Con_SF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_NE$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005.5,1.3e6,paste("AHS app. mean: ",2*round(mean(summary_NE$Con_SF_calc[1:15])),sep = ""),cex=0.8)
text(2005,1.2e6,paste("Model mean: ",round(mean(stockModel_NE$Con_SF[1:15])),sep = ""),cex=0.8)
text(2005,1.1e6,paste("NRC mean: ",1000*round(mean(con2yr[con2yr$Region=="Northeast",]$SF2yr)),sep = ""),cex=0.8)
text(2005,1e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_NE$Con_SF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_NE$Year, summary_NE$VR_SF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.085,1.135), main = "Model validation: SF Vac Rates, NE")
lines(stockModel_NE$Year,stockModel_NE$VR_SF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2010,1.11,paste("AHS mean: ",round(mean(summary_NE$VR_SF),3),sep = ""))
text(2010,1.107,paste("Model mean: ",round(mean(stockModel_NE$VR_SF),3),sep = ""))

windows() 
plot(summary_NE$Year[1:15], summary_NE$Con_Rate_SF[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(-0.5,3.5), xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: SF Con Rates, NE")
lines(stockModel_NE$Year[1:15],stockModel_NE$Con_Rate_SF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Northeast",]$Year,50000*con2yr[con2yr$Region=="Northeast",]$SF2yr/summary_NE$Tot_HU_SF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,0.8,paste("AHS mean: ",round(100*mean(summary_NE$Con_Rate_SF[1:16]),3),sep = ""))
text(1995,0.65,paste("Model mean: ",round(mean(100*stockModel_NE$Con_Rate_SF[1:16]),3),sep = ""))
text(1995,0.5,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="Northeast",]$SF2yr/summary_NE$Tot_HU_SF[1:16]),3),sep = ""))

windows()
plot(summary_NE$Year[1:15], 100*summary_NE$Dem_Rate_SF[1:15], type = "b",frame=FALSE, ylim=c(0.2,0.8), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: SF Demolition Rates, NE")
lines(stockModel_NE$Year[1:15],100*stockModel_NE$Dem_Rate_SF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.40,paste("AHS mean: ",100*round(mean(summary_NE$Dem_Rate_SF[1:15]),4),"%",sep = ""))
text(2000,0.35,paste("Model mean: ",100*round(mean(stockModel_NE$Dem_Rate_SF[1:15]),4),"%",sep = ""))

windows()
plot(summary_NE$Year[1:15],100*summary_NE$Demol_Rate_SF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, SF")
text(2000,25,paste("Mean percent: ",round(mean(100*summary_NE$Demol_Rate_SF[1:15]),1),"%",sep = ""))

# MF
windows()
plot(summary_NE$Year, summary_NE$Tot_HU_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.65e7,0.85e7),ylab = "Tot Mul-Fam Units",main = "Model validation: Total MF Units, NE")
lines(stockModel_NE$Year,stockModel_NE$Tot_HU_MF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_NE$Year,stockModel_NE$Occ_HU_MF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_NE$Year[1:15], 2*summary_NE$Con_MF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,0.9e6),xlab = "Year",ylab = "New Mul-Fam Units every 2 years",main = "Model validation: Total MF Construction, NE")
lines(stockModel_NE$Year[1:15],stockModel_NE$Con_MF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Northeast",]$Year,1000*con2yr[con2yr$Region=="Northeast",]$MF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_NE$Year[1:15],2*summary_NE$Con_MF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_NE$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005,0.8e6,paste("AHS app. mean: ",2*round(mean(summary_NE$Con_MF_calc[1:15])),sep = ""),cex=0.8)
text(2005,0.7e6,paste("Model mean: ",round(mean(stockModel_NE$Con_MF[1:15])),sep = ""),cex=0.8)
text(2005,0.6e6,paste("NRC mean: ",round(mean(1000*con2yr[con2yr$Region=="Northeast",]$MF2yr)),sep = ""),cex=0.8)
text(2005,0.5e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_NE$Con_MF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_NE$Year, summary_NE$VR_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.05,1.2), main = "Model validation: MF Vac Rates, NE")
lines(stockModel_NE$Year,stockModel_NE$VR_MF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.18,paste("AHS mean: ",round(mean(summary_NE$VR_MF),3),sep = ""))
text(1990,1.16,paste("Model mean: ",round(mean(stockModel_NE$VR_MF),3),sep = ""))

windows() 
plot(summary_NE$Year[1:15], summary_NE$Con_Rate_MF[1:15]*100, type = "b",frame=FALSE,ylim=c(-0.9,4), pch=19,col="red",xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MF Con Rates, NE")
lines(stockModel_NE$Year[1:15],stockModel_NE$Con_Rate_MF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Northeast",]$Year,50000*con2yr[con2yr$Region=="Northeast",]$MF2yr/summary_NE$Tot_HU_MF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,1.8,paste("AHS mean: ",100*round(mean(summary_NE$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,1.6,paste("Model mean: ",100*round(mean(stockModel_NE$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,1.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="Northeast",]$MF2yr/summary_NE$Tot_HU_MF[1:16]),3),sep = ""))


windows()
plot(summary_NE$Year[1:15], 100*summary_NE$Dem_Rate_MF[1:15], type = "b",frame=FALSE, ylim=c(0.5,1.5), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MF Demolition Rates, NE")
lines(stockModel_NE$Year[1:15],100*stockModel_NE$Dem_Rate_MF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.68,paste("AHS mean: ",100*round(mean(summary_NE$Dem_Rate_MF[1:15]),4),"%",sep = ""))
text(2000,0.60,paste("Model mean: ",100*round(mean(stockModel_NE$Dem_Rate_MF[1:15]),4),"%",sep = ""))

windows()
plot(summary_NE$Year[1:15],100*summary_NE$Demol_Rate_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MF")
text(1995,25,paste("Mean percent: ",round(mean(100*summary_NE$Demol_Rate_MF[1:15]),1),"%",sep = ""))

# MH
windows()
plot(summary_NE$Year, summary_NE$Tot_HU_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.4e6,0.8e6),ylab = "Tot Man. Home Units",main = "Model validation: Total MH Units, NE")
lines(stockModel_NE$Year,stockModel_NE$Tot_HU_MH,col="blue",type = "b",lty=2,pch=18) 
lines(stockModel_NE$Year,stockModel_NE$Occ_HU_MH,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows() # need to get regional MH shipment data
plot(summary_NE$Year[1:15], 2*summary_NE$Con_MH_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.5e5,0.2e6),xlab = "Year",ylab = "New Man. Home Units every 2 years",main = "Model validation: Total MH Construction, NE")
lines(stockModel_NE$Year[1:15],stockModel_NE$Con_MH[1:15],col="blue",type = "b",lty=2,pch=18)
# lines(mhs2yr$Year,1000*mhs2yr$MH2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_NE$Year[1:15],2*summary_NE$Con_MH[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_NE$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","MHS data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2000,2e5,paste("AHS app. mean: ",2*round(mean(summary_NE$Con_MH_calc[1:15])),sep = ""),cex=0.8)
text(2000,1.8e5,paste("Model mean: ",round(mean(stockModel_NE$Con_MH[1:15])),sep = ""),cex=0.8)
# text(2000,1.6e5,paste("MHS mean: ",round(mean(1000*mhs2yr$MH2yr)),sep = ""),cex=0.8)
text(2000,1.4e5,paste("AHS nxt-yr mean: ",2*round(mean(summary_NE$Con_MH[1:15])),sep = ""),cex=0.8) 

windows()
plot(summary_NE$Year, summary_NE$VR_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.1,1.5), main = "Model validation: MH Vac Rates, NE")
lines(stockModel_NE$Year,stockModel_NE$VR_MH,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.45,paste("AHS mean: ",round(mean(summary_NE$VR_MH),3),sep = ""))
text(1990,1.41,paste("Model mean: ",round(mean(stockModel_NE$VR_MH),3),sep = ""))

windows() 
plot(summary_NE$Year[1:15], summary_NE$Con_Rate_MH[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,30),xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MH Con Rates, NE")
lines(stockModel_NE$Year[1:15],stockModel_NE$Con_Rate_MH[1:15]*100,col="blue",type = "b",lty=2,pch=18)
# lines(mhs2yr$Year,50000*mhs2yr$MH2yr/summary_NE$Tot_HU_MH[1:15],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","MHS values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,2,paste("AHS mean: ",round(100*mean(summary_NE$Con_Rate_MH[1:15]),3),sep = ""))
text(1995,1.5,paste("Model mean: ",round(mean(100*stockModel_NE$Con_Rate_MH[1:15]),3),sep = ""))

windows()
plot(summary_NE$Year[1:15], 100*summary_NE$Dem_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MH Demolition Rates, NE")
lines(stockModel_NE$Year[1:15],100*stockModel_NE$Dem_Rate_MH[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1995,2.2,paste("AHS mean: ",100*round(mean(summary_NE$Dem_Rate_MH[1:15]),4),"%",sep = ""))
text(1995,2.0,paste("Model mean: ",100*round(mean(stockModel_NE$Dem_Rate_MH[1:15]),4),"%",sep = ""))

windows()
plot(summary_NE$Year[1:15],100*summary_NE$Demol_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MH")
text(1990,40,paste("Mean percent: ",round(mean(100*summary_NE$Demol_Rate_MH[1:15]),1),"%",sep = ""))

# make some plots to compare the years, MW ############
graphics.off()
windows()
plot(summary_MW$Year, summary_MW$Tot_HU_SF, type = "b",frame=FALSE, pch=19,col="red",ylim=c(1.5e7,2.5e7),xlab = "Year",ylab = "Tot Sin-Fam Units",main = "Model validation: Total SF Units, MW")
lines(stockModel_MW$Year,stockModel_MW$Tot_HU_SF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_MW$Year,stockModel_MW$Occ_HU_SF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_MW$Year[1:15], 2*summary_MW$Con_SF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,1.3e6),xlab = "Year",ylab = "New Sin-Fam Units every 2 years",main = "Model validation: Total SF Construction, MW")
lines(stockModel_MW$Year[1:15],stockModel_MW$Con_SF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Midwest",]$Year,1000*con2yr[con2yr$Region=="Midwest",]$SF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_MW$Year[1:15],2*summary_MW$Con_SF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_MW$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005.5,1.3e6,paste("AHS app. mean: ",2*round(mean(summary_MW$Con_SF_calc[1:15])),sep = ""),cex=0.8)
text(2005,1.2e6,paste("Model mean: ",round(mean(stockModel_MW$Con_SF[1:15])),sep = ""),cex=0.8)
text(2005,1.1e6,paste("NRC mean: ",1000*round(mean(con2yr[con2yr$Region=="Midwest",]$SF2yr)),sep = ""),cex=0.8)
text(2005,1e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_MW$Con_SF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_MW$Year, summary_MW$VR_SF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.07,1.135), main = "Model validation: SF Vac Rates, MW")
lines(stockModel_MW$Year,stockModel_MW$VR_SF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2010,1.11,paste("AHS mean: ",round(mean(summary_MW$VR_SF),3),sep = ""))
text(2010,1.107,paste("Model mean: ",round(mean(stockModel_MW$VR_SF),3),sep = ""))

windows() 
plot(summary_MW$Year[1:15], summary_MW$Con_Rate_SF[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(-0.5,3.5), xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: SF Con Rates, MW")
lines(stockModel_MW$Year[1:15],stockModel_MW$Con_Rate_SF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Midwest",]$Year,50000*con2yr[con2yr$Region=="Midwest",]$SF2yr/summary_MW$Tot_HU_SF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,0.8,paste("AHS mean: ",round(100*mean(summary_MW$Con_Rate_SF[1:16]),3),sep = ""))
text(1995,0.65,paste("Model mean: ",round(mean(100*stockModel_MW$Con_Rate_SF[1:16]),3),sep = ""))
text(1995,0.5,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="Midwest",]$SF2yr/summary_MW$Tot_HU_SF[1:16]),3),sep = ""))

windows()
plot(summary_MW$Year[1:15], 100*summary_MW$Dem_Rate_SF[1:15], type = "b",frame=FALSE, ylim=c(0.3,0.7), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: SF Demolition Rates, MW")
lines(stockModel_MW$Year[1:15],100*stockModel_MW$Dem_Rate_SF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.40,paste("AHS mean: ",100*round(mean(summary_MW$Dem_Rate_SF[1:15]),4),"%",sep = ""))
text(2000,0.35,paste("Model mean: ",100*round(mean(stockModel_MW$Dem_Rate_SF[1:15]),4),"%",sep = ""))

windows()
plot(summary_MW$Year[1:15],100*summary_MW$Demol_Rate_SF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, SF")
text(2000,30,paste("Mean percent: ",round(mean(100*summary_MW$Demol_Rate_SF[1:15]),1),"%",sep = ""))

# MF
windows()
plot(summary_MW$Year, summary_MW$Tot_HU_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.5e7,0.7e7),ylab = "Tot Mul-Fam Units",main = "Model validation: Total MF Units, MW")
lines(stockModel_MW$Year,stockModel_MW$Tot_HU_MF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_MW$Year,stockModel_MW$Occ_HU_MF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_MW$Year[1:15], 2*summary_MW$Con_MF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,0.9e6),xlab = "Year",ylab = "New Mul-Fam Units every 2 years",main = "Model validation: Total MF Construction, MW")
lines(stockModel_MW$Year[1:15],stockModel_MW$Con_MF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Midwest",]$Year,1000*con2yr[con2yr$Region=="Midwest",]$MF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_MW$Year[1:15],2*summary_MW$Con_MF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_MW$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005,0.8e6,paste("AHS app. mean: ",2*round(mean(summary_MW$Con_MF_calc[1:15])),sep = ""),cex=0.8)
text(2005,0.7e6,paste("Model mean: ",round(mean(stockModel_MW$Con_MF[1:15])),sep = ""),cex=0.8)
text(2005,0.6e6,paste("NRC mean: ",round(mean(1000*con2yr[con2yr$Region=="Midwest",]$MF2yr)),sep = ""),cex=0.8)
text(2005,0.5e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_MW$Con_MF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_MW$Year, summary_MW$VR_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.1,1.3), main = "Model validation: MF Vac Rates, MW")
lines(stockModel_MW$Year,stockModel_MW$VR_MF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.18,paste("AHS mean: ",round(mean(summary_MW$VR_MF),3),sep = ""))
text(1990,1.16,paste("Model mean: ",round(mean(stockModel_MW$VR_MF),3),sep = ""))

windows() 
plot(summary_MW$Year[1:15], summary_MW$Con_Rate_MF[1:15]*100, type = "b",frame=FALSE,ylim=c(-0.9,4), pch=19,col="red",xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MF Con Rates, MW")
lines(stockModel_MW$Year[1:15],stockModel_MW$Con_Rate_MF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="Midwest",]$Year,50000*con2yr[con2yr$Region=="Midwest",]$MF2yr/summary_MW$Tot_HU_MF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,1.8,paste("AHS mean: ",100*round(mean(summary_MW$Con_Rate_MF[1:16]),4),sep = ""))
text(1995,1.6,paste("Model mean: ",100*round(mean(stockModel_MW$Con_Rate_MF[1:16]),4),sep = ""))
text(1995,1.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="Midwest",]$MF2yr/summary_MW$Tot_HU_MF[1:16]),3),sep = ""))

windows()
plot(summary_MW$Year[1:15], 100*summary_MW$Dem_Rate_MF[1:15], type = "b",frame=FALSE, ylim=c(0.5,2), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MF Demolition Rates, MW")
lines(stockModel_MW$Year[1:15],100*stockModel_MW$Dem_Rate_MF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.68,paste("AHS mean: ",100*round(mean(summary_MW$Dem_Rate_MF[1:15]),4),"%",sep = ""))
text(2000,0.60,paste("Model mean: ",100*round(mean(stockModel_MW$Dem_Rate_MF[1:15]),4),"%",sep = ""))

windows()
plot(summary_MW$Year[1:15],100*summary_MW$Demol_Rate_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MF")
text(1995,25,paste("Mean percent: ",round(mean(100*summary_MW$Demol_Rate_MF[1:15]),1),"%",sep = ""))

# MH
windows()
plot(summary_MW$Year, summary_MW$Tot_HU_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.8e6,1.6e6),ylab = "Tot Man. Home Units",main = "Model validation: Total MH Units, MW")
lines(stockModel_MW$Year,stockModel_MW$Tot_HU_MH,col="blue",type = "b",lty=2,pch=18) 
lines(stockModel_MW$Year,stockModel_MW$Occ_HU_MH,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows() # need to get regional MH shipment data
plot(summary_MW$Year[1:15], 2*summary_MW$Con_MH_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.5e5,0.4e6),xlab = "Year",ylab = "New Man. Home Units every 2 years",main = "Model validation: Total MH Construction, MW")
lines(stockModel_MW$Year[1:15],stockModel_MW$Con_MH[1:15],col="blue",type = "b",lty=2,pch=18)
# lines(mhs2yr$Year,1000*mhs2yr$MH2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_MW$Year[1:15],2*summary_MW$Con_MH[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_MW$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","MHS data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2000,2e5,paste("AHS app. mean: ",2*round(mean(summary_MW$Con_MH_calc[1:15])),sep = ""),cex=0.8)
text(2000,1.8e5,paste("Model mean: ",round(mean(stockModel_MW$Con_MH[1:15])),sep = ""),cex=0.8)
text(2000,1.4e5,paste("AHS nxt-yr mean: ",2*round(mean(summary_MW$Con_MH[1:15])),sep = ""),cex=0.8) 

windows()
plot(summary_MW$Year, summary_MW$VR_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.1,1.5), main = "Model validation: MH Vac Rates, MW")
lines(stockModel_MW$Year,stockModel_MW$VR_MH,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.45,paste("AHS mean: ",round(mean(summary_MW$VR_MH),3),sep = ""))
text(1990,1.41,paste("Model mean: ",round(mean(stockModel_MW$VR_MH),3),sep = ""))

windows() 
plot(summary_MW$Year[1:15], summary_MW$Con_Rate_MH[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,20),xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MH Con Rates, MW")
lines(stockModel_MW$Year[1:15],stockModel_MW$Con_Rate_MH[1:15]*100,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values","MHS values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,2,paste("AHS mean: ",round(100*mean(summary_MW$Con_Rate_MH[1:15]),3),sep = ""))
text(1995,1.5,paste("Model mean: ",round(mean(100*stockModel_MW$Con_Rate_MH[1:15]),3),sep = ""))

windows()
plot(summary_MW$Year[1:15], 100*summary_MW$Dem_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MH Demolition Rates, MW")
lines(stockModel_MW$Year[1:15],100*stockModel_MW$Dem_Rate_MH[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1995,2.2,paste("AHS mean: ",100*round(mean(summary_MW$Dem_Rate_MH[1:15]),4),"%",sep = ""))
text(1995,2.0,paste("Model mean: ",100*round(mean(stockModel_MW$Dem_Rate_MH[1:15]),4),"%",sep = ""))

windows()
plot(summary_MW$Year[1:15],100*summary_MW$Demol_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MH")
text(1990,40,paste("Mean percent: ",round(mean(100*summary_MW$Demol_Rate_MH[1:15]),1),"%",sep = ""))

# make some plots to compare the years, S ############
graphics.off()
windows()
plot(summary_S$Year, summary_S$Tot_HU_SF, type = "b",frame=FALSE, pch=19,col="red",ylim=c(2e7,4e7),xlab = "Year",ylab = "Tot Sin-Fam Units",main = "Model validation: Total SF Units, S")
lines(stockModel_S$Year,stockModel_S$Tot_HU_SF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_S$Year,stockModel_S$Occ_HU_SF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_S$Year[1:15], 2*summary_S$Con_SF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,2e6),xlab = "Year",ylab = "New Sin-Fam Units every 2 years",main = "Model validation: Total SF Construction, S")
lines(stockModel_S$Year[1:15],stockModel_S$Con_SF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="South",]$Year,1000*con2yr[con2yr$Region=="South",]$SF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_S$Year[1:15],2*summary_S$Con_SF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_S$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005.5,1.3e6,paste("AHS app. mean: ",2*round(mean(summary_S$Con_SF_calc[1:15])),sep = ""),cex=0.8)
text(2005,1.2e6,paste("Model mean: ",round(mean(stockModel_S$Con_SF[1:15])),sep = ""),cex=0.8)
text(2005,1.1e6,paste("NRC mean: ",1000*round(mean(con2yr[con2yr$Region=="South",]$SF2yr)),sep = ""),cex=0.8)
text(2005,1e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_S$Con_SF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_S$Year, summary_S$VR_SF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.08,1.15), main = "Model validation: SF Vac Rates, S")
lines(stockModel_S$Year,stockModel_S$VR_SF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2010,1.11,paste("AHS mean: ",round(mean(summary_S$VR_SF),3),sep = ""))
text(2010,1.107,paste("Model mean: ",round(mean(stockModel_S$VR_SF),3),sep = ""))

windows() 
plot(summary_S$Year[1:15], summary_S$Con_Rate_SF[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,5), xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: SF Con Rates, S")
lines(stockModel_S$Year[1:15],stockModel_S$Con_Rate_SF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="South",]$Year,50000*con2yr[con2yr$Region=="South",]$SF2yr/summary_S$Tot_HU_SF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,0.8,paste("AHS mean: ",round(100*mean(summary_S$Con_Rate_SF[1:15]),3),sep = ""))
text(1995,0.6,paste("Model mean: ",round(mean(100*stockModel_S$Con_Rate_SF[1:15]),3),sep = ""))
text(1995,0.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="South",]$SF2yr/summary_S$Tot_HU_SF[1:16]),3),sep = ""))


windows()
plot(summary_S$Year[1:15], 100*summary_S$Dem_Rate_SF[1:15], type = "b",frame=FALSE, ylim=c(0.3,1), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: SF Demolition Rates, S")
lines(stockModel_S$Year[1:15],100*stockModel_S$Dem_Rate_SF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.40,paste("AHS mean: ",100*round(mean(summary_S$Dem_Rate_SF[1:15]),4),"%",sep = ""))
text(2000,0.35,paste("Model mean: ",100*round(mean(stockModel_S$Dem_Rate_SF[1:15]),4),"%",sep = ""))

windows()
plot(summary_S$Year[1:15],100*summary_S$Demol_Rate_SF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, SF")
text(2000,30,paste("Mean percent: ",round(mean(100*summary_S$Demol_Rate_SF[1:15]),1),"%",sep = ""))

# MF
windows()
plot(summary_S$Year, summary_S$Tot_HU_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.5e7,1.2e7),ylab = "Tot Mul-Fam Units",main = "Model validation: Total MF Units, S")
lines(stockModel_S$Year,stockModel_S$Tot_HU_MF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_S$Year,stockModel_S$Occ_HU_MF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_S$Year[1:15], 2*summary_S$Con_MF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,1e6),xlab = "Year",ylab = "New Mul-Fam Units every 2 years",main = "Model validation: Total MF Construction, S")
lines(stockModel_S$Year[1:15],stockModel_S$Con_MF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="South",]$Year,1000*con2yr[con2yr$Region=="South",]$MF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_S$Year[1:15],2*summary_S$Con_MF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_S$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005,0.8e6,paste("AHS app. mean: ",2*round(mean(summary_S$Con_MF_calc[1:15])),sep = ""),cex=0.8)
text(2005,0.7e6,paste("Model mean: ",round(mean(stockModel_S$Con_MF[1:15])),sep = ""),cex=0.8)
text(2005,0.6e6,paste("NRC mean: ",round(mean(1000*con2yr[con2yr$Region=="South",]$MF2yr)),sep = ""),cex=0.8)
text(2005,0.5e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_S$Con_MF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_S$Year, summary_S$VR_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.15,1.4), main = "Model validation: MF Vac Rates, S")
lines(stockModel_S$Year,stockModel_S$VR_MF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.18,paste("AHS mean: ",round(mean(summary_S$VR_MF),3),sep = ""))
text(1990,1.16,paste("Model mean: ",round(mean(stockModel_S$VR_MF),3),sep = ""))

windows() 
plot(summary_S$Year[1:15], summary_S$Con_Rate_MF[1:15]*100, type = "b",frame=FALSE,ylim=c(-0.9,5), pch=19,col="red",xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MF Con Rates, S")
lines(stockModel_S$Year[1:15],stockModel_S$Con_Rate_MF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="South",]$Year,50000*con2yr[con2yr$Region=="South",]$MF2yr/summary_S$Tot_HU_MF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,4,paste("AHS mean: ",100*round(mean(summary_S$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,3.7,paste("Model mean: ",100*round(mean(stockModel_S$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,3.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="South",]$MF2yr/summary_S$Tot_HU_MF[1:16]),3),sep = ""))

windows()
plot(summary_S$Year[1:15], 100*summary_S$Dem_Rate_MF[1:15], type = "b",frame=FALSE, ylim=c(0.5,2), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MF Demolition Rates, S")
lines(stockModel_S$Year[1:15],100*stockModel_S$Dem_Rate_MF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.68,paste("AHS mean: ",100*round(mean(summary_S$Dem_Rate_MF[1:15]),4),"%",sep = ""))
text(2000,0.60,paste("Model mean: ",100*round(mean(stockModel_S$Dem_Rate_MF[1:15]),4),"%",sep = ""))

windows()
plot(summary_S$Year[1:15],100*summary_S$Demol_Rate_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MF")
text(1995,25,paste("Mean percent: ",round(mean(100*summary_S$Demol_Rate_MF[1:15]),1),"%",sep = ""))

# MH
windows()
plot(summary_S$Year, summary_S$Tot_HU_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(2e6,6e6),ylab = "Tot Man. Home Units",main = "Model validation: Total MH Units, S")
lines(stockModel_S$Year,stockModel_S$Tot_HU_MH,col="blue",type = "b",lty=2,pch=18) 
lines(stockModel_S$Year,stockModel_S$Occ_HU_MH,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows() # need to get regional MH shipment data
plot(summary_S$Year[1:15], 2*summary_S$Con_MH_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.5e5,1e6),xlab = "Year",ylab = "New Man. Home Units every 2 years",main = "Model validation: Total MH Construction, S")
lines(stockModel_S$Year[1:15],stockModel_S$Con_MH[1:15],col="blue",type = "b",lty=2,pch=18)
lines(summary_S$Year[1:15],2*summary_S$Con_MH[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_S$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","AHS next-year values"),col=c("red","blue","gold"),lty=c(1,2,2),cex=0.75)
text(1995,2e5,paste("AHS app. mean: ",2*round(mean(summary_S$Con_MH_calc[1:15])),sep = ""),cex=0.8)
text(1995,1.6e5,paste("Model mean: ",round(mean(stockModel_S$Con_MH[1:15])),sep = ""),cex=0.8)
text(1995,0.8e5,paste("AHS nxt-yr mean: ",2*round(mean(summary_S$Con_MH[1:15])),sep = ""),cex=0.8) 

windows()
plot(summary_S$Year, summary_S$VR_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.2,1.4), main = "Model validation: MH Vac Rates, S")
lines(stockModel_S$Year,stockModel_S$VR_MH,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.35,paste("AHS mean: ",round(mean(summary_S$VR_MH),3),sep = ""))
text(1990,1.33,paste("Model mean: ",round(mean(stockModel_S$VR_MH),3),sep = ""))

windows() 
plot(summary_S$Year[1:15], summary_S$Con_Rate_MH[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,15),xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MH Con Rates, S")
lines(stockModel_S$Year[1:15],stockModel_S$Con_Rate_MH[1:15]*100,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values","MHS values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,2,paste("AHS mean: ",round(100*mean(summary_S$Con_Rate_MH[1:15]),3),sep = ""))
text(1995,1.5,paste("Model mean: ",round(mean(100*stockModel_S$Con_Rate_MH[1:15]),3),sep = ""))


windows()
plot(summary_S$Year[1:15], 100*summary_S$Dem_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MH Demolition Rates, S")
lines(stockModel_S$Year[1:15],100*stockModel_S$Dem_Rate_MH[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1995,2.2,paste("AHS mean: ",100*round(mean(summary_S$Dem_Rate_MH[1:15]),4),"%",sep = ""))
text(1995,2.0,paste("Model mean: ",100*round(mean(stockModel_S$Dem_Rate_MH[1:15]),4),"%",sep = ""))

windows()
plot(summary_S$Year[1:15],100*summary_S$Demol_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MH")
text(1990,40,paste("Mean percent: ",round(mean(100*summary_S$Demol_Rate_MH[1:15]),1),"%",sep = ""))

# make some plots to compare the years, W ############
graphics.off()
windows()
plot(summary_W$Year, summary_W$Tot_HU_SF, type = "b",frame=FALSE, pch=19,col="red",ylim=c(1e7,2.2e7),xlab = "Year",ylab = "Tot Sin-Fam Units",main = "Model validation: Total SF Units, W")
lines(stockModel_W$Year,stockModel_W$Tot_HU_SF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_W$Year,stockModel_W$Occ_HU_SF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_W$Year[1:15], 2*summary_W$Con_SF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,1.3e6),xlab = "Year",ylab = "New Sin-Fam Units every 2 years",main = "Model validation: Total SF Construction, W")
lines(stockModel_W$Year[1:15],stockModel_W$Con_SF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="West",]$Year,1000*con2yr[con2yr$Region=="West",]$SF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_W$Year[1:15],2*summary_W$Con_SF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_W$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005.5,1.3e6,paste("AHS app. mean: ",2*round(mean(summary_W$Con_SF_calc[1:15])),sep = ""),cex=0.8)
text(2005,1.2e6,paste("Model mean: ",round(mean(stockModel_W$Con_SF[1:15])),sep = ""),cex=0.8)
text(2005,1.1e6,paste("NRC mean: ",1000*round(mean(con2yr[con2yr$Region=="West",]$SF2yr)),sep = ""),cex=0.8)
text(2005,1e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_W$Con_SF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_W$Year, summary_W$VR_SF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.07,1.14), main = "Model validation: SF Vac Rates, W")
lines(stockModel_W$Year,stockModel_W$VR_SF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2010,1.11,paste("AHS mean: ",round(mean(summary_W$VR_SF),3),sep = ""))
text(2010,1.107,paste("Model mean: ",round(mean(stockModel_W$VR_SF),3),sep = ""))

windows() 
plot(summary_W$Year[1:15], summary_W$Con_Rate_SF[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(0,4.5), xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: SF Con Rates, W")
lines(stockModel_W$Year[1:15],stockModel_W$Con_Rate_SF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="West",]$Year,50000*con2yr[con2yr$Region=="West",]$SF2yr/summary_W$Tot_HU_SF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,0.8,paste("AHS mean: ",round(100*mean(summary_W$Con_Rate_SF[1:15]),3),sep = ""))
text(1995,0.6,paste("Model mean: ",round(mean(100*stockModel_W$Con_Rate_SF[1:15]),3),sep = ""))
text(1995,0.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="West",]$SF2yr/summary_W$Tot_HU_SF[1:16]),3),sep = ""))

windows()
plot(summary_W$Year[1:15], 100*summary_W$Dem_Rate_SF[1:15], type = "b",frame=FALSE, ylim=c(0.3,1), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: SF Demolition Rates, W")
lines(stockModel_W$Year[1:15],100*stockModel_W$Dem_Rate_SF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.40,paste("AHS mean: ",100*round(mean(summary_W$Dem_Rate_SF[1:15]),4),"%",sep = ""))
text(2000,0.35,paste("Model mean: ",100*round(mean(stockModel_W$Dem_Rate_SF[1:15]),4),"%",sep = ""))

windows()
plot(summary_W$Year[1:15],100*summary_W$Demol_Rate_SF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, SF")
text(2000,30,paste("Mean percent: ",round(mean(100*summary_W$Demol_Rate_SF[1:15]),1),"%",sep = ""))

# MF
windows()
plot(summary_W$Year, summary_W$Tot_HU_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.5e7,0.8e7),ylab = "Tot Mul-Fam Units",main = "Model validation: Total MF Units, W")
lines(stockModel_W$Year,stockModel_W$Tot_HU_MF,col="blue",type = "b",lty=2,pch=18)
lines(stockModel_W$Year,stockModel_W$Occ_HU_MF,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows()
plot(summary_W$Year[1:15], 2*summary_W$Con_MF_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.2e6,0.9e6),xlab = "Year",ylab = "New Mul-Fam Units every 2 years",main = "Model validation: Total MF Construction, W")
lines(stockModel_W$Year[1:15],stockModel_W$Con_MF[1:15],col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="West",]$Year,1000*con2yr[con2yr$Region=="West",]$MF2yr,col="forestgreen",type="b",pch=15,lty=1)
lines(summary_W$Year[1:15],2*summary_W$Con_MF[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_W$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","NRC data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2005,0.8e6,paste("AHS app. mean: ",2*round(mean(summary_W$Con_MF_calc[1:15])),sep = ""),cex=0.8)
text(2005,0.7e6,paste("Model mean: ",round(mean(stockModel_W$Con_MF[1:15])),sep = ""),cex=0.8)
text(2005,0.6e6,paste("NRC mean: ",round(mean(1000*con2yr[con2yr$Region=="West",]$MF2yr)),sep = ""),cex=0.8)
text(2005,0.5e6,paste("AHS nxt-yr mean: ",2*round(mean(summary_W$Con_MF[1:15])),sep = ""),cex=0.8)

windows()
plot(summary_W$Year, summary_W$VR_MF, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.1,1.2), main = "Model validation: MF Vac Rates, W")
lines(stockModel_W$Year,stockModel_W$VR_MF,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.18,paste("AHS mean: ",round(mean(summary_W$VR_MF),3),sep = ""))
text(1990,1.17,paste("Model mean: ",round(mean(stockModel_W$VR_MF),3),sep = ""))

windows() 
plot(summary_W$Year[1:15], summary_W$Con_Rate_MF[1:15]*100, type = "b",frame=FALSE,ylim=c(-0.9,5), pch=19,col="red",xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MF Con Rates, W")
lines(stockModel_W$Year[1:15],stockModel_W$Con_Rate_MF[1:15]*100,col="blue",type = "b",lty=2,pch=18)
lines(con2yr[con2yr$Region=="West",]$Year,50000*con2yr[con2yr$Region=="West",]$MF2yr/summary_W$Tot_HU_MF[1:16],col="forestgreen",type="b",pch=15,lty=1)
legend("topleft",legend = c("AHS values","Modeled values","NRC values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,1.8,paste("AHS mean: ",100*round(mean(summary_W$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,1.6,paste("Model mean: ",100*round(mean(stockModel_W$Con_Rate_MF[1:15]),4),sep = ""))
text(1995,1.4,paste("NRC mean: ",round(mean(50000*con2yr[con2yr$Region=="West",]$MF2yr/summary_W$Tot_HU_MF[1:16]),3),sep = ""))

windows()
plot(summary_W$Year[1:15], 100*summary_W$Dem_Rate_MF[1:15], type = "b",frame=FALSE, ylim=c(0.2,1.5), pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MF Demolition Rates, W")
lines(stockModel_W$Year[1:15],100*stockModel_W$Dem_Rate_MF[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(2000,0.68,paste("AHS mean: ",100*round(mean(summary_W$Dem_Rate_MF[1:15]),4),"%",sep = ""))
text(2000,0.60,paste("Model mean: ",100*round(mean(stockModel_W$Dem_Rate_MF[1:15]),4),"%",sep = ""))

windows()
plot(summary_W$Year[1:15],100*summary_W$Demol_Rate_MF[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MF")
text(1995,25,paste("Mean percent: ",round(mean(100*summary_W$Demol_Rate_MF[1:15]),1),"%",sep = ""))

# MH
windows()
plot(summary_W$Year, summary_W$Tot_HU_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylim=c(0.8e6,2e6),ylab = "Tot Man. Home Units",main = "Model validation: Total MH Units, W")
lines(stockModel_W$Year,stockModel_W$Tot_HU_MH,col="blue",type = "b",lty=2,pch=18) 
lines(stockModel_W$Year,stockModel_W$Occ_HU_MH,col="darkmagenta",type = "b",lty=1,pch=15)
legend("topleft",legend = c("AHS values","Modeled values","AHS Occ. Units"),col=c("red","blue","darkmagenta"),lty=c(1,2,1),cex=0.75)

windows() # need to get regional MH shipment data
plot(summary_W$Year[1:15], 2*summary_W$Con_MH_calc[1:15], type = "b",frame=FALSE, pch=19,col="red",ylim=c(-.5e5,0.3e6),xlab = "Year",ylab = "New Man. Home Units every 2 years",main = "Model validation: Total MH Construction, W")
lines(stockModel_W$Year[1:15],stockModel_W$Con_MH[1:15],col="blue",type = "b",lty=2,pch=18)
lines(summary_W$Year[1:15],2*summary_W$Con_MH[1:15],col="gold3",type = "b",lty=2,pch=17)
lines(summary_W$Year[1:15],rep(0,15),col="black",type = "l",lty=1)
legend("topleft",legend = c("AHS apparent values","Modeled values","MHS data","AHS next-year values"),col=c("red","blue","forestgreen","gold"),lty=c(1,2,1,2),cex=0.75)
text(2000,2e5,paste("AHS app. mean: ",2*round(mean(summary_W$Con_MH_calc[1:15])),sep = ""),cex=0.8)
text(2000,1.8e5,paste("Model mean: ",round(mean(stockModel_W$Con_MH[1:15])),sep = ""),cex=0.8)
text(2000,1.4e5,paste("AHS nxt-yr mean: ",2*round(mean(summary_W$Con_MH[1:15])),sep = ""),cex=0.8) 

windows()
plot(summary_W$Year, summary_W$VR_MH, type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Vacancy Ratio",ylim=c(1.1,1.4), main = "Model validation: MH Vac Rates, W")
lines(stockModel_W$Year,stockModel_W$VR_MH,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1990,1.35,paste("AHS mean: ",round(mean(summary_W$VR_MH),3),sep = ""))
text(1990,1.31,paste("Model mean: ",round(mean(stockModel_W$VR_MH),3),sep = ""))

windows() 
plot(summary_W$Year[1:15], summary_W$Con_Rate_MH[1:15]*100, type = "b",frame=FALSE, pch=19,col="red",ylim=c(-2,10),xlab = "Year",ylab = "Construction Rate (%)",main = "Model validation: MH Con Rates, W")
lines(stockModel_W$Year[1:15],stockModel_W$Con_Rate_MH[1:15]*100,col="blue",type = "b",lty=2,pch=18)
legend("topleft",legend = c("AHS values","Modeled values","MHS values"),col=c("red","blue","forestgreen"),lty=c(1,2,1),cex=0.75)
text(1995,2,paste("AHS mean: ",round(100*mean(summary_W$Con_Rate_MH[1:15]),3),sep = ""))
text(1995,1.5,paste("Model mean: ",round(mean(100*stockModel_W$Con_Rate_MH[1:15]),3),sep = ""))

windows()
plot(summary_W$Year[1:15], 100*summary_W$Dem_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Annual demolition rate (%)",main = "Model validation: MH Demolition Rates, W")
lines(stockModel_W$Year[1:15],100*stockModel_W$Dem_Rate_MH[1:15],type = "b",pch=18,col="blue",lty=2)
legend("topleft",legend = c("AHS values","Modeled values"),col=c("red","blue"),lty=1:2,cex=0.75)
text(1995,2.2,paste("AHS mean: ",100*round(mean(summary_W$Dem_Rate_MH[1:15]),4),"%",sep = ""))
text(1995,2.0,paste("Model mean: ",100*round(mean(stockModel_W$Dem_Rate_MH[1:15]),4),"%",sep = ""))

windows()
plot(summary_W$Year[1:15],100*summary_W$Demol_Rate_MH[1:15], type = "b",frame=FALSE, pch=19,col="red",xlab = "Year",ylab = "Demolition / Tot Removals (%)",main = "Demolition percent of total stock removals, MH")
text(1990,40,paste("Mean percent: ",round(mean(100*summary_W$Demol_Rate_MH[1:15]),1),"%",sep = ""))