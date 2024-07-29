# script to extract the total m2 of total (occ + vacant) housing stock by county, type and cohort in 2019, 
# and then calculate the materials in the stock in 2019, based on per county material intensities by type

# Peter Berrill June 26 2022

rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(stringr)
#setwd("~/Yale Courses/Research/Final Paper/HSM_github")

# this containts the total stock of housing by county, type, and cohort in 2019
load("Intermediate_results/TotalHousing2019.RData")

# now to calc m2, I need the avg m2 per house type and cohort by county. we have that in the County_FloorArea object
load("HSM_results/County_FloorArea.RData")
smop<-smop_base_FA[,-3]
rm(list=ls(pattern = 'smop_'))

stock<-merge(smop,h19tb_new[,-c(2:15,34:53)])
# calc FA for SF
stock[,c("m2_SF_pre40","m2_SF_4059","m2_SF_6079","m2_SF_8099","m2_SF_2000","m2_SF_2010")]<-
  stock[,c(22:27)]*stock[,c(6,9,12,15,18,21)]
# calc FA for MF
stock[,c("m2_MF_pre40","m2_MF_4059","m2_MF_6079","m2_MF_8099","m2_MF_2000","m2_MF_2010")]<-
  stock[,c(28:33)]*stock[,c(4,7,10,13,16,19)]
# calc FA for MH
stock[,c("m2_MH_pre40","m2_MH_4059","m2_MH_6079","m2_MH_8099","m2_MH_2000","m2_MH_2010")]<-
  stock[,c(34:39)]*stock[,c(5,8,11,14,17,20)]

# now load material intensities 
load('Material_Intensities/MI_tc.RData')

stock<-merge(stock,m_tc_all[,-2])
# calculate material totals for SF 
stock[,c('kt_Cement_SF_pre40','kt_Cement_SF_4059','kt_Cement_SF_6079','kt_Cement_SF_8099','kt_Cement_SF_2000','kt_Cement_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Cement*1e-6
stock[,c('kt_Concrete_SF_pre40','kt_Concrete_SF_4059','kt_Concrete_SF_6079','kt_Concrete_SF_8099','kt_Concrete_SF_2000','kt_Concrete_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Concrete*1e-6
stock[,c('kt_Steel_SF_pre40','kt_Steel_SF_4059','kt_Steel_SF_6079','kt_Steel_SF_8099','kt_Steel_SF_2000','kt_Steel_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Steel*1e-6
stock[,c('kt_Glass_SF_pre40','kt_Glass_SF_4059','kt_Glass_SF_6079','kt_Glass_SF_8099','kt_Glass_SF_2000','kt_Glass_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Glass*1e-6
stock[,c('kt_Wood_SF_pre40','kt_Wood_SF_4059','kt_Wood_SF_6079','kt_Wood_SF_8099','kt_Wood_SF_2000','kt_Wood_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Wood*1e-6
stock[,c('kt_Gypsum_SF_pre40','kt_Gypsum_SF_4059','kt_Gypsum_SF_6079','kt_Gypsum_SF_8099','kt_Gypsum_SF_2000','kt_Gypsum_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Gypsum*1e-6
stock[,c('kt_Insulation_SF_pre40','kt_Insulation_SF_4059','kt_Insulation_SF_6079','kt_Insulation_SF_8099','kt_Insulation_SF_2000','kt_Insulation_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Insulation*1e-6
stock[,c('kt_Fibreglass_SF_pre40','kt_Fibreglass_SF_4059','kt_Fibreglass_SF_6079','kt_Fibreglass_SF_8099','kt_Fibreglass_SF_2000','kt_Fibreglass_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Fibreglass*1e-6
stock[,c('kt_Sand&Aggregate_SF_pre40','kt_Sand&Aggregate_SF_4059','kt_Sand&Aggregate_SF_6079','kt_Sand&Aggregate_SF_8099','kt_Sand&Aggregate_SF_2000','kt_Sand&Aggregate_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Sand_Aggregate*1e-6
stock[,c('kt_Other_SF_pre40','kt_Other_SF_4059','kt_Other_SF_6079','kt_Other_SF_8099','kt_Other_SF_2000','kt_Other_SF_2010')]<-
  stock[,c('m2_SF_pre40','m2_SF_4059','m2_SF_6079','m2_SF_8099','m2_SF_2000','m2_SF_2010')]*stock$SF_Other*1e-6

# calculate material totals for MF 
stock[,c('kt_Cement_MF_pre40','kt_Cement_MF_4059','kt_Cement_MF_6079','kt_Cement_MF_8099','kt_Cement_MF_2000','kt_Cement_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Cement*1e-6
stock[,c('kt_Concrete_MF_pre40','kt_Concrete_MF_4059','kt_Concrete_MF_6079','kt_Concrete_MF_8099','kt_Concrete_MF_2000','kt_Concrete_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Concrete*1e-6
stock[,c('kt_Steel_MF_pre40','kt_Steel_MF_4059','kt_Steel_MF_6079','kt_Steel_MF_8099','kt_Steel_MF_2000','kt_Steel_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Steel*1e-6
stock[,c('kt_Glass_MF_pre40','kt_Glass_MF_4059','kt_Glass_MF_6079','kt_Glass_MF_8099','kt_Glass_MF_2000','kt_Glass_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Glass*1e-6
stock[,c('kt_Wood_MF_pre40','kt_Wood_MF_4059','kt_Wood_MF_6079','kt_Wood_MF_8099','kt_Wood_MF_2000','kt_Wood_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Wood*1e-6
stock[,c('kt_Gypsum_MF_pre40','kt_Gypsum_MF_4059','kt_Gypsum_MF_6079','kt_Gypsum_MF_8099','kt_Gypsum_MF_2000','kt_Gypsum_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Gypsum*1e-6
stock[,c('kt_Insulation_MF_pre40','kt_Insulation_MF_4059','kt_Insulation_MF_6079','kt_Insulation_MF_8099','kt_Insulation_MF_2000','kt_Insulation_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Insulation*1e-6
stock[,c('kt_Fibreglass_MF_pre40','kt_Fibreglass_MF_4059','kt_Fibreglass_MF_6079','kt_Fibreglass_MF_8099','kt_Fibreglass_MF_2000','kt_Fibreglass_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Fibreglass*1e-6
stock[,c('kt_Sand&Aggregate_MF_pre40','kt_Sand&Aggregate_MF_4059','kt_Sand&Aggregate_MF_6079','kt_Sand&Aggregate_MF_8099','kt_Sand&Aggregate_MF_2000','kt_Sand&Aggregate_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Sand_Aggregate*1e-6
stock[,c('kt_Other_MF_pre40','kt_Other_MF_4059','kt_Other_MF_6079','kt_Other_MF_8099','kt_Other_MF_2000','kt_Other_MF_2010')]<-
  stock[,c('m2_MF_pre40','m2_MF_4059','m2_MF_6079','m2_MF_8099','m2_MF_2000','m2_MF_2010')]*stock$MF_Other*1e-6

# calculate material totals for MH 
stock[,c('kt_Cement_MH_pre40','kt_Cement_MH_4059','kt_Cement_MH_6079','kt_Cement_MH_8099','kt_Cement_MH_2000','kt_Cement_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Cement*1e-6
stock[,c('kt_Concrete_MH_pre40','kt_Concrete_MH_4059','kt_Concrete_MH_6079','kt_Concrete_MH_8099','kt_Concrete_MH_2000','kt_Concrete_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Concrete*1e-6
stock[,c('kt_Steel_MH_pre40','kt_Steel_MH_4059','kt_Steel_MH_6079','kt_Steel_MH_8099','kt_Steel_MH_2000','kt_Steel_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Steel*1e-6
stock[,c('kt_Glass_MH_pre40','kt_Glass_MH_4059','kt_Glass_MH_6079','kt_Glass_MH_8099','kt_Glass_MH_2000','kt_Glass_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Glass*1e-6
stock[,c('kt_Wood_MH_pre40','kt_Wood_MH_4059','kt_Wood_MH_6079','kt_Wood_MH_8099','kt_Wood_MH_2000','kt_Wood_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Wood*1e-6
stock[,c('kt_Gypsum_MH_pre40','kt_Gypsum_MH_4059','kt_Gypsum_MH_6079','kt_Gypsum_MH_8099','kt_Gypsum_MH_2000','kt_Gypsum_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Gypsum*1e-6
stock[,c('kt_Insulation_MH_pre40','kt_Insulation_MH_4059','kt_Insulation_MH_6079','kt_Insulation_MH_8099','kt_Insulation_MH_2000','kt_Insulation_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Insulation*1e-6
stock[,c('kt_Fibreglass_MH_pre40','kt_Fibreglass_MH_4059','kt_Fibreglass_MH_6079','kt_Fibreglass_MH_8099','kt_Fibreglass_MH_2000','kt_Fibreglass_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Fibreglass*1e-6
stock[,c('kt_Sand&Aggregate_MH_pre40','kt_Sand&Aggregate_MH_4059','kt_Sand&Aggregate_MH_6079','kt_Sand&Aggregate_MH_8099','kt_Sand&Aggregate_MH_2000','kt_Sand&Aggregate_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Sand_Aggregate*1e-6
stock[,c('kt_Other_MH_pre40','kt_Other_MH_4059','kt_Other_MH_6079','kt_Other_MH_8099','kt_Other_MH_2000','kt_Other_MH_2010')]<-
  stock[,c('m2_MH_pre40','m2_MH_4059','m2_MH_6079','m2_MH_8099','m2_MH_2000','m2_MH_2010')]*stock$MH_Other*1e-6

mat_stock_US<-as.data.frame(colSums(stock[,c(88:267)]))
mat_stock_US$key<-rownames(mat_stock_US)
mat_stock_US$Material<-mat_stock_US$Type<-mat_stock_US$Cohort<-'Unassigned'
for (k in 1:nrow(mat_stock_US)) {
  mat_stock_US$Material[k]<-str_split(mat_stock_US$key[k], "_")[[1]][2]
  mat_stock_US$Type[k]<-str_split(mat_stock_US$key[k], "_")[[1]][3]
  mat_stock_US$Cohort[k]<-str_split(mat_stock_US$key[k], "_")[[1]][4]
  
}

row.names(mat_stock_US)<-1:nrow(mat_stock_US)
mat_stock_US<-mat_stock_US[,-c(2)]                         
names(mat_stock_US)[1]<-'Mass_kt'

mat_stock_US[mat_stock_US$Cohort=='pre40',]$Cohort<-'<1940'
mat_stock_US[mat_stock_US$Cohort=='4059',]$Cohort<-'1940-1959'
mat_stock_US[mat_stock_US$Cohort=='6079',]$Cohort<-'1960-1979'
mat_stock_US[mat_stock_US$Cohort=='8099',]$Cohort<-'1980-1999'
mat_stock_US[mat_stock_US$Cohort=='2000',]$Cohort<-'2000-2009'
mat_stock_US[mat_stock_US$Cohort=='2010',]$Cohort<-'2010+'

FA_stock_US<-as.data.frame(colSums(stock[,c(40:57)]))
FA_stock_US$key<-rownames(FA_stock_US)

for (k in 1:nrow(FA_stock_US)) {
  FA_stock_US$Type[k]<-str_split(FA_stock_US$key[k], "_")[[1]][2]
  FA_stock_US$Cohort[k]<-str_split(FA_stock_US$key[k], "_")[[1]][3]
}

row.names(FA_stock_US)<-1:nrow(FA_stock_US)
FA_stock_US<-FA_stock_US[,-c(2)]                         
names(FA_stock_US)[1]<-'km2'
FA_stock_US$km2<-FA_stock_US$km2*1e-6

FA_stock_US[FA_stock_US$Cohort=='pre40',]$Cohort<-'<1940'
FA_stock_US[FA_stock_US$Cohort=='4059',]$Cohort<-'1940-1959'
FA_stock_US[FA_stock_US$Cohort=='6079',]$Cohort<-'1960-1979'
FA_stock_US[FA_stock_US$Cohort=='8099',]$Cohort<-'1980-1999'
FA_stock_US[FA_stock_US$Cohort=='2000',]$Cohort<-'2000-2009'
FA_stock_US[FA_stock_US$Cohort=='2010',]$Cohort<-'2010+'

mat_FA<-merge(FA_stock_US,mat_stock_US)
mat_FA$Mat_Int_kg_m2<-mat_FA$Mass_kt/mat_FA$km2

Units_stock_US<-as.data.frame(colSums(stock[,c(22:39)]))
Units_stock_US$key<-rownames(Units_stock_US)

for (k in 1:nrow(Units_stock_US)) {
  Units_stock_US$Type[k]<-str_split(Units_stock_US$key[k], "_")[[1]][1]
  Units_stock_US$Cohort[k]<-str_split(Units_stock_US$key[k], "_")[[1]][2]
}

row.names(Units_stock_US)<-1:nrow(Units_stock_US)
Units_stock_US<-Units_stock_US[,-c(2)]  
names(Units_stock_US)[1]<-'Units'

Units_stock_US[Units_stock_US$Cohort=='pre40',]$Cohort<-'<1940'
Units_stock_US[Units_stock_US$Cohort=='4059',]$Cohort<-'1940-1959'
Units_stock_US[Units_stock_US$Cohort=='6079',]$Cohort<-'1960-1979'
Units_stock_US[Units_stock_US$Cohort=='8099',]$Cohort<-'1980-1999'
Units_stock_US[Units_stock_US$Cohort=='2000',]$Cohort<-'2000-2009'
Units_stock_US[Units_stock_US$Cohort=='2010',]$Cohort<-'2010+'
Units_FA<-merge(FA_stock_US,Units_stock_US)

write.csv(mat_FA,file='HSM_Results/MatStock_FA_MI_2019.csv',row.names = FALSE)
write.csv(Units_FA,file='HSM_Results/MatStock_FA_Units_2019.csv',row.names = FALSE)
write.csv(mat_stock_US,file='HSM_Results/MatStock2019.csv',row.names = FALSE)

# add adjustment for AK and HI
mat_FA_AKHI<-mat_FA
mat_FA_AKHI[,c('km2',	'Mass_kt')]<-mat_FA[,c('km2',	'Mass_kt')]/0.994
write.csv(mat_FA_AKHI,file='HSM_Results/MatStock_FA_MI_2019_AKHI.csv',row.names = FALSE)

Units_FA_AKHI<-Units_FA
Units_FA_AKHI[,c('km2',	'Units')]<-Units_FA_AKHI[,c('km2',	'Units')]/0.994
write.csv(Units_FA_AKHI,file='HSM_Results/MatStock_FA_Units_2019_AKHI.csv',row.names = FALSE)

mat_stock_US_AKHI<-mat_stock_US
mat_stock_US_AKHI$Mass_kt<-mat_stock_US_AKHI$Mass_kt/0.994
write.csv(mat_stock_US_AKHI,file='HSM_Results/MatStock2019_AKHI.csv',row.names = FALSE)


mat_stock_US_agg<-as.data.frame(tapply(mat_stock_US_AKHI$Mass_kt,mat_stock_US_AKHI$Material,sum))
colnames(mat_stock_US_AKHI)<-'Mass_kt'
write.csv(mat_stock_US_agg,file='HSM_Results/MatStock2019agg.csv',row.names = TRUE)

mat_int_agg<-as.data.frame(tapply(mat_FA$Mass_kt,mat_FA$Material,sum)/tapply(mat_FA$km2,mat_FA$Material,sum))
colnames(mat_int_agg)<-'kg/m2'
write.csv(mat_int_agg,file='HSM_Results/MatInt2019agg.csv',row.names = TRUE)

load("~/projects/Yale/HSM_github/HSM_results/US_FA_GHG_summaries.RData")

us_bas_dem_mat<-us_base_FA[,c("Year","M_Dem_Cem","M_Dem_Con","M_Dem_Stl","M_Dem_Gls","M_Dem_Gyp","M_Dem_Ins","M_Dem_Wod" ,"M_Dem_Fbg","M_Dem_SnA","M_Dem_Oth","M_Dem")]
us_bas_dem_mat[,c("M_Dem_Cem","M_Dem_Con","M_Dem_Stl","M_Dem_Gls","M_Dem_Gyp","M_Dem_Ins","M_Dem_Wod" ,"M_Dem_Fbg","M_Dem_SnA","M_Dem_Oth","M_Dem")]<-1e-9*us_bas_dem_mat[,c("M_Dem_Cem","M_Dem_Con","M_Dem_Stl","M_Dem_Gls","M_Dem_Gyp","M_Dem_Ins","M_Dem_Wod" ,"M_Dem_Fbg","M_Dem_SnA","M_Dem_Oth","M_Dem")]

write.csv(us_bas_dem_mat,file='HSM_Results/MatDem.csv',row.names = FALSE)

# estimate share of housing stock from <2020 that exists throughout the modelling period
#load("~/projects/Yale/HSM_github/HSM_results/US_smop_scenarios.RData")
# load large county level results dframe
load("~/projects/Yale/HSM_github/HSM_results/County_FloorArea_Mat.RData")

sum_con_dem<-data.frame(rep(smop_base_FA$GeoID,each=41))
names(sum_con_dem)<-'GeoID'
sum_con_dem$Year<-rep(2020:2060,3108)
sum_con_dem[,c('FA_Dem_SF','FA_Dem_MF','FA_Dem_MH','FA_Con_SF','FA_Con_MF','FA_Con_MH')]<-0
for (k in 1:3108) {
  print(k)
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k],]$FA_Dem_SF<-smop_base_FA[[3]][[k]]$Dem_SF_m2
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k],]$FA_Dem_MF<-smop_base_FA[[3]][[k]]$Dem_MF_m2
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k],]$FA_Dem_MH<-smop_base_FA[[3]][[k]]$Dem_MH_m2
  
  
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k],]$FA_Con_SF<-smop_base_FA[[3]][[k]]$NC_SF_m2
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k],]$FA_Con_MF<-smop_base_FA[[3]][[k]]$NC_MF_m2
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k],]$FA_Con_MH<-smop_base_FA[[3]][[k]]$NC_MH_m2
  
  sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k] & sum_con_dem$Year==2060,c('FA_Dem_SF', 'FA_Dem_MF', 'FA_Dem_MH', 'FA_Con_SF', 'FA_Con_MF', 'FA_Con_MH')]<-
    sum_con_dem[sum_con_dem$GeoID==smop_base_FA$GeoID[k] & sum_con_dem$Year==2059,c('FA_Dem_SF', 'FA_Dem_MF', 'FA_Dem_MH', 'FA_Con_SF', 'FA_Con_MF', 'FA_Con_MH')]
}


sum_FA_vint<-data.frame(rep(smop_base_FA$GeoID,each=41))
names(sum_FA_vint)<-'GeoID'
sum_FA_vint$Year<-rep(2020:2060,3108)
sum_FA_vint[,c('FA_SF_p1940','FA_SF_1940_59','FA_SF_1960_79','FA_SF_1980_99','FA_SF_2000_09','FA_SF_2010_19','FA_SF_2020_29','FA_SF_2030_39','FA_SF_2040_49','FA_SF_2050_60')]<-0
sum_FA_vint[,c('FA_MF_p1940','FA_MF_1940_59','FA_MF_1960_79','FA_MF_1980_99','FA_MF_2000_09','FA_MF_2010_19','FA_MF_2020_29','FA_MF_2030_39','FA_MF_2040_49','FA_MF_2050_60')]<-0
sum_FA_vint[,c('FA_MH_p1940','FA_MH_1940_59','FA_MH_1960_79','FA_MH_1980_99','FA_MH_2000_09','FA_MH_2010_19','FA_MH_2020_29','FA_MH_2030_39','FA_MH_2040_49','FA_MH_2050_60')]<-0

sum_FA_vint[,c('N_SF_p1940','N_SF_1940_59','N_SF_1960_79','N_SF_1980_99','N_SF_2000_09','N_SF_2010_19','N_SF_2020_29','N_SF_2030_39','N_SF_2040_49','N_SF_2050_60')]<-0
sum_FA_vint[,c('N_MF_p1940','N_MF_1940_59','N_MF_1960_79','N_MF_1980_99','N_MF_2000_09','N_MF_2010_19','N_MF_2020_29','N_MF_2030_39','N_MF_2040_49','N_MF_2050_60')]<-0
sum_FA_vint[,c('N_MH_p1940','N_MH_1940_59','N_MH_1960_79','N_MH_1980_99','N_MH_2000_09','N_MH_2010_19','N_MH_2020_29','N_MH_2030_39','N_MH_2040_49','N_MH_2050_60')]<-0

sum_FA_occ<-sum_FA_vint

# this takes a while
for (i in 1:3108) {
  print(i)
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_p1940<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_p1940','Tot_HU_SF_Vac_p1940')]* smop_base_FA$`FA.SF.<1940`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_1940_59<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1940_59','Tot_HU_SF_Vac_1940_59')]* smop_base_FA$`FA.SF.1940-59`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_1960_79<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1960_79','Tot_HU_SF_Vac_1960_79')]* smop_base_FA$`FA.SF.1960-79`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_1980_99<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1980_99','Tot_HU_SF_Vac_1980_99')]* smop_base_FA$`FA.SF.1980-99`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2000_09<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2000_09','Tot_HU_SF_Vac_2000_09')]* smop_base_FA$`FA.SF.2000-09`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2010_19<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2010_19','Tot_HU_SF_Vac_2010_19')]* smop_base_FA$`FA.SF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2020_29<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2020_29','Tot_HU_SF_Vac_2020_29')]* smop_base_FA$`FA.SF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2030_39<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2030_39','Tot_HU_SF_Vac_2030_39')]* smop_base_FA$`FA.SF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2040_49<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2040_49','Tot_HU_SF_Vac_2040_49')]* smop_base_FA$`FA.SF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2050_60<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2050_60','Tot_HU_SF_Vac_2050_60')]* smop_base_FA$`FA.SF.2010s`[i])
  
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_p1940<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_p1940','Tot_HU_MF_Vac_p1940')]* smop_base_FA$`FA.MF.<1940`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_1940_59<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1940_59','Tot_HU_MF_Vac_1940_59')]* smop_base_FA$`FA.MF.1940-59`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_1960_79<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1960_79','Tot_HU_MF_Vac_1960_79')]* smop_base_FA$`FA.MF.1960-79`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_1980_99<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1980_99','Tot_HU_MF_Vac_1980_99')]* smop_base_FA$`FA.MF.1980-99`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2000_09<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2000_09','Tot_HU_MF_Vac_2000_09')]* smop_base_FA$`FA.MF.2000-09`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2010_19<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2010_19','Tot_HU_MF_Vac_2010_19')]* smop_base_FA$`FA.MF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2020_29<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2020_29','Tot_HU_MF_Vac_2020_29')]* smop_base_FA$`FA.MF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2030_39<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2030_39','Tot_HU_MF_Vac_2030_39')]* smop_base_FA$`FA.MF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2040_49<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2040_49','Tot_HU_MF_Vac_2040_49')]* smop_base_FA$`FA.MF.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2050_60<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2050_60','Tot_HU_MF_Vac_2050_60')]* smop_base_FA$`FA.MF.2010s`[i])
  
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_p1940<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_p1940','Tot_HU_MH_Vac_p1940')]* smop_base_FA$`FA.MH.<1940`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_1940_59<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1940_59','Tot_HU_MH_Vac_1940_59')]* smop_base_FA$`FA.MH.1940-59`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_1960_79<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1960_79','Tot_HU_MH_Vac_1960_79')]* smop_base_FA$`FA.MH.1960-79`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_1980_99<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1980_99','Tot_HU_MH_Vac_1980_99')]* smop_base_FA$`FA.MH.1980-99`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2000_09<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2000_09','Tot_HU_MH_Vac_2000_09')]* smop_base_FA$`FA.MH.2000-09`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2010_19<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2010_19','Tot_HU_MH_Vac_2010_19')]* smop_base_FA$`FA.MH.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2020_29<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2020_29','Tot_HU_MH_Vac_2020_29')]* smop_base_FA$`FA.MH.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2030_39<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2030_39','Tot_HU_MH_Vac_2030_39')]* smop_base_FA$`FA.MH.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2040_49<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2040_49','Tot_HU_MH_Vac_2040_49')]* smop_base_FA$`FA.MH.2010s`[i])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2050_60<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2050_60','Tot_HU_MH_Vac_2050_60')]* smop_base_FA$`FA.MH.2010s`[i])
  
  # Add also for N units
  
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_p1940<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_p1940','Tot_HU_SF_Vac_p1940')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_1940_59<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1940_59','Tot_HU_SF_Vac_1940_59')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_1960_79<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1960_79','Tot_HU_SF_Vac_1960_79')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_1980_99<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1980_99','Tot_HU_SF_Vac_1980_99')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_2000_09<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2000_09','Tot_HU_SF_Vac_2000_09')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_2010_19<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2010_19','Tot_HU_SF_Vac_2010_19')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_2020_29<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2020_29','Tot_HU_SF_Vac_2020_29')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_2030_39<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2030_39','Tot_HU_SF_Vac_2030_39')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_2040_49<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2040_49','Tot_HU_SF_Vac_2040_49')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_SF_2050_60<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2050_60','Tot_HU_SF_Vac_2050_60')])
  
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_p1940<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_p1940','Tot_HU_MF_Vac_p1940')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_1940_59<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1940_59','Tot_HU_MF_Vac_1940_59')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_1960_79<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1960_79','Tot_HU_MF_Vac_1960_79')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_1980_99<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1980_99','Tot_HU_MF_Vac_1980_99')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_2000_09<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2000_09','Tot_HU_MF_Vac_2000_09')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_2010_19<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2010_19','Tot_HU_MF_Vac_2010_19')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_2020_29<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2020_29','Tot_HU_MF_Vac_2020_29')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_2030_39<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2030_39','Tot_HU_MF_Vac_2030_39')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_2040_49<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2040_49','Tot_HU_MF_Vac_2040_49')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MF_2050_60<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2050_60','Tot_HU_MF_Vac_2050_60')])
  
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_p1940<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_p1940','Tot_HU_MH_Vac_p1940')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_1940_59<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1940_59','Tot_HU_MH_Vac_1940_59')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_1960_79<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1960_79','Tot_HU_MH_Vac_1960_79')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_1980_99<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1980_99','Tot_HU_MH_Vac_1980_99')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_2000_09<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2000_09','Tot_HU_MH_Vac_2000_09')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_2010_19<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2010_19','Tot_HU_MH_Vac_2010_19')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_2020_29<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2020_29','Tot_HU_MH_Vac_2020_29')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_2030_39<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2030_39','Tot_HU_MH_Vac_2030_39')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_2040_49<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2040_49','Tot_HU_MH_Vac_2040_49')])
  sum_FA_vint[sum_FA_vint$GeoID==smop_base_FA$GeoID[i],]$N_MH_2050_60<-rowSums(smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2050_60','Tot_HU_MH_Vac_2050_60')])
  
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_p1940<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_p1940')]* smop_base_FA$`FA.SF.<1940`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_1940_59<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1940_59')]* smop_base_FA$`FA.SF.1940-59`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_1960_79<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1960_79')]* smop_base_FA$`FA.SF.1960-79`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_1980_99<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1980_99')]* smop_base_FA$`FA.SF.1980-99`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2000_09<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2000_09')]* smop_base_FA$`FA.SF.2000-09`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2010_19<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2010_19')]* smop_base_FA$`FA.SF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2020_29<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2020_29')]* smop_base_FA$`FA.SF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2030_39<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2030_39')]* smop_base_FA$`FA.SF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2040_49<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2040_49')]* smop_base_FA$`FA.SF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_SF_2050_60<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2050_60')]* smop_base_FA$`FA.SF.2010s`[i]
  
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_p1940<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_p1940')]* smop_base_FA$`FA.MF.<1940`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_1940_59<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1940_59')]* smop_base_FA$`FA.MF.1940-59`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_1960_79<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1960_79')]* smop_base_FA$`FA.MF.1960-79`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_1980_99<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1980_99')]* smop_base_FA$`FA.MF.1980-99`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2000_09<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2000_09')]* smop_base_FA$`FA.MF.2000-09`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2010_19<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2010_19')]* smop_base_FA$`FA.MF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2020_29<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2020_29')]* smop_base_FA$`FA.MF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2030_39<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2030_39')]* smop_base_FA$`FA.MF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2040_49<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2040_49')]* smop_base_FA$`FA.MF.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MF_2050_60<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2050_60')]* smop_base_FA$`FA.MF.2010s`[i]
  
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_p1940<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_p1940')]* smop_base_FA$`FA.MH.<1940`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_1940_59<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1940_59')]* smop_base_FA$`FA.MH.1940-59`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_1960_79<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1960_79')]* smop_base_FA$`FA.MH.1960-79`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_1980_99<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1980_99')]* smop_base_FA$`FA.MH.1980-99`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2000_09<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2000_09')]* smop_base_FA$`FA.MH.2000-09`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2010_19<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2010_19')]* smop_base_FA$`FA.MH.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2020_29<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2020_29')]* smop_base_FA$`FA.MH.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2030_39<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2030_39')]* smop_base_FA$`FA.MH.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2040_49<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2040_49')]* smop_base_FA$`FA.MH.2010s`[i]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$FA_MH_2050_60<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2050_60')]* smop_base_FA$`FA.MH.2010s`[i]
  
  # again, with total units
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_p1940<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_p1940')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_1940_59<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1940_59')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_1960_79<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1960_79')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_1980_99<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_1980_99')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_2000_09<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2000_09')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_2010_19<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2010_19')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_2020_29<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2020_29')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_2030_39<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2030_39')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_2040_49<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2040_49')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_SF_2050_60<-smop_base_FA[[3]][[i]][,c('Tot_HU_SF_Occ_2050_60')]
  
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_p1940<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_p1940')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_1940_59<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1940_59')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_1960_79<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1960_79')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_1980_99<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_1980_99')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_2000_09<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2000_09')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_2010_19<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2010_19')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_2020_29<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2020_29')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_2030_39<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2030_39')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_2040_49<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2040_49')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MF_2050_60<-smop_base_FA[[3]][[i]][,c('Tot_HU_MF_Occ_2050_60')]
  
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_p1940<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_p1940')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_1940_59<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1940_59')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_1960_79<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1960_79')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_1980_99<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_1980_99')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_2000_09<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2000_09')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_2010_19<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2010_19')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_2020_29<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2020_29')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_2030_39<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2030_39')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_2040_49<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2040_49')]
  sum_FA_occ[sum_FA_occ$GeoID==smop_base_FA$GeoID[i],]$N_MH_2050_60<-smop_base_FA[[3]][[i]][,c('Tot_HU_MH_Occ_2050_60')]

}


sum_FA_vint$FA_p1940<-sum_FA_vint$FA_SF_p1940+sum_FA_vint$FA_MF_p1940+sum_FA_vint$FA_MH_p1940
sum_FA_vint$FA_1940_59<-sum_FA_vint$FA_SF_1940_59+sum_FA_vint$FA_MF_1940_59+sum_FA_vint$FA_MH_1940_59
sum_FA_vint$FA_1960_79<-sum_FA_vint$FA_SF_1960_79+sum_FA_vint$FA_MF_1960_79+sum_FA_vint$FA_MH_1960_79
sum_FA_vint$FA_1980_99<-sum_FA_vint$FA_SF_1980_99+sum_FA_vint$FA_MF_1980_99+sum_FA_vint$FA_MH_1980_99
sum_FA_vint$FA_2000_09<-sum_FA_vint$FA_SF_2000_09+sum_FA_vint$FA_MF_2000_09+sum_FA_vint$FA_MH_2000_09
sum_FA_vint$FA_2010_19<-sum_FA_vint$FA_SF_2010_19+sum_FA_vint$FA_MF_2010_19+sum_FA_vint$FA_MH_2010_19
sum_FA_vint$FA_2020_29<-sum_FA_vint$FA_SF_2020_29+sum_FA_vint$FA_MF_2020_29+sum_FA_vint$FA_MH_2020_29
sum_FA_vint$FA_2030_39<-sum_FA_vint$FA_SF_2030_39+sum_FA_vint$FA_MF_2030_39+sum_FA_vint$FA_MH_2030_39
sum_FA_vint$FA_2040_49<-sum_FA_vint$FA_SF_2040_49+sum_FA_vint$FA_MF_2040_49+sum_FA_vint$FA_MH_2040_49
sum_FA_vint$FA_2050_60<-sum_FA_vint$FA_SF_2050_60+sum_FA_vint$FA_MF_2050_60+sum_FA_vint$FA_MH_2050_60

# sum_FA_vint$N_p1940<-sum_FA_vint$N_SF_p1940+sum_FA_vint$N_MF_p1940+sum_FA_vint$N_MH_p1940
# sum_FA_vint$N_1940_59<-sum_FA_vint$N_SF_1940_59+sum_FA_vint$N_MF_1940_59+sum_FA_vint$N_MH_1940_59
# sum_FA_vint$N_1960_79<-sum_FA_vint$N_SF_1960_79+sum_FA_vint$N_MF_1960_79+sum_FA_vint$N_MH_1960_79
# sum_FA_vint$N_1980_99<-sum_FA_vint$N_SF_1980_99+sum_FA_vint$N_MF_1980_99+sum_FA_vint$N_MH_1980_99
# sum_FA_vint$N_2000_09<-sum_FA_vint$N_SF_2000_09+sum_FA_vint$N_MF_2000_09+sum_FA_vint$N_MH_2000_09
# sum_FA_vint$N_2010_19<-sum_FA_vint$N_SF_2010_19+sum_FA_vint$N_MF_2010_19+sum_FA_vint$N_MH_2010_19
# sum_FA_vint$N_2020_29<-sum_FA_vint$N_SF_2020_29+sum_FA_vint$N_MF_2020_29+sum_FA_vint$N_MH_2020_29
# sum_FA_vint$N_2030_39<-sum_FA_vint$N_SF_2030_39+sum_FA_vint$N_MF_2030_39+sum_FA_vint$N_MH_2030_39
# sum_FA_vint$N_2040_49<-sum_FA_vint$N_SF_2040_49+sum_FA_vint$N_MF_2040_49+sum_FA_vint$N_MH_2040_49
# sum_FA_vint$N_2050_60<-sum_FA_vint$N_SF_2050_60+sum_FA_vint$N_MF_2050_60+sum_FA_vint$N_MH_2050_60

sum_FA_vint$FA_SF<-rowSums(sum_FA_vint[,3:12])
sum_FA_vint$FA_MF<-rowSums(sum_FA_vint[,13:22])
sum_FA_vint$FA_MH<-rowSums(sum_FA_vint[,23:32])

sum_FA_occ$FA_p1940<-sum_FA_occ$FA_SF_p1940+sum_FA_occ$FA_MF_p1940+sum_FA_occ$FA_MH_p1940
sum_FA_occ$FA_1940_59<-sum_FA_occ$FA_SF_1940_59+sum_FA_occ$FA_MF_1940_59+sum_FA_occ$FA_MH_1940_59
sum_FA_occ$FA_1960_79<-sum_FA_occ$FA_SF_1960_79+sum_FA_occ$FA_MF_1960_79+sum_FA_occ$FA_MH_1960_79
sum_FA_occ$FA_1980_99<-sum_FA_occ$FA_SF_1980_99+sum_FA_occ$FA_MF_1980_99+sum_FA_occ$FA_MH_1980_99
sum_FA_occ$FA_2000_09<-sum_FA_occ$FA_SF_2000_09+sum_FA_occ$FA_MF_2000_09+sum_FA_occ$FA_MH_2000_09
sum_FA_occ$FA_2010_19<-sum_FA_occ$FA_SF_2010_19+sum_FA_occ$FA_MF_2010_19+sum_FA_occ$FA_MH_2010_19
sum_FA_occ$FA_2020_29<-sum_FA_occ$FA_SF_2020_29+sum_FA_occ$FA_MF_2020_29+sum_FA_occ$FA_MH_2020_29
sum_FA_occ$FA_2030_39<-sum_FA_occ$FA_SF_2030_39+sum_FA_occ$FA_MF_2030_39+sum_FA_occ$FA_MH_2030_39
sum_FA_occ$FA_2040_49<-sum_FA_occ$FA_SF_2040_49+sum_FA_occ$FA_MF_2040_49+sum_FA_occ$FA_MH_2040_49
sum_FA_occ$FA_2050_60<-sum_FA_occ$FA_SF_2050_60+sum_FA_occ$FA_MF_2050_60+sum_FA_occ$FA_MH_2050_60

sum_FA_occ$FA_SF<-rowSums(sum_FA_occ[,3:12])
sum_FA_occ$FA_MF<-rowSums(sum_FA_occ[,13:22])
sum_FA_occ$FA_MH<-rowSums(sum_FA_occ[,23:32])

#sum_FA_vint2<-melt(sum_FA_vint,id=c('GeoID','Year'))
Year<-(2020:2060)
US_FA_vint<-data.frame(Year)


# add in FA to account for AK and HI
US_FA_vint$FA_SF_p1940<-tapply(sum_FA_vint$FA_SF_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_1940_59<-tapply(sum_FA_vint$FA_SF_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_1960_79<-tapply(sum_FA_vint$FA_SF_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_1980_99<-tapply(sum_FA_vint$FA_SF_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_2000_09<-tapply(sum_FA_vint$FA_SF_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_2010_19<-tapply(sum_FA_vint$FA_SF_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_2020_29<-tapply(sum_FA_vint$FA_SF_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_2030_39<-tapply(sum_FA_vint$FA_SF_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_2040_49<-tapply(sum_FA_vint$FA_SF_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_SF_2050_60<-tapply(sum_FA_vint$FA_SF_2050_60,sum_FA_vint$Year,sum)/0.994

US_FA_vint$FA_MF_p1940<-tapply(sum_FA_vint$FA_MF_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_1940_59<-tapply(sum_FA_vint$FA_MF_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_1960_79<-tapply(sum_FA_vint$FA_MF_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_1980_99<-tapply(sum_FA_vint$FA_MF_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_2000_09<-tapply(sum_FA_vint$FA_MF_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_2010_19<-tapply(sum_FA_vint$FA_MF_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_2020_29<-tapply(sum_FA_vint$FA_MF_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_2030_39<-tapply(sum_FA_vint$FA_MF_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_2040_49<-tapply(sum_FA_vint$FA_MF_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MF_2050_60<-tapply(sum_FA_vint$FA_MF_2050_60,sum_FA_vint$Year,sum)/0.994

US_FA_vint$FA_MH_p1940<-tapply(sum_FA_vint$FA_MH_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_1940_59<-tapply(sum_FA_vint$FA_MH_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_1960_79<-tapply(sum_FA_vint$FA_MH_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_1980_99<-tapply(sum_FA_vint$FA_MH_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_2000_09<-tapply(sum_FA_vint$FA_MH_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_2010_19<-tapply(sum_FA_vint$FA_MH_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_2020_29<-tapply(sum_FA_vint$FA_MH_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_2030_39<-tapply(sum_FA_vint$FA_MH_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_2040_49<-tapply(sum_FA_vint$FA_MH_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_MH_2050_60<-tapply(sum_FA_vint$FA_MH_2050_60,sum_FA_vint$Year,sum)/0.994

# NOW sum up housing units
US_FA_vint$N_SF_p1940<-tapply(sum_FA_vint$N_SF_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_1940_59<-tapply(sum_FA_vint$N_SF_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_1960_79<-tapply(sum_FA_vint$N_SF_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_1980_99<-tapply(sum_FA_vint$N_SF_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_2000_09<-tapply(sum_FA_vint$N_SF_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_2010_19<-tapply(sum_FA_vint$N_SF_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_2020_29<-tapply(sum_FA_vint$N_SF_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_2030_39<-tapply(sum_FA_vint$N_SF_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_2040_49<-tapply(sum_FA_vint$N_SF_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_SF_2050_60<-tapply(sum_FA_vint$N_SF_2050_60,sum_FA_vint$Year,sum)/0.994

US_FA_vint$N_MF_p1940<-tapply(sum_FA_vint$N_MF_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_1940_59<-tapply(sum_FA_vint$N_MF_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_1960_79<-tapply(sum_FA_vint$N_MF_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_1980_99<-tapply(sum_FA_vint$N_MF_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_2000_09<-tapply(sum_FA_vint$N_MF_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_2010_19<-tapply(sum_FA_vint$N_MF_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_2020_29<-tapply(sum_FA_vint$N_MF_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_2030_39<-tapply(sum_FA_vint$N_MF_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_2040_49<-tapply(sum_FA_vint$N_MF_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MF_2050_60<-tapply(sum_FA_vint$N_MF_2050_60,sum_FA_vint$Year,sum)/0.994

US_FA_vint$N_MH_p1940<-tapply(sum_FA_vint$N_MH_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_1940_59<-tapply(sum_FA_vint$N_MH_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_1960_79<-tapply(sum_FA_vint$N_MH_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_1980_99<-tapply(sum_FA_vint$N_MH_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_2000_09<-tapply(sum_FA_vint$N_MH_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_2010_19<-tapply(sum_FA_vint$N_MH_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_2020_29<-tapply(sum_FA_vint$N_MH_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_2030_39<-tapply(sum_FA_vint$N_MH_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_2040_49<-tapply(sum_FA_vint$N_MH_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$N_MH_2050_60<-tapply(sum_FA_vint$N_MH_2050_60,sum_FA_vint$Year,sum)/0.994

# sum up occupied floor area too
US_FA_occ<-data.frame(Year)
US_FA_occ$FA_SF_p1940<-tapply(sum_FA_occ$FA_SF_p1940,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_1940_59<-tapply(sum_FA_occ$FA_SF_1940_59,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_1960_79<-tapply(sum_FA_occ$FA_SF_1960_79,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_1980_99<-tapply(sum_FA_occ$FA_SF_1980_99,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_2000_09<-tapply(sum_FA_occ$FA_SF_2000_09,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_2010_19<-tapply(sum_FA_occ$FA_SF_2010_19,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_2020_29<-tapply(sum_FA_occ$FA_SF_2020_29,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_2030_39<-tapply(sum_FA_occ$FA_SF_2030_39,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_2040_49<-tapply(sum_FA_occ$FA_SF_2040_49,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_SF_2050_60<-tapply(sum_FA_occ$FA_SF_2050_60,sum_FA_occ$Year,sum)/0.994

US_FA_occ$FA_MF_p1940<-tapply(sum_FA_occ$FA_MF_p1940,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_1940_59<-tapply(sum_FA_occ$FA_MF_1940_59,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_1960_79<-tapply(sum_FA_occ$FA_MF_1960_79,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_1980_99<-tapply(sum_FA_occ$FA_MF_1980_99,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_2000_09<-tapply(sum_FA_occ$FA_MF_2000_09,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_2010_19<-tapply(sum_FA_occ$FA_MF_2010_19,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_2020_29<-tapply(sum_FA_occ$FA_MF_2020_29,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_2030_39<-tapply(sum_FA_occ$FA_MF_2030_39,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_2040_49<-tapply(sum_FA_occ$FA_MF_2040_49,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MF_2050_60<-tapply(sum_FA_occ$FA_MF_2050_60,sum_FA_occ$Year,sum)/0.994

US_FA_occ$FA_MH_p1940<-tapply(sum_FA_occ$FA_MH_p1940,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_1940_59<-tapply(sum_FA_occ$FA_MH_1940_59,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_1960_79<-tapply(sum_FA_occ$FA_MH_1960_79,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_1980_99<-tapply(sum_FA_occ$FA_MH_1980_99,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_2000_09<-tapply(sum_FA_occ$FA_MH_2000_09,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_2010_19<-tapply(sum_FA_occ$FA_MH_2010_19,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_2020_29<-tapply(sum_FA_occ$FA_MH_2020_29,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_2030_39<-tapply(sum_FA_occ$FA_MH_2030_39,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_2040_49<-tapply(sum_FA_occ$FA_MH_2040_49,sum_FA_occ$Year,sum)/0.994
US_FA_occ$FA_MH_2050_60<-tapply(sum_FA_occ$FA_MH_2050_60,sum_FA_occ$Year,sum)/0.994

# sum up occupied units area too
US_FA_occ$N_SF_p1940<-tapply(sum_FA_occ$N_SF_p1940,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_1940_59<-tapply(sum_FA_occ$N_SF_1940_59,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_1960_79<-tapply(sum_FA_occ$N_SF_1960_79,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_1980_99<-tapply(sum_FA_occ$N_SF_1980_99,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_2000_09<-tapply(sum_FA_occ$N_SF_2000_09,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_2010_19<-tapply(sum_FA_occ$N_SF_2010_19,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_2020_29<-tapply(sum_FA_occ$N_SF_2020_29,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_2030_39<-tapply(sum_FA_occ$N_SF_2030_39,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_2040_49<-tapply(sum_FA_occ$N_SF_2040_49,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_SF_2050_60<-tapply(sum_FA_occ$N_SF_2050_60,sum_FA_occ$Year,sum)/0.994

US_FA_occ$N_MF_p1940<-tapply(sum_FA_occ$N_MF_p1940,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_1940_59<-tapply(sum_FA_occ$N_MF_1940_59,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_1960_79<-tapply(sum_FA_occ$N_MF_1960_79,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_1980_99<-tapply(sum_FA_occ$N_MF_1980_99,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_2000_09<-tapply(sum_FA_occ$N_MF_2000_09,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_2010_19<-tapply(sum_FA_occ$N_MF_2010_19,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_2020_29<-tapply(sum_FA_occ$N_MF_2020_29,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_2030_39<-tapply(sum_FA_occ$N_MF_2030_39,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_2040_49<-tapply(sum_FA_occ$N_MF_2040_49,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MF_2050_60<-tapply(sum_FA_occ$N_MF_2050_60,sum_FA_occ$Year,sum)/0.994

US_FA_occ$N_MH_p1940<-tapply(sum_FA_occ$N_MH_p1940,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_1940_59<-tapply(sum_FA_occ$N_MH_1940_59,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_1960_79<-tapply(sum_FA_occ$N_MH_1960_79,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_1980_99<-tapply(sum_FA_occ$N_MH_1980_99,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_2000_09<-tapply(sum_FA_occ$N_MH_2000_09,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_2010_19<-tapply(sum_FA_occ$N_MH_2010_19,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_2020_29<-tapply(sum_FA_occ$N_MH_2020_29,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_2030_39<-tapply(sum_FA_occ$N_MH_2030_39,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_2040_49<-tapply(sum_FA_occ$N_MH_2040_49,sum_FA_occ$Year,sum)/0.994
US_FA_occ$N_MH_2050_60<-tapply(sum_FA_occ$N_MH_2050_60,sum_FA_occ$Year,sum)/0.994


# add in material intensities to sum_FA_vint
US_FA_vint_melt<-data.frame(melt(US_FA_vint[,1:31],id='Year'))

US_FA_vint_melt$Type<-substr(US_FA_vint_melt$variable,4,5)
US_FA_vint_melt$Cohort<-substring(US_FA_vint_melt$variable,7)
US_FA_vint_melt<-subset(US_FA_vint_melt, select = -c(variable))
US_FA_vint_melt<-US_FA_vint_melt %>% rename('FA_m2'='value')

# # calculate total number of housing units
US_N_vint<-US_FA_vint[,c(1,32:61)]
US_N_occ<-US_FA_occ[,c(1,32:61)]

# US_N_vint_melt<-data.frame(melt(US_FA_vint[,c(1,32:61)],id='Year'))
# US_N_vint_melt$Type<-substr(US_N_vint_melt$variable,3,4)
# US_N_vint_melt$Cohort<-substring(US_N_vint_melt$variable,6)
# US_N_vint_melt<-subset(US_N_vint_melt, select = -c(variable))
# US_N_vint_melt<-US_N_vint_melt %>% rename('Units'='value')

# same for occupied 
US_FA_occ_melt<-data.frame(melt(US_FA_occ[,1:31],id='Year'))

US_FA_occ_melt$Type<-substr(US_FA_occ_melt$variable,4,5)
US_FA_occ_melt$Cohort<-substring(US_FA_occ_melt$variable,7)
US_FA_occ_melt<-subset(US_FA_occ_melt, select = -c(variable))
US_FA_occ_melt<-US_FA_occ_melt %>% rename('FA_m2'='value')

# # calculate total number of housing units
# US_N_occ_melt<-data.frame(melt(US_FA_occ[,c(1,32:61)],id='Year'))
# US_N_occ_melt$Type<-substr(US_N_occ_melt$variable,3,4)
# US_N_occ_melt$Cohort<-substring(US_N_occ_melt$variable,6)
# US_N_occ_melt<-subset(US_N_occ_melt, select = -c(variable))
# US_N_occ_melt<-US_N_occ_melt %>% rename('Units'='value')

# divide by .994 to estimate FA including AK and HI which are excluded, and account for 0.6% of US housing stock
US_FA_vint$FA_p1940<-tapply(sum_FA_vint$FA_p1940,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_1940_59<-tapply(sum_FA_vint$FA_1940_59,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_1960_79<-tapply(sum_FA_vint$FA_1960_79,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_1980_99<-tapply(sum_FA_vint$FA_1980_99,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_2000_09<-tapply(sum_FA_vint$FA_2000_09,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_2010_19<-tapply(sum_FA_vint$FA_2010_19,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_2020_29<-tapply(sum_FA_vint$FA_2020_29,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_2030_39<-tapply(sum_FA_vint$FA_2030_39,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_2040_49<-tapply(sum_FA_vint$FA_2040_49,sum_FA_vint$Year,sum)/0.994
US_FA_vint$FA_2050_60<-tapply(sum_FA_vint$FA_2050_60,sum_FA_vint$Year,sum)/0.994

US_FA_vint$FA_pre2020<-US_FA_vint$FA_p1940+US_FA_vint$FA_1940_59+US_FA_vint$FA_1960_79+US_FA_vint$FA_1980_99+US_FA_vint$FA_2000_09+US_FA_vint$FA_2010_19
US_FA_vint$FA_post2020<-US_FA_vint$FA_2020_29+US_FA_vint$FA_2030_39+US_FA_vint$FA_2040_49+US_FA_vint$FA_2050_60


### 
US_FA_vint_melt$Vintage<-US_FA_vint_melt$Cohort
US_FA_vint_melt$Cohort<-gsub('_','-',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('p','<',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('-59','-1959',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('-79','-1979',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('-99','-1999',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('-09','-2009',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('2010-19','2010+',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('2020-29','2010+',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('2030-39','2010+',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('2040-49','2010+',US_FA_vint_melt$Cohort)
US_FA_vint_melt$Cohort<-gsub('2050-60','2010+',US_FA_vint_melt$Cohort)

comb<-merge(US_FA_vint_melt,mat_FA_AKHI)
comb$Mass_Mt<-1e-9*comb$FA_m2*comb$Mat_Int_kg_m2
comb$Cohort_agg<-'post2020'
comb[comb$Vintage %in% c('p1940','1940_59','1960_79','1980_99','2000_09','2010_19'),'Cohort_agg']<-'pre2020'

mat_ann<-comb %>% group_by(Year,Cohort_agg,Type,Material) %>% summarise(Stock=sum(Mass_Mt))

mat_ann$Dem_pre2020<-0

for (y in Year[1:40]) {
  mat_ann[mat_ann$Cohort_agg=='pre2020' & mat_ann$Year==y,]$Dem_pre2020<-mat_ann[mat_ann$Cohort_agg=='pre2020' & mat_ann$Year==y,]$Stock-mat_ann[mat_ann$Cohort_agg=='pre2020' & mat_ann$Year==y+1,]$Stock
}
  
write.csv(mat_ann,file='HSM_Results/MatStock_2020_60.csv',row.names = FALSE)

# stock of total FA 2020-2060
FA_SF_ctyyr<-sum_FA_vint[,c('GeoID', 'Year','FA_SF')] %>% dcast(GeoID ~ Year,value.var = 'FA_SF')
FA_MF_ctyyr<-sum_FA_vint[,c('GeoID', 'Year','FA_MF')] %>% dcast(GeoID ~ Year,value.var = 'FA_MF')
FA_MH_ctyyr<-sum_FA_vint[,c('GeoID', 'Year','FA_MH')] %>% dcast(GeoID ~ Year,value.var = 'FA_MH')

# stock of occupied FA 2020-2060
FA_SF_occ_ctyyr<-sum_FA_occ[,c('GeoID', 'Year','FA_SF')] %>% dcast(GeoID ~ Year,value.var = 'FA_SF')
FA_MF_occ_ctyyr<-sum_FA_occ[,c('GeoID', 'Year','FA_MF')] %>% dcast(GeoID ~ Year,value.var = 'FA_MF')
FA_MH_occ_ctyyr<-sum_FA_occ[,c('GeoID', 'Year','FA_MH')] %>% dcast(GeoID ~ Year,value.var = 'FA_MH')

# demolition
Dem_SF_ctyyr<-sum_con_dem[,c('GeoID', 'Year','FA_Dem_SF')] %>% dcast(GeoID ~ Year,value.var = 'FA_Dem_SF')
Dem_MF_ctyyr<-sum_con_dem[,c('GeoID', 'Year','FA_Dem_MF')] %>% dcast(GeoID ~ Year,value.var = 'FA_Dem_MF')
Dem_MH_ctyyr<-sum_con_dem[,c('GeoID', 'Year','FA_Dem_MH')] %>% dcast(GeoID ~ Year,value.var = 'FA_Dem_MH')

# construction
Con_SF_ctyyr<-sum_con_dem[,c('GeoID', 'Year','FA_Con_SF')] %>% dcast(GeoID ~ Year,value.var = 'FA_Con_SF')
Con_MF_ctyyr<-sum_con_dem[,c('GeoID', 'Year','FA_Con_MF')] %>% dcast(GeoID ~ Year,value.var = 'FA_Con_MF')
Con_MH_ctyyr<-sum_con_dem[,c('GeoID', 'Year','FA_Con_MH')] %>% dcast(GeoID ~ Year,value.var = 'FA_Con_MH')

write.csv(FA_SF_ctyyr,file = 'HSM_Results/DJ_Data/Stock_SF_m2.csv',row.names = FALSE)
write.csv(FA_MF_ctyyr,file = 'HSM_Results/DJ_Data/Stock_MF_m2.csv',row.names = FALSE)
write.csv(FA_MH_ctyyr,file = 'HSM_Results/DJ_Data/Stock_MH_m2.csv',row.names = FALSE)

write.csv(FA_SF_occ_ctyyr,file = 'HSM_Results/DJ_Data/OccStock_SF_m2.csv',row.names = FALSE)
write.csv(FA_MF_occ_ctyyr,file = 'HSM_Results/DJ_Data/OccStock_MF_m2.csv',row.names = FALSE)
write.csv(FA_MH_occ_ctyyr,file = 'HSM_Results/DJ_Data/OccStock_MH_m2.csv',row.names = FALSE)

write.csv(Dem_SF_ctyyr,file = 'HSM_Results/DJ_Data/Dem_SF_m2.csv',row.names = FALSE)
write.csv(Dem_MF_ctyyr,file = 'HSM_Results/DJ_Data/Dem_MF_m2.csv',row.names = FALSE)
write.csv(Dem_MH_ctyyr,file = 'HSM_Results/DJ_Data/Dem_MH_m2.csv',row.names = FALSE)

write.csv(Con_SF_ctyyr,file = 'HSM_Results/DJ_Data/Con_SF_m2.csv',row.names = FALSE)
write.csv(Con_MF_ctyyr,file = 'HSM_Results/DJ_Data/Con_MF_m2.csv',row.names = FALSE)
write.csv(Con_MH_ctyyr,file = 'HSM_Results/DJ_Data/Con_MH_m2.csv',row.names = FALSE)

write.csv(US_FA_vint,file = 'HSM_Results/US_stock_Tot.csv',row.names = FALSE)
write.csv(US_FA_occ,file = 'HSM_Results/US_stock_Occ.csv',row.names = FALSE)

write.csv(US_N_vint,file = 'HSM_Results/US_stock_Units_Tot.csv',row.names = FALSE)
write.csv(US_N_occ,file = 'HSM_Results/US_stock_Units_Occ.csv',row.names = FALSE)

