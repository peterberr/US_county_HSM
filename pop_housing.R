# Script to get total and occupied housing units, and population, by type and county, for 2019

rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
# make sure plyr is not loaded
library(dplyr)
library(tidyr)
setwd("C:/Users/pb637/Documents/Yale Courses/Research/Final Paper/HSM_github")
load('~/Yale Courses/Research/US Housing/Popn Survey/2017 USCB Pop Projections/Hauer files/SSP_asrc/Hauer.RData') # Hauer data, based on 'SSP_asrc.csv' file, accessible at https://osf.io/uh5sj/

ssp1<-tapply(d$SSP1,d$YEAR,sum) # summary populations each 5 years 2020-2010, ssp1
ssp2<-tapply(d$SSP2,d$YEAR,sum) # summary populations each 5 years 2020-2010, ssp2
ssp3<-tapply(d$SSP3,d$YEAR,sum) # summary populations each 5 years 2020-2010, ssp3
ssp4<-tapply(d$SSP4,d$YEAR,sum) # summary populations each 5 years 2020-2010, ssp4

# census bureau total population projection, main series, accessible at https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv
cb<-read.csv('~/Yale Courses/Research/US Housing/Popn Survey/2017 USCB Pop Projections/np2017_d1_mid.csv') 
cb<-cb[1:45,] # only interested in national totals 
cb<-select(cb,YEAR, TOTAL_POP)
cbp<-cb[c(seq(5,45,5)),] # census bureau population projections for same years as Hauer
# census bureau total population projection, main series, accessible at https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_low.csv
cbl<-read.csv('~/Yale Courses/Research/US Housing/Popn Survey/2017 USCB Pop Projections/np2017_d1_low.csv') 
cbl<-select(cbl,YEAR, TOTAL_POP)
cblp<-cbl[c(seq(5,45,5)),] # census bureau population projections for same years as Hauer

# actual US population in Jul1 2020 is ~ 329.5 million, close to the USCB low immigration scenario of 330.6, lower than the midrange scenario of 332.6, and lower than the Hauer SSP projections
# SSP1 and SSP2 2020 valuer are 336.7, 335.7. Also lower than the ssp3 ssp4 values of 333.6 and 328.8.
# The  Hauer-SSP scenarios most comparable to USCB projections are Hauer ssp2 for USCB mid range, and Hauer ssp4 for USCB low immigration.

######### household population by house type, from ACS Table B25033 ############### downloaded from data.census.gov
# 5-year estimate, contains data for all counties in US and Puerto Rico
pocc2019<-read.csv("~/Yale Courses/Research/US Housing/ACS/B25033_2019/ACSDT5Y2019.B25033_data_with_overlays_2020-12-22T132625.csv")
pocc2019$id<-substr(pocc2019$GEO_ID,nchar(pocc2019$GEO_ID)-4,nchar(pocc2019$GEO_ID)) # create the 5-digit county FIPS codes
pocc2019$StID<-substr(pocc2019$id,1,2) # extract the 2-digit State FIPS codes
# clean the data frame column names and remove margin of error columns
name<-names(pocc2019)
rmmar<-which(substr(name,nchar(name),nchar(name))=="M")
pocc2019<-pocc2019[,-rmmar]
pocc2019<-pocc2019[,c(dim(pocc2019)[2]-1,dim(pocc2019)[2],2:(dim(pocc2019)[2]-2))] # reorder columns and remove the redundant long GeoID
n<-as.character(pocc2019[1,])
pocc2019[1,5:16]<-substr(n[5:16],19,nchar(n[5:16]))
n<-as.character(pocc2019[1,])
pocc2019[1,]<-sub("1, detached or attached","1",n)
n<-as.character(pocc2019[1,])
pocc2019[1,]<-gsub(":!!","_",n)
n<-as.character(pocc2019[1,])
pocc2019[1,]<-gsub(":","",n)
n<-as.character(pocc2019[1,])
names(pocc2019)<-n
pocc2019<-pocc2019[-1,]
names(pocc2019)[1:3]<-c("GeoID","StID","County,State")
pocc2019<-pocc2019[1:3142,] # remove counties in Puerto Rico
pocc2019[,4:16]<-as.numeric(unlist(pocc2019[,4:16])) # convert numeric columns to numeric format

# this pocc2019 dataframe gives total household population in 2019 based on the 5yr ACS survey. 
# Household population is approx. 97.5% of the resident population, which includes population living in group quarters, and population without homes
# Hauer data represent total resident population, see the description under 'Group Quarters' in Hauer (2019) https://www.nature.com/articles/sdata20195

pocc2019<-pocc2019[order(pocc2019$GeoID),] # order by GeoID

#sum up populations by house type by adding together owner and renter occupied housing populations
pocc2019$SF<-pocc2019$`Owner occupied_1`+pocc2019$`Renter occupied_1`
pocc2019$MF<-pocc2019$`Owner occupied_2 to 4`+pocc2019$`Owner occupied_5 or more`+
  pocc2019$`Renter occupied_2 to 4`+pocc2019$`Renter occupied_5 or more`
pocc2019$MH<-pocc2019$`Owner occupied_Mobile home`+pocc2019$`Owner occupied_Boat, RV, van, etc.`+pocc2019$`Renter occupied_Mobile home`+pocc2019$`Renter occupied_Boat, RV, van, etc.`
# pocc2019$Year<-2019
pocc2019<-pocc2019[,c(1:4,17:19)] 

colnames(pocc2019)[4:7]<-c("HhPop","SF_Pop","MF_Pop","MH_Pop") # household population by house type
pocc2019[,8:10]<-pocc2019[,5:7]/pocc2019[,4] # express housing units in percentages
colnames(pocc2019)[8:10]<-c("SFpc","MFpc","MHpc")
cid<-unique(d$GEOID)
cid2<-as.numeric(unique(pocc2019$GeoID))
Hmiss<-cid2[!cid2 %in% cid] # which counties does Hauer miss? 5 counties in Alaska. These are also not included by ResStock
# load in census estimates of total resident population, which is a consistent definition with Hauer (including GQ) and the census projections
# census population totals available at https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-annres.xlsx
pres1019<-read.csv("~/Yale Courses/Research/US Housing/Census/co-est2019-annres.csv") # converted to csv before reading
pres1019$County.State<-as.character(pres1019$County.State)
# are there string mismatches? (If TRUE, no)
all.equal(pres1019$County.State,pocc2019$County,State)
# locate string mismatches
s<-pres1019$County.State
s2<-pocc2019$`County,State`
# s2<-pocc2019$County.State
s2[!s2 %in% s] # check if any differences
pocc2019[pocc2019$GeoID=="35013",]$`County,State`<-"Dona Ana County, New Mexico"
# combine data frames of household and resident population
p19<-merge(pres1019,pocc2019,by.x = "County.State",by.y="County,State")
p19<-p19[,c(14,1,15,13,16:22)] # extract only GeoID, County.State, StateID, Resident pop in 2019, then hh population total, by type and percentages
p19<-p19[order(p19$GeoID),] # order by GeoID
# adjust spelling of La Salle, LA
p19[p19$GeoID==22059,]$County.State<-"La Salle Parish, Louisiana"
# check population ratios to see if there are any unusual differences, or to see if the matching by county name failed
# generally we should see that the census residential population is larger than the household population
p19$popr<-p19$X2019/p19$HhPop
rownames(p19)<-1:3142
large_under<-p19$County.State[which(p19$popr>1.4)] # counties with ACS estimate of HH pop much less than census estimate of resident pop
# big underestimate in Alaskan counties, OK
# big underestimate in Lincoln Cty AR, two prisons there (Cummins Unit and the Varner Unit)
# big underestimate in Lassen Cty CA, two state prisons
# same in Bent County CO
# Lincoln County CO tiny pop
# Union Cty FL, prison
# Chattahoochee County GA military base
# Stweary GA small pop
# East Carroll Parish, Louisiana, famring population, also small population
# Pershing Cty NV desert county small population
# Harding Cty NM tiny pop
# big differences in household and resident population in Forest Cty PA likely due to the prison there https://en.wikipedia.org/wiki/Forest_County,_Pennsylvania
# Lake Cty TN, prison
# Jones Cty TX, prison
# Williamsburg VA colleget town

# big difference in Dagget Utah (49009) likely due to the low population there 
# big difference in Lexington city VA (51678) likely due to the existence of the Virginia Military Institute there
# otherwise, looks ok\
# some of the counties with unusually low estimates of resident population includ"
# Cottle County Texas (low pop)

# Clark County, Idaho (tiny pop)
large_over<-p19$County.State[which(p19$popr<0.85)] # counties with ACS estimate of HH pop much more than census estimate of resident pop
# Kenedy County Texas (tiny pop)
# Piute County, Utah (low pop)
# Logan County, Nebraska (tiny pop)


# scale population by house type, based on resident population
p19$SF_rPop<-p19$SFpc*p19$X2019
p19$MF_rPop<-p19$MFpc*p19$X2019
p19$MH_rPop<-p19$MHpc*p19$X2019
p19$geoid_num<-as.numeric(p19$GeoID)
pocc2019$geoid_num<-as.numeric(pocc2019$GeoID)
# make Hauer ssp2 data consistent with census midrange projections, then further divide all data so 2020 population is equal to 329.5 million, roughly equal to July 1 2020 population from https://www.census.gov/popclock/
# first put Hauer data into county total format
# missing counties,. seems like all related to the creation of https://en.wikipedia.org/wiki/Petersburg_Borough,_Alaska
pocc2019[pocc2019$geoid_num==Hmiss[1],3]
pocc2019[pocc2019$geoid_num==Hmiss[2],3]
pocc2019[pocc2019$geoid_num==Hmiss[3],3]
pocc2019[pocc2019$geoid_num==Hmiss[4],3]
pocc2019[pocc2019$geoid_num==Hmiss[5],3]
ssp2Cty<-d %>%
  group_by(GEOID,YEAR) %>%
  summarize(pop = sum(SSP2, na.rm = TRUE))

ssp2Cty<-as.data.frame(ssp2Cty) # get rid of annoying list properties

ssp4Cty<-d %>%
  group_by(GEOID,YEAR) %>%
  summarize(pop = sum(SSP4, na.rm = TRUE))
ssp4Cty<-as.data.frame(ssp4Cty) # get rid of annoying list properties

# To compare end-points of projections in SSP2 SSP4, and CB mid and low-ranage projections: 
# SSP2 - 420.1 million, SSP4 - 386.1 million, CBmid - 404.5, CBlow - 376.2.
# The mid-range projection adjusted downward by actual 2020 population is 401.1 million
# I stick with SSP2, adjusted to census mid-range
# population growth ratios 2050-2060 wrt 2020, for introduced AK counties, based on 2100, Haines Borough
prat<-ssp2Cty[ssp2Cty$GEOID==2100,3][2:9]/ssp2Cty[ssp2Cty$GEOID==2100,3][1]
# add in 5 missing counties
r<-matrix(0,9,5)
r[,]<-dim(ssp2Cty)[1]+1:45

# add in data for every 5 years for the AK missing counties, 2020-2060, this adds 45 rows to ssp2cty
for (i in 1:5) { 
ssp2Cty[r[1,i],]<-c(Hmiss[i],2020, pres1019[pres1019$County.State==pocc2019[pocc2019$geoid_num==Hmiss[i],3],]$X2019) # cty, year, pop in 2020
ssp2Cty[r[2:9,i],1]<-Hmiss[i] # cty code, for 2025-2060
ssp2Cty[r[2:9,i],2]<-seq(2025,2060,5) # year for 2025-2060
ssp2Cty[r[2:9,i],3]<-ssp2Cty[r[1,i],3]*prat # population, 2025-2060
}
ssp2Cty<-ssp2Cty[order(ssp2Cty$GEOID),] # order by GeoID
# scale census burea mid range population by actual 2020 population recently reported https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html
sf<-cbp[1,2]/329484123# scaling factor, census bureau 2020 projection divided by actual 2020 pop
cbp_scale<-cbp
cbp_scale$TOTAL_POP<-cbp$TOTAL_POP/sf # divide cb projection by sf between 2020 projection and 2020 actual, this prodocues a 2060 pop of 400mill

# scaling factor per 5 year period to bring Hauer SSP2 totals in line with mid-range census projections, adjusted to actual 2020 value
sf2<-tapply(ssp2Cty$pop,ssp2Cty$YEAR,sum)[1:9]/cbp_scale$TOTAL_POP
# get data frame of Hauer SSP2 data to 2060 only
ssp2Cty60<-ssp2Cty[ssp2Cty$YEAR<2061,]
# scale Hauer SSP2 to match 2020-adjusted census projections
ssp2Cty60$pop_adj<-ssp2Cty60$pop/as.numeric(sf2)
cbp$TOTAL_POP/ssp2[1:9] # this shows how much the scaled census bureau population projection is lower than Hauer SSP2.

# adjusted population projection per each county for each 5 year 
s2t<-ssp2Cty60[,c(1,2,4)]
s2t$GEOID<-as.character(s2t$GEOID)
s2t[nchar(s2t$GEOID)<5,]$GEOID<-paste("0",s2t[nchar(s2t$GEOID)<5,]$GEOID,sep = "")
id9<-rep(p19$GeoID,each=9) # rep each geoID 0 times

# check that the geoids match
all.equal(id9,s2t$GEOID,tolerance=1.0)
s2t$SFp<-s2t$pop_adj*rep(p19$SFpc,each=9) # population in SF homes
s2t$MFp<-s2t$pop_adj*rep(p19$MFpc,each=9) # population in MF homes
s2t$MHp<-s2t$pop_adj*rep(p19$MHpc,each=9) # population in MH homes

## load data of occupied houses, calculate 2019 household size by house type and county #########
load('Intermediate_results/OccHousing2019.RData') # produced by script housing_stock.R

ho19t$sf<-ho19t$Tot_OU/(ho19t$SF+ho19t$MF+ho19t$MH) # scaling factor, ensure house types add up to the total
ho19t$SF<-ho19t$sf*ho19t$SF
ho19t$MF<-ho19t$sf*ho19t$MF
ho19t$MH<-ho19t$sf*ho19t$MH
# extract data for totals by type and cohort, and by type only
ho19<-ho19t[,c(1,5:25)]

names(ho19)[2:22]<-paste(names(ho19)[2:22],"ho",sep="") # housing, occupied
# merge population and occupied housing data
hp19<-merge(p19,ho19,by = 'GeoID') 

# load in data of total occupied stock in 2019, and calculate vacancy rate by type for each county
load('Intermediate_results/TotalHousing2019.RData') # produced by script housing_stock.R
h19t<-h19tb_new[,c(1:8,52,10,16:33)] # extract required columns
#make sure things match up, i.e. that totals are the sum of their parts
h19t$SF<-rowSums(h19t[,11:16]) # sf is the sum of sf by cohort
h19t$MF<-rowSums(h19t[,17:22]) # mf is the sum of mf by cohort
h19t$MH<-rowSums(h19t[,23:28]) # mh is the sum of mh by cohort
h19t$Total_HUht<-h19t$SF+h19t$MF+h19t$MH
colnames(h19t)[5:28]<-paste(names(h19t[5:28]),"ht",sep = "")# house, total
# merge occupied housing and population data with total housing data
hpt19<-merge(hp19,h19t,by = 'GeoID')
# colnames(hpt19)[20:22]<-c("SFho","MFho","MHho") # house, occupied
# define occupancy/vacancy rates by type
hpt19$SFvr<-hpt19$SFho/hpt19$SFht
hpt19$MFvr<-hpt19$MFho/hpt19$MFht
hpt19$MFvr[is.nan(hpt19$MFvr)]<-0
hpt19$MHvr<-hpt19$MHho/hpt19$MHht
hpt19$MHvr[is.nan(hpt19$MHvr)]<-0

# Divide resident (_r) population by occupied units to estimate avg household size
hpt19$SFhhs<-hpt19$SF_rPop/hpt19$SFho 
hpt19$MFhhs<-hpt19$MF_rPop/hpt19$MFho
hpt19$MHhhs<-hpt19$MH_rPop/hpt19$MHho
# convert NAs to 1. Problem in later calculations if 0. 
# also convert any hhs<1 to 1
hpt19$SFhhs[is.nan(hpt19$SFhhs)|hpt19$SFhhs<1]<-1
hpt19$MFhhs[is.nan(hpt19$MFhhs)|hpt19$MFhhs<1]<-1
hpt19$MHhhs[is.nan(hpt19$MHhhs)|hpt19$MHhhs<1]<-1
hpt19$MH_Pop[is.infinite(hpt19$MHhhs)]<-hpt19$MH_rPop[is.infinite(hpt19$MHhhs)]<-0 # exception for Alexandria City VA, which had population of 108 in MH, but no MH units
hpt19$MHhhs[is.infinite(hpt19$MHhhs)]<-1 # exception for Alexandria City VA, which had population of 108 in MH, but no MH units

# recalc occupancy/vacancy rates by type
hpt19$SFvr<-hpt19$SFho/hpt19$SFht
hpt19$MFvr<-hpt19$MFho/hpt19$MFht
hpt19$MFvr[is.nan(hpt19$MFvr)]<-0
hpt19$MHvr<-hpt19$MHho/hpt19$MHht
hpt19$MHvr[is.nan(hpt19$MHvr)]<-0
# split total housing units into percentages by type, cohort, and occ/vac
h19pc<-hpt19[,1:2]
h19pc$Tot_Hous_Units<-hpt19$SFht+hpt19$MFht+hpt19$MHht
h19pc[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]<-hpt19[,c("SFht","MFht","MHht")]

h19pc[,c("pcTot_HU_SF_Occ_p1940","pcTot_HU_SF_Occ_1940_59","pcTot_HU_SF_Occ_1960_79","pcTot_HU_SF_Occ_1980_99",
          "pcTot_HU_SF_Occ_2000_09","pcTot_HU_SF_Occ_2010_19")]<-hpt19[,17:22]/h19pc$Tot_HU_SF
h19pc[,c("pcTot_HU_SF_Occ_2020_29","pcTot_HU_SF_Occ_2030_39","pcTot_HU_SF_Occ_2040_49","pcTot_HU_SF_Occ_2050_60")]<-0

sfvac<-hpt19[,47:52]-hpt19[,17:22] # vacant units = total units (by cohort) - occupied units (by cohort)
sfvact<-rowSums(sfvac)
sfvac[sfvac<0]<-0 # no negative vacancy values
sfvacpc<-sfvac/sfvact
sfvacpcsum<-rowSums(sfvacpc)
# fix potential infeasible percentage vacancy values
for (i in 1:length(sfvacpcsum)){
  if (sfvacpcsum[i]<0) {sfvacpc[i,]<-0} # none less than zero
  if (sfvacpcsum[i]>1) {sfvacpc[i,]<-sfvacpc[i,]/sfvacpcsum[i]} # none greater than 1 (100%)
}
sfvac<-sfvacpc*sfvact
h19pc[,c("pcTot_HU_SF_Vac_p1940","pcTot_HU_SF_Vac_1940_59","pcTot_HU_SF_Vac_1960_79","pcTot_HU_SF_Vac_1980_99",
         "pcTot_HU_SF_Vac_2000_09","pcTot_HU_SF_Vac_2010_19")]<-sfvac/h19pc$Tot_HU_SF
h19pc[,c("pcTot_HU_SF_Vac_2020_29","pcTot_HU_SF_Vac_2030_39","pcTot_HU_SF_Vac_2040_49","pcTot_HU_SF_Vac_2050_60")]<-0

# MF
h19pc[,c("pcTot_HU_MF_Occ_p1940","pcTot_HU_MF_Occ_1940_59","pcTot_HU_MF_Occ_1960_79","pcTot_HU_MF_Occ_1980_99",
         "pcTot_HU_MF_Occ_2000_09","pcTot_HU_MF_Occ_2010_19")]<-hpt19[,23:28]/h19pc$Tot_HU_MF
h19pc[,c("pcTot_HU_MF_Occ_2020_29","pcTot_HU_MF_Occ_2030_39","pcTot_HU_MF_Occ_2040_49","pcTot_HU_MF_Occ_2050_60")]<-0

mfvac<-hpt19[,53:58]-hpt19[,23:28] # vacant units = total units (by cohort) - occupied units (by cohort)
mfvact<-rowSums(mfvac)
mfvac[mfvac<0]<-0
mfvacpc<-mfvac/mfvact
mfvacpc[is.na(mfvacpc)]<-0
mfvacpcsum<-rowSums(mfvacpc)
# fix potential infeasible percentage vacancy values
for (i in 1:length(mfvacpcsum)){
  if (mfvacpcsum[i]<0) {mfvacpc[i,]<-0} # none less than zero
  if (mfvacpcsum[i]>1) {mfvacpc[i,]<-mfvacpc[i,]/mfvacpcsum[i]}  # none greater than 1 (100%)
}
mfvac<-mfvacpc*mfvact
h19pc[,c("pcTot_HU_MF_Vac_p1940","pcTot_HU_MF_Vac_1940_59","pcTot_HU_MF_Vac_1960_79","pcTot_HU_MF_Vac_1980_99",
         "pcTot_HU_MF_Vac_2000_09","pcTot_HU_MF_Vac_2010_19")]<-mfvac/h19pc$Tot_HU_MF
h19pc[,c("pcTot_HU_MF_Vac_2020_29","pcTot_HU_MF_Vac_2030_39","pcTot_HU_MF_Vac_2040_49","pcTot_HU_MF_Vac_2050_60")]<-0

# MH
h19pc[,c("pcTot_HU_MH_Occ_p1940","pcTot_HU_MH_Occ_1940_59","pcTot_HU_MH_Occ_1960_79","pcTot_HU_MH_Occ_1980_99",
         "pcTot_HU_MH_Occ_2000_09","pcTot_HU_MH_Occ_2010_19")]<-hpt19[,29:34]/h19pc$Tot_HU_MH
h19pc[,c("pcTot_HU_MH_Occ_2020_29","pcTot_HU_MH_Occ_2030_39","pcTot_HU_MH_Occ_2040_49","pcTot_HU_MH_Occ_2050_60")]<-0

mhvac<-hpt19[,59:64]-hpt19[,29:34] # vacant units = total units (by cohort) - occupied units (by cohort)
mhvact<-rowSums(mhvac)
mhvac[mhvac<0]<-0
mhvacpc<-mhvac/mhvact
mhvacpc[is.na(mhvacpc)]<-0
mhvacpcsum<-rowSums(mhvacpc)
# fix potential infeasible percentage vacancy values
for (i in 1:length(mhvacpcsum)){
  if (mhvacpcsum[i]<0) {mhvacpc[i,]<-0} # none less than zero
  if (mhvacpcsum[i]>1) {mhvacpc[i,]<-mhvacpc[i,]/mhvacpcsum[i]} # none greater than 1 (100%)
}
mhvac<-mhvacpc*mhvact
h19pc[,c("pcTot_HU_MH_Vac_p1940","pcTot_HU_MH_Vac_1940_59","pcTot_HU_MH_Vac_1960_79","pcTot_HU_MH_Vac_1980_99",
         "pcTot_HU_MH_Vac_2000_09","pcTot_HU_MH_Vac_2010_19")]<-mhvac/h19pc$Tot_HU_MH
h19pc[,c("pcTot_HU_MH_Vac_2020_29","pcTot_HU_MH_Vac_2030_39","pcTot_HU_MH_Vac_2040_49","pcTot_HU_MH_Vac_2050_60")]<-0
# remove nas
h19pc[7:66][is.na(h19pc[7:66])]<-0
# add in vacancy rates and population data to h19pc
h19pc$VR_SF<-rowSums(h19pc[,7:26])/rowSums(h19pc[,7:16])
h19pc$VR_SF[is.na(h19pc$VR_SF)]<-1
h19pc$VR_MF<-rowSums(h19pc[,27:46])/rowSums(h19pc[,27:36])
h19pc$VR_MF[is.na(h19pc$VR_MF)]<-1
h19pc$VR_MF[is.infinite(h19pc$VR_MF)]<-1
h19pc$VR_MH<-rowSums(h19pc[,47:66])/rowSums(h19pc[,47:56])
h19pc$VR_MH[is.na(h19pc$VR_MH)]<-1
h19pc$VR_MH[is.infinite(h19pc$VR_MH)]<-1

h19pc$Pop_SF<-hpt19$SF_rPop
h19pc$Pop_MF<-hpt19$MF_rPop
h19pc$Pop_MH<-hpt19$MH_rPop
h19pc$HHS_SF<-hpt19$SFhhs
h19pc$HHS_MF<-hpt19$MFhhs
h19pc$HHS_MH<-hpt19$MHhhs
# start to define the housing stock and population data based on the 2019 percentage values scaled by 2020 absolute values
h20pc<-h19pc
s2t20<-s2t[s2t$YEAR==2020,] # 2020 adjusted county population data
h20pc$Pop_SF<-s2t20$SFp
h20pc$Pop_MF<-s2t20$MFp
h20pc$Pop_MH<-s2t20$MHp

# in many counties (around 2,253 out of 3,142) the population shows a decline between 2019 and 2020: length(which(h20pc$Pop_SF<h19pc$Pop_SF))
# this method for estimating total units in 2020 assumes that vacancy rates stay the same (and thus total units decline), rather than an increase in vacancy rates and/or a decrease in household size
h20pc$Tot_HU_SF<-h20pc$Pop_SF/h20pc$HHS_SF*h20pc$VR_SF
h20pc$Tot_HU_MF<-h20pc$Pop_MF/h20pc$HHS_MF*h20pc$VR_MF
h20pc$Tot_HU_MH<-h20pc$Pop_MH/h20pc$HHS_MH*h20pc$VR_MH
h20pc$Tot_Hous_Units<-h20pc$Tot_HU_SF+h20pc$Tot_HU_MF+h20pc$Tot_HU_MH 
save(h20pc,file = "Intermediate_results/InitStock20.RData") # this is used as an input to hsm_cty.R
# This will serve as the data for initial HS by type, cohort, and vacancy status in the county HSM
# next step is to create population and HHS every year by type and cohort, for each county. I think that's already partially/fully done in the script below
s2t[,c("Pop_Share_SF","Pop_Share_MF","Pop_Share_MH")]<-s2t[,c("SFp","MFp","MHp")]/s2t$pop_adj
# average national household size reductions, from JHCS, see Table 1 in Yale Courses\Research\US Housing\Popn Survey\JCHS\Harvard_JCHS_McCue_Household_Projections_AppendixTables.xlsx
h<-data.frame("Year" = seq(2020,2060,5), hhs =c(2.5515,2.5213,2.4984,2.4822,2.4723,2.4679,2.465,2.4625,2.460))
h$hr<-1 # househod size ratio, based on 2020 value
h$hr[2:9]<-h$hhs[2:9]/h$hhs[1]
# apply average reductions in household size to each house type in all counties, but don't let household size go below 1.
s2t$HHS_SF<-rep(h20pc$HHS_SF,each=9)*h$hr
s2t$HHS_SF[s2t$HHS_SF<1]<-1 # don't go below HHS of 1 
s2t$HHS_MF<-rep(h20pc$HHS_MF,each=9)*h$hr
s2t$HHS_MF[s2t$HHS_MF<1]<-1 # don't go below HHS of 1 
s2t$HHS_MH<-rep(h20pc$HHS_MH,each=9)*h$hr
s2t$HHS_MH[s2t$HHS_MH<1]<-1 # don't go below HHS of 1 

# interpolate with spline function to create population and hhs for every year for each county ############
cid2<-as.character(cid2)
cid2[nchar(cid2)<5]<-paste("0",cid2[nchar(cid2)<5],sep = "")

for (k in 1:3142) { print(k)
re<-s2t[s2t$GEOID==cid2[k],][1,]
re[2:41,1]<-rep(re[1,1],40) # copy geoid
re$YEAR[2:41]<-2021:2060 # insert year
y5<-c(seq(6,41,5)) # extract the 5 years
re$pop_adj[y5]<-s2t[s2t$GEOID==cid2[k],]$pop_adj[2:9]
re$HHS_SF[y5]<-s2t[s2t$GEOID==cid2[k],]$HHS_SF[2:9]
re$HHS_MF[y5]<-s2t[s2t$GEOID==cid2[k],]$HHS_MF[2:9]
re$HHS_MH[y5]<-s2t[s2t$GEOID==cid2[k],]$HHS_MH[2:9]
# loop to fill in between 5 years for population and household size. Will need to add in vacancy rate here too when/if it changes
# fill in total pop
sd <- data.frame(
  with(select(s2t[s2t$GEOID==cid2[k],],YEAR,pop_adj),
       spline(YEAR, pop_adj, xout = seq(2020, 2060, by = 1))
  ),
  method = "spline()"
)
re$pop_adj<-sd$y
# now for SF hhs
sd <- data.frame(
  with(select(s2t[s2t$GEOID==cid2[k],],YEAR,HHS_SF),
       spline(YEAR, HHS_SF, xout = seq(2020, 2060, by = 1))
  ),
  method = "spline()"
)
re$HHS_SF<-sd$y

# now for MF hhs
sd <- data.frame(
  with(select(s2t[s2t$GEOID==cid2[k],],YEAR,HHS_MF),
       spline(YEAR, HHS_MF, xout = seq(2020, 2060, by = 1))
  ),
  method = "spline()"
)
re$HHS_MF<-sd$y

# now for MH hhs
sd <- data.frame(
  with(select(s2t[s2t$GEOID==cid2[k],],YEAR,HHS_MH),
       spline(YEAR, HHS_MH, xout = seq(2020, 2060, by = 1))
  ),
  method = "spline()"
)
re$HHS_MH<-sd$y
# Assuming static share of households among types
re$Pop_Share_SF<-re$Pop_Share_SF[1]
re$Pop_Share_MF<-re$Pop_Share_MF[1]
re$Pop_Share_MH<-re$Pop_Share_MH[1]

re$SFp[2:41]<-re$pop_adj[2:41]*re$Pop_Share_SF[2:41]
re$MFp[2:41]<-re$pop_adj[2:41]*re$MFp[1]/re$pop_adj[1]
re$MHp[2:41]<-re$pop_adj[2:41]*re$MHp[1]/re$pop_adj[1]

if (k==1) {
  sall<-re
} else  { sall<-bind_rows(sall,re)
}
}

# reorder, rename, nest and save #######
names(sall)[3:6]<-c("Population","Pop_SF","Pop_MF","Pop_MH")

sall<-sall[,c(1:3,7:9,4:6,10:12)]
ss<-sall %>% group_by(GEOID) %>% nest()
save(ss,file= "Intermediate_results/CountyProjections.RData") # this is used as an input to hsm_cty.R