# script to load, analyze, and combine by scenario bs.csv files
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
setwd("~/Yale Courses/Research/Final Paper/HSM_github")
# bs2020<-read.csv('../resstock_projections/scen_bscsv/bs2020_180k.csv') # read the large file containing 180k sample of the 2020 occupied housing stock
# save(bs2020,file="Resstock_outputs/bs2020_180k.RData")
load("Resstock_outputs/bs2020_180k.RData")
load("Resstock_outputs/bs_baseRFA.RData") # created by bs_adjust in the energy GHG projection repository
# load("Resstock_outputs/bs_hiDRRFA.RData") 
# load("Resstock_outputs/bs_hiMFRFA.RData")
# load("Resstock_outputs/bs_base.RData") 
load("Intermediate_results/InitStock20.RData")
load("HSM_results/US_smop_scenarios.RData") # us summary of stock model, created in the smvis3 script, with the updated (#3) stock model
load("Intermediate_results/ctycode.RData")

bsRFA<-bs_baseRFA[,1:113]
# bshiDRRFA<-bs_hiDRRFA[,1:113]
# bshiMFRFA<-bs_hiMFRFA[,1:113]
# check the distribution of housing types
ht<-table(bs2020$Geometry.Building.Type.RECS)/nrow(bs2020) # as expected, SF makes up about 68% of total occupied units
# lets show that it matches well
sum(ht[4:5]) # tot SF share from sample 68.2%
us_base[1,15]/us_base[1,14] # tot SF share from HSM, 68.2%

sum(ht[2:3]) # tot MF share from sample, 26.2%
us_base[1,16]/us_base[1,14] # tot MF share from HSM, 26.3%

ht[1] # MH share from sample, 5.5%
us_base[1,17]/us_base[1,14] # MH share from HSM, 5.5%

h20<-h20pc[-c(which(substr(h20pc$GeoID,1,2) %in% c("02","15"))),] # remove the counties in Alaska and Hawaii which are not part of the bs sample.
h20<-merge(h20,ctycode,by="GeoID")
# which counties are captured by the sample?
cc<-unique(bs2020$County)
# rsid<-unique(bs2020$rsid)
c<-which(!h20$RS_ID %in% cc)
# these ones are omitted, 24 low population counties with the 180k samples
h20$RS_ID[c]

ccfut<-unique(bs_baseRFA$County)
cfut<-which(!h20$RS_ID %in% ccfut)
futomit<-h20$RS_ID[cfut]

cty_table<-table(bs2020$County)
# add description of simple type to bs file
bs2020$Type3<-"SF"
bs2020[bs2020$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units"),]$Type3<-"MF"
bs2020[bs2020$Geometry.Building.Type.RECS == "Mobile Home",]$Type3<-"MH"
tcc_table<-as.data.frame(table(bs2020$County,bs2020$Vintage.ACS,bs2020$Type3))
names(tcc_table)<-c("County","Vintage ACS","Type3","count")
tcd_table<-as.data.frame(table(bs2020$Census.Division,bs2020$Vintage.ACS,bs2020$Type3))
names(tcd_table)<-c("Census.Division","Vintage ACS","Type3","count")

tcs_table<-as.data.frame(table(bs2020$State,bs2020$Vintage.ACS,bs2020$Type3))
names(tcs_table)<-c("State","Vintage ACS","Type3","count")

# do same for bsRFA
bsRFA$Type3<-"SF"
bsRFA[bsRFA$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units"),]$Type3<-"MF"
bsRFA[bsRFA$Geometry.Building.Type.RECS == "Mobile Home",]$Type3<-"MH"
tcc_tableRFA<-as.data.frame(table(bsRFA$County,bsRFA$Vintage.ACS,bsRFA$Type3))
names(tcc_tableRFA)<-c("County","Vintage ACS","Type3","count")
tcd_tableRFA<-as.data.frame(table(bsRFA$Census.Division,bsRFA$Vintage.ACS,bsRFA$Type3))
names(tcd_tableRFA)<-c("Census.Division","Vintage ACS","Type3","count")
tcs_tableRFA<-as.data.frame(table(bsRFA$State,bsRFA$Vintage.ACS,bsRFA$Type3))
names(tcs_tableRFA)<-c("State","Vintage ACS","Type3","count")

# add floor area estiamtes to each bs row, based on details in options_lookup #######
bs2020$Floor.Area.m2<-0
bs2020[bs2020$Geometry.Floor.Area=="0-499"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(328/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="0-499"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(317/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="0-499"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(333/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="500-749"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(633/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="500-749"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(617/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="500-749"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(617/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="750-999"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(885/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="750-999"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(866/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="750-999"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(853/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="1000-1499"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(1220/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="1000-1499"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(1202/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="1000-1499"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(1138/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="1500-1999"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(1690/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="1500-1999"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(1675/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="1500-1999"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(1623/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="2000-2499"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(2176/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="2000-2499"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(2152/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="2000-2499"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(2115/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="2500-2999"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(2663/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="2500-2999"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(2631/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="2500-2999"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(2590/10.765,1)

bs2020[bs2020$Geometry.Floor.Area=="3000-3999"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(3301/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="3000-3999"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(3241/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="3000-3999"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(3138/10.765,1)
# 4000+. Using my own estimates here, consistent with my changes to options_lookup, but creating different value for MH
bs2020[bs2020$Geometry.Floor.Area=="4000+"&bs2020$Geometry.Building.Type.RECS %in% c("Single-Family Detached"),]$Floor.Area.m2<-round(7500/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="4000+"&bs2020$Geometry.Building.Type.RECS %in% c("Mobile Home"),]$Floor.Area.m2<-round(4200/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="4000+"&bs2020$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(7000/10.765,1)
bs2020[bs2020$Geometry.Floor.Area=="4000+"&bs2020$Type3=="MF",]$Floor.Area.m2<-round(7000/10.765,1)
# check distribution of house t-c in different regions ##########
h20$Occ_SF<-h20$Tot_HU_SF*rowSums(h20[,c(7:16)])
h20$Occ_MF<-h20$Tot_HU_MF*rowSums(h20[,c(27:36)])
h20$Occ_MH<-h20$Tot_HU_MH*rowSums(h20[,c(47:56)])
h20$Tot_Occ_Units<-h20$Occ_SF+h20$Occ_MF+h20$Occ_MH
locs<-unique(bs2020[,c("County","State","Census.Division")])
locs<-locs[order(locs$County),]
missing<-h20$RS_ID[c]
locsm<-data.frame("County"=missing,State=substr(missing,1,2))

StDiv<-unique(bs2020[,c("State","Census.Division")])
locsm<-merge(locsm,StDiv,by="State")
locs_all<-rbind(locs,locsm)
# add state and division to h20
h20<-merge(h20,locs,by.x = "RS_ID",by.y="County",all.x = TRUE) 

# RFA add floor area estiamtes to each bs row, based on details in options_lookup #######
bsRFA$Floor.Area.m2<-0
bsRFA[bsRFA$Geometry.Floor.Area=="0-499"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(328/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="0-499"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(317/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="0-499"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(333/10.765,1)

bsRFA[bsRFA$Geometry.Floor.Area=="500-749"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(633/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="500-749"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(617/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="500-749"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(617/10.765,1)

bsRFA[bsRFA$Geometry.Floor.Area=="750-999"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(885/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="750-999"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(866/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="750-999"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(853/10.765,1)

bsRFA[bsRFA$Geometry.Floor.Area=="1000-1499"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(1220/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="1000-1499"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(1202/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="1000-1499"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(1138/10.765,1)

bsRFA[bsRFA$Geometry.Floor.Area=="1500-1999"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(1690/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="1500-1999"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(1675/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="1500-1999"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(1623/10.765,1)

bsRFA[bsRFA$Geometry.Floor.Area=="2000-2499"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(2176/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="2000-2499"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(2152/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="2000-2499"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(2115/10.765,1)

bsRFA[bsRFA$Geometry.Floor.Area=="2500-2999"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(2663/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="2500-2999"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(2631/10.765,1)
bsRFA[bsRFA$Geometry.Floor.Area=="2500-2999"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(2590/10.765,1)
# 
# bsRFA[bsRFA$Geometry.Floor.Area=="3000-3999"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Mobile Home"),]$Floor.Area.m2<-round(3301/10.765,1)
# bsRFA[bsRFA$Geometry.Floor.Area=="3000-3999"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(3241/10.765,1)
# bsRFA[bsRFA$Geometry.Floor.Area=="3000-3999"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(3138/10.765,1)
# # 4000+. Using my own estimates here, consistent with my changes to options_lookup, but creating different value for MH
# bsRFA[bsRFA$Geometry.Floor.Area=="4000+"&bsRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached"),]$Floor.Area.m2<-round(7500/10.765,1)
# bsRFA[bsRFA$Geometry.Floor.Area=="4000+"&bsRFA$Geometry.Building.Type.RECS %in% c("Mobile Home"),]$Floor.Area.m2<-round(4200/10.765,1)
# bsRFA[bsRFA$Geometry.Floor.Area=="4000+"&bsRFA$Geometry.Building.Type.RECS == "Single-Family Attached",]$Floor.Area.m2<-round(7000/10.765,1)
# bsRFA[bsRFA$Geometry.Floor.Area=="4000+"&bsRFA$Type3=="MF",]$Floor.Area.m2<-round(7000/10.765,1)

# calculate mean floor area by type, cohort, and geography #######
# type and cohort, nationally
tapply(bs2020$Floor.Area.m2, list(bs2020$Type3,bs2020$Vintage), mean)
meanFA_tc<-round(as.data.frame(tapply(bs2020$Floor.Area.m2, list(bs2020$Type3,bs2020$Vintage), mean)),2)
meanFA_tc$Type<-rownames(meanFA_tc)
mFA<-melt(meanFA_tc)
names(mFA)<-c("Type","Cohort","FloorArea")
mFA[mFA$Type=="MH" & mFA$Cohort=="<1940", ]$FloorArea<-80 # adjust floor area of <1940 MH
mFA[mFA$Type=="MH" & mFA$Cohort=="1940s", ]$FloorArea<-85 # adjust floor area of 1940s MH
windows()
ggplot(mFA,aes(x=Cohort,y=FloorArea,group=Type))+geom_line(aes(color=Type),size=1)+geom_point(aes(color=Type),size=2) + 
  labs(title = "Mean floor area of house types by cohort", y = "Mean Floor Area (m2)") + theme_bw() + ylim(c(10,260)) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
# type and cohort, nationally, RFA
tapply(bsRFA$Floor.Area.m2, list(bsRFA$Type3,bsRFA$Vintage), mean)
meanFA_tc<-round(as.data.frame(tapply(bsRFA$Floor.Area.m2, list(bsRFA$Type3,bsRFA$Vintage), mean)),2)
meanFA_tc$Type<-rownames(meanFA_tc)
mFA_RFA<-melt(meanFA_tc)
names(mFA_RFA)<-c("Type","Cohort","FloorArea")
mFA$Scenario<-"1-4"
mFA_RFA$Scenario<-"5. Red. FA"

mFA[37:39,]<-mFA[34:36,]<-mFA[31:33,]<-mFA[28:30,]<-mFA[25:27,]
mFA$Cohort<-as.character(mFA$Cohort)
mFA[28:39,]$Cohort<-rep(c("2020s","2030s","2040s","2050s"),each=3)
mFA_all<-rbind(mFA,mFA_RFA)
mFA_all$Type_Scen<-paste(mFA_all$Type,mFA_all$Scenario,sep="_")
mFA_all$Characteristics<-"Base"
mFA_all[mFA_all$Scenario=="5. Red. FA",]$Characteristics<-"Reduced FA"
windows()
ggplot(mFA_all,aes(x=Cohort,y=FloorArea,group=Type_Scen))+geom_line(aes(color=Type,linetype=Characteristics),size=1)+geom_point(aes(color=Type),size=2) + 
  labs(title = "Mean floor area of house types by cohort", y = "Mean Floor Area (m2)") + theme_bw() + ylim(c(10,260)) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face = "bold"),plot.title = element_text(size = 15, face = "bold"), 
        legend.title=element_text(size=12), legend.text=element_text(size=11))
# mFA by geograpy, base #####
# mean floor area by type, cohort, and region
meanFA_tcr<-round(as.data.frame(tapply(bs2020$Floor.Area.m2, list(bs2020$Census.Region,bs2020$Type3,bs2020$Vintage.ACS), mean)),2)
meanFA_tcr$`MH.<1940`<-80 # adjust floor area of <1940 MH
# at the regional level, if there is no data for a cohort, use the average for all non NA regions for that cohort for all regions. This just affects MH
for (k in 1:ncol(meanFA_tcr)) {if(any(is.na(meanFA_tcr[,k]))) {meanFA_tcr[,k]<- round(mean(meanFA_tcr[,k],na.rm = TRUE),2) }}

meanFA_tcr$Region<-rownames(meanFA_tcr)
rownames(meanFA_tcr)<-1:nrow(meanFA_tcr)
meanFA_tcr<-meanFA_tcr[,c(19,1:18)]
# type, cohort, and division
meanFA_tcd<-round(as.data.frame(tapply(bs2020$Floor.Area.m2, list(bs2020$Census.Division,bs2020$Type3,bs2020$Vintage.ACS), mean)),2)
meanFA_tcd$`MH.<1940`<-80
meanFA_tcd$Census.Division<-rownames(meanFA_tcd)
rownames(meanFA_tcd)<-1:nrow(meanFA_tcd)
DivReg<-unique(bs2020[,c("Census.Division","Census.Region")])
meanFA_tcd<-merge(meanFA_tcd,DivReg)
tcds<-tcd_table[tcd_table$count<5,] # which tcd combos have a small # of observations?
for (j in 1:nrow(tcds)) {
  meanFA_tcd[meanFA_tcd$Census.Division==tcds$Census.Division[j], paste(tcds$Type3[j],tcds$`Vintage ACS`[j],sep = ".")]<-NA
}

# fill in blanks based on regional average
for (k in 2:19) { 
  for (r in 1:9) {
   if (is.na(meanFA_tcd[r,k])) {
     meanFA_tcd[r,k]<-round(meanFA_tcr[meanFA_tcr$Region==meanFA_tcd[r,"Census.Region"],k],2)
   } 
  }
}
# type, cohort, and state
meanFA_tcs<-round(as.data.frame(tapply(bs2020$Floor.Area.m2, list(bs2020$State,bs2020$Type3,bs2020$Vintage.ACS), mean)),2)
meanFA_tcs$State<-rownames(meanFA_tcs)
rownames(meanFA_tcs)<-1:nrow(meanFA_tcs)
meanFA_tcs<-meanFA_tcs[,c(19,1:18)]
meanFA_tcs<-merge(meanFA_tcs,StDiv)
tcss<-tcs_table[tcs_table$count<5,] # which tcs combos have a small # of observations?
for (j in 1:nrow(tcss)) {
  meanFA_tcs[meanFA_tcs$State==tcss$State[j], paste(tcss$Type3[j],tcss$`Vintage ACS`[j],sep = ".")]<-NA
}
# fill in blanks based on division average
for (k in 2:19) { 
  for (r in 1:49) {
    if (is.na(meanFA_tcs[r,k])) {
      meanFA_tcs[r,k]<-round(meanFA_tcd[meanFA_tcd$Census.Division==meanFA_tcs[r,"Census.Division"],k],2)
    } 
  }
}

# type, cohort, and county
meanFA_tcc<-round(as.data.frame(tapply(bs2020$Floor.Area.m2, list(bs2020$County,bs2020$Type3,bs2020$Vintage.ACS), mean)),2)
meanFA_tcc$RS_ID<-rownames(meanFA_tcc)
rownames(meanFA_tcc)<-1:nrow(meanFA_tcc)
meanFA_tcc<-merge(meanFA_tcc,ctycode,all = TRUE)
meanFA_tcc<-meanFA_tcc[,c(20,1:19)]
meanFA_tcc<-meanFA_tcc[order(meanFA_tcc$GeoID),]
meanFA_tcc<-meanFA_tcc[-c(which(substr(meanFA_tcc$GeoID,1,2) %in% c("02","15"))),]
tccs<-tcc_table[tcc_table$count<5,] # which tcc combos have a small # of observations?
for (j in 1:nrow(tccs)) {
  meanFA_tcc[meanFA_tcc$RS_ID==tccs$County[j], paste(tccs$Type3[j],tccs$`Vintage ACS`[j],sep = ".")]<-NA
}

meanFA_tcc$State<-substr(meanFA_tcc$RS_ID,1,2)
# fill in blanks based on state average
for (k in 3:20) { 
  for (r in 1:3108) {
    if (is.na(meanFA_tcc[r,k])) {
      meanFA_tcc[r,k]<-round(meanFA_tcs[meanFA_tcs$State==meanFA_tcc[r,"State"],k-1],2)
    } 
  }
}

# mFA by geograpy, RFA #####
# mean floor area by type, cohort, and region
meanFA_tcr_RFA<-round(as.data.frame(tapply(bsRFA$Floor.Area.m2, list(bsRFA$Census.Region,bsRFA$Type3,bsRFA$Vintage.ACS), mean)),2)

meanFA_tcr_RFA$Region<-rownames(meanFA_tcr_RFA)
rownames(meanFA_tcr_RFA)<-1:nrow(meanFA_tcr_RFA)
meanFA_tcr_RFA<-meanFA_tcr_RFA[,c(13,1:12)]
# type, cohort, and division
meanFA_tcd_RFA<-round(as.data.frame(tapply(bsRFA$Floor.Area.m2, list(bsRFA$Census.Division,bsRFA$Type3,bsRFA$Vintage.ACS), mean)),2)
meanFA_tcd_RFA$Census.Division<-rownames(meanFA_tcd_RFA)
rownames(meanFA_tcd_RFA)<-1:nrow(meanFA_tcd_RFA)
DivReg<-unique(bsRFA[,c("Census.Division","Census.Region")])
meanFA_tcd_RFA<-merge(meanFA_tcd_RFA,DivReg)
tcd_RFAs<-tcd_tableRFA[tcd_tableRFA$count<5,] # which tcd_RFA combos have a small # of observations?
# only needs done if tcd_RFAs was > 0
# for (j in 1:nrow(tcd_RFAs)) {
#   meanFA_tcd_RFA[meanFA_tcd_RFA$Census.Division==tcd_RFAs$Census.Division[j], paste(tcd_RFAs$Type3[j],tcd_RFAs$`Vintage ACS`[j],sep = ".")]<-NA
# }

# # fill in blanks based on regional average
# for (k in 2:19) { 
#   for (r in 1:9) {
#     if (is.na(meanFA_tcd_RFA[r,k])) {
#       meanFA_tcd_RFA[r,k]<-round(meanFA_tcr_RFA[meanFA_tcr_RFA$Region==meanFA_tcd_RFA[r,"Census.Region"],k],2)
#     } 
#   }
# }
# type, cohort, and state
meanFA_tcs_RFA<-round(as.data.frame(tapply(bsRFA$Floor.Area.m2, list(bsRFA$State,bsRFA$Type3,bsRFA$Vintage.ACS), mean)),2)
meanFA_tcs_RFA$State<-rownames(meanFA_tcs_RFA)
rownames(meanFA_tcs_RFA)<-1:nrow(meanFA_tcs_RFA)
meanFA_tcs_RFA<-meanFA_tcs_RFA[,c(13,1:12)]
meanFA_tcs_RFA<-merge(meanFA_tcs_RFA,StDiv)
tcs_RFAs<-tcs_tableRFA[tcs_tableRFA$count<5,] # which tcs_RFA combos have a small # of observations?
for (j in 1:nrow(tcs_RFAs)) {
  meanFA_tcs_RFA[meanFA_tcs_RFA$State==tcs_RFAs$State[j], paste(tcs_RFAs$Type3[j],tcs_RFAs$`Vintage ACS`[j],sep = ".")]<-NA
}
# fill in blanks based on division average
for (k in 2:13) { 
  for (r in 1:49) {
    if (is.na(meanFA_tcs_RFA[r,k])) {
      meanFA_tcs_RFA[r,k]<-round(meanFA_tcd_RFA[meanFA_tcd_RFA$Census.Division==meanFA_tcs_RFA[r,"Census.Division"],k],2)
    } 
  }
}

# type, cohort, and county
meanFA_tcc_RFA<-round(as.data.frame(tapply(bsRFA$Floor.Area.m2, list(bsRFA$County,bsRFA$Type3,bsRFA$Vintage.ACS), mean)),2)
meanFA_tcc_RFA$RS_ID<-rownames(meanFA_tcc_RFA)
rownames(meanFA_tcc_RFA)<-1:nrow(meanFA_tcc_RFA)
meanFA_tcc_RFA<-merge(meanFA_tcc_RFA,ctycode,all = TRUE)
meanFA_tcc_RFA<-meanFA_tcc_RFA[,c(14,1:13)]
meanFA_tcc_RFA<-meanFA_tcc_RFA[order(meanFA_tcc_RFA$GeoID),]
meanFA_tcc_RFA<-meanFA_tcc_RFA[-c(which(substr(meanFA_tcc_RFA$GeoID,1,2) %in% c("02","15"))),] # remove AK, HI
tcc_RFAs<-tcc_tableRFA[tcc_tableRFA$count<5,] # which tcc_RFA combos have a small # of observations?
for (j in 1:nrow(tcc_RFAs)) {
  meanFA_tcc_RFA[meanFA_tcc_RFA$RS_ID==tcc_RFAs$County[j], paste(tcc_RFAs$Type3[j],tcc_RFAs$`Vintage ACS`[j],sep = ".")]<-NA
}

meanFA_tcc_RFA$State<-substr(meanFA_tcc_RFA$RS_ID,1,2)
# fill in blanks based on state average
for (k in 3:14) { 
  for (r in 1:3108) {
    if (is.na(meanFA_tcc_RFA[r,k])) {
      meanFA_tcc_RFA[r,k]<-round(meanFA_tcs_RFA[meanFA_tcs_RFA$State==meanFA_tcc_RFA[r,"State"],k-1],2)
    } 
  }
}

# alter meanFA dfs so that the RFA version never has larger floor areas than the regular version

for (j in 1:3108) { 
  if (meanFA_tcc_RFA$MF.2020s[j]>meanFA_tcc$MF.2010s[j]) {
    meanFA_tcc_RFA$MF.2020s[j]<-meanFA_tcc$MF.2010s[j]
  }
  if (meanFA_tcc_RFA$SF.2020s[j]>meanFA_tcc$SF.2010s[j]) {
    meanFA_tcc_RFA$SF.2020s[j]<-meanFA_tcc$SF.2010s[j]
  }
  if (meanFA_tcc_RFA$MH.2020s[j]>meanFA_tcc$MH.2010s[j]) {
    meanFA_tcc_RFA$MH.2020s[j]<-meanFA_tcc$MH.2010s[j]
  }
}

# area and material  calculations ############
load("HSM_results/County_Scenario_SM_Results.RData") # 
SF_dem_factor<-0.35 # how much of SF homes leaving the stock are actually demolished right away?
MF_dem_factor<-0.2 # how much of MF homes leaving the stock are actually demolished right away?
MH_dem_factor<-0.5 # how much of MH homes leaving the stock are actually demolished right away? This was potentially going to be changed to 0.45?
# add the resstock county ID's
smb<-smop_base
smop_base<-merge(smop_base,ctycode)
smop_hiDR<-merge(smop_hiDR,ctycode)
smop_hiMF<-merge(smop_hiMF,ctycode)
smop_hiDRMF<-merge(smop_hiDRMF,ctycode)
# remove AK and HI
smop_base<-smop_base[-c(which(substr(smop_base$GeoID,1,2) %in% c("02","15"))),]
smop_hiDR<-smop_hiDR[-c(which(substr(smop_hiDR$GeoID,1,2) %in% c("02","15"))),]
smop_hiMF<-smop_hiMF[-c(which(substr(smop_hiMF$GeoID,1,2) %in% c("02","15"))),]
smop_hiDRMF<-smop_hiDRMF[-c(which(substr(smop_hiDRMF$GeoID,1,2) %in% c("02","15"))),]

# define the RFA data frames, only for the first 3 stock scenarios
smop_RFA<-smop_base
smop_hiDR_RFA<-smop_hiDR
smop_hiMF_RFA<-smop_hiMF

all.equal(meanFA_tcc$GeoID,smop_base$GeoID) # if this is true, can just transplant the mean FA into the smop files
all.equal(meanFA_tcc$GeoID,smop_hiDR$GeoID) # if this is true, can just transplant the mean FA into the smop files
all.equal(meanFA_tcc$GeoID,smop_hiMF$GeoID) # if this is true, can just transplant the mean FA into the smop files
all.equal(meanFA_tcc$GeoID,smop_hiDRMF$GeoID) # if this is true, can just transplant the mean FA into the smop files
all.equal(meanFA_tcc_RFA$GeoID,smop_RFA$GeoID) # if this is true, can just transplant the mean FA into the smop files
q<-which(is.na(meanFA_tcc[,3:20]))
# if no na.s in q, add average floor areas per type and ACS vintage
smop_base[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]
smop_hiDR[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]
smop_hiMF[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]
smop_hiDRMF[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]
smop_RFA[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]
smop_hiDR_RFA[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]
smop_hiMF_RFA[,paste("FA",names(meanFA_tcc)[3:20],sep=".")]<-meanFA_tcc[,3:20]

# define different floor areas for new construction in the RFA scenarios
smop_RFA[,paste("FA",names(meanFA_tcc_RFA)[3:14],sep=".")]<-meanFA_tcc_RFA[,3:14]
smop_hiDR_RFA[,paste("FA",names(meanFA_tcc_RFA)[3:14],sep=".")]<-meanFA_tcc_RFA[,3:14]
smop_hiMF_RFA[,paste("FA",names(meanFA_tcc_RFA)[3:14],sep=".")]<-meanFA_tcc_RFA[,3:14]

# now calculate m2/cap per house type in each county for each scenario ###########
for (i in c(1:3108)) { # these loops are slow, take about 4 seconds per county
  print(i)
  smop_base[[3]][[i]]<-as.data.frame(smop_base[[3]][[i]])
  
  smop_base[[3]][[i]]$m2cap<-smop_base[[3]][[i]]$m2cap_MH<-smop_base[[3]][[i]]$m2cap_MF<- smop_base[[3]][[i]]$m2cap_SF<-
    smop_base[[3]][[i]]$Occ_m2<-smop_base[[3]][[i]]$Occ_m2_MH<-smop_base[[3]][[i]]$Occ_m2_MF<-smop_base[[3]][[i]]$Occ_m2_SF<-
    smop_base[[3]][[i]]$Tot_NC_m2<-smop_base[[3]][[i]]$NC_MH_m2<-smop_base[[3]][[i]]$NC_MF_m2<-smop_base[[3]][[i]]$NC_SF_m2<-
    smop_base[[3]][[i]]$Tot_Dem_m2<-smop_base[[3]][[i]]$Dem_MH_m2<-smop_base[[3]][[i]]$Dem_MF_m2<-smop_base[[3]][[i]]$Dem_SF_m2<-
    smop_base[[3]][[i]]$Tot_NC<-smop_base[[3]][[i]]$NC_MH<-smop_base[[3]][[i]]$NC_MF<-smop_base[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_base[[3]][[i]]$NC_SF[y]<-smop_base[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_base[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_base[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_base[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_base[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_base[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_base[[3]][[i]]$NC_MF[y]<-smop_base[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_base[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_base[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_base[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_base[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_base[[3]][[i]]$NC_MH[y]<-smop_base[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_base[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_base[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_base[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_base[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_base[[3]][[i]]$NC_SF[y]<-smop_base[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_base[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_base[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_base[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_base[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_base[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_base[[3]][[i]]$NC_MF[y]<-smop_base[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_base[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_base[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_base[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_base[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_base[[3]][[i]]$NC_MH[y]<-smop_base[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_base[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_base[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_base[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_base[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_base[[3]][[i]]$NC_SF[y]<-smop_base[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_base[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_base[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_base[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_base[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_base[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_base[[3]][[i]]$NC_MF[y]<-smop_base[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_base[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_base[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_base[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_base[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_base[[3]][[i]]$NC_MH[y]<-smop_base[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_base[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_base[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_base[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_base[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_base[[3]][[i]]$NC_SF[y]<-smop_base[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_base[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_base[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_base[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_base[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_base[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_base[[3]][[i]]$NC_MF[y]<-smop_base[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_base[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_base[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_base[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_base[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_base[[3]][[i]]$NC_MH[y]<-smop_base[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_base[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_base[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_base[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_base[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_base[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_base[[3]][[i]][,316:318][which(smop_base[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0 # make sure no negative inflows
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_base[[3]][[i]]$NC_SF[1:40]/smop_base[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_base[[3]][[i]]$NC_SF[1:40]/smop_base[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_base[[3]][[i]]$NC_SF[1:40]/smop_base[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_base[[3]][[i]]$NC_SF[1:40]/smop_base[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_base[[3]][[i]]$NC_SF[1:40]<-smop_base[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_base[[3]][[i]]$NC_SF[1:40])/sum(smop_base[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_base[[3]][[i]]$NC_MF[1:40]/smop_base[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_base[[3]][[i]]$NC_MF[1:40]/smop_base[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_base[[3]][[i]]$NC_MF[1:40]/smop_base[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_base[[3]][[i]]$NC_MF[1:40]/smop_base[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_base[[3]][[i]]$NC_MF[1:40]<-smop_base[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_base[[3]][[i]]$NC_MF[1:40])/sum(smop_base[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_base[[3]][[i]]$NC_MH[1:40]/smop_base[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_base[[3]][[i]]$NC_MH[1:40]/smop_base[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_base[[3]][[i]]$NC_MH[1:40]/smop_base[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_base[[3]][[i]]$NC_MH[1:40]/smop_base[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_base[[3]][[i]]$NC_MH[1:40]<-smop_base[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_base[[3]][[i]]$NC_MH[1:40])/sum(smop_base[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_base[[3]][[i]]$Tot_NC<-smop_base[[3]][[i]]$NC_SF+smop_base[[3]][[i]]$NC_MF+smop_base[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_base[[3]][[i]]$NC_SF_m2<-smop_base[[3]][[i]]$NC_SF*smop_base$FA.SF.2010s[i]
  smop_base[[3]][[i]]$NC_MF_m2<-smop_base[[3]][[i]]$NC_MF*smop_base$FA.MF.2010s[i]
  smop_base[[3]][[i]]$NC_MH_m2<-smop_base[[3]][[i]]$NC_MH*smop_base$FA.MH.2010s[i]
  smop_base[[3]][[i]]$Tot_NC_m2<-smop_base[[3]][[i]]$NC_SF_m2+smop_base[[3]][[i]]$NC_MF_m2+smop_base[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_base[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_base[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_base[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))+
    rowSums(smop_base[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_base[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_base[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_base[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_base[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))+
    rowSums(smop_base[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_base[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_base[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_base[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_base[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))+
    rowSums(smop_base[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_base[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_base[[3]][[i]]$Tot_Dem_m2<-smop_base[[3]][[i]]$Dem_SF_m2+smop_base[[3]][[i]]$Dem_MF_m2+smop_base[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type, assuming that 2010 m2/house are constant through 2050s
  smop_base[[3]][[i]]$Occ_m2_SF<-rowSums(smop_base[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_base[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_base[[3]][[i]]$Occ_m2_MF<-rowSums(smop_base[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_base[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_base[[3]][[i]]$Occ_m2_MH<-rowSums(smop_base[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_base[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_base[[3]][[i]]$Occ_m2<-smop_base[[3]][[i]]$Occ_m2_SF+smop_base[[3]][[i]]$Occ_m2_MF+smop_base[[3]][[i]]$Occ_m2_MH
  
  smop_base[[3]][[i]]$m2cap_SF<-smop_base[[3]][[i]]$Occ_m2_SF/smop_base[[3]][[i]]$Pop_SF
  smop_base[[3]][[i]]$m2cap_MF<-smop_base[[3]][[i]]$Occ_m2_MF/smop_base[[3]][[i]]$Pop_MF
  smop_base[[3]][[i]]$m2cap_MH<-smop_base[[3]][[i]]$Occ_m2_MH/smop_base[[3]][[i]]$Pop_MH
  smop_base[[3]][[i]]$m2cap<-smop_base[[3]][[i]]$Occ_m2/smop_base[[3]][[i]]$Population
}

for (i in c(1:3108)) {
  print(i)
  smop_hiDR[[3]][[i]]<-as.data.frame(smop_hiDR[[3]][[i]])
  
  smop_hiDR[[3]][[i]]$m2cap<-smop_hiDR[[3]][[i]]$m2cap_MH<-smop_hiDR[[3]][[i]]$m2cap_MF<- smop_hiDR[[3]][[i]]$m2cap_SF<-
    smop_hiDR[[3]][[i]]$Occ_m2<-smop_hiDR[[3]][[i]]$Occ_m2_MH<-smop_hiDR[[3]][[i]]$Occ_m2_MF<-smop_hiDR[[3]][[i]]$Occ_m2_SF<-
    smop_hiDR[[3]][[i]]$Tot_NC_m2<-smop_hiDR[[3]][[i]]$NC_MH_m2<-smop_hiDR[[3]][[i]]$NC_MF_m2<-smop_hiDR[[3]][[i]]$NC_SF_m2<-
    smop_hiDR[[3]][[i]]$Tot_Dem_m2<-smop_hiDR[[3]][[i]]$Dem_MH_m2<-smop_hiDR[[3]][[i]]$Dem_MF_m2<-smop_hiDR[[3]][[i]]$Dem_SF_m2<-
    smop_hiDR[[3]][[i]]$Tot_NC<-smop_hiDR[[3]][[i]]$NC_MH<-smop_hiDR[[3]][[i]]$NC_MF<-smop_hiDR[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_hiDR[[3]][[i]]$NC_SF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_hiDR[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_hiDR[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_hiDR[[3]][[i]]$NC_MF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_hiDR[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_hiDR[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_hiDR[[3]][[i]]$NC_MH[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_hiDR[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_hiDR[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_hiDR[[3]][[i]]$NC_SF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_hiDR[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_hiDR[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_hiDR[[3]][[i]]$NC_MF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_hiDR[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_hiDR[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_hiDR[[3]][[i]]$NC_MH[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_hiDR[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_hiDR[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_hiDR[[3]][[i]]$NC_SF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_hiDR[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_hiDR[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_hiDR[[3]][[i]]$NC_MF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_hiDR[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_hiDR[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_hiDR[[3]][[i]]$NC_MH[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_hiDR[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_hiDR[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_hiDR[[3]][[i]]$NC_SF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_hiDR[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_hiDR[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_hiDR[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_hiDR[[3]][[i]]$NC_MF[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_hiDR[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_hiDR[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_hiDR[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_hiDR[[3]][[i]]$NC_MH[y]<-smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_hiDR[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_hiDR[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_hiDR[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_hiDR[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_hiDR[[3]][[i]][,316:318][which(smop_hiDR[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_hiDR[[3]][[i]]$NC_SF[1:40]/smop_hiDR[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_hiDR[[3]][[i]]$NC_SF[1:40]/smop_hiDR[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_hiDR[[3]][[i]]$NC_SF[1:40]/smop_hiDR[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_hiDR[[3]][[i]]$NC_SF[1:40]/smop_hiDR[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_hiDR[[3]][[i]]$NC_SF[1:40]<-smop_hiDR[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_hiDR[[3]][[i]]$NC_SF[1:40])/sum(smop_hiDR[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_hiDR[[3]][[i]]$NC_MF[1:40]/smop_hiDR[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_hiDR[[3]][[i]]$NC_MF[1:40]/smop_hiDR[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_hiDR[[3]][[i]]$NC_MF[1:40]/smop_hiDR[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_hiDR[[3]][[i]]$NC_MF[1:40]/smop_hiDR[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_hiDR[[3]][[i]]$NC_MF[1:40]<-smop_hiDR[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_hiDR[[3]][[i]]$NC_MF[1:40])/sum(smop_hiDR[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_hiDR[[3]][[i]]$NC_MH[1:40]/smop_hiDR[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_hiDR[[3]][[i]]$NC_MH[1:40]/smop_hiDR[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_hiDR[[3]][[i]]$NC_MH[1:40]/smop_hiDR[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_hiDR[[3]][[i]]$NC_MH[1:40]/smop_hiDR[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_hiDR[[3]][[i]]$NC_MH[1:40]<-smop_hiDR[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_hiDR[[3]][[i]]$NC_MH[1:40])/sum(smop_hiDR[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_hiDR[[3]][[i]]$Tot_NC<-smop_hiDR[[3]][[i]]$NC_SF+smop_hiDR[[3]][[i]]$NC_MF+smop_hiDR[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_hiDR[[3]][[i]]$NC_SF_m2<-smop_hiDR[[3]][[i]]$NC_SF*smop_hiDR$FA.SF.2010s[i]
  smop_hiDR[[3]][[i]]$NC_MF_m2<-smop_hiDR[[3]][[i]]$NC_MF*smop_hiDR$FA.MF.2010s[i]
  smop_hiDR[[3]][[i]]$NC_MH_m2<-smop_hiDR[[3]][[i]]$NC_MH*smop_hiDR$FA.MH.2010s[i]
  smop_hiDR[[3]][[i]]$Tot_NC_m2<-smop_hiDR[[3]][[i]]$NC_SF_m2+smop_hiDR[[3]][[i]]$NC_MF_m2+smop_hiDR[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_hiDR[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_hiDR[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_hiDR[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))+
    rowSums(smop_hiDR[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_hiDR[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_hiDR[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_hiDR[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_hiDR[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))+
    rowSums(smop_hiDR[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_hiDR[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_hiDR[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_hiDR[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_hiDR[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))+
    rowSums(smop_hiDR[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_hiDR[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_hiDR[[3]][[i]]$Tot_Dem_m2<-smop_hiDR[[3]][[i]]$Dem_SF_m2+smop_hiDR[[3]][[i]]$Dem_MF_m2+smop_hiDR[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type, assuming that 2010 m2/house are constant through 2050s
  smop_hiDR[[3]][[i]]$Occ_m2_SF<-rowSums(smop_hiDR[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_hiDR[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_hiDR[[3]][[i]]$Occ_m2_MF<-rowSums(smop_hiDR[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_hiDR[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_hiDR[[3]][[i]]$Occ_m2_MH<-rowSums(smop_hiDR[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_hiDR[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_hiDR[[3]][[i]]$Occ_m2<-smop_hiDR[[3]][[i]]$Occ_m2_SF+smop_hiDR[[3]][[i]]$Occ_m2_MF+smop_hiDR[[3]][[i]]$Occ_m2_MH
  
  smop_hiDR[[3]][[i]]$m2cap_SF<-smop_hiDR[[3]][[i]]$Occ_m2_SF/smop_hiDR[[3]][[i]]$Pop_SF
  smop_hiDR[[3]][[i]]$m2cap_MF<-smop_hiDR[[3]][[i]]$Occ_m2_MF/smop_hiDR[[3]][[i]]$Pop_MF
  smop_hiDR[[3]][[i]]$m2cap_MH<-smop_hiDR[[3]][[i]]$Occ_m2_MH/smop_hiDR[[3]][[i]]$Pop_MH
  smop_hiDR[[3]][[i]]$m2cap<-smop_hiDR[[3]][[i]]$Occ_m2/smop_hiDR[[3]][[i]]$Population
}

for (i in c(1:3108)) {
  print(i)
  smop_hiMF[[3]][[i]]<-as.data.frame(smop_hiMF[[3]][[i]])
  
  smop_hiMF[[3]][[i]]$m2cap<-smop_hiMF[[3]][[i]]$m2cap_MH<-smop_hiMF[[3]][[i]]$m2cap_MF<- smop_hiMF[[3]][[i]]$m2cap_SF<-
    smop_hiMF[[3]][[i]]$Occ_m2<-smop_hiMF[[3]][[i]]$Occ_m2_MH<-smop_hiMF[[3]][[i]]$Occ_m2_MF<-smop_hiMF[[3]][[i]]$Occ_m2_SF<-
    smop_hiMF[[3]][[i]]$Tot_NC_m2<-smop_hiMF[[3]][[i]]$NC_MH_m2<-smop_hiMF[[3]][[i]]$NC_MF_m2<-smop_hiMF[[3]][[i]]$NC_SF_m2<-
    smop_hiMF[[3]][[i]]$Tot_Dem_m2<-smop_hiMF[[3]][[i]]$Dem_MH_m2<-smop_hiMF[[3]][[i]]$Dem_MF_m2<-smop_hiMF[[3]][[i]]$Dem_SF_m2<-
    smop_hiMF[[3]][[i]]$Tot_NC<-smop_hiMF[[3]][[i]]$NC_MH<-smop_hiMF[[3]][[i]]$NC_MF<-smop_hiMF[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_hiMF[[3]][[i]]$NC_SF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_hiMF[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_hiMF[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_hiMF[[3]][[i]]$NC_MF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_hiMF[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_hiMF[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_hiMF[[3]][[i]]$NC_MH[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_hiMF[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_hiMF[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_hiMF[[3]][[i]]$NC_SF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_hiMF[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_hiMF[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_hiMF[[3]][[i]]$NC_MF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_hiMF[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_hiMF[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_hiMF[[3]][[i]]$NC_MH[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_hiMF[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_hiMF[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_hiMF[[3]][[i]]$NC_SF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_hiMF[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_hiMF[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_hiMF[[3]][[i]]$NC_MF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_hiMF[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_hiMF[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_hiMF[[3]][[i]]$NC_MH[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_hiMF[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_hiMF[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_hiMF[[3]][[i]]$NC_SF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_hiMF[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_hiMF[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_hiMF[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_hiMF[[3]][[i]]$NC_MF[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_hiMF[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_hiMF[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_hiMF[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_hiMF[[3]][[i]]$NC_MH[y]<-smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_hiMF[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_hiMF[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_hiMF[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_hiMF[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_hiMF[[3]][[i]][,316:318][which(smop_hiMF[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_hiMF[[3]][[i]]$NC_SF[1:40]/smop_hiMF[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_hiMF[[3]][[i]]$NC_SF[1:40]/smop_hiMF[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_hiMF[[3]][[i]]$NC_SF[1:40]/smop_hiMF[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_hiMF[[3]][[i]]$NC_SF[1:40]/smop_hiMF[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_hiMF[[3]][[i]]$NC_SF[1:40]<-smop_hiMF[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_hiMF[[3]][[i]]$NC_SF[1:40])/sum(smop_hiMF[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_hiMF[[3]][[i]]$NC_MF[1:40]/smop_hiMF[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_hiMF[[3]][[i]]$NC_MF[1:40]/smop_hiMF[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_hiMF[[3]][[i]]$NC_MF[1:40]/smop_hiMF[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_hiMF[[3]][[i]]$NC_MF[1:40]/smop_hiMF[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_hiMF[[3]][[i]]$NC_MF[1:40]<-smop_hiMF[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_hiMF[[3]][[i]]$NC_MF[1:40])/sum(smop_hiMF[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_hiMF[[3]][[i]]$NC_MH[1:40]/smop_hiMF[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_hiMF[[3]][[i]]$NC_MH[1:40]/smop_hiMF[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_hiMF[[3]][[i]]$NC_MH[1:40]/smop_hiMF[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_hiMF[[3]][[i]]$NC_MH[1:40]/smop_hiMF[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_hiMF[[3]][[i]]$NC_MH[1:40]<-smop_hiMF[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_hiMF[[3]][[i]]$NC_MH[1:40])/sum(smop_hiMF[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_hiMF[[3]][[i]]$Tot_NC<-smop_hiMF[[3]][[i]]$NC_SF+smop_hiMF[[3]][[i]]$NC_MF+smop_hiMF[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_hiMF[[3]][[i]]$NC_SF_m2<-smop_hiMF[[3]][[i]]$NC_SF*smop_hiMF$FA.SF.2010s[i]
  smop_hiMF[[3]][[i]]$NC_MF_m2<-smop_hiMF[[3]][[i]]$NC_MF*smop_hiMF$FA.MF.2010s[i]
  smop_hiMF[[3]][[i]]$NC_MH_m2<-smop_hiMF[[3]][[i]]$NC_MH*smop_hiMF$FA.MH.2010s[i]
  smop_hiMF[[3]][[i]]$Tot_NC_m2<-smop_hiMF[[3]][[i]]$NC_SF_m2+smop_hiMF[[3]][[i]]$NC_MF_m2+smop_hiMF[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_hiMF[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_hiMF[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_hiMF[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))+
    rowSums(smop_hiMF[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_hiMF[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_hiMF[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_hiMF[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_hiMF[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))+
    rowSums(smop_hiMF[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_hiMF[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_hiMF[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_hiMF[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_hiMF[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))+
    rowSums(smop_hiMF[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_hiMF[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_hiMF[[3]][[i]]$Tot_Dem_m2<-smop_hiMF[[3]][[i]]$Dem_SF_m2+smop_hiMF[[3]][[i]]$Dem_MF_m2+smop_hiMF[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type, assuming that 2010 m2/house are constant through 2050s
  smop_hiMF[[3]][[i]]$Occ_m2_SF<-rowSums(smop_hiMF[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_hiMF[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_hiMF[[3]][[i]]$Occ_m2_MF<-rowSums(smop_hiMF[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_hiMF[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_hiMF[[3]][[i]]$Occ_m2_MH<-rowSums(smop_hiMF[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_hiMF[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_hiMF[[3]][[i]]$Occ_m2<-smop_hiMF[[3]][[i]]$Occ_m2_SF+smop_hiMF[[3]][[i]]$Occ_m2_MF+smop_hiMF[[3]][[i]]$Occ_m2_MH
  
  smop_hiMF[[3]][[i]]$m2cap_SF<-smop_hiMF[[3]][[i]]$Occ_m2_SF/smop_hiMF[[3]][[i]]$Pop_SF
  smop_hiMF[[3]][[i]]$m2cap_MF<-smop_hiMF[[3]][[i]]$Occ_m2_MF/smop_hiMF[[3]][[i]]$Pop_MF
  smop_hiMF[[3]][[i]]$m2cap_MH<-smop_hiMF[[3]][[i]]$Occ_m2_MH/smop_hiMF[[3]][[i]]$Pop_MH
  smop_hiMF[[3]][[i]]$m2cap<-smop_hiMF[[3]][[i]]$Occ_m2/smop_hiMF[[3]][[i]]$Population
}

for (i in c(1:3108)) {
  print(i)
  smop_hiDRMF[[3]][[i]]<-as.data.frame(smop_hiDRMF[[3]][[i]])
  
  smop_hiDRMF[[3]][[i]]$m2cap<-smop_hiDRMF[[3]][[i]]$m2cap_MH<-smop_hiDRMF[[3]][[i]]$m2cap_MF<- smop_hiDRMF[[3]][[i]]$m2cap_SF<-
    smop_hiDRMF[[3]][[i]]$Occ_m2<-smop_hiDRMF[[3]][[i]]$Occ_m2_MH<-smop_hiDRMF[[3]][[i]]$Occ_m2_MF<-smop_hiDRMF[[3]][[i]]$Occ_m2_SF<-
    smop_hiDRMF[[3]][[i]]$Tot_NC_m2<-smop_hiDRMF[[3]][[i]]$NC_MH_m2<-smop_hiDRMF[[3]][[i]]$NC_MF_m2<-smop_hiDRMF[[3]][[i]]$NC_SF_m2<-
    smop_hiDRMF[[3]][[i]]$Tot_Dem_m2<-smop_hiDRMF[[3]][[i]]$Dem_MH_m2<-smop_hiDRMF[[3]][[i]]$Dem_MF_m2<-smop_hiDRMF[[3]][[i]]$Dem_SF_m2<-
    smop_hiDRMF[[3]][[i]]$Tot_NC<-smop_hiDRMF[[3]][[i]]$NC_MH<-smop_hiDRMF[[3]][[i]]$NC_MF<-smop_hiDRMF[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_hiDRMF[[3]][[i]]$NC_SF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_hiDRMF[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_hiDRMF[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_hiDRMF[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MH[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_hiDRMF[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_hiDRMF[[3]][[i]]$NC_SF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_hiDRMF[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_hiDRMF[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_hiDRMF[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MH[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_hiDRMF[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_hiDRMF[[3]][[i]]$NC_SF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_hiDRMF[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_hiDRMF[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_hiDRMF[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_hiDRMF[[3]][[i]]$NC_MH[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_hiDRMF[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_hiDRMF[[3]][[i]]$NC_SF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_hiDRMF[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_hiDRMF[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MF[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_hiDRMF[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_hiDRMF[[3]][[i]]$NC_MH[y]<-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_hiDRMF[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_hiDRMF[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_hiDRMF[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_hiDRMF[[3]][[i]][,316:318][which(smop_hiDRMF[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_hiDRMF[[3]][[i]]$NC_SF[1:40]/smop_hiDRMF[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_hiDRMF[[3]][[i]]$NC_SF[1:40]/smop_hiDRMF[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_hiDRMF[[3]][[i]]$NC_SF[1:40]/smop_hiDRMF[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_hiDRMF[[3]][[i]]$NC_SF[1:40]/smop_hiDRMF[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_hiDRMF[[3]][[i]]$NC_SF[1:40]<-smop_hiDRMF[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_hiDRMF[[3]][[i]]$NC_SF[1:40])/sum(smop_hiDRMF[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_hiDRMF[[3]][[i]]$NC_MF[1:40]/smop_hiDRMF[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_hiDRMF[[3]][[i]]$NC_MF[1:40]/smop_hiDRMF[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_hiDRMF[[3]][[i]]$NC_MF[1:40]/smop_hiDRMF[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_hiDRMF[[3]][[i]]$NC_MF[1:40]/smop_hiDRMF[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_hiDRMF[[3]][[i]]$NC_MF[1:40]<-smop_hiDRMF[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_hiDRMF[[3]][[i]]$NC_MF[1:40])/sum(smop_hiDRMF[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_hiDRMF[[3]][[i]]$NC_MH[1:40]/smop_hiDRMF[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_hiDRMF[[3]][[i]]$NC_MH[1:40]/smop_hiDRMF[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_hiDRMF[[3]][[i]]$NC_MH[1:40]/smop_hiDRMF[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_hiDRMF[[3]][[i]]$NC_MH[1:40]/smop_hiDRMF[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_hiDRMF[[3]][[i]]$NC_MH[1:40]<-smop_hiDRMF[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_hiDRMF[[3]][[i]]$NC_MH[1:40])/sum(smop_hiDRMF[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_hiDRMF[[3]][[i]]$Tot_NC<-smop_hiDRMF[[3]][[i]]$NC_SF+smop_hiDRMF[[3]][[i]]$NC_MF+smop_hiDRMF[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_hiDRMF[[3]][[i]]$NC_SF_m2<-smop_hiDRMF[[3]][[i]]$NC_SF*smop_hiDRMF$FA.SF.2010s[i]
  smop_hiDRMF[[3]][[i]]$NC_MF_m2<-smop_hiDRMF[[3]][[i]]$NC_MF*smop_hiDRMF$FA.MF.2010s[i]
  smop_hiDRMF[[3]][[i]]$NC_MH_m2<-smop_hiDRMF[[3]][[i]]$NC_MH*smop_hiDRMF$FA.MH.2010s[i]
  smop_hiDRMF[[3]][[i]]$Tot_NC_m2<-smop_hiDRMF[[3]][[i]]$NC_SF_m2+smop_hiDRMF[[3]][[i]]$NC_MF_m2+smop_hiDRMF[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_hiDRMF[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_hiDRMF[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_hiDRMF[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))+
    rowSums(smop_hiDRMF[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_hiDRMF[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_hiDRMF[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_hiDRMF[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_hiDRMF[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))+
    rowSums(smop_hiDRMF[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_hiDRMF[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_hiDRMF[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_hiDRMF[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_hiDRMF[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))+
    rowSums(smop_hiDRMF[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_hiDRMF[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_hiDRMF[[3]][[i]]$Tot_Dem_m2<-smop_hiDRMF[[3]][[i]]$Dem_SF_m2+smop_hiDRMF[[3]][[i]]$Dem_MF_m2+smop_hiDRMF[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type, assuming that 2010 m2/house are constant through 2050s
  smop_hiDRMF[[3]][[i]]$Occ_m2_SF<-rowSums(smop_hiDRMF[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_hiDRMF[i,c(7,10,13,16,19,22,22,22,22,22)]),each=41),41,10))
  smop_hiDRMF[[3]][[i]]$Occ_m2_MF<-rowSums(smop_hiDRMF[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_hiDRMF[i,c(5,8,11,14,17,20,20,20,20,20)]),each=41),41,10))
  smop_hiDRMF[[3]][[i]]$Occ_m2_MH<-rowSums(smop_hiDRMF[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_hiDRMF[i,c(6,9,12,15,18,21,21,21,21,21)]),each=41),41,10))
  smop_hiDRMF[[3]][[i]]$Occ_m2<-smop_hiDRMF[[3]][[i]]$Occ_m2_SF+smop_hiDRMF[[3]][[i]]$Occ_m2_MF+smop_hiDRMF[[3]][[i]]$Occ_m2_MH
  
  smop_hiDRMF[[3]][[i]]$m2cap_SF<-smop_hiDRMF[[3]][[i]]$Occ_m2_SF/smop_hiDRMF[[3]][[i]]$Pop_SF
  smop_hiDRMF[[3]][[i]]$m2cap_MF<-smop_hiDRMF[[3]][[i]]$Occ_m2_MF/smop_hiDRMF[[3]][[i]]$Pop_MF
  smop_hiDRMF[[3]][[i]]$m2cap_MH<-smop_hiDRMF[[3]][[i]]$Occ_m2_MH/smop_hiDRMF[[3]][[i]]$Pop_MH
  smop_hiDRMF[[3]][[i]]$m2cap<-smop_hiDRMF[[3]][[i]]$Occ_m2/smop_hiDRMF[[3]][[i]]$Population
}

for (i in c(1:3108)) { # these loops are slow, take about 4 seconds per county. Can redo the providence file here with runnung only i=2281
  print(i)
  smop_RFA[[3]][[i]]<-as.data.frame(smop_RFA[[3]][[i]])
  
  smop_RFA[[3]][[i]]$m2cap<-smop_RFA[[3]][[i]]$m2cap_MH<-smop_RFA[[3]][[i]]$m2cap_MF<- smop_RFA[[3]][[i]]$m2cap_SF<-
    smop_RFA[[3]][[i]]$Occ_m2<-smop_RFA[[3]][[i]]$Occ_m2_MH<-smop_RFA[[3]][[i]]$Occ_m2_MF<-smop_RFA[[3]][[i]]$Occ_m2_SF<-
    smop_RFA[[3]][[i]]$Tot_NC_m2<-smop_RFA[[3]][[i]]$NC_MH_m2<-smop_RFA[[3]][[i]]$NC_MF_m2<-smop_RFA[[3]][[i]]$NC_SF_m2<-
    smop_RFA[[3]][[i]]$Tot_Dem_m2<-smop_RFA[[3]][[i]]$Dem_MH_m2<-smop_RFA[[3]][[i]]$Dem_MF_m2<-smop_RFA[[3]][[i]]$Dem_SF_m2<-
    smop_RFA[[3]][[i]]$Tot_NC<-smop_RFA[[3]][[i]]$NC_MH<-smop_RFA[[3]][[i]]$NC_MF<-smop_RFA[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_RFA[[3]][[i]]$NC_SF[y]<-smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_RFA[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_RFA[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_RFA[[3]][[i]]$NC_MF[y]<-smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_RFA[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_RFA[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_RFA[[3]][[i]]$NC_MH[y]<-smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_RFA[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_RFA[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_RFA[[3]][[i]]$NC_SF[y]<-smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_RFA[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_RFA[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_RFA[[3]][[i]]$NC_MF[y]<-smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_RFA[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_RFA[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_RFA[[3]][[i]]$NC_MH[y]<-smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_RFA[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_RFA[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_RFA[[3]][[i]]$NC_SF[y]<-smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_RFA[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_RFA[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_RFA[[3]][[i]]$NC_MF[y]<-smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_RFA[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_RFA[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_RFA[[3]][[i]]$NC_MH[y]<-smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_RFA[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_RFA[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_RFA[[3]][[i]]$NC_SF[y]<-smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_RFA[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_RFA[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_RFA[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_RFA[[3]][[i]]$NC_MF[y]<-smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_RFA[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_RFA[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_RFA[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_RFA[[3]][[i]]$NC_MH[y]<-smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_RFA[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_RFA[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_RFA[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_RFA[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_RFA[[3]][[i]][,316:318][which(smop_RFA[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0 # make sure no negative inflows
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_RFA[[3]][[i]]$NC_SF[1:40]/smop_RFA[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_RFA[[3]][[i]]$NC_SF[1:40]/smop_RFA[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_RFA[[3]][[i]]$NC_SF[1:40]/smop_RFA[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_RFA[[3]][[i]]$NC_SF[1:40]/smop_RFA[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_RFA[[3]][[i]]$NC_SF[1:40]<-smop_RFA[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_RFA[[3]][[i]]$NC_SF[1:40])/sum(smop_RFA[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_RFA[[3]][[i]]$NC_MF[1:40]/smop_RFA[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_RFA[[3]][[i]]$NC_MF[1:40]/smop_RFA[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_RFA[[3]][[i]]$NC_MF[1:40]/smop_RFA[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_RFA[[3]][[i]]$NC_MF[1:40]/smop_RFA[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_RFA[[3]][[i]]$NC_MF[1:40]<-smop_RFA[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_RFA[[3]][[i]]$NC_MF[1:40])/sum(smop_RFA[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_RFA[[3]][[i]]$NC_MH[1:40]/smop_RFA[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_RFA[[3]][[i]]$NC_MH[1:40]/smop_RFA[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_RFA[[3]][[i]]$NC_MH[1:40]/smop_RFA[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_RFA[[3]][[i]]$NC_MH[1:40]/smop_RFA[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_RFA[[3]][[i]]$NC_MH[1:40]<-smop_RFA[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_RFA[[3]][[i]]$NC_MH[1:40])/sum(smop_RFA[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_RFA[[3]][[i]]$Tot_NC<-smop_RFA[[3]][[i]]$NC_SF+smop_RFA[[3]][[i]]$NC_MF+smop_RFA[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_RFA[[3]][[i]]$NC_SF_m2<-smop_RFA[[3]][[i]]$NC_SF*c(rep(smop_RFA$FA.SF.2020s[i],10),rep(smop_RFA$FA.SF.2030s[i],10),rep(smop_RFA$FA.SF.2040s[i],10),rep(smop_RFA$FA.SF.2050s[i],11))
  smop_RFA[[3]][[i]]$NC_MF_m2<-smop_RFA[[3]][[i]]$NC_MF*c(rep(smop_RFA$FA.MF.2020s[i],10),rep(smop_RFA$FA.MF.2030s[i],10),rep(smop_RFA$FA.MF.2040s[i],10),rep(smop_RFA$FA.MF.2050s[i],11))
  smop_RFA[[3]][[i]]$NC_MH_m2<-smop_RFA[[3]][[i]]$NC_MH*c(rep(smop_RFA$FA.MH.2020s[i],10),rep(smop_RFA$FA.MH.2030s[i],10),rep(smop_RFA$FA.MH.2040s[i],10),rep(smop_RFA$FA.MH.2050s[i],11))
  smop_RFA[[3]][[i]]$Tot_NC_m2<-smop_RFA[[3]][[i]]$NC_SF_m2+smop_RFA[[3]][[i]]$NC_MF_m2+smop_RFA[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_RFA[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_RFA[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))+
    rowSums(smop_RFA[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))
  smop_RFA[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_RFA[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))+
    rowSums(smop_RFA[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))
  smop_RFA[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_RFA[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))+
    rowSums(smop_RFA[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))
  smop_RFA[[3]][[i]]$Tot_Dem_m2<-smop_RFA[[3]][[i]]$Dem_SF_m2+smop_RFA[[3]][[i]]$Dem_MF_m2+smop_RFA[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type,now using update RFA assumption on floor area of new housing
  smop_RFA[[3]][[i]]$Occ_m2_SF<-rowSums(smop_RFA[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))
  smop_RFA[[3]][[i]]$Occ_m2_MF<-rowSums(smop_RFA[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))
  smop_RFA[[3]][[i]]$Occ_m2_MH<-rowSums(smop_RFA[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))
  smop_RFA[[3]][[i]]$Occ_m2<-smop_RFA[[3]][[i]]$Occ_m2_SF+smop_RFA[[3]][[i]]$Occ_m2_MF+smop_RFA[[3]][[i]]$Occ_m2_MH
  
  smop_RFA[[3]][[i]]$m2cap_SF<-smop_RFA[[3]][[i]]$Occ_m2_SF/smop_RFA[[3]][[i]]$Pop_SF
  smop_RFA[[3]][[i]]$m2cap_MF<-smop_RFA[[3]][[i]]$Occ_m2_MF/smop_RFA[[3]][[i]]$Pop_MF
  smop_RFA[[3]][[i]]$m2cap_MH<-smop_RFA[[3]][[i]]$Occ_m2_MH/smop_RFA[[3]][[i]]$Pop_MH
  smop_RFA[[3]][[i]]$m2cap<-smop_RFA[[3]][[i]]$Occ_m2/smop_RFA[[3]][[i]]$Population
}

for (i in c(1:3108)) { # these loops are slow, take about 4 seconds per county. Can redo the providence file here with runnung only i=2281
  print(i)
  smop_hiDR_RFA[[3]][[i]]<-as.data.frame(smop_hiDR_RFA[[3]][[i]])
  
  smop_hiDR_RFA[[3]][[i]]$m2cap<-smop_hiDR_RFA[[3]][[i]]$m2cap_MH<-smop_hiDR_RFA[[3]][[i]]$m2cap_MF<- smop_hiDR_RFA[[3]][[i]]$m2cap_SF<-
    smop_hiDR_RFA[[3]][[i]]$Occ_m2<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_MH<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_MF<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_SF<-
    smop_hiDR_RFA[[3]][[i]]$Tot_NC_m2<-smop_hiDR_RFA[[3]][[i]]$NC_MH_m2<-smop_hiDR_RFA[[3]][[i]]$NC_MF_m2<-smop_hiDR_RFA[[3]][[i]]$NC_SF_m2<-
    smop_hiDR_RFA[[3]][[i]]$Tot_Dem_m2<-smop_hiDR_RFA[[3]][[i]]$Dem_MH_m2<-smop_hiDR_RFA[[3]][[i]]$Dem_MF_m2<-smop_hiDR_RFA[[3]][[i]]$Dem_SF_m2<-
    smop_hiDR_RFA[[3]][[i]]$Tot_NC<-smop_hiDR_RFA[[3]][[i]]$NC_MH<-smop_hiDR_RFA[[3]][[i]]$NC_MF<-smop_hiDR_RFA[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_hiDR_RFA[[3]][[i]]$NC_SF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_hiDR_RFA[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MH[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_hiDR_RFA[[3]][[i]]$NC_SF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_hiDR_RFA[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MH[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_hiDR_RFA[[3]][[i]]$NC_SF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_hiDR_RFA[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_hiDR_RFA[[3]][[i]]$NC_MH[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_hiDR_RFA[[3]][[i]]$NC_SF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_hiDR_RFA[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MF[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_hiDR_RFA[[3]][[i]]$NC_MH[y]<-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_hiDR_RFA[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_hiDR_RFA[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_hiDR_RFA[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_hiDR_RFA[[3]][[i]][,316:318][which(smop_hiDR_RFA[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0 # make sure no negative inflows
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]<-smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40])/sum(smop_hiDR_RFA[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]<-smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40])/sum(smop_hiDR_RFA[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiDR_RFA[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]<-smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40])/sum(smop_hiDR_RFA[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_hiDR_RFA[[3]][[i]]$Tot_NC<-smop_hiDR_RFA[[3]][[i]]$NC_SF+smop_hiDR_RFA[[3]][[i]]$NC_MF+smop_hiDR_RFA[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_hiDR_RFA[[3]][[i]]$NC_SF_m2<-smop_hiDR_RFA[[3]][[i]]$NC_SF*c(rep(smop_hiDR_RFA$FA.SF.2020s[i],10),rep(smop_hiDR_RFA$FA.SF.2030s[i],10),rep(smop_hiDR_RFA$FA.SF.2040s[i],10),rep(smop_hiDR_RFA$FA.SF.2050s[i],11))
  smop_hiDR_RFA[[3]][[i]]$NC_MF_m2<-smop_hiDR_RFA[[3]][[i]]$NC_MF*c(rep(smop_hiDR_RFA$FA.MF.2020s[i],10),rep(smop_hiDR_RFA$FA.MF.2030s[i],10),rep(smop_hiDR_RFA$FA.MF.2040s[i],10),rep(smop_hiDR_RFA$FA.MF.2050s[i],11))
  smop_hiDR_RFA[[3]][[i]]$NC_MH_m2<-smop_hiDR_RFA[[3]][[i]]$NC_MH*c(rep(smop_hiDR_RFA$FA.MH.2020s[i],10),rep(smop_hiDR_RFA$FA.MH.2030s[i],10),rep(smop_hiDR_RFA$FA.MH.2040s[i],10),rep(smop_hiDR_RFA$FA.MH.2050s[i],11))
  smop_hiDR_RFA[[3]][[i]]$Tot_NC_m2<-smop_hiDR_RFA[[3]][[i]]$NC_SF_m2+smop_hiDR_RFA[[3]][[i]]$NC_MF_m2+smop_hiDR_RFA[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_hiDR_RFA[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_hiDR_RFA[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))+
    rowSums(smop_hiDR_RFA[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))
  smop_hiDR_RFA[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_hiDR_RFA[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))+
    rowSums(smop_hiDR_RFA[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))
  smop_hiDR_RFA[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_hiDR_RFA[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))+
    rowSums(smop_hiDR_RFA[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))
  smop_hiDR_RFA[[3]][[i]]$Tot_Dem_m2<-smop_hiDR_RFA[[3]][[i]]$Dem_SF_m2+smop_hiDR_RFA[[3]][[i]]$Dem_MF_m2+smop_hiDR_RFA[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type,now using update RFA assumption on floor area of new housing
  smop_hiDR_RFA[[3]][[i]]$Occ_m2_SF<-rowSums(smop_hiDR_RFA[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))
  smop_hiDR_RFA[[3]][[i]]$Occ_m2_MF<-rowSums(smop_hiDR_RFA[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))
  smop_hiDR_RFA[[3]][[i]]$Occ_m2_MH<-rowSums(smop_hiDR_RFA[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_hiDR_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))
  smop_hiDR_RFA[[3]][[i]]$Occ_m2<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_SF+smop_hiDR_RFA[[3]][[i]]$Occ_m2_MF+smop_hiDR_RFA[[3]][[i]]$Occ_m2_MH
  
  smop_hiDR_RFA[[3]][[i]]$m2cap_SF<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_SF/smop_hiDR_RFA[[3]][[i]]$Pop_SF
  smop_hiDR_RFA[[3]][[i]]$m2cap_MF<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_MF/smop_hiDR_RFA[[3]][[i]]$Pop_MF
  smop_hiDR_RFA[[3]][[i]]$m2cap_MH<-smop_hiDR_RFA[[3]][[i]]$Occ_m2_MH/smop_hiDR_RFA[[3]][[i]]$Pop_MH
  smop_hiDR_RFA[[3]][[i]]$m2cap<-smop_hiDR_RFA[[3]][[i]]$Occ_m2/smop_hiDR_RFA[[3]][[i]]$Population
}

for (i in c(1:3108)) { # these loops are slow, take about 4 seconds per county. Can redo the providence file here with runnung only i=2281
  print(i)
  smop_hiMF_RFA[[3]][[i]]<-as.data.frame(smop_hiMF_RFA[[3]][[i]])
  
  smop_hiMF_RFA[[3]][[i]]$m2cap<-smop_hiMF_RFA[[3]][[i]]$m2cap_MH<-smop_hiMF_RFA[[3]][[i]]$m2cap_MF<- smop_hiMF_RFA[[3]][[i]]$m2cap_SF<-
    smop_hiMF_RFA[[3]][[i]]$Occ_m2<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_MH<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_MF<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_SF<-
    smop_hiMF_RFA[[3]][[i]]$Tot_NC_m2<-smop_hiMF_RFA[[3]][[i]]$NC_MH_m2<-smop_hiMF_RFA[[3]][[i]]$NC_MF_m2<-smop_hiMF_RFA[[3]][[i]]$NC_SF_m2<-
    smop_hiMF_RFA[[3]][[i]]$Tot_Dem_m2<-smop_hiMF_RFA[[3]][[i]]$Dem_MH_m2<-smop_hiMF_RFA[[3]][[i]]$Dem_MF_m2<-smop_hiMF_RFA[[3]][[i]]$Dem_SF_m2<-
    smop_hiMF_RFA[[3]][[i]]$Tot_NC<-smop_hiMF_RFA[[3]][[i]]$NC_MH<-smop_hiMF_RFA[[3]][[i]]$NC_MF<-smop_hiMF_RFA[[3]][[i]]$NC_SF<-0
  
  for (y in 1:10) { #2020-2029 New construction
    smop_hiMF_RFA[[3]][[i]]$NC_SF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2020_29[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2020_29[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_SF_Occ_2020_29[y]+smop_hiMF_RFA[[3]][[i]]$Dem_SF_Vac_2020_29[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2020_29[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2020_29[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MF_Occ_2020_29[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MF_Vac_2020_29[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MH[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2020_29[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2020_29[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MH_Occ_2020_29[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MH_Vac_2020_29[y]
  }
  
  for (y in 11:20) { #2030-2039 New construction
    smop_hiMF_RFA[[3]][[i]]$NC_SF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2030_39[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2030_39[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_SF_Occ_2030_39[y]+smop_hiMF_RFA[[3]][[i]]$Dem_SF_Vac_2030_39[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2030_39[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2030_39[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MF_Occ_2030_39[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MF_Vac_2030_39[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MH[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2030_39[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2030_39[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MH_Occ_2030_39[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MH_Vac_2030_39[y]
  }
  
  for (y in 21:30) { #2040-2049 New construction
    smop_hiMF_RFA[[3]][[i]]$NC_SF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2040_49[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2040_49[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_SF_Occ_2040_49[y]+smop_hiMF_RFA[[3]][[i]]$Dem_SF_Vac_2040_49[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2040_49[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2040_49[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MF_Occ_2040_49[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MF_Vac_2040_49[y]
    
    
    smop_hiMF_RFA[[3]][[i]]$NC_MH[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2040_49[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2040_49[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MH_Occ_2040_49[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MH_Vac_2040_49[y]
  }
  for (y in 31:40) { #2050-2059 New construction
    smop_hiMF_RFA[[3]][[i]]$NC_SF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Occ_2050_60[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_SF_Vac_2050_60[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_SF_Occ_2050_60[y]+smop_hiMF_RFA[[3]][[i]]$Dem_SF_Vac_2050_60[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MF[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Occ_2050_60[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MF_Vac_2050_60[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MF_Occ_2050_60[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MF_Vac_2050_60[y]
    
    smop_hiMF_RFA[[3]][[i]]$NC_MH[y]<-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y+1]+smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y+1]-
      smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Occ_2050_60[y]-smop_hiMF_RFA[[3]][[i]]$Tot_HU_MH_Vac_2050_60[y]+
      smop_hiMF_RFA[[3]][[i]]$Dem_MH_Occ_2050_60[y]+smop_hiMF_RFA[[3]][[i]]$Dem_MH_Vac_2050_60[y]
  }
  smop_hiMF_RFA[[3]][[i]][,316:318][which(smop_hiMF_RFA[[3]][[i]][,316:318]<0,arr.ind = TRUE)]<-0 # make sure no negative inflows
  # NC_SFpc[is.infinite(NC_SFpc)]<-NaN
  # correct for the extra loading of new construction towards the end of decades, by making the new construction be the same percentage of total construction in every year of each decade
  # if (any(!is.na(smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_SF[1:40]))){ 
  if (any(!is.na(smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_SF[1:40])&!is.infinite(smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_SF[1:40]))) {
    NC_SFpc<-smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_SF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_SFpc[is.infinite(NC_SFpc)]<-NaN
    NC_SFpc[which(is.nan(NC_SFpc))]<-mean(NC_SFpc,na.rm = TRUE)
    smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]<-smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc*sum(smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40])/sum(smop_hiMF_RFA[[3]][[i]]$NC_SF[1:40]*mean(NC_SFpc)/NC_SFpc) }
  
  # if (any(!is.na(smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MF[1:40]))){ 
  if (any(!is.na(smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MF[1:40])&!is.infinite(smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MF[1:40]))) {
    NC_MFpc<-smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MF[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MFpc[is.infinite(NC_MFpc)]<-NaN
    NC_MFpc[which(is.nan(NC_MFpc))]<-mean(NC_MFpc,na.rm = TRUE)
    smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]<-smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc*sum(smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40])/sum(smop_hiMF_RFA[[3]][[i]]$NC_MF[1:40]*mean(NC_MFpc)/NC_MFpc) }
  
  # if (any(!is.na(smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MH[1:40]))){ 
  if (any(!is.na(smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MH[1:40])&!is.infinite(smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MH[1:40]))) {
    NC_MHpc<-smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]/smop_hiMF_RFA[[3]][[i]]$Con_MH[1:40]
    # convert Infs to Nans, so that they can be removed in the mean function with na.rm
    NC_MHpc[is.infinite(NC_MHpc)]<-NaN
    NC_MHpc[which(is.nan(NC_MHpc))]<-mean(NC_MHpc,na.rm = TRUE)
    smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]<-smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc*sum(smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40])/sum(smop_hiMF_RFA[[3]][[i]]$NC_MH[1:40]*mean(NC_MHpc)/NC_MHpc) }
  # sum up total inflows to stock
  smop_hiMF_RFA[[3]][[i]]$Tot_NC<-smop_hiMF_RFA[[3]][[i]]$NC_SF+smop_hiMF_RFA[[3]][[i]]$NC_MF+smop_hiMF_RFA[[3]][[i]]$NC_MH
  
  # New construction floor area inflows
  smop_hiMF_RFA[[3]][[i]]$NC_SF_m2<-smop_hiMF_RFA[[3]][[i]]$NC_SF*c(rep(smop_hiMF_RFA$FA.SF.2020s[i],10),rep(smop_hiMF_RFA$FA.SF.2030s[i],10),rep(smop_hiMF_RFA$FA.SF.2040s[i],10),rep(smop_hiMF_RFA$FA.SF.2050s[i],11))
  smop_hiMF_RFA[[3]][[i]]$NC_MF_m2<-smop_hiMF_RFA[[3]][[i]]$NC_MF*c(rep(smop_hiMF_RFA$FA.MF.2020s[i],10),rep(smop_hiMF_RFA$FA.MF.2030s[i],10),rep(smop_hiMF_RFA$FA.MF.2040s[i],10),rep(smop_hiMF_RFA$FA.MF.2050s[i],11))
  smop_hiMF_RFA[[3]][[i]]$NC_MH_m2<-smop_hiMF_RFA[[3]][[i]]$NC_MH*c(rep(smop_hiMF_RFA$FA.MH.2020s[i],10),rep(smop_hiMF_RFA$FA.MH.2030s[i],10),rep(smop_hiMF_RFA$FA.MH.2040s[i],10),rep(smop_hiMF_RFA$FA.MH.2050s[i],11))
  smop_hiMF_RFA[[3]][[i]]$Tot_NC_m2<-smop_hiMF_RFA[[3]][[i]]$NC_SF_m2+smop_hiMF_RFA[[3]][[i]]$NC_MF_m2+smop_hiMF_RFA[[3]][[i]]$NC_MH_m2
  # Demolition floor area outflows, using dem factors to convert stock losses to actual demolitions. Not currently calculating columns for total demolitions by type. but can sum up the groups of columns if needed to do so
  smop_hiMF_RFA[[3]][[i]]$Dem_SF_m2<-SF_dem_factor*rowSums(smop_hiMF_RFA[[3]][[i]][,248:257]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))+
    rowSums(smop_hiMF_RFA[[3]][[i]][,258:267]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))
  smop_hiMF_RFA[[3]][[i]]$Dem_MF_m2<-MF_dem_factor*rowSums(smop_hiMF_RFA[[3]][[i]][,268:277]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))+
    rowSums(smop_hiMF_RFA[[3]][[i]][,278:287]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))
  smop_hiMF_RFA[[3]][[i]]$Dem_MH_m2<-MH_dem_factor*rowSums(smop_hiMF_RFA[[3]][[i]][,288:297]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))+
    rowSums(smop_hiMF_RFA[[3]][[i]][,298:307]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))
  smop_hiMF_RFA[[3]][[i]]$Tot_Dem_m2<-smop_hiMF_RFA[[3]][[i]]$Dem_SF_m2+smop_hiMF_RFA[[3]][[i]]$Dem_MF_m2+smop_hiMF_RFA[[3]][[i]]$Dem_MH_m2
  
  # total occupied floor area stock by type,now using update RFA assumption on floor area of new housing
  smop_hiMF_RFA[[3]][[i]]$Occ_m2_SF<-rowSums(smop_hiMF_RFA[[3]][[i]][,110:119]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(7,10,13,16,19,22,25,28,31,34)]),each=41),41,10))
  smop_hiMF_RFA[[3]][[i]]$Occ_m2_MF<-rowSums(smop_hiMF_RFA[[3]][[i]][,130:139]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(5,8,11,14,17,20,23,26,29,32)]),each=41),41,10))
  smop_hiMF_RFA[[3]][[i]]$Occ_m2_MH<-rowSums(smop_hiMF_RFA[[3]][[i]][,150:159]*matrix(rep(as.numeric(smop_hiMF_RFA[i,c(6,9,12,15,18,21,24,27,30,33)]),each=41),41,10))
  smop_hiMF_RFA[[3]][[i]]$Occ_m2<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_SF+smop_hiMF_RFA[[3]][[i]]$Occ_m2_MF+smop_hiMF_RFA[[3]][[i]]$Occ_m2_MH
  
  smop_hiMF_RFA[[3]][[i]]$m2cap_SF<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_SF/smop_hiMF_RFA[[3]][[i]]$Pop_SF
  smop_hiMF_RFA[[3]][[i]]$m2cap_MF<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_MF/smop_hiMF_RFA[[3]][[i]]$Pop_MF
  smop_hiMF_RFA[[3]][[i]]$m2cap_MH<-smop_hiMF_RFA[[3]][[i]]$Occ_m2_MH/smop_hiMF_RFA[[3]][[i]]$Pop_MH
  smop_hiMF_RFA[[3]][[i]]$m2cap<-smop_hiMF_RFA[[3]][[i]]$Occ_m2/smop_hiMF_RFA[[3]][[i]]$Population
}


# overwrite providence FA for new MF MH in RFA, will need to fix otherwise by fixing more of the rows in Geom Floor Area which have small samples and unusual values
# have now fixed this instead by ensuring RFA mean FA never exceed standard FA
# smop_RFA_FA[2281,c(23,24,26,27,29,30,32,33)]<-rep(as.numeric(meanFA_tcc[2281,c(18,19)]),4)
# smop_RFA<-smop_RFA_FA
# # then run the function to define smop_RFA for row 2281 only
# smop_RFA_FA<-smop_RFA

smop_base_FA<-smop_base
smop_hiDR_FA<-smop_hiDR
smop_hiMF_FA<-smop_hiMF
smop_hiDRMF_FA<-smop_hiDRMF
smop_RFA_FA<-smop_RFA
smop_hiDR_RFA_FA<-smop_hiDR_RFA
smop_hiMF_RFA_FA<-smop_hiMF_RFA
save(smop_base_FA,smop_hiDR_FA,smop_hiMF_FA,smop_hiDRMF_FA,smop_RFA_FA,file="HSM_results/County_FloorArea.RData") 
save(smop_base_FA,smop_hiDR_FA,smop_hiMF_FA,smop_RFA_FA,smop_hiDR_RFA_FA,smop_hiMF_RFA_FA,file="../resstock_projections/ExtData/County_FloorArea.RData")
load("HSM_results/County_FloorArea.RData")

# load in materian and ghg intensity data for house types, pick up here when these data are updated ##########
MGI<-read.csv("Data/mat_GHG_int.csv")
load("Material_Intensities/Arch_intensities.RData")
# remove metadata
MGI<-MGI[,1:6]
names(MGI)<-c("Year","Material","Type","MI_kg.m2","GI_kg.kg","GI_kg.m2")
cties<-smop_base_FA[,1:2]
rownames(cties)<-1:nrow(cties)

# calculate here occupied m2 at a national level. And inflows to FA stock.

# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# us_base_FA<-as.data.frame(smop_base_FA[[3]][[1]][,c(1:31,315,319:325)]) # extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, and total occupied m2 by type
us_base_FA<-as.data.frame(smop_base_FA[[3]][[1]][,c(1:31,315:335)])
# add<-c(3,7:9,14:17,22:31,33:39) # columns to add from the initial base dataframe
add<-c(3,7:9,14:17,22:31,33:48)
add_smop<-c(3,7:9,14:17,22:31,316:331)
# turn all variables except year to 0
us_base_FA[,c(1,3:31,33:52)]<-0
# nc_base<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
  us_base_FA[,add]<-us_base_FA[,add]+smop_base_FA[[3]][[i]][,add_smop]
}
# us_base_FA$Tot_Con<-us_base_FA$Con_SF+us_base_FA$Con_MF+us_base_FA$Con_MH
# calculate shares of population by housing type
us_base_FA[,4:6]<-us_base_FA[,7:9]/us_base_FA$Population
# calculation total occ hous_base_FAing units (currently affected by Infinity issue) - not anymore?
# us_base_FA[,14]<-rowSums(us_base_FA[,15:17])
# calculate HHS
us_base_FA[,10:12]<-us_base_FA[,7:9]/us_base_FA[,15:17]
us_base_FA$HH_Size<-us_base_FA$Population/us_base_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_base_FA[,18:21]<-us_base_FA[,22:25]/us_base_FA[,14:17]
# us_base_FA[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_base_FA[,c("Con_SF","Con_MF","Con_MH")]/us_base_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# us_base_FA[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_base_FA[,c("Dem_SF","Dem_MF","Dem_MH")]/us_base_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# calculate m2/cap by type
us_base_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_base_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_base_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate total inflows to FA
# us_base_FA$Tot_NC_m2<-us_base_FA$NC_SF_m2+us_base_FA$NC_MF_m2+us_base_FA$NC_MH_m2

#hi DR
us_hiDR_FA<-as.data.frame(smop_hiDR_FA[[3]][[1]][,c(1:31,315:335)])
# add<-c(3,7:9,14:17,22:31,33:39) # columns to add from the initial hiDR dataframe
add<-c(3,7:9,14:17,22:31,33:48)
add_smop<-c(3,7:9,14:17,22:31,316:331)
# turn all variables except year to 0
us_hiDR_FA[,c(1,3:31,33:52)]<-0
# nc_hiDR<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
  us_hiDR_FA[,add]<-us_hiDR_FA[,add]+smop_hiDR_FA[[3]][[i]][,add_smop]
}
# us_hiDR_FA$Tot_Con<-us_hiDR_FA$Con_SF+us_hiDR_FA$Con_MF+us_hiDR_FA$Con_MH
# calculate shares of population by housing type
us_hiDR_FA[,4:6]<-us_hiDR_FA[,7:9]/us_hiDR_FA$Population
# calculation total occ hous_hiDR_FAing units 
# us_hiDR_FA[,14]<-rowSums(us_hiDR_FA[,15:17])
# calculate HHS
us_hiDR_FA[,10:12]<-us_hiDR_FA[,7:9]/us_hiDR_FA[,15:17]
us_hiDR_FA$HH_Size<-us_hiDR_FA$Population/us_hiDR_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDR_FA[,18:21]<-us_hiDR_FA[,22:25]/us_hiDR_FA[,14:17]
# us_hiDR_FA[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_hiDR_FA[,c("Con_SF","Con_MF","Con_MH")]/us_hiDR_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# us_hiDR_FA[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_hiDR_FA[,c("Dem_SF","Dem_MF","Dem_MH")]/us_hiDR_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# calculate m2/cap by type
us_hiDR_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiDR_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiDR_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]

#hi MF
us_hiMF_FA<-as.data.frame(smop_hiMF_FA[[3]][[1]][,c(1:31,315:335)])
# add<-c(3,7:9,14:17,22:31,33:39) # columns to add from the initial hiMF dataframe
add<-c(3,7:9,14:17,22:31,33:48)
add_smop<-c(3,7:9,14:17,22:31,316:331)
# turn all variables except year to 0
us_hiMF_FA[,c(1,3:31,33:52)]<-0
# nc_hiMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
  us_hiMF_FA[,add]<-us_hiMF_FA[,add]+smop_hiMF_FA[[3]][[i]][,add_smop]
}
# us_hiMF_FA$Tot_Con<-us_hiMF_FA$Con_SF+us_hiMF_FA$Con_MF+us_hiMF_FA$Con_MH
# calculate shares of population by housing type
us_hiMF_FA[,4:6]<-us_hiMF_FA[,7:9]/us_hiMF_FA$Population
# calculation total occ hous_hiMF_FAing units 
# us_hiMF_FA[,14]<-rowSums(us_hiMF_FA[,15:17])
# calculate HHS
us_hiMF_FA[,10:12]<-us_hiMF_FA[,7:9]/us_hiMF_FA[,15:17]
us_hiMF_FA$HH_Size<-us_hiMF_FA$Population/us_hiMF_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiMF_FA[,18:21]<-us_hiMF_FA[,22:25]/us_hiMF_FA[,14:17]
# us_hiMF_FA[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_hiMF_FA[,c("Con_SF","Con_MF","Con_MH")]/us_hiMF_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# us_hiMF_FA[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_hiMF_FA[,c("Dem_SF","Dem_MF","Dem_MH")]/us_hiMF_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# calculate m2/cap by type
us_hiMF_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiMF_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiMF_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]


#hi DRMF
us_hiDRMF_FA<-as.data.frame(smop_hiDRMF_FA[[3]][[1]][,c(1:31,315:335)])
# add<-c(3,7:9,14:17,22:31,33:39) # columns to add from the initial hiDRMF dataframe
add<-c(3,7:9,14:17,22:31,33:48)
add_smop<-c(3,7:9,14:17,22:31,316:331)
# turn all variables except year to 0
us_hiDRMF_FA[,c(1,3:31,33:52)]<-0
# nc_hiDRMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
  us_hiDRMF_FA[,add]<-us_hiDRMF_FA[,add]+smop_hiDRMF_FA[[3]][[i]][,add_smop]
}
# us_hiDRMF_FA$Tot_Con<-us_hiDRMF_FA$Con_SF+us_hiDRMF_FA$Con_MF+us_hiDRMF_FA$Con_MH
# calculate shares of population by housing type
us_hiDRMF_FA[,4:6]<-us_hiDRMF_FA[,7:9]/us_hiDRMF_FA$Population
# calculation total occ hous_hiDRMF_FAing units 
# us_hiDRMF_FA[,14]<-rowSums(us_hiDRMF_FA[,15:17])
# calculate HHS
us_hiDRMF_FA[,10:12]<-us_hiDRMF_FA[,7:9]/us_hiDRMF_FA[,15:17]
us_hiDRMF_FA$HH_Size<-us_hiDRMF_FA$Population/us_hiDRMF_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDRMF_FA[,18:21]<-us_hiDRMF_FA[,22:25]/us_hiDRMF_FA[,14:17]
# us_hiDRMF_FA[,c("Con_Rate_SF","Con_Rate_MF","Con_Rate_MH")]<-us_hiDRMF_FA[,c("Con_SF","Con_MF","Con_MH")]/us_hiDRMF_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# us_hiDRMF_FA[,c("Dem_Rate_SF","Dem_Rate_MF","Dem_Rate_MH")]<-us_hiDRMF_FA[,c("Dem_SF","Dem_MF","Dem_MH")]/us_hiDRMF_FA[,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")]
# calculate m2/cap by type
us_hiDRMF_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiDRMF_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiDRMF_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]

# RFA
us_RFA_FA<-as.data.frame(smop_RFA_FA[[3]][[1]][,c(1:31,315:335)])
add<-c(3,7:9,14:17,22:31,33:48)
add_smop<-c(3,7:9,14:17,22:31,316:331)
# turn all variables except year to 0
us_RFA_FA[,c(1,3:31,33:52)]<-0
# nc_RFA<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
  us_RFA_FA[,add]<-us_RFA_FA[,add]+smop_RFA_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_RFA_FA[,4:6]<-us_RFA_FA[,7:9]/us_RFA_FA$Population
# calculate HHS
us_RFA_FA[,10:12]<-us_RFA_FA[,7:9]/us_RFA_FA[,15:17]
us_RFA_FA$HH_Size<-us_RFA_FA$Population/us_RFA_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_RFA_FA[,18:21]<-us_RFA_FA[,22:25]/us_RFA_FA[,14:17]
# calculate m2/cap by type
us_RFA_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_RFA_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_RFA_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]

# plot comparative m2/cap
# windows()
# plot(us_base_FA$m2cap)
# lines(us_hiDR_FA$m2cap)
# lines(us_hiMF_FA$m2cap)
# lines(us_hiDRMF_FA$m2cap)

m2c_base<-as.data.frame(cbind(us_base_FA$Year,us_base_FA$HS_Scenario,us_base_FA$m2cap))
m2c_hiDR<-as.data.frame(cbind(us_hiDR_FA$Year,us_hiDR_FA$HS_Scenario,us_hiDR_FA$m2cap))
m2c_hiMF<-as.data.frame(cbind(us_hiMF_FA$Year,us_hiMF_FA$HS_Scenario,us_hiMF_FA$m2cap))
m2c_hiDRMF<-as.data.frame(cbind(us_hiDRMF_FA$Year,us_hiDRMF_FA$HS_Scenario,us_hiDRMF_FA$m2cap))
m2c_RFA<-as.data.frame(cbind(us_RFA_FA$Year,us_RFA_FA$HS_Scenario,us_RFA_FA$m2cap))
m2c_RFA[,2]<-5

m2<-rbind(m2c_base,m2c_hiDR,m2c_hiMF,m2c_hiDRMF,m2c_RFA)
names(m2)<-c("Year","Scenario","m2/cap")
m2[m2$Scenario==1,]$Scenario<-"1. Baseline"
m2[m2$Scenario==2,]$Scenario<-"2. High Turnover"
m2[m2$Scenario==3,]$Scenario<-"3. High Multifamily"
m2[m2$Scenario==4,]$Scenario<-"4. High TO & MF"
m2[m2$Scenario==5,]$Scenario<-"5. Red. Floor Area "
windows(width = 7.5,height = 6)
ggplot(m2,aes(Year,`m2/cap`,group=Scenario)) + geom_line(aes(color=Scenario),size=1)+ ylim(50,73) +
  labs(title ="Floor area per capita by housing stock scenario, 2020-2060") + theme_bw() + scale_color_brewer(palette="Set1") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# get total (all materials) ghgi only for sf and mf avg.
mgi_sf<-MGI[MGI$Type %in% c("SF/MH") & MGI$Material=="Materials & Construction",]
mgi_mf<-MGI[MGI$Type %in% c("MF_avg") & MGI$Material=="Materials & Construction",]

# get concrete MI for sf and mf avg.
ci_sf<-MGI[MGI$Type %in% c("SF/MH") & MGI$Material=="Concrete",]
ci_mf<-MGI[MGI$Type %in% c("MF_avg") & MGI$Material=="Concrete",]

# interpolate to get annual GHGI, SF
sd<-data.frame(
  with(select(mgi_sf,Year,GI_kg.m2),
       spline(Year, GI_kg.m2, xout = seq(2020, 2060, by = 1))),
  method = "spline()")
# add to us summary df for SF
us_RFA_FA$GI_SF<-us_base_FA$GI_SF<-us_hiDR_FA$GI_SF<-us_hiMF_FA$GI_SF<-us_hiDRMF_FA$GI_SF<-sd$y
# interpolate to get annual GHGI, MF
sd<-data.frame(
  with(select(mgi_mf,Year,GI_kg.m2),
       spline(Year, GI_kg.m2, xout = seq(2020, 2060, by = 1))),
  method = "spline()")
# add to us summary df for MF
us_RFA_FA$GI_MF<-us_base_FA$GI_MF<-us_hiDR_FA$GI_MF<-us_hiMF_FA$GI_MF<-us_hiDRMF_FA$GI_MF<-sd$y

# interpolate to get annual concrete int, SF
sd<-data.frame(
  with(select(ci_sf,Year,MI_kg.m2),
       spline(Year, MI_kg.m2, xout = seq(2020, 2060, by = 1))),
  method = "spline()")
# add to us summary df for SF
us_RFA_FA$CI_SF<-us_base_FA$CI_SF<-us_hiDR_FA$CI_SF<-us_hiMF_FA$CI_SF<-us_hiDRMF_FA$CI_SF<-sd$y
# interpolate to get annual concrete int,  MF
sd<-data.frame(
  with(select(ci_mf,Year,MI_kg.m2),
       spline(Year, MI_kg.m2, xout = seq(2020, 2060, by = 1))),
  method = "spline()")
# add to ghgis us summary df for MF
us_RFA_FA$CI_MF<-us_base_FA$CI_MF<-us_hiDR_FA$CI_MF<-us_hiMF_FA$CI_MF<-us_hiDRMF_FA$CI_MF<-sd$y

# calculate total embodied carbon from materials in new construction, for each house type, in each scenario
us_base_FA$EmGHG_SF<-us_base_FA$NC_SF_m2*us_base_FA$GI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_base_FA$EmGHG_MF<-us_base_FA$NC_MF_m2*us_base_FA$GI_MF*1e-9
us_base_FA$EmGHG_MH<-us_base_FA$NC_MH_m2*us_base_FA$GI_SF*1e-9 # use ghg intensity of SF for MH
us_base_FA$EmGHG_tot<-us_base_FA$EmGHG_SF+us_base_FA$EmGHG_MF+us_base_FA$EmGHG_MH

us_hiDR_FA$EmGHG_SF<-us_hiDR_FA$NC_SF_m2*us_hiDR_FA$GI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiDR_FA$EmGHG_MF<-us_hiDR_FA$NC_MF_m2*us_hiDR_FA$GI_MF*1e-9
us_hiDR_FA$EmGHG_MH<-us_hiDR_FA$NC_MH_m2*us_hiDR_FA$GI_SF*1e-9 # use ghg intensity of SF for MH
us_hiDR_FA$EmGHG_tot<-us_hiDR_FA$EmGHG_SF+us_hiDR_FA$EmGHG_MF+us_hiDR_FA$EmGHG_MH

us_hiMF_FA$EmGHG_SF<-us_hiMF_FA$NC_SF_m2*us_hiMF_FA$GI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiMF_FA$EmGHG_MF<-us_hiMF_FA$NC_MF_m2*us_hiMF_FA$GI_MF*1e-9
us_hiMF_FA$EmGHG_MH<-us_hiMF_FA$NC_MH_m2*us_hiMF_FA$GI_SF*1e-9 # use ghg intensity of SF for MH
us_hiMF_FA$EmGHG_tot<-us_hiMF_FA$EmGHG_SF+us_hiMF_FA$EmGHG_MF+us_hiMF_FA$EmGHG_MH

us_hiDRMF_FA$EmGHG_SF<-us_hiDRMF_FA$NC_SF_m2*us_hiDRMF_FA$GI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiDRMF_FA$EmGHG_MF<-us_hiDRMF_FA$NC_MF_m2*us_hiDRMF_FA$GI_MF*1e-9
us_hiDRMF_FA$EmGHG_MH<-us_hiDRMF_FA$NC_MH_m2*us_hiDRMF_FA$GI_SF*1e-9 # use ghg intensity of SF for MH
us_hiDRMF_FA$EmGHG_tot<-us_hiDRMF_FA$EmGHG_SF+us_hiDRMF_FA$EmGHG_MF+us_hiDRMF_FA$EmGHG_MH

us_RFA_FA$EmGHG_SF<-us_RFA_FA$NC_SF_m2*us_RFA_FA$GI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_RFA_FA$EmGHG_MF<-us_RFA_FA$NC_MF_m2*us_RFA_FA$GI_MF*1e-9
us_RFA_FA$EmGHG_MH<-us_RFA_FA$NC_MH_m2*us_RFA_FA$GI_SF*1e-9 # use ghg intensity of SF for MH
us_RFA_FA$EmGHG_tot<-us_RFA_FA$EmGHG_SF+us_RFA_FA$EmGHG_MF+us_RFA_FA$EmGHG_MH

# how do total embodied emissions compare for each scenario?
sum(us_base_FA$EmGHG_tot) # 4,971 Mt
sum(us_hiDR_FA$EmGHG_tot) # 6,158 Mt
sum(us_hiMF_FA$EmGHG_tot) # 4,368 Mt
sum(us_hiDRMF_FA$EmGHG_tot) # 5,523 Mt
sum(us_RFA_FA$EmGHG_tot) # 4002 Mt
# so the difference between the baseline and the hig DRMF scenario over 40 years is ~ 732 Mt, which is approximately 1 year of current emissions, or a bit more.
# We will see later if these additional emissions will offset the reduction from having more efficient newer (and more MF) housing
windows()
plot(us_base_FA$Year[1:40],us_base_FA$EmGHG_tot[1:40])
lines(us_hiDR_FA$Year[1:40],us_hiDR_FA$EmGHG_tot[1:40])
lines(us_hiMF_FA$Year[1:40],us_hiMF_FA$EmGHG_tot[1:40],col="blue")
lines(us_hiDRMF_FA$Year[1:40],us_hiDRMF_FA$EmGHG_tot[1:40],col="purple")

# calculate and plot cumulative EM GHG
us_base_FA$EmGHG_cum<-0
for (r in 1:41) {us_base_FA$EmGHG_cum[r]<-sum(us_base_FA$EmGHG_tot[1:r])}
us_hiDR_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiDR_FA$EmGHG_cum[r]<-sum(us_hiDR_FA$EmGHG_tot[1:r])}
us_hiMF_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiMF_FA$EmGHG_cum[r]<-sum(us_hiMF_FA$EmGHG_tot[1:r])}
us_hiDRMF_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiDRMF_FA$EmGHG_cum[r]<-sum(us_hiDRMF_FA$EmGHG_tot[1:r])}
us_RFA_FA$EmGHG_cum<-0
for (r in 1:41) {us_RFA_FA$EmGHG_cum[r]<-sum(us_RFA_FA$EmGHG_tot[1:r])}
# windows()
# plot(us_base_FA$EmGHG_cum)
# lines(us_hiDR_FA$EmGHG_cum)
# lines(us_hiMF_FA$EmGHG_cum)
# lines(us_hiDRMF_FA$EmGHG_cum)

m2$EmGHG_cum<-c(us_base_FA$EmGHG_cum,us_hiDR_FA$EmGHG_cum,us_hiMF_FA$EmGHG_cum,us_hiDRMF_FA$EmGHG_cum,us_RFA_FA$EmGHG_cum)
windows(width = 7.3,height = 6)
ggplot(m2[m2$Year<2060,],aes(Year,EmGHG_cum,group=Scenario)) + geom_line(aes(color=Scenario),size=1)+ scale_y_continuous(labels = scales::comma,breaks=seq(0,6000,1000)) + 
  labs(title ="b) Cumulative GHG from new construction, 2020-2060",y="Mton CO2e") + theme_bw() + scale_color_brewer(palette="Set1") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# now m2 inflow and outflow in each scenario
m2f_base<-as.data.frame(cbind(us_base_FA$Year,us_base_FA$HS_Scenario,us_base_FA$Tot_NC_m2,us_base_FA$Tot_Dem_m2))
m2f_hiDR<-as.data.frame(cbind(us_hiDR_FA$Year,us_hiDR_FA$HS_Scenario,us_hiDR_FA$Tot_NC_m2,us_hiDR_FA$Tot_Dem_m2))
m2f_hiMF<-as.data.frame(cbind(us_hiMF_FA$Year,us_hiMF_FA$HS_Scenario,us_hiMF_FA$Tot_NC_m2,us_hiMF_FA$Tot_Dem_m2))
m2f_hiDRMF<-as.data.frame(cbind(us_hiDRMF_FA$Year,us_hiDRMF_FA$HS_Scenario,us_hiDRMF_FA$Tot_NC_m2,us_hiDRMF_FA$Tot_Dem_m2))
m2f_RFA<-as.data.frame(cbind(us_RFA_FA$Year,us_RFA_FA$HS_Scenario,us_RFA_FA$Tot_NC_m2,us_RFA_FA$Tot_Dem_m2))
m2f_RFA[,2]<-5
m2f<-rbind(m2f_base,m2f_hiDR,m2f_hiMF,m2f_hiDRMF,m2f_RFA)
names(m2f)<-c("Year","Scenario","Construction","Demolition")
m2f[m2f$Scenario==1,]$Scenario<-"1. Baseline"
m2f[m2f$Scenario==2,]$Scenario<-"2. High Turnover"
m2f[m2f$Scenario==3,]$Scenario<-"3. High Multifamily"
m2f[m2f$Scenario==4,]$Scenario<-"4. High TO & MF"
m2f[m2f$Scenario==5,]$Scenario<-"5. Red. Floor Area"
mm<-melt(m2f,id.vars = c("Year","Scenario"))
names(mm)[3]<-"Flow"
mm$Scenario_Flow<-paste(mm$Scenario,mm$Flow,sep="_")
windows(width = 7.3,height = 6)
ggplot(mm[mm$Year<2060,],aes(Year,1e-6*value,group=Scenario_Flow)) + geom_line(aes(color=Scenario,linetype=Flow),size=1)+ scale_y_continuous(labels = scales::comma,limits = c(10,550)) + 
  labs(title ="a) Floor area construction and demolition flows, 2020-2060",y="Million m2") + theme_bw() + scale_color_brewer(palette="Set1") + scale_linetype_manual(values=c("solid", "twodash")) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# Get Concrete Flows associated with construction and demolition
# calculate total concrete flows in new construction, for each house type, in each scenario
us_base_FA$ConcInflow_SF<-us_base_FA$NC_SF_m2*us_base_FA$CI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_base_FA$ConcInflow_MF<-us_base_FA$NC_MF_m2*us_base_FA$CI_MF*1e-9
us_base_FA$ConcInflow_MH<-us_base_FA$NC_MH_m2*us_base_FA$CI_SF*1e-9 # use ghg intensity of SF for MH
us_base_FA$ConcInflow_tot<-us_base_FA$ConcInflow_SF+us_base_FA$ConcInflow_MF+us_base_FA$ConcInflow_MH

us_hiDR_FA$ConcInflow_SF<-us_hiDR_FA$NC_SF_m2*us_hiDR_FA$CI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiDR_FA$ConcInflow_MF<-us_hiDR_FA$NC_MF_m2*us_hiDR_FA$CI_MF*1e-9
us_hiDR_FA$ConcInflow_MH<-us_hiDR_FA$NC_MH_m2*us_hiDR_FA$CI_SF*1e-9 # use ghg intensity of SF for MH
us_hiDR_FA$ConcInflow_tot<-us_hiDR_FA$ConcInflow_SF+us_hiDR_FA$ConcInflow_MF+us_hiDR_FA$ConcInflow_MH

us_hiMF_FA$ConcInflow_SF<-us_hiMF_FA$NC_SF_m2*us_hiMF_FA$CI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiMF_FA$ConcInflow_MF<-us_hiMF_FA$NC_MF_m2*us_hiMF_FA$CI_MF*1e-9
us_hiMF_FA$ConcInflow_MH<-us_hiMF_FA$NC_MH_m2*us_hiMF_FA$CI_SF*1e-9 # use ghg intensity of SF for MH
us_hiMF_FA$ConcInflow_tot<-us_hiMF_FA$ConcInflow_SF+us_hiMF_FA$ConcInflow_MF+us_hiMF_FA$ConcInflow_MH

us_hiDRMF_FA$ConcInflow_SF<-us_hiDRMF_FA$NC_SF_m2*us_hiDRMF_FA$CI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiDRMF_FA$ConcInflow_MF<-us_hiDRMF_FA$NC_MF_m2*us_hiDRMF_FA$CI_MF*1e-9
us_hiDRMF_FA$ConcInflow_MH<-us_hiDRMF_FA$NC_MH_m2*us_hiDRMF_FA$CI_SF*1e-9 # use ghg intensity of SF for MH
us_hiDRMF_FA$ConcInflow_tot<-us_hiDRMF_FA$ConcInflow_SF+us_hiDRMF_FA$ConcInflow_MF+us_hiDRMF_FA$ConcInflow_MH

us_RFA_FA$ConcInflow_SF<-us_RFA_FA$NC_SF_m2*us_RFA_FA$CI_SF*1e-9 # convert from kg to Mt, i.e. Mega tons
us_RFA_FA$ConcInflow_MF<-us_RFA_FA$NC_MF_m2*us_RFA_FA$CI_MF*1e-9
us_RFA_FA$ConcInflow_MH<-us_RFA_FA$NC_MH_m2*us_RFA_FA$CI_SF*1e-9 # use ghg intensity of SF for MH
us_RFA_FA$ConcInflow_tot<-us_RFA_FA$ConcInflow_SF+us_RFA_FA$ConcInflow_MF+us_RFA_FA$ConcInflow_MH

# calculate total concrete flows in demolition for each house type, in each scenario. Unlike construction, demolition concrete intensities do not change from 2020 value
us_base_FA$ConcOutflow_SF<-us_base_FA$Dem_SF_m2*us_base_FA$CI_SF[1]*1e-9 # convert from kg to Mt, i.e. Mega tons
us_base_FA$ConcOutflow_MF<-us_base_FA$Dem_MF_m2*us_base_FA$CI_MF[1]*1e-9
us_base_FA$ConcOutflow_MH<-us_base_FA$Dem_MH_m2*us_base_FA$CI_SF[1]*1e-9 # use ghg intensity of SF for MH
us_base_FA$ConcOutflow_tot<-us_base_FA$ConcOutflow_SF+us_base_FA$ConcOutflow_MF+us_base_FA$ConcOutflow_MH

us_hiDR_FA$ConcOutflow_SF<-us_hiDR_FA$Dem_SF_m2*us_hiDR_FA$CI_SF[1]*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiDR_FA$ConcOutflow_MF<-us_hiDR_FA$Dem_MF_m2*us_hiDR_FA$CI_MF[1]*1e-9
us_hiDR_FA$ConcOutflow_MH<-us_hiDR_FA$Dem_MH_m2*us_hiDR_FA$CI_SF[1]*1e-9 # use ghg intensity of SF for MH
us_hiDR_FA$ConcOutflow_tot<-us_hiDR_FA$ConcOutflow_SF+us_hiDR_FA$ConcOutflow_MF+us_hiDR_FA$ConcOutflow_MH

us_hiMF_FA$ConcOutflow_SF<-us_hiMF_FA$Dem_SF_m2*us_hiMF_FA$CI_SF[1]*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiMF_FA$ConcOutflow_MF<-us_hiMF_FA$Dem_MF_m2*us_hiMF_FA$CI_MF[1]*1e-9
us_hiMF_FA$ConcOutflow_MH<-us_hiMF_FA$Dem_MH_m2*us_hiMF_FA$CI_SF[1]*1e-9 # use ghg intensity of SF for MH
us_hiMF_FA$ConcOutflow_tot<-us_hiMF_FA$ConcOutflow_SF+us_hiMF_FA$ConcOutflow_MF+us_hiMF_FA$ConcOutflow_MH

us_hiDRMF_FA$ConcOutflow_SF<-us_hiDRMF_FA$Dem_SF_m2*us_hiDRMF_FA$CI_SF[1]*1e-9 # convert from kg to Mt, i.e. Mega tons
us_hiDRMF_FA$ConcOutflow_MF<-us_hiDRMF_FA$Dem_MF_m2*us_hiDRMF_FA$CI_MF[1]*1e-9
us_hiDRMF_FA$ConcOutflow_MH<-us_hiDRMF_FA$Dem_MH_m2*us_hiDRMF_FA$CI_SF[1]*1e-9 # use ghg intensity of SF for MH
us_hiDRMF_FA$ConcOutflow_tot<-us_hiDRMF_FA$ConcOutflow_SF+us_hiDRMF_FA$ConcOutflow_MF+us_hiDRMF_FA$ConcOutflow_MH

us_RFA_FA$ConcOutflow_SF<-us_RFA_FA$Dem_SF_m2*us_RFA_FA$CI_SF[1]*1e-9 # convert from kg to Mt, i.e. Mega tons
us_RFA_FA$ConcOutflow_MF<-us_RFA_FA$Dem_MF_m2*us_RFA_FA$CI_MF[1]*1e-9
us_RFA_FA$ConcOutflow_MH<-us_RFA_FA$Dem_MH_m2*us_RFA_FA$CI_SF[1]*1e-9 # use ghg intensity of SF for MH
us_RFA_FA$ConcOutflow_tot<-us_RFA_FA$ConcOutflow_SF+us_RFA_FA$ConcOutflow_MF+us_RFA_FA$ConcOutflow_MH

save(us_base_FA,us_hiMF_FA,us_hiDR_FA,us_hiDRMF_FA,us_RFA_FA,file="Summary_results/US_FA_summaries.RData")
load("Summary_results/US_FA_summaries.RData")
# now caclulate the concrete flows for the demo counties
rnm2<-c(1786,2590,1249,2281) # San Juan NM, Harrix TX, Marquette MI, Prov RI
for (i in rnm2) {
  smop_base_FA[[3]][[i]]$ConcInflow_SF<-smop_base_FA[[3]][[i]]$NC_SF_m2*us_base_FA$CI_SF*1e-6
  smop_base_FA[[3]][[i]]$ConcInflow_MF<-smop_base_FA[[3]][[i]]$NC_MF_m2*us_base_FA$CI_MF*1e-6
  smop_base_FA[[3]][[i]]$ConcInflow_MH<-smop_base_FA[[3]][[i]]$NC_MH_m2*us_base_FA$CI_SF*1e-6
  smop_base_FA[[3]][[i]]$ConcInflow_tot<-smop_base_FA[[3]][[i]]$ConcInflow_SF+smop_base_FA[[3]][[i]]$ConcInflow_MF+smop_base_FA[[3]][[i]]$ConcInflow_MH

  smop_base_FA[[3]][[i]]$ConcOutflow_SF<-smop_base_FA[[3]][[i]]$Dem_SF_m2*us_base_FA$CI_SF[1]*1e-6
  smop_base_FA[[3]][[i]]$ConcOutflow_MF<-smop_base_FA[[3]][[i]]$Dem_MF_m2*us_base_FA$CI_MF[1]*1e-6
  smop_base_FA[[3]][[i]]$ConcOutflow_MH<-smop_base_FA[[3]][[i]]$Dem_MH_m2*us_base_FA$CI_SF[1]*1e-6
  smop_base_FA[[3]][[i]]$ConcOutflow_tot<-smop_base_FA[[3]][[i]]$ConcOutflow_SF+smop_base_FA[[3]][[i]]$ConcOutflow_MF+smop_base_FA[[3]][[i]]$ConcOutflow_MH
  
  smop_hiDR_FA[[3]][[i]]$ConcInflow_SF<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*us_hiDR_FA$CI_SF*1e-6
  smop_hiDR_FA[[3]][[i]]$ConcInflow_MF<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2*us_hiDR_FA$CI_MF*1e-6
  smop_hiDR_FA[[3]][[i]]$ConcInflow_MH<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2*us_hiDR_FA$CI_SF*1e-6
  smop_hiDR_FA[[3]][[i]]$ConcInflow_tot<-smop_hiDR_FA[[3]][[i]]$ConcInflow_SF+smop_hiDR_FA[[3]][[i]]$ConcInflow_MF+smop_hiDR_FA[[3]][[i]]$ConcInflow_MH
  
  smop_hiDR_FA[[3]][[i]]$ConcOutflow_SF<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*us_hiDR_FA$CI_SF[1]*1e-6
  smop_hiDR_FA[[3]][[i]]$ConcOutflow_MF<-smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*us_hiDR_FA$CI_MF[1]*1e-6
  smop_hiDR_FA[[3]][[i]]$ConcOutflow_MH<-smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*us_hiDR_FA$CI_SF[1]*1e-6
  smop_hiDR_FA[[3]][[i]]$ConcOutflow_tot<-smop_hiDR_FA[[3]][[i]]$ConcOutflow_SF+smop_hiDR_FA[[3]][[i]]$ConcOutflow_MF+smop_hiDR_FA[[3]][[i]]$ConcOutflow_MH
  
  smop_hiMF_FA[[3]][[i]]$ConcInflow_SF<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*us_hiMF_FA$CI_SF*1e-6
  smop_hiMF_FA[[3]][[i]]$ConcInflow_MF<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2*us_hiMF_FA$CI_MF*1e-6
  smop_hiMF_FA[[3]][[i]]$ConcInflow_MH<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2*us_hiMF_FA$CI_SF*1e-6
  smop_hiMF_FA[[3]][[i]]$ConcInflow_tot<-smop_hiMF_FA[[3]][[i]]$ConcInflow_SF+smop_hiMF_FA[[3]][[i]]$ConcInflow_MF+smop_hiMF_FA[[3]][[i]]$ConcInflow_MH
  
  smop_hiMF_FA[[3]][[i]]$ConcOutflow_SF<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*us_hiMF_FA$CI_SF[1]*1e-6
  smop_hiMF_FA[[3]][[i]]$ConcOutflow_MF<-smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*us_hiMF_FA$CI_MF[1]*1e-6
  smop_hiMF_FA[[3]][[i]]$ConcOutflow_MH<-smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*us_hiMF_FA$CI_SF[1]*1e-6
  smop_hiMF_FA[[3]][[i]]$ConcOutflow_tot<-smop_hiMF_FA[[3]][[i]]$ConcOutflow_SF+smop_hiMF_FA[[3]][[i]]$ConcOutflow_MF+smop_hiMF_FA[[3]][[i]]$ConcOutflow_MH
  
  smop_hiDRMF_FA[[3]][[i]]$ConcInflow_SF<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*us_hiDRMF_FA$CI_SF*1e-6
  smop_hiDRMF_FA[[3]][[i]]$ConcInflow_MF<-smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*us_hiDRMF_FA$CI_MF*1e-6
  smop_hiDRMF_FA[[3]][[i]]$ConcInflow_MH<-smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*us_hiDRMF_FA$CI_SF*1e-6
  smop_hiDRMF_FA[[3]][[i]]$ConcInflow_tot<-smop_hiDRMF_FA[[3]][[i]]$ConcInflow_SF+smop_hiDRMF_FA[[3]][[i]]$ConcInflow_MF+smop_hiDRMF_FA[[3]][[i]]$ConcInflow_MH
  
  smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_SF<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*us_hiDRMF_FA$CI_SF[1]*1e-6
  smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_MF<-smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*us_hiDRMF_FA$CI_MF[1]*1e-6
  smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_MH<-smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*us_hiDRMF_FA$CI_SF[1]*1e-6
  smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_tot<-smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_SF+smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_MF+smop_hiDRMF_FA[[3]][[i]]$ConcOutflow_MH
  
  smop_RFA_FA[[3]][[i]]$ConcInflow_SF<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*us_RFA_FA$CI_SF*1e-6
  smop_RFA_FA[[3]][[i]]$ConcInflow_MF<-smop_RFA_FA[[3]][[i]]$NC_MF_m2*us_RFA_FA$CI_MF*1e-6
  smop_RFA_FA[[3]][[i]]$ConcInflow_MH<-smop_RFA_FA[[3]][[i]]$NC_MH_m2*us_RFA_FA$CI_SF*1e-6
  smop_RFA_FA[[3]][[i]]$ConcInflow_tot<-smop_RFA_FA[[3]][[i]]$ConcInflow_SF+smop_RFA_FA[[3]][[i]]$ConcInflow_MF+smop_RFA_FA[[3]][[i]]$ConcInflow_MH
  
  smop_RFA_FA[[3]][[i]]$ConcOutflow_SF<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*us_RFA_FA$CI_SF[1]*1e-6
  smop_RFA_FA[[3]][[i]]$ConcOutflow_MF<-smop_RFA_FA[[3]][[i]]$Dem_MF_m2*us_RFA_FA$CI_MF[1]*1e-6
  smop_RFA_FA[[3]][[i]]$ConcOutflow_MH<-smop_RFA_FA[[3]][[i]]$Dem_MH_m2*us_RFA_FA$CI_SF[1]*1e-6
  smop_RFA_FA[[3]][[i]]$ConcOutflow_tot<-smop_RFA_FA[[3]][[i]]$ConcOutflow_SF+smop_RFA_FA[[3]][[i]]$ConcOutflow_MF+smop_RFA_FA[[3]][[i]]$ConcOutflow_MH
} # convert from kg to kt

CtyCon_base<-as.data.frame(smop_base_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")])
CtyCon_hiDR<-as.data.frame(smop_hiDR_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")])
CtyCon_hiMF<-as.data.frame(smop_hiMF_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")])
CtyCon_hiDRMF<-as.data.frame(smop_hiDRMF_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")])
CtyCon_RFA<-as.data.frame(smop_RFA_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")])

for (r in rnm2[1:4]) {
  CtyCon_base<-rbind(CtyCon_base,as.data.frame(smop_base_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")]))
  CtyCon_hiDR<-rbind(CtyCon_hiDR,as.data.frame(smop_hiDR_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")]))
  CtyCon_hiMF<-rbind(CtyCon_hiMF,as.data.frame(smop_hiMF_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")]))
  CtyCon_hiDRMF<-rbind(CtyCon_hiDRMF,as.data.frame(smop_hiDRMF_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")]))
  CtyCon_RFA<-rbind(CtyCon_RFA,as.data.frame(smop_RFA_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","ConcInflow_tot","ConcOutflow_tot")]))
  
}
CtyCon_RFA$HS_Scenario<-5
CtyCon<-rbind(CtyCon_base,CtyCon_hiDR,CtyCon_hiMF,CtyCon_hiDRMF,CtyCon_RFA)
names(CtyCon)[4:5]<-c("Construction","Demolition")
Con<-melt(CtyCon,id.vars = c("Year","GeoID","HS_Scenario"))
names(Con)[3:4]<-c("Scenario", "Flow")
Con[Con$Scenario==1,]$Scenario<-"1. Baseline"
Con[Con$Scenario==2,]$Scenario<-"2. High Turnover"
Con[Con$Scenario==3,]$Scenario<-"3. High Multifamily"
Con[Con$Scenario==4,]$Scenario<-"4. High TO & MF"
Con[Con$Scenario==5,]$Scenario<-"5. Red. Floor Area"

r=rnm2[3] # 1, San Juan; 3, Marquette; 4,Providence
ConCty<-Con[Con$GeoID==cties[r,]$GeoID,]
location<-ctycode[ctycode$GeoID==cties[r,]$GeoID,]$RS_ID# will fix this to RS_ID
ConCty$Scenario_Flow<-paste(ConCty$Scenario,ConCty$Flow,sep="_")
# cols<-c("#E41A1C","#E41A1C","#984EA3","#984EA3")
# cols<-c("#E41A1C","#E41A1C","#984EA3","#984EA3")
cols<-c("#E41A1C","#377EB8","#E41A1C","#377EB8","#FF7F00") # update to 5 colours
windows(width = 6.8,height = 5.3)
ggplot(ConCty[ConCty$Year<2060,],aes(Year,value,group=Scenario_Flow)) + geom_line(aes(color=Scenario,linetype=Flow),size=1)+ scale_y_continuous(labels = scales::comma) + scale_color_manual(values = cols) +
  labs(title =paste("Concrete inflows and outflows,",location),y="kt Concrete") + theme_bw() + scale_linetype_manual(values=c("solid", "twodash")) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

r=rnm2[2] # 2, Harris
ConCty<-Con[Con$GeoID==cties[r,]$GeoID,]
location<-ctycode[ctycode$GeoID==cties[r,]$GeoID,]$RS_ID# will fix this to RS_ID
ConCty$Scenario_Flow<-paste(ConCty$Scenario,ConCty$Flow,sep="_")
# cols<-c("#E41A1C","#E41A1C","#984EA3","#984EA3")
cols<-c("#E41A1C","#377EB8","#E41A1C","#377EB8","#FF7F00") # update to 5 colours
windows(width = 6.8,height = 5.3)
ggplot(ConCty[ConCty$Year<2060,],aes(Year,value,group=Scenario_Flow)) + geom_line(aes(color=Scenario,linetype=Flow),size=1)+ scale_y_continuous(labels = scales::comma) + #scale_color_manual(values = cols) +
  labs(title =paste("Concrete inflows and outflows,",location),y="kt Concrete") + theme_bw() + scale_linetype_manual(values=c("solid", "twodash")) +  scale_color_brewer(palette="Set1") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# Providence overwrite
# ConCty[ConCty$Scenario=="3. High Multifamily",]$value<-ConCty[ConCty$Scenario=="1. Baseline",]$value
# ConCty[ConCty$Scenario=="4. High TO & MF",]$value<-ConCty[ConCty$Scenario=="2. High Turnover",]$value

# energy calculations #################
# load("EnergyLinModels.RData")
# 
# bs2020$FuelOil_MJ<-bs2020$Propane_MJ<-bs2020$NaturalGas_MJ<-bs2020$Elec_MJ<-0
# bs2020$Vintage.LM<-bs2020$Vintage
# bs2020[bs2020$Vintage=="1940s",]$Vintage.LM<-"1950s"
# 
# bs2020$ASHRAE.IECC.Climate.Zone.LM<-bs2020$ASHRAE.IECC.Climate.Zone.2004
# bs2020[bs2020$ASHRAE.IECC.Climate.Zone.2004 =="1A",]$ASHRAE.IECC.Climate.Zone.LM<-"2A"
# bs2020[bs2020$ASHRAE.IECC.Climate.Zone.2004 =="4B",]$ASHRAE.IECC.Climate.Zone.LM<-"3B"
# bs2020[bs2020$ASHRAE.IECC.Climate.Zone.2004 =="6B",]$ASHRAE.IECC.Climate.Zone.LM<-"6A"
# bs2020[bs2020$ASHRAE.IECC.Climate.Zone.2004 =="7B",]$ASHRAE.IECC.Climate.Zone.LM<-"7A"
# 
# bs2020$Heating.Fuel.LM<-bs2020$Heating.Fuel
# bs2020[bs2020$Heating.Fuel.LM=="Electricity",]$Heating.Fuel.LM<-"Electricity Oth"
# bs2020[bs2020$HVAC.Heating.Type.And.Fuel %in% c("Electricity ASHP","Electricity MSHP"),]$Heating.Fuel.LM<-"Electricity HP"
# bs2020[bs2020$HVAC.Heating.Type.And.Fuel %in% c("Electricity Electric Furnace"),]$Heating.Fuel.LM<-"Electricity Ducted"
# 
# bs2020$Clothes.Dryer.Fuel.LM<-"Gas"
# bs2020[substr(bs2020$Clothes.Dryer,1,4) %in% c("Elec"),]$Clothes.Dryer.Fuel.LM<-"Electricity"
# bs2020[substr(bs2020$Clothes.Dryer,1,4) %in% c("Prop"),]$Clothes.Dryer.Fuel.LM<-"Propane"
# bs2020[substr(bs2020$Clothes.Dryer,1,4) %in% c("None"),]$Clothes.Dryer.Fuel.LM<-"None"
# 
# bs2020$HVAC.Cooling.Type.LM<-bs2020$HVAC.Cooling.Type
# bs2020[bs2020$HVAC.Cooling.Type=="Heat Pump",]$HVAC.Cooling.Type.LM<-"Central AC"
# 
# # changed models here to the v2
# # bs2020$Elec_MJ<-round(predict(lmEL,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
# #                             FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,NHSLDMEM=bs2020$Occupants)))
# bs2020$Elec_MJ<-round(predict(lmEL2,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
#                                                FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,ACType=bs2020$HVAC.Cooling.Type.LM,WaterHeatFuel=bs2020$Water.Heater.Fuel, DryerFuel=bs2020$Clothes.Dryer.Fuel.LM )))
# bs2020$Elec_MJ[which(bs2020$Elec_MJ<5000)]<-5000 # set 5000 MJ (1389 kWh) as the min annual electricity consumption
# 
# # bs2020$NaturalGas_MJ<-round(predict(lmNG,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
# #                                               FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,NHSLDMEM=bs2020$Occupants)))
# bs2020$NaturalGas_MJ<-round(predict(lmNG,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
#                                                     FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,WaterHeatFuel=bs2020$Water.Heater.Fuel, DryerFuel=bs2020$Clothes.Dryer.Fuel.LM )))
# bs2020$NaturalGas_MJ[which(bs2020$NaturalGas_MJ<0)]<-bs2020$NaturalGas_MJ[which(bs2020$NaturalGas_MJ<0)]+5000
# bs2020$NaturalGas_MJ[which(bs2020$NaturalGas_MJ<0)]<-0# set 0 MJ as the min annual gas consumption
# 
# # bs2020$FuelOil_MJ<-round(predict(lmFO,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
# #                                                     FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,NHSLDMEM=bs2020$Occupants)))
# bs2020$FuelOil_MJ<-round(predict(lmFO2,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
#                                                  FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,WaterHeatFuel=bs2020$Water.Heater.Fuel)))
# bs2020$FuelOil_MJ[which(bs2020$FuelOil_MJ<0)]<- 0# set 0 MJ as the min annual oil consumption
# bs2020[!bs2020$Heating.Fuel=="Fuel Oil" & !bs2020$Water.Heater.Fuel=="Fuel Oil",]$FuelOil_MJ<-0 # turn oil to 0 if neither water or space heater is oil
# 
# # bs2020$Propane_MJ<-round(predict(lmPr,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
# #                                                  FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,NHSLDMEM=bs2020$Occupants)))
# bs2020$Propane_MJ<-round(predict(lmPr2,data.frame(Type=bs2020$Geometry.Building.Type.RECS,Vintage=bs2020$Vintage.LM,IECC_Climate_Pub=bs2020$ASHRAE.IECC.Climate.Zone.LM,
#                                                  FloorArea=bs2020$Geometry.Floor.Area, HeatFuel=bs2020$Heating.Fuel,WaterHeatFuel=bs2020$Water.Heater.Fuel, DryerFuel=bs2020$Clothes.Dryer.Fuel.LM )))
# bs2020$Propane_MJ[which(bs2020$Propane_MJ<0)]<- 0# set 0 MJ as the min annual gas consumption
# bs2020$Total_Energy<-bs2020$Elec_MJ+bs2020$NaturalGas_MJ+bs2020$Propane_MJ+bs2020$FuelOil_MJ
# bring in decay factors ##############
load("decayFactors3.RData")
# first of all, what is the initial weighting, how many buildings are represented in 2020?
tot_occ_20<-sum(h20$Tot_Occ_Units)
bs2020$base_weight<-tot_occ_20/nrow(bs2020) # with 150 sims, we have around 1 sim for every 817 homes. no decay factors for base year
bs2020$Year<-2020
# bring in GHG intensity of electricity ##################
# i can either use intensities at the level of 18 RTOs. or 134 balancing areas.
# maybe rto is preferable, but i would like to discuss with NREL
ctycode_num<-ctycode
ctycode_num$GeoID<-as.numeric(ctycode_num$GeoID)
# load("GHGI_LowRECost.RData")
# gicty_rto<-gicty_rto_LREC

load("GHGI_MidCase.RData")
gicty_rto[gicty_rto$geoid10==46113,]$geoid10<-46102 # replace Shannon County SD with Oglala Lakota Cty
gicty_rto[gicty_rto$geoid10==2270,]$geoid10<-2158 # replace Wade Hampton AK with Kusilvak AK
gicty_rto<-merge(gicty_rto,ctycode_num,by.x="geoid10",by.y="GeoID") # this will remove values for the no longer existing Beford City VA (51515)
gicty_rto_2020<-gicty_rto[gicty_rto$Year %in% c(2020),] # get only the RS simulation years
bs2020<-merge(bs2020,gicty_rto_2020,by.x="County",by.y="RS_ID")
bs2020<-bs2020[,!names(bs2020) %in% c("Year.y")]
bs2020$GHG_int<-bs2020$GHG_int/3600 # convert from kg/MWh to kg/MJ
# ghg intensity of energy from the EPA 2009 file
GHGI_FO<-((.07396)+(25*3e-6)+(298*6e-7))/1.055  # intensity for heating oil (DFO #2) in kgCO2eq / MJ
GHGI_NG<-((0.05302)+(25*10e-6) + (298*1e-7))/1.055  # intensity for natural gas in kgCO2eq / MJ
GHGI_LP<-((.06298)+(25*3e-6)+(298*6e-7))/1.055   # intensity for LPG in kgCO2eq / MJ

bs2020$EnGHG<-(bs2020$NaturalGas_MJ*GHGI_NG)+(bs2020$FuelOil_MJ*GHGI_FO)+(bs2020$Propane_MJ*GHGI_LP)+(bs2020$Elec_MJ*bs2020$GHG_int) # household emissions in kg 
# calculate GHG by fuel type
bs2020$ElGHG<-bs2020$Elec_MJ*bs2020$GHG_int
bs2020$NGGHG<-bs2020$NaturalGas_MJ*GHGI_NG
bs2020$FOGHG<-bs2020$FuelOil_MJ*GHGI_FO
bs2020$PrGHG<-bs2020$Propane_MJ*GHGI_LP

GHG_En_2020<-sum(bs2020$base_weight*bs2020$EnGHG)*1e-9 # estimate of total residential emissions in 2020, occupied housing only, excluding AK and HI, in million tons GHG-eq
GHG_cap_2020<-1e-3*sum(bs2020$base_weight*bs2020$EnGHG)/sum(h20$Pop_MF+h20$Pop_SF+h20$Pop_MH) # aroudn 2.3 ton/cap, relatively consistent with Goldstein PNAS Fig4A

# get average GHG/hh by type by state
g_ts<-as.data.frame(round(1e-3*tapply(bs2020$EnGHG,list(bs2020$State,bs2020$Type3),mean),3)) # in tons per household, by type/state
# now per person
h20$State<-substr(h20$RS_ID,1,2)
Pop_State<-as.data.frame(tapply(h20$Pop_SF,h20$State,sum))
names(Pop_State)<-"SF"
Pop_State$MF<-tapply(h20$Pop_MF,h20$State,sum)
Pop_State$MH<-tapply(h20$Pop_MH,h20$State,sum)
G_ts<-as.data.frame(round(1e-3*tapply(bs2020$EnGHG*bs2020$base_weight,list(bs2020$State,bs2020$Type3),sum),3)) # total tons per type/state
G_ts<-G_ts[,c(3,1,2)]
G_ts[is.na(G_ts)]<-0
all.equal(rownames(Pop_State),rownames(G_ts))
G_ts[,c("SF_pc","MF_pc","MH_pc")]<-round(G_ts[,c("SF","MF","MH")]/Pop_State[,c("SF","MF","MH")],2)

bs2020GHG<-bs2020
save(bs2020GHG,h20,file="bs2020GHG2.RData") # #2 saved with the #2 linear energy models
# save(bs2020GHG,h20,file="bs2020GHG_LREC.RData") # #2 saved with the #2 linear energy models

