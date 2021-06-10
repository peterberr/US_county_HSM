# script to calculate Mat. and GHG intensities per house type and cohort and county, for projections
# script to load, analyze, and combine by scenario bs.csv files
# Peter Berrill April 2021
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
setwd("~/Yale Courses/Research/Final Paper/HSM_github")
load("Resstock_outputs/bs_base.RData") # too large to store on github
load("Resstock_outputs/bs_baseRFA.RData") # too large to store on github
load("Material_Intensities/Arch_intensities.RData") # produced by mgInt.R
load("Intermediate_results/ctycode.RData")
StDiv<-unique(bs_base[,c("State","Census.Division")])

# adjust Athena based impacts of emissions from transport and construction by factor 2, except for MH which have much lower construction energy requirements
mgi_all[!mgi_all$Type=="MH",]$`gi20_Transport/Construction`<-2*mgi_all[!mgi_all$Type=="MH",]$`gi20_Transport/Construction`
mgi_all[!mgi_all$Type=="MH",]$`gi60_Transport/Construction`<-2*mgi_all[!mgi_all$Type=="MH",]$`gi60_Transport/Construction`
# readjust the totals with updated transport and construction emissions
mgi_all$gi20_Tot<-rowSums(mgi_all[,19:29])
mgi_all$gi60_Tot<-rowSums(mgi_all[,31:41])

# GHG intensities for baseline floor area ########
bs_base$arch<-0
# define all archetype groups
# slab SF
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories==1 &
          !bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-1

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-2

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories==1 &
          !bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-3

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-4

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories>1 &
          !bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-5

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-6

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-7
# this one needs redefined, represent with arch 7 for now, small SF with multiple stories and garage
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Slab" & bs_base$Geometry.Stories>1 &
          !bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-7
# Basement SF
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories==1 &
          !bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-8

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-9

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories==1 &
          !bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-10

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-11

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories>1 &
          !bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-12

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-13

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-14
# this one needs redefined, represent with arch 14 for now, small SF with multiple stories and garage
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_base$Geometry.Stories>1 &
          !bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-14
# MF Mid-Rise
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units") & !bs_base$Geometry.Floor.Area.Bin == "0-1499" & 
          !bs_base$Geometry.Building.Number.Units.HL=="50 or more Units high", ]$arch<-15 # Large MF Mid-rise
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units") & bs_base$Geometry.Floor.Area.Bin == "0-1499" & 
          !bs_base$Geometry.Building.Number.Units.HL=="50 or more Units high", ]$arch<-16 # Small MF Mid-rise

# Crawlspace SF
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories==1 &
          !bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-17

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-18

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories==1 &
          !bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-19

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-20

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories>1 &
          !bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-21

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-22

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-23
# this one needs redefined, represent with arch 23 for now, small SF with multiple stories and garage
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Crawl" & bs_base$Geometry.Stories>1 &
          !bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-23


# Pier and Beam SF
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Pier and Beam" & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-24

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Pier and Beam" & bs_base$Geometry.Stories==1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-25

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Pier and Beam" & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & !bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-26

bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_base$Geometry.Foundation.Type=="Pier and Beam" & bs_base$Geometry.Stories>1 &
          bs_base$Geometry.Garage=="None" & bs_base$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-27

# MH 
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Mobile Home") & !bs_base$Geometry.Floor.Area.Bin == "0-1499", ]$arch<-28
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Mobile Home") & bs_base$Geometry.Floor.Area.Bin == "0-1499", ]$arch<-29

# MF high-rise
bs_base[bs_base$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units") & bs_base$Geometry.Building.Number.Units.HL=="50 or more Units high", ]$arch<-30

mgi_all$Division<-"New England"
mgi_all[mgi_all$Div=="MA",]$Division<-"Middle Atlantic"
mgi_all[mgi_all$Div=="ENC",]$Division<-"East North Central"
mgi_all[mgi_all$Div=="WNC",]$Division<-"West North Central"
mgi_all[mgi_all$Div=="SA",]$Division<-"South Atlantic"
mgi_all[mgi_all$Div=="ESC",]$Division<-"East South Central"
mgi_all[mgi_all$Div=="WSC",]$Division<-"West South Central"
mgi_all[mgi_all$Div=="MT",]$Division<-"Mountain"
mgi_all[mgi_all$Div=="PAC",]$Division<-"Pacific"

mgi_all$arch_div<-paste(mgi_all$arch,mgi_all$Division,sep="_")

bs_base$arch_div<-paste(bs_base$arch,bs_base$Census.Division,sep="_")

mgi_all_comb<-mgi_all[,c(45,7:42)]

bs_base0<-merge(bs_base,mgi_all_comb,by="arch_div")

bs_base0$Type3<-"MF"
bs_base0[bs_base0$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached"),]$Type3<-"SF"
bs_base0[bs_base0$Geometry.Building.Type.RECS == "Mobile Home",]$Type3<-"MH"

# now calculate mateial and ghg intensities 
# do for all materials with a function 
ts_table<-as.data.frame(table(bs_base0$Type3,bs_base0$State))
names(ts_table)<-c("Type3","State","count")

tc_table<-as.data.frame(table(bs_base0$Type3,bs_base0$County))
names(tc_table)<-c("Type3","County","count")

tcs<-tc_table[tc_table$count<4,]

int<-function(mg,mat,yr,bs) {
  bsf<-bs
  ym<-paste(mg,yr,'_',mat,sep = '')
  gc<-tapply(bsf[,ym],list(bsf$Type3,bsf$County),mean)
  
  # ghg int by division
  gd<-round(as.data.frame(tapply(bsf[,ym],list(bsf$Census.Division,bsf$Type3),mean)),2)
  gd$Census.Division<-rownames(gd)
  rownames(gd)<-1:nrow(gd)
  gd<-gd[,c(4,1:3)]
  
  # ghg int by state
  gs<-round(as.data.frame(tapply(bsf[,ym],list(bsf$State,bsf$Type3),mean)),2)
  gs$State<-rownames(gs)
  rownames(gs)<-1:nrow(gs)
  gs<-gs[,c(4,1:3)]
  gs<-merge(gs,StDiv)
  
  # fill in state blanks based on division average
  for (k in 2:4) { 
    for (r in 1:49) {
      if (is.na(gs[r,k])) {
        gs[r,k]<-gd[gd$Census.Division==gs[r,"Census.Division"],k]
      } 
    }
  }
  
  # ghg int by county
  gc<-round(as.data.frame(tapply(bsf[,ym],list(bsf$County,bsf$Type3),mean)),2)
  gc$County<-rownames(gc)
  rownames(gc)<-1:nrow(gc)
  gc<-gc[,c(4,1:3)]
  gc<-merge(gc,ctycode,by.x="County",by.y="RS_ID",all=TRUE)
  gc<-gc[order(gc$GeoID),]
  gc<-gc[-c(which(substr(gc$GeoID,1,2) %in% c("02","15"))),]
  gc$State<-substr(gc$County,1,2)
  rownames(gc)<-1:3108
  # if mean value based on excessively small number of data points, change to NA
  for (j in 1:nrow(tcs)) {
    gc[gc$County==tcs$County[j],as.character(tcs$Type3[j])]<-NA
  }
  # fill in county blanks based on state average
  for (k in 2:4) { 
    for (r in 1:3108) {
      if (is.na(gc[r,k])) {
        gc[r,k]<-gs[gs$State==gc[r,"State"],k]
      } 
    }
  }
  gc$Material<-mat
  if(is.numeric(yr)) { gc$Year<-2000+yr}
  gc
}

g20_tc_Cem<-int('gi','Cement',20,bs_base0)
g60_tc_Cem<-int('gi','Cement',60,bs_base0)

g20_tc_Con<-int('gi','Concrete',20,bs_base0)
g60_tc_Con<-int('gi','Concrete',60,bs_base0)

g20_tc_Stl<-int('gi','Steel',20,bs_base0)
g60_tc_Stl<-int('gi','Steel',60,bs_base0)

g20_tc_Gls<-int('gi','Glass',20,bs_base0)
g60_tc_Gls<-int('gi','Glass',60,bs_base0)

g20_tc_Gyp<-int('gi','Gypsum',20,bs_base0)
g60_tc_Gyp<-int('gi','Gypsum',60,bs_base0)

g20_tc_Ins<-int('gi','Insulation',20,bs_base0)
g60_tc_Ins<-int('gi','Insulation',60,bs_base0)

g20_tc_Wod<-int('gi','Wood',20,bs_base0)
g60_tc_Wod<-int('gi','Wood',60,bs_base0)

g20_tc_Fbg<-int('gi','Fibreglass',20,bs_base0)
g60_tc_Fbg<-int('gi','Fibreglass',60,bs_base0)

g20_tc_SnA<-int('gi','Sand/Aggregate',20,bs_base0)
g60_tc_SnA<-int('gi','Sand/Aggregate',60,bs_base0)

g20_tc_Oth<-int('gi','Other',20,bs_base0)
g60_tc_Oth<-int('gi','Other',60,bs_base0)

g20_tc_TnC<-int('gi','Transport/Construction',20,bs_base0)
g60_tc_TnC<-int('gi','Transport/Construction',60,bs_base0)

g20_tc_Tot<-int('gi','Tot',20,bs_base0)
g60_tc_Tot<-int('gi','Tot',60,bs_base0)

m_tc_Cem<-int('mi','Cement','',bs_base0)
m_tc_Con<-int('mi','Concrete','',bs_base0)
m_tc_Stl<-int('mi','Steel','',bs_base0)
m_tc_Gls<-int('mi','Glass','',bs_base0)
m_tc_Wod<-int('mi','Wood','',bs_base0)
m_tc_Gyp<-int('mi','Gypsum','',bs_base0)
m_tc_Ins<-int('mi','Insulation','',bs_base0)
m_tc_Fbg<-int('mi','Fibreglass','',bs_base0)
m_tc_SnA<-int('mi','Sand/Aggregate','',bs_base0)
m_tc_Oth<-int('mi','Other','',bs_base0)
m_tc_Tot<-int('mi','Tot','',bs_base0)

# GHG intensities for baseline reudced floor area ########
bs_baseRFA$arch<-0
# define all archetype groups
# slab SF
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories==1 &
          !bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-1

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-2

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories==1 &
          !bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-3

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-4

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories>1 &
          !bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-5

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-6

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-7
# this one needs redefined, represent with arch 7 for now, small SF with multiple stories and garage
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Slab" & bs_baseRFA$Geometry.Stories>1 &
          !bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-7
# Basement SF
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories==1 &
          !bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-8

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-9

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories==1 &
          !bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-10

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-11

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories>1 &
          !bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-12

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-13

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-14
# this one needs redefined, represent with arch 14 for now, small SF with multiple stories and garage
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type %in% c("Heated Basement","Unheated Basement") & bs_baseRFA$Geometry.Stories>1 &
          !bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-14
# MF Mid-Rise
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units") & !bs_baseRFA$Geometry.Floor.Area.Bin == "0-1499" &
          !bs_baseRFA$Geometry.Building.Number.Units.HL=="50 or more Units high", ]$arch<-15 # Large MF Mid-rise
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units") & bs_baseRFA$Geometry.Floor.Area.Bin == "0-1499" &
          !bs_baseRFA$Geometry.Building.Number.Units.HL=="50 or more Units high", ]$arch<-16 # Small MF Mid-rise

# Crawlspace SF
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories==1 &
          !bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-17

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-18

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories==1 &
          !bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-19

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-20

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories>1 &
          !bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-21

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-22

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-23
# this one needs redefined, represent with arch 23 for now, small SF with multiple stories and garage
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Crawl" & bs_baseRFA$Geometry.Stories>1 &
          !bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-23


# Pier and Beam SF
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Pier and Beam" & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-24

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Pier and Beam" & bs_baseRFA$Geometry.Stories==1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-25

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Pier and Beam" & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & !bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-26

bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached") & bs_baseRFA$Geometry.Foundation.Type=="Pier and Beam" & bs_baseRFA$Geometry.Stories>1 &
          bs_baseRFA$Geometry.Garage=="None" & bs_baseRFA$Geometry.Floor.Area %in% c("0-499","500-749","750-999","1000-1499","1500-1999"), ]$arch<-27

# MH
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Mobile Home") & !bs_baseRFA$Geometry.Floor.Area.Bin == "0-1499", ]$arch<-28
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Mobile Home") & bs_baseRFA$Geometry.Floor.Area.Bin == "0-1499", ]$arch<-29

# MF high-rise
bs_baseRFA[bs_baseRFA$Geometry.Building.Type.RECS %in% c("Multi-Family with 2 - 4 Units","Multi-Family with 5+ Units") & bs_baseRFA$Geometry.Building.Number.Units.HL=="50 or more Units high", ]$arch<-30

bs_baseRFA$arch_div<-paste(bs_baseRFA$arch,bs_baseRFA$Census.Division,sep="_")

bs_baseRFA0<-merge(bs_baseRFA,mgi_all_comb,by="arch_div")

bs_baseRFA0$Type3<-"MF"
bs_baseRFA0[bs_baseRFA0$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached"),]$Type3<-"SF"
bs_baseRFA0[bs_baseRFA0$Geometry.Building.Type.RECS == "Mobile Home",]$Type3<-"MH"
# 
ts_table<-as.data.frame(table(bs_baseRFA0$Type3,bs_baseRFA0$State))
names(ts_table)<-c("Type3","State","count")

tc_table<-as.data.frame(table(bs_baseRFA0$Type3,bs_baseRFA0$County))
names(tc_table)<-c("Type3","County","count")
tcs<-tc_table[tc_table$count<4,]

g20_tc_Cem_RFA<-int('gi','Cement',20,bs_baseRFA0)
g60_tc_Cem_RFA<-int('gi','Cement',60,bs_baseRFA0)

g20_tc_Con_RFA<-int('gi','Concrete',20,bs_baseRFA0)
g60_tc_Con_RFA<-int('gi','Concrete',60,bs_baseRFA0)

g20_tc_Stl_RFA<-int('gi','Steel',20,bs_baseRFA0)
g60_tc_Stl_RFA<-int('gi','Steel',60,bs_baseRFA0)

g20_tc_Gls_RFA<-int('gi','Glass',20,bs_baseRFA0)
g60_tc_Gls_RFA<-int('gi','Glass',60,bs_baseRFA0)

g20_tc_Gyp_RFA<-int('gi','Gypsum',20,bs_baseRFA0)
g60_tc_Gyp_RFA<-int('gi','Gypsum',60,bs_baseRFA0)

g20_tc_Ins_RFA<-int('gi','Insulation',20,bs_baseRFA0)
g60_tc_Ins_RFA<-int('gi','Insulation',60,bs_baseRFA0)

g20_tc_Wod_RFA<-int('gi','Wood',20,bs_baseRFA0)
g60_tc_Wod_RFA<-int('gi','Wood',60,bs_baseRFA0)

g20_tc_Fbg_RFA<-int('gi','Fibreglass',20,bs_baseRFA0)
g60_tc_Fbg_RFA<-int('gi','Fibreglass',60,bs_baseRFA0)

g20_tc_SnA_RFA<-int('gi','Sand/Aggregate',20,bs_baseRFA0)
g60_tc_SnA_RFA<-int('gi','Sand/Aggregate',60,bs_baseRFA0)

g20_tc_Oth_RFA<-int('gi','Other',20,bs_baseRFA0)
g60_tc_Oth_RFA<-int('gi','Other',60,bs_baseRFA0)

g20_tc_TnC_RFA<-int('gi','Transport/Construction',20,bs_baseRFA0)
g60_tc_TnC_RFA<-int('gi','Transport/Construction',60,bs_baseRFA0)

g20_tc_Tot_RFA<-int('gi','Tot',20,bs_baseRFA0)
g60_tc_Tot_RFA<-int('gi','Tot',60,bs_baseRFA0)

m_tc_Cem_RFA<-int('mi','Cement','',bs_baseRFA0)
m_tc_Con_RFA<-int('mi','Concrete','',bs_baseRFA0)
m_tc_Stl_RFA<-int('mi','Steel','',bs_baseRFA0)
m_tc_Gls_RFA<-int('mi','Glass','',bs_baseRFA0)
m_tc_Wod_RFA<-int('mi','Wood','',bs_baseRFA0)
m_tc_Gyp_RFA<-int('mi','Gypsum','',bs_baseRFA0)
m_tc_Ins_RFA<-int('mi','Insulation','',bs_baseRFA0)
m_tc_Fbg_RFA<-int('mi','Fibreglass','',bs_baseRFA0)
m_tc_SnA_RFA<-int('mi','Sand/Aggregate','',bs_baseRFA0)
m_tc_Oth_RFA<-int('mi','Other','',bs_baseRFA0)
m_tc_Tot_RFA<-int('mi','Tot','',bs_baseRFA0)

# calculate GHG flows #########
load("HSM_results/County_FloorArea.RData") # loads 5 smop_*_FA files, one for each scenario base, hiDR, hiDRMF, hiMF, RFA
load("../resstock_projections/ExtData/County_FloorArea.RData") # loads 6 smop*_FA files, one for each scenario base, hiDR, hi MF, each with and witout RFA 

# base, hiDR, hiMF, hiDRMF
for (i in 1:3108) { print(i) # this takes longer, about 2hr
  # first get GHG intensities 
  GHGI_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Tot$SF[i],g60_tc_Tot$SF[i]),n=41)[2]))
  GHGI_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Tot$MF[i],g60_tc_Tot$MF[i]),n=41)[2]))
  GHGI_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Tot$MH[i],g60_tc_Tot$MH[i]),n=41)[2]))
  
  GHGI_Cem_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Cem$SF[i],g60_tc_Cem$SF[i]),n=41)[2]))
  GHGI_Cem_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Cem$MF[i],g60_tc_Cem$MF[i]),n=41)[2]))
  GHGI_Cem_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Cem$MH[i],g60_tc_Cem$MH[i]),n=41)[2]))
  
  GHGI_Con_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Con$SF[i],g60_tc_Con$SF[i]),n=41)[2]))
  GHGI_Con_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Con$MF[i],g60_tc_Con$MF[i]),n=41)[2]))
  GHGI_Con_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Con$MH[i],g60_tc_Con$MH[i]),n=41)[2]))
  
  GHGI_Stl_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Stl$SF[i],g60_tc_Stl$SF[i]),n=41)[2]))
  GHGI_Stl_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Stl$MF[i],g60_tc_Stl$MF[i]),n=41)[2]))
  GHGI_Stl_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Stl$MH[i],g60_tc_Stl$MH[i]),n=41)[2]))
  
  GHGI_Gls_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gls$SF[i],g60_tc_Gls$SF[i]),n=41)[2]))
  GHGI_Gls_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gls$MF[i],g60_tc_Gls$MF[i]),n=41)[2]))
  GHGI_Gls_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gls$MH[i],g60_tc_Gls$MH[i]),n=41)[2]))
  
  GHGI_Gyp_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gyp$SF[i],g60_tc_Gyp$SF[i]),n=41)[2]))
  GHGI_Gyp_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gyp$MF[i],g60_tc_Gyp$MF[i]),n=41)[2]))
  GHGI_Gyp_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gyp$MH[i],g60_tc_Gyp$MH[i]),n=41)[2]))
  
  GHGI_Ins_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Ins$SF[i],g60_tc_Ins$SF[i]),n=41)[2]))
  GHGI_Ins_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Ins$MF[i],g60_tc_Ins$MF[i]),n=41)[2]))
  GHGI_Ins_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Ins$MH[i],g60_tc_Ins$MH[i]),n=41)[2]))
  
  GHGI_Wod_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Wod$SF[i],g60_tc_Wod$SF[i]),n=41)[2]))
  GHGI_Wod_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Wod$MF[i],g60_tc_Wod$MF[i]),n=41)[2]))
  GHGI_Wod_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Wod$MH[i],g60_tc_Wod$MH[i]),n=41)[2]))
  
  GHGI_Fbg_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Fbg$SF[i],g60_tc_Fbg$SF[i]),n=41)[2]))
  GHGI_Fbg_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Fbg$MF[i],g60_tc_Fbg$MF[i]),n=41)[2]))
  GHGI_Fbg_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Fbg$MH[i],g60_tc_Fbg$MH[i]),n=41)[2]))
  
  GHGI_SnA_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_SnA$SF[i],g60_tc_SnA$SF[i]),n=41)[2]))
  GHGI_SnA_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_SnA$MF[i],g60_tc_SnA$MF[i]),n=41)[2]))
  GHGI_SnA_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_SnA$MH[i],g60_tc_SnA$MH[i]),n=41)[2]))
  
  GHGI_Oth_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Oth$SF[i],g60_tc_Oth$SF[i]),n=41)[2]))
  GHGI_Oth_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Oth$MF[i],g60_tc_Oth$MF[i]),n=41)[2]))
  GHGI_Oth_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Oth$MH[i],g60_tc_Oth$MH[i]),n=41)[2]))
  
  GHGI_TnC_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_TnC$SF[i],g60_tc_TnC$SF[i]),n=41)[2]))
  GHGI_TnC_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_TnC$MF[i],g60_tc_TnC$MF[i]),n=41)[2]))
  GHGI_TnC_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_TnC$MH[i],g60_tc_TnC$MH[i]),n=41)[2]))
  
  # now calculate GHG absolute flows, total and per material for baseline
  smop_base_FA[[3]][[i]]$GHG_NC_SF<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_base_FA[[3]][[i]]$GHG_NC_MF<-smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_base_FA[[3]][[i]]$GHG_NC_MH<-smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_base_FA[[3]][[i]]$GHG_NC_Cem<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Con<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Stl<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Gls<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Gyp<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Ins<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Wod<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Fbg<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_base_FA[[3]][[i]]$GHG_NC_SnA<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_base_FA[[3]][[i]]$GHG_NC_Oth<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_base_FA[[3]][[i]]$GHG_NC_TnC<-smop_base_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_base_FA[[3]][[i]]$GHG_NC<-smop_base_FA[[3]][[i]]$GHG_NC_SF+smop_base_FA[[3]][[i]]$GHG_NC_MF+smop_base_FA[[3]][[i]]$GHG_NC_MH
  
  # now calculate GHG absolute flows, total and per material for hiDR
  smop_hiDR_FA[[3]][[i]]$GHG_NC_SF<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MF<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MH<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Cem<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Con<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Stl<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Gls<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Gyp<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Ins<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Wod<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Fbg<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_SnA<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_Oth<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_hiDR_FA[[3]][[i]]$GHG_NC_TnC<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_hiDR_FA[[3]][[i]]$GHG_NC<-smop_hiDR_FA[[3]][[i]]$GHG_NC_SF+smop_hiDR_FA[[3]][[i]]$GHG_NC_MF+smop_hiDR_FA[[3]][[i]]$GHG_NC_MH
  
  # now calculate GHG absolute flows, total and per material for hiMF
  smop_hiMF_FA[[3]][[i]]$GHG_NC_SF<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MF<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MH<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Cem<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Con<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Stl<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Gls<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Gyp<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Ins<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Wod<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Fbg<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_SnA<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_Oth<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_hiMF_FA[[3]][[i]]$GHG_NC_TnC<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_hiMF_FA[[3]][[i]]$GHG_NC<-smop_hiMF_FA[[3]][[i]]$GHG_NC_SF+smop_hiMF_FA[[3]][[i]]$GHG_NC_MF+smop_hiMF_FA[[3]][[i]]$GHG_NC_MH
  
  # now calculate GHG absolute flows, total and per material for hiDRMF
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_SF<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_MF<-smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_MH<-smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Cem<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Con<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Stl<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Gls<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Gyp<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Ins<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Wod<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Fbg<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_SnA<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_Oth<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC_TnC<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_hiDRMF_FA[[3]][[i]]$GHG_NC<-smop_hiDRMF_FA[[3]][[i]]$GHG_NC_SF+smop_hiDRMF_FA[[3]][[i]]$GHG_NC_MF+smop_hiDRMF_FA[[3]][[i]]$GHG_NC_MH
  
  # now get material intensities
  MI_SF<-m_tc_Tot$SF[i] 
  MI_MF<-m_tc_Tot$MF[i] 
  MI_MH<-m_tc_Tot$MH[i] 
  
  MI_Cem_SF<-m_tc_Cem$SF[i] 
  MI_Cem_MF<-m_tc_Cem$MF[i]
  MI_Cem_MH<-m_tc_Cem$MH[i]
  
  MI_Con_SF<-m_tc_Con$SF[i] 
  MI_Con_MF<-m_tc_Con$MF[i] 
  MI_Con_MH<-m_tc_Con$MH[i] 
  
  MI_Stl_SF<-m_tc_Stl$SF[i] 
  MI_Stl_MF<-m_tc_Stl$MF[i] 
  MI_Stl_MH<-m_tc_Stl$MH[i] 
  
  MI_Gls_SF<-m_tc_Gls$SF[i] 
  MI_Gls_MF<-m_tc_Gls$MF[i] 
  MI_Gls_MH<-m_tc_Gls$MH[i] 
  
  MI_Gyp_SF<-m_tc_Gyp$SF[i] 
  MI_Gyp_MF<-m_tc_Gyp$MF[i] 
  MI_Gyp_MH<-m_tc_Gyp$MH[i] 
  
  MI_Ins_SF<-m_tc_Ins$SF[i] 
  MI_Ins_MF<-m_tc_Ins$MF[i] 
  MI_Ins_MH<-m_tc_Ins$MH[i] 
  
  MI_Wod_SF<-m_tc_Wod$SF[i] 
  MI_Wod_MF<-m_tc_Wod$MF[i] 
  MI_Wod_MH<-m_tc_Wod$MH[i] 
  
  MI_Fbg_SF<-m_tc_Fbg$SF[i] 
  MI_Fbg_MF<-m_tc_Fbg$MF[i] 
  MI_Fbg_MH<-m_tc_Fbg$MH[i] 
  
  MI_SnA_SF<-m_tc_SnA$SF[i] 
  MI_SnA_MF<-m_tc_SnA$MF[i] 
  MI_SnA_MH<-m_tc_SnA$MH[i] 
  
  MI_Oth_SF<-m_tc_Oth$SF[i] 
  MI_Oth_MF<-m_tc_Oth$MF[i] 
  MI_Oth_MH<-m_tc_Oth$MH[i] 
  
  #inflows first
  # now calculate material absolute flows, total and per material for base
  smop_base_FA[[3]][[i]]$M_NC_SF<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_base_FA[[3]][[i]]$M_NC_MF<-smop_base_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_base_FA[[3]][[i]]$M_NC_MH<-smop_base_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_base_FA[[3]][[i]]$M_NC_Cem<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_base_FA[[3]][[i]]$M_NC_Con<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_base_FA[[3]][[i]]$M_NC_Stl<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_base_FA[[3]][[i]]$M_NC_Gls<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_base_FA[[3]][[i]]$M_NC_Gyp<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_base_FA[[3]][[i]]$M_NC_Ins<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_base_FA[[3]][[i]]$M_NC_Wod<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_base_FA[[3]][[i]]$M_NC_Fbg<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_base_FA[[3]][[i]]$M_NC_SnA<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_base_FA[[3]][[i]]$M_NC_Oth<-smop_base_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_base_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_base_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_base_FA[[3]][[i]]$M_NC<-smop_base_FA[[3]][[i]]$M_NC_SF+smop_base_FA[[3]][[i]]$M_NC_MF+smop_base_FA[[3]][[i]]$M_NC_MH
  
  # now calculate material absolute flows, total and per material for hiDR
  smop_hiDR_FA[[3]][[i]]$M_NC_SF<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_hiDR_FA[[3]][[i]]$M_NC_MF<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_hiDR_FA[[3]][[i]]$M_NC_MH<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_hiDR_FA[[3]][[i]]$M_NC_Cem<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Con<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Stl<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Gls<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Gyp<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Ins<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Wod<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Fbg<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_SnA<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_hiDR_FA[[3]][[i]]$M_NC_Oth<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_hiDR_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_hiDR_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_hiDR_FA[[3]][[i]]$M_NC<-smop_hiDR_FA[[3]][[i]]$M_NC_SF+smop_hiDR_FA[[3]][[i]]$M_NC_MF+smop_hiDR_FA[[3]][[i]]$M_NC_MH
  
  # now calculate material absolute flows, total and per material for hiMF
  smop_hiMF_FA[[3]][[i]]$M_NC_SF<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_hiMF_FA[[3]][[i]]$M_NC_MF<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_hiMF_FA[[3]][[i]]$M_NC_MH<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_hiMF_FA[[3]][[i]]$M_NC_Cem<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Con<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Stl<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Gls<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Gyp<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Ins<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Wod<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Fbg<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_SnA<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_hiMF_FA[[3]][[i]]$M_NC_Oth<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_hiMF_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_hiMF_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_hiMF_FA[[3]][[i]]$M_NC<-smop_hiMF_FA[[3]][[i]]$M_NC_SF+smop_hiMF_FA[[3]][[i]]$M_NC_MF+smop_hiMF_FA[[3]][[i]]$M_NC_MH
  
  # now calculate material absolute flows, total and per material for hiDRMF
  smop_hiDRMF_FA[[3]][[i]]$M_NC_SF<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_hiDRMF_FA[[3]][[i]]$M_NC_MF<-smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_hiDRMF_FA[[3]][[i]]$M_NC_MH<-smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Cem<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Con<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Stl<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Gls<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Gyp<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Ins<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Wod<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Fbg<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_SnA<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_hiDRMF_FA[[3]][[i]]$M_NC_Oth<-smop_hiDRMF_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_hiDRMF_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_hiDRMF_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_hiDRMF_FA[[3]][[i]]$M_NC<-smop_hiDRMF_FA[[3]][[i]]$M_NC_SF+smop_hiDRMF_FA[[3]][[i]]$M_NC_MF+smop_hiDRMF_FA[[3]][[i]]$M_NC_MH
  
  # now material outflows
  smop_base_FA[[3]][[i]]$M_Dem_SF<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_base_FA[[3]][[i]]$M_Dem_MF<-smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_base_FA[[3]][[i]]$M_Dem_MH<-smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_base_FA[[3]][[i]]$M_Dem_Cem<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_base_FA[[3]][[i]]$M_Dem_Con<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_base_FA[[3]][[i]]$M_Dem_Stl<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_base_FA[[3]][[i]]$M_Dem_Gls<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_base_FA[[3]][[i]]$M_Dem_Gyp<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_base_FA[[3]][[i]]$M_Dem_Ins<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_base_FA[[3]][[i]]$M_Dem_Wod<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_base_FA[[3]][[i]]$M_Dem_Fbg<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_base_FA[[3]][[i]]$M_Dem_SnA<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_base_FA[[3]][[i]]$M_Dem_Oth<-smop_base_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_base_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_base_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_base_FA[[3]][[i]]$M_Dem<-smop_base_FA[[3]][[i]]$M_Dem_SF+smop_base_FA[[3]][[i]]$M_Dem_MF+smop_base_FA[[3]][[i]]$M_Dem_MH
  
  smop_base_FA[[3]][[i]]$M_Rat_Con<-smop_base_FA[[3]][[i]]$M_Dem_Con/smop_base_FA[[3]][[i]]$M_NC_Con
  smop_base_FA[[3]][[i]]$M_Rat_Wod<-smop_base_FA[[3]][[i]]$M_Dem_Wod/smop_base_FA[[3]][[i]]$M_NC_Wod
  smop_base_FA[[3]][[i]]$M_Rat_Stl<-smop_base_FA[[3]][[i]]$M_Dem_Stl/smop_base_FA[[3]][[i]]$M_NC_Stl
  smop_base_FA[[3]][[i]]$M_Rat_Tot<-smop_base_FA[[3]][[i]]$M_Dem/smop_base_FA[[3]][[i]]$M_NC
  
  # now calculate material absolute OUTflows, total and per material for hiDR
  smop_hiDR_FA[[3]][[i]]$M_Dem_SF<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_hiDR_FA[[3]][[i]]$M_Dem_MF<-smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_hiDR_FA[[3]][[i]]$M_Dem_MH<-smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_hiDR_FA[[3]][[i]]$M_Dem_Cem<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Con<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Stl<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Gls<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Gyp<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Ins<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Wod<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Fbg<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_SnA<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_hiDR_FA[[3]][[i]]$M_Dem_Oth<-smop_hiDR_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_hiDR_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_hiDR_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_hiDR_FA[[3]][[i]]$M_Dem<-smop_hiDR_FA[[3]][[i]]$M_Dem_SF+smop_hiDR_FA[[3]][[i]]$M_Dem_MF+smop_hiDR_FA[[3]][[i]]$M_Dem_MH
  
  smop_hiDR_FA[[3]][[i]]$M_Rat_Con<-smop_hiDR_FA[[3]][[i]]$M_Dem_Con/smop_hiDR_FA[[3]][[i]]$M_NC_Con
  smop_hiDR_FA[[3]][[i]]$M_Rat_Wod<-smop_hiDR_FA[[3]][[i]]$M_Dem_Wod/smop_hiDR_FA[[3]][[i]]$M_NC_Wod
  smop_hiDR_FA[[3]][[i]]$M_Rat_Stl<-smop_hiDR_FA[[3]][[i]]$M_Dem_Stl/smop_hiDR_FA[[3]][[i]]$M_NC_Stl
  smop_hiDR_FA[[3]][[i]]$M_Rat_Tot<-smop_hiDR_FA[[3]][[i]]$M_Dem/smop_hiDR_FA[[3]][[i]]$M_NC
  
  # now calculate material absolute flows, total and per material for hiMF
  smop_hiMF_FA[[3]][[i]]$M_Dem_SF<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_hiMF_FA[[3]][[i]]$M_Dem_MF<-smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_hiMF_FA[[3]][[i]]$M_Dem_MH<-smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_hiMF_FA[[3]][[i]]$M_Dem_Cem<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Con<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Stl<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Gls<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Gyp<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Ins<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Wod<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Fbg<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_SnA<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_hiMF_FA[[3]][[i]]$M_Dem_Oth<-smop_hiMF_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_hiMF_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_hiMF_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_hiMF_FA[[3]][[i]]$M_Dem<-smop_hiMF_FA[[3]][[i]]$M_Dem_SF+smop_hiMF_FA[[3]][[i]]$M_Dem_MF+smop_hiMF_FA[[3]][[i]]$M_Dem_MH
  
  smop_hiMF_FA[[3]][[i]]$M_Rat_Con<-smop_hiMF_FA[[3]][[i]]$M_Dem_Con/smop_hiMF_FA[[3]][[i]]$M_NC_Con
  smop_hiMF_FA[[3]][[i]]$M_Rat_Wod<-smop_hiMF_FA[[3]][[i]]$M_Dem_Wod/smop_hiMF_FA[[3]][[i]]$M_NC_Wod
  smop_hiMF_FA[[3]][[i]]$M_Rat_Stl<-smop_hiMF_FA[[3]][[i]]$M_Dem_Stl/smop_hiMF_FA[[3]][[i]]$M_NC_Stl
  smop_hiMF_FA[[3]][[i]]$M_Rat_Tot<-smop_hiMF_FA[[3]][[i]]$M_Dem/smop_hiMF_FA[[3]][[i]]$M_NC
  
  # now calculate material absolute flows, total and per material for hiDRMF
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_SF<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_MF<-smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_MH<-smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Cem<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Con<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Stl<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Gls<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Gyp<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Ins<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Wod<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Fbg<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_SnA<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_hiDRMF_FA[[3]][[i]]$M_Dem_Oth<-smop_hiDRMF_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_hiDRMF_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_hiDRMF_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_hiDRMF_FA[[3]][[i]]$M_Dem<-smop_hiDRMF_FA[[3]][[i]]$M_Dem_SF+smop_hiDRMF_FA[[3]][[i]]$M_Dem_MF+smop_hiDRMF_FA[[3]][[i]]$M_Dem_MH
  
  smop_hiDRMF_FA[[3]][[i]]$M_Rat_Con<-smop_hiDRMF_FA[[3]][[i]]$M_Dem_Con/smop_hiDRMF_FA[[3]][[i]]$M_NC_Con
  smop_hiDRMF_FA[[3]][[i]]$M_Rat_Wod<-smop_hiDRMF_FA[[3]][[i]]$M_Dem_Wod/smop_hiDRMF_FA[[3]][[i]]$M_NC_Wod
  smop_hiDRMF_FA[[3]][[i]]$M_Rat_Stl<-smop_hiDRMF_FA[[3]][[i]]$M_Dem_Stl/smop_hiDRMF_FA[[3]][[i]]$M_NC_Stl
  smop_hiDRMF_FA[[3]][[i]]$M_Rat_Tot<-smop_hiDRMF_FA[[3]][[i]]$M_Dem/smop_hiDRMF_FA[[3]][[i]]$M_NC
}

# extract template for making US summary results, for each scenario ########## 
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type,
# material flows (in and out) by material type and and construction related GHG by material type
# m2/cap, emissions and materials in new construction by material, by tpye, and total
us_base_FA<-as.data.frame(smop_base_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial base dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_base_FA[,c(1,3:31,33:99)]<-0
# nc_base<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
  us_base_FA[,add]<-us_base_FA[,add]+smop_base_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_base_FA[,4:6]<-us_base_FA[,7:9]/us_base_FA$Population
# calculate HHS
us_base_FA[,10:12]<-us_base_FA[,7:9]/us_base_FA[,15:17]
us_base_FA$HH_Size<-us_base_FA$Population/us_base_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_base_FA[,18:21]<-us_base_FA[,22:25]/us_base_FA[,14:17]
# calculate m2/cap by type
us_base_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_base_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_base_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate the ratios
us_base_FA[,96:99]<-us_base_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_base_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

# same for hiDR
us_hiDR_FA<-as.data.frame(smop_hiDR_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial hiDR dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_hiDR_FA[,c(1,3:31,33:99)]<-0
# nc_hiDR<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
  us_hiDR_FA[,add]<-us_hiDR_FA[,add]+smop_hiDR_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_hiDR_FA[,4:6]<-us_hiDR_FA[,7:9]/us_hiDR_FA$Population
# calculate HHS
us_hiDR_FA[,10:12]<-us_hiDR_FA[,7:9]/us_hiDR_FA[,15:17]
us_hiDR_FA$HH_Size<-us_hiDR_FA$Population/us_hiDR_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDR_FA[,18:21]<-us_hiDR_FA[,22:25]/us_hiDR_FA[,14:17]
# calculate m2/cap by type
us_hiDR_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiDR_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiDR_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate the ratios
us_hiDR_FA[,96:99]<-us_hiDR_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_hiDR_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

# same for hiMF
us_hiMF_FA<-as.data.frame(smop_hiMF_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial hiMF dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_hiMF_FA[,c(1,3:31,33:99)]<-0
# nc_hiMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
  us_hiMF_FA[,add]<-us_hiMF_FA[,add]+smop_hiMF_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_hiMF_FA[,4:6]<-us_hiMF_FA[,7:9]/us_hiMF_FA$Population
# calculate HHS
us_hiMF_FA[,10:12]<-us_hiMF_FA[,7:9]/us_hiMF_FA[,15:17]
us_hiMF_FA$HH_Size<-us_hiMF_FA$Population/us_hiMF_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiMF_FA[,18:21]<-us_hiMF_FA[,22:25]/us_hiMF_FA[,14:17]
# calculate m2/cap by type
us_hiMF_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiMF_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiMF_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate the ratios
us_hiMF_FA[,96:99]<-us_hiMF_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_hiMF_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

# same for hiDRMF
us_hiDRMF_FA<-as.data.frame(smop_hiDRMF_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial hiDRMF dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_hiDRMF_FA[,c(1,3:31,33:99)]<-0
# nc_hiDRMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
  us_hiDRMF_FA[,add]<-us_hiDRMF_FA[,add]+smop_hiDRMF_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_hiDRMF_FA[,4:6]<-us_hiDRMF_FA[,7:9]/us_hiDRMF_FA$Population
# calculate HHS
us_hiDRMF_FA[,10:12]<-us_hiDRMF_FA[,7:9]/us_hiDRMF_FA[,15:17]
us_hiDRMF_FA$HH_Size<-us_hiDRMF_FA$Population/us_hiDRMF_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDRMF_FA[,18:21]<-us_hiDRMF_FA[,22:25]/us_hiDRMF_FA[,14:17]
# calculate m2/cap by type
us_hiDRMF_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiDRMF_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiDRMF_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate the ratios
us_hiDRMF_FA[,96:99]<-us_hiDRMF_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_hiDRMF_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

# now calculate the embodied material and GHG flows for the reduced floor area scenarios
for (i in 1:3108) { print(i) # this takes a while now, probably 2 hours
  # first get GHG intensities 
  GHGI_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Tot$SF[i],g60_tc_Tot$SF[i]),n=41)[2]))
  GHGI_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Tot$MF[i],g60_tc_Tot$MF[i]),n=41)[2]))
  GHGI_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Tot$MH[i],g60_tc_Tot$MH[i]),n=41)[2]))
  
  GHGI_Cem_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Cem$SF[i],g60_tc_Cem$SF[i]),n=41)[2]))
  GHGI_Cem_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Cem$MF[i],g60_tc_Cem$MF[i]),n=41)[2]))
  GHGI_Cem_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Cem$MH[i],g60_tc_Cem$MH[i]),n=41)[2]))
  
  GHGI_Con_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Con$SF[i],g60_tc_Con$SF[i]),n=41)[2]))
  GHGI_Con_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Con$MF[i],g60_tc_Con$MF[i]),n=41)[2]))
  GHGI_Con_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Con$MH[i],g60_tc_Con$MH[i]),n=41)[2]))
  
  GHGI_Stl_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Stl$SF[i],g60_tc_Stl$SF[i]),n=41)[2]))
  GHGI_Stl_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Stl$MF[i],g60_tc_Stl$MF[i]),n=41)[2]))
  GHGI_Stl_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Stl$MH[i],g60_tc_Stl$MH[i]),n=41)[2]))
  
  GHGI_Gls_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gls$SF[i],g60_tc_Gls$SF[i]),n=41)[2]))
  GHGI_Gls_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gls$MF[i],g60_tc_Gls$MF[i]),n=41)[2]))
  GHGI_Gls_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gls$MH[i],g60_tc_Gls$MH[i]),n=41)[2]))
  
  GHGI_Gyp_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gyp$SF[i],g60_tc_Gyp$SF[i]),n=41)[2]))
  GHGI_Gyp_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gyp$MF[i],g60_tc_Gyp$MF[i]),n=41)[2]))
  GHGI_Gyp_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Gyp$MH[i],g60_tc_Gyp$MH[i]),n=41)[2]))
  
  GHGI_Ins_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Ins$SF[i],g60_tc_Ins$SF[i]),n=41)[2]))
  GHGI_Ins_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Ins$MF[i],g60_tc_Ins$MF[i]),n=41)[2]))
  GHGI_Ins_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Ins$MH[i],g60_tc_Ins$MH[i]),n=41)[2]))
  
  GHGI_Wod_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Wod$SF[i],g60_tc_Wod$SF[i]),n=41)[2]))
  GHGI_Wod_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Wod$MF[i],g60_tc_Wod$MF[i]),n=41)[2]))
  GHGI_Wod_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Wod$MH[i],g60_tc_Wod$MH[i]),n=41)[2]))
  
  GHGI_Fbg_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Fbg$SF[i],g60_tc_Fbg$SF[i]),n=41)[2]))
  GHGI_Fbg_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Fbg$MF[i],g60_tc_Fbg$MF[i]),n=41)[2]))
  GHGI_Fbg_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Fbg$MH[i],g60_tc_Fbg$MH[i]),n=41)[2]))
  
  GHGI_SnA_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_SnA$SF[i],g60_tc_SnA$SF[i]),n=41)[2]))
  GHGI_SnA_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_SnA$MF[i],g60_tc_SnA$MF[i]),n=41)[2]))
  GHGI_SnA_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_SnA$MH[i],g60_tc_SnA$MH[i]),n=41)[2]))
  
  GHGI_Oth_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Oth$SF[i],g60_tc_Oth$SF[i]),n=41)[2]))
  GHGI_Oth_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Oth$MF[i],g60_tc_Oth$MF[i]),n=41)[2]))
  GHGI_Oth_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_Oth$MH[i],g60_tc_Oth$MH[i]),n=41)[2]))
  
  GHGI_TnC_SF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_TnC$SF[i],g60_tc_TnC$SF[i]),n=41)[2]))
  GHGI_TnC_MF<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_TnC$MF[i],g60_tc_TnC$MF[i]),n=41)[2]))
  GHGI_TnC_MH<-as.numeric(unlist(approx(c(2020,2060),c(g20_tc_TnC$MH[i],g60_tc_TnC$MH[i]),n=41)[2]))
  
  # now calculate GHG absolute flows, total and per material for the baseline RFA scenario
  smop_RFA_FA[[3]][[i]]$GHG_NC_SF<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_RFA_FA[[3]][[i]]$GHG_NC_MF<-smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_RFA_FA[[3]][[i]]$GHG_NC_MH<-smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_RFA_FA[[3]][[i]]$GHG_NC_Cem<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Con<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Stl<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Gls<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Gyp<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Ins<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Wod<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Fbg<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_SnA<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_Oth<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_RFA_FA[[3]][[i]]$GHG_NC_TnC<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_RFA_FA[[3]][[i]]$GHG_NC<-smop_RFA_FA[[3]][[i]]$GHG_NC_SF+smop_RFA_FA[[3]][[i]]$GHG_NC_MF+smop_RFA_FA[[3]][[i]]$GHG_NC_MH
  
  # now calculate GHG absolute flows, total and per material for the hiDR_RFA scenario
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Cem<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Con<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Stl<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Gls<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Gyp<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Ins<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Wod<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Fbg<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SnA<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_Oth<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_TnC<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC<-smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF+smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF+smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH
  
  # now calculate GHG absolute flows, total and per material for the hiMF_RFA scenario
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_SF
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_MF
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_MH
  
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Cem<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Cem_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Cem_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Cem_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Con<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Con_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Con_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Con_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Stl<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Stl_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Stl_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Stl_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Gls<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Gls_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Gls_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Gls_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Gyp<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Gyp_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Gyp_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Gyp_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Ins<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Ins_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Ins_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Ins_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Wod<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Wod_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Wod_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Wod_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Fbg<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Fbg_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Fbg_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Fbg_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SnA<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_SnA_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_SnA_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_SnA_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_Oth<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_Oth_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_Oth_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_Oth_MH
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_TnC<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*GHGI_TnC_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*GHGI_TnC_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*GHGI_TnC_MH
  # total emissions from new construction
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC<-smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF+smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF+smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH
  
  # now get material intensities
  MI_SF<-m_tc_Tot$SF[i] 
  MI_MF<-m_tc_Tot$MF[i] 
  MI_MH<-m_tc_Tot$MH[i] 
  
  MI_Cem_SF<-m_tc_Cem$SF[i] 
  MI_Cem_MF<-m_tc_Cem$MF[i]
  MI_Cem_MH<-m_tc_Cem$MH[i]
  
  MI_Con_SF<-m_tc_Con$SF[i] 
  MI_Con_MF<-m_tc_Con$MF[i] 
  MI_Con_MH<-m_tc_Con$MH[i] 
  
  MI_Stl_SF<-m_tc_Stl$SF[i] 
  MI_Stl_MF<-m_tc_Stl$MF[i] 
  MI_Stl_MH<-m_tc_Stl$MH[i] 
  
  MI_Gls_SF<-m_tc_Gls$SF[i] 
  MI_Gls_MF<-m_tc_Gls$MF[i] 
  MI_Gls_MH<-m_tc_Gls$MH[i] 
  
  MI_Gyp_SF<-m_tc_Gyp$SF[i] 
  MI_Gyp_MF<-m_tc_Gyp$MF[i] 
  MI_Gyp_MH<-m_tc_Gyp$MH[i] 
  
  MI_Ins_SF<-m_tc_Ins$SF[i] 
  MI_Ins_MF<-m_tc_Ins$MF[i] 
  MI_Ins_MH<-m_tc_Ins$MH[i] 
  
  MI_Wod_SF<-m_tc_Wod$SF[i] 
  MI_Wod_MF<-m_tc_Wod$MF[i] 
  MI_Wod_MH<-m_tc_Wod$MH[i] 
  
  MI_Fbg_SF<-m_tc_Fbg$SF[i] 
  MI_Fbg_MF<-m_tc_Fbg$MF[i] 
  MI_Fbg_MH<-m_tc_Fbg$MH[i] 
  
  MI_SnA_SF<-m_tc_SnA$SF[i] 
  MI_SnA_MF<-m_tc_SnA$MF[i] 
  MI_SnA_MH<-m_tc_SnA$MH[i] 
  
  MI_Oth_SF<-m_tc_Oth$SF[i] 
  MI_Oth_MF<-m_tc_Oth$MF[i] 
  MI_Oth_MH<-m_tc_Oth$MH[i] 
  
  # Total material flows in the baseline RFA scenario
  smop_RFA_FA[[3]][[i]]$M_NC_SF<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_RFA_FA[[3]][[i]]$M_NC_MF<-smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_RFA_FA[[3]][[i]]$M_NC_MH<-smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_RFA_FA[[3]][[i]]$M_NC_Cem<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Con<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Stl<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Gls<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Gyp<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Ins<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Wod<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Fbg<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_RFA_FA[[3]][[i]]$M_NC_SnA<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_RFA_FA[[3]][[i]]$M_NC_Oth<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_RFA_FA[[3]][[i]]$M_NC<-smop_RFA_FA[[3]][[i]]$M_NC_SF+smop_RFA_FA[[3]][[i]]$M_NC_MF+smop_RFA_FA[[3]][[i]]$M_NC_MH
  
  # Total material flows in the hiDR_RFA scenario
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_SF<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_MF<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_MH<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Cem<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Con<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Stl<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Gls<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Gyp<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Ins<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Wod<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Fbg<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_SnA<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Oth<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_hiDR_RFA_FA[[3]][[i]]$M_NC<-smop_hiDR_RFA_FA[[3]][[i]]$M_NC_SF+smop_hiDR_RFA_FA[[3]][[i]]$M_NC_MF+smop_hiDR_RFA_FA[[3]][[i]]$M_NC_MH
  
  # Total material flows in the hiMF_RFA scenario
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_SF<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_SF
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_MF<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_MF
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_MH<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_MH
  
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Cem<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Cem_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Cem_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Cem_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Con<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Con_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Con_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Con_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Stl<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Stl_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Stl_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Stl_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Gls<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Gls_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Gls_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Gls_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Gyp<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Gyp_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Gyp_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Gyp_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Ins<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Ins_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Ins_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Ins_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Wod<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Wod_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Wod_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Wod_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Fbg<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Fbg_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Fbg_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Fbg_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_SnA<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_SnA_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_SnA_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_SnA_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Oth<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*MI_Oth_SF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*MI_Oth_MF+smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*MI_Oth_MH
  
  smop_hiMF_RFA_FA[[3]][[i]]$M_NC<-smop_hiMF_RFA_FA[[3]][[i]]$M_NC_SF+smop_hiMF_RFA_FA[[3]][[i]]$M_NC_MF+smop_hiMF_RFA_FA[[3]][[i]]$M_NC_MH
  
  # now material outflows, first baseline RFA, we use the base material intensities here, not the RFA ones
  # get material intensities
  MI_SF<-m_tc_Tot$SF[i] 
  MI_MF<-m_tc_Tot$MF[i] 
  MI_MH<-m_tc_Tot$MH[i] 
  
  MI_Cem_SF<-m_tc_Cem$SF[i] 
  MI_Cem_MF<-m_tc_Cem$MF[i]
  MI_Cem_MH<-m_tc_Cem$MH[i]
  
  MI_Con_SF<-m_tc_Con$SF[i] 
  MI_Con_MF<-m_tc_Con$MF[i] 
  MI_Con_MH<-m_tc_Con$MH[i] 
  
  MI_Stl_SF<-m_tc_Stl$SF[i] 
  MI_Stl_MF<-m_tc_Stl$MF[i] 
  MI_Stl_MH<-m_tc_Stl$MH[i] 
  
  MI_Gls_SF<-m_tc_Gls$SF[i] 
  MI_Gls_MF<-m_tc_Gls$MF[i] 
  MI_Gls_MH<-m_tc_Gls$MH[i] 
  
  MI_Gyp_SF<-m_tc_Gyp$SF[i] 
  MI_Gyp_MF<-m_tc_Gyp$MF[i] 
  MI_Gyp_MH<-m_tc_Gyp$MH[i] 
  
  MI_Ins_SF<-m_tc_Ins$SF[i] 
  MI_Ins_MF<-m_tc_Ins$MF[i] 
  MI_Ins_MH<-m_tc_Ins$MH[i] 
  
  MI_Wod_SF<-m_tc_Wod$SF[i] 
  MI_Wod_MF<-m_tc_Wod$MF[i] 
  MI_Wod_MH<-m_tc_Wod$MH[i] 
  
  MI_Fbg_SF<-m_tc_Fbg$SF[i] 
  MI_Fbg_MF<-m_tc_Fbg$MF[i] 
  MI_Fbg_MH<-m_tc_Fbg$MH[i] 
  
  MI_SnA_SF<-m_tc_SnA$SF[i] 
  MI_SnA_MF<-m_tc_SnA$MF[i] 
  MI_SnA_MH<-m_tc_SnA$MH[i] 
  
  MI_Oth_SF<-m_tc_Oth$SF[i] 
  MI_Oth_MF<-m_tc_Oth$MF[i] 
  MI_Oth_MH<-m_tc_Oth$MH[i] 
  smop_RFA_FA[[3]][[i]]$M_Dem_SF<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_RFA_FA[[3]][[i]]$M_Dem_MF<-smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_RFA_FA[[3]][[i]]$M_Dem_MH<-smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_RFA_FA[[3]][[i]]$M_Dem_Cem<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Con<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Stl<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Gls<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Gyp<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Ins<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Wod<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Fbg<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_SnA<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_RFA_FA[[3]][[i]]$M_Dem_Oth<-smop_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_RFA_FA[[3]][[i]]$M_Dem<-smop_RFA_FA[[3]][[i]]$M_Dem_SF+smop_RFA_FA[[3]][[i]]$M_Dem_MF+smop_RFA_FA[[3]][[i]]$M_Dem_MH
  
  smop_RFA_FA[[3]][[i]]$M_Rat_Con<-smop_RFA_FA[[3]][[i]]$M_Dem_Con/smop_RFA_FA[[3]][[i]]$M_NC_Con
  smop_RFA_FA[[3]][[i]]$M_Rat_Wod<-smop_RFA_FA[[3]][[i]]$M_Dem_Wod/smop_RFA_FA[[3]][[i]]$M_NC_Wod
  smop_RFA_FA[[3]][[i]]$M_Rat_Stl<-smop_RFA_FA[[3]][[i]]$M_Dem_Stl/smop_RFA_FA[[3]][[i]]$M_NC_Stl
  smop_RFA_FA[[3]][[i]]$M_Rat_Tot<-smop_RFA_FA[[3]][[i]]$M_Dem/smop_RFA_FA[[3]][[i]]$M_NC
  
  # now calculate material absolute OUTflows, total and per material for hiDR_RFA
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_SF<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_MF<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_MH<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Cem<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Con<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Stl<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Gls<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Gyp<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Ins<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Wod<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Fbg<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_SnA<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Oth<-smop_hiDR_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_hiDR_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_hiDR_RFA_FA[[3]][[i]]$M_Dem<-smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_SF+smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_MF+smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_MH
  
  smop_hiDR_RFA_FA[[3]][[i]]$M_Rat_Con<-smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Con/smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Con
  smop_hiDR_RFA_FA[[3]][[i]]$M_Rat_Wod<-smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Wod/smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Wod
  smop_hiDR_RFA_FA[[3]][[i]]$M_Rat_Stl<-smop_hiDR_RFA_FA[[3]][[i]]$M_Dem_Stl/smop_hiDR_RFA_FA[[3]][[i]]$M_NC_Stl
  smop_hiDR_RFA_FA[[3]][[i]]$M_Rat_Tot<-smop_hiDR_RFA_FA[[3]][[i]]$M_Dem/smop_hiDR_RFA_FA[[3]][[i]]$M_NC
  
  # now calculate material absolute flows, total and per material for hiMF_RFA
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_SF<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_SF
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_MF<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_MF
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_MH<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_MH
  
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Cem<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Cem_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Cem_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Cem_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Con<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Con_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Con_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Con_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Stl<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Stl_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Stl_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Stl_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Gls<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Gls_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Gls_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Gls_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Gyp<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Gyp_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Gyp_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Gyp_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Ins<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Ins_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Ins_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Ins_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Wod<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Wod_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Wod_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Wod_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Fbg<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Fbg_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Fbg_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Fbg_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_SnA<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_SnA_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_SnA_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_SnA_MH
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Oth<-smop_hiMF_RFA_FA[[3]][[i]]$Dem_SF_m2*MI_Oth_SF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MF_m2*MI_Oth_MF+smop_hiMF_RFA_FA[[3]][[i]]$Dem_MH_m2*MI_Oth_MH
  
  smop_hiMF_RFA_FA[[3]][[i]]$M_Dem<-smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_SF+smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_MF+smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_MH
  
  smop_hiMF_RFA_FA[[3]][[i]]$M_Rat_Con<-smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Con/smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Con
  smop_hiMF_RFA_FA[[3]][[i]]$M_Rat_Wod<-smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Wod/smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Wod
  smop_hiMF_RFA_FA[[3]][[i]]$M_Rat_Stl<-smop_hiMF_RFA_FA[[3]][[i]]$M_Dem_Stl/smop_hiMF_RFA_FA[[3]][[i]]$M_NC_Stl
  smop_hiMF_RFA_FA[[3]][[i]]$M_Rat_Tot<-smop_hiMF_RFA_FA[[3]][[i]]$M_Dem/smop_hiMF_RFA_FA[[3]][[i]]$M_NC
}
# calculate the US summary files. In the morning, run these and then save all as below
us_RFA_FA<-as.data.frame(smop_RFA_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial RFA dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_RFA_FA[,c(1,3:31,33:99)]<-0
# nc_RFA<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
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
# calculate the ratios
us_RFA_FA[,96:99]<-us_RFA_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_RFA_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

# same for hiDR_RFA
us_hiDR_RFA_FA<-as.data.frame(smop_hiDR_RFA_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial hiDR_RFA dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_hiDR_RFA_FA[,c(1,3:31,33:99)]<-0
# nc_hiDR_RFA<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
  us_hiDR_RFA_FA[,add]<-us_hiDR_RFA_FA[,add]+smop_hiDR_RFA_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_hiDR_RFA_FA[,4:6]<-us_hiDR_RFA_FA[,7:9]/us_hiDR_RFA_FA$Population
# calculate HHS
us_hiDR_RFA_FA[,10:12]<-us_hiDR_RFA_FA[,7:9]/us_hiDR_RFA_FA[,15:17]
us_hiDR_RFA_FA$HH_Size<-us_hiDR_RFA_FA$Population/us_hiDR_RFA_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiDR_RFA_FA[,18:21]<-us_hiDR_RFA_FA[,22:25]/us_hiDR_RFA_FA[,14:17]
# calculate m2/cap by type
us_hiDR_RFA_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiDR_RFA_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiDR_RFA_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate the ratios
us_hiDR_RFA_FA[,96:99]<-us_hiDR_RFA_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_hiDR_RFA_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

# same for hiMF_RFA
us_hiMF_RFA_FA<-as.data.frame(smop_hiMF_RFA_FA[[3]][[1]][,c(1:31,315:382)]) # until 382 rather than 364
# columns to add from the initial hiMF_RFA dataframe: pop, occ HU, tot HU, dem and con, new con and dem units and m2, occ m2, tot ghg and material flows
add<-c(3,7:9,14:17,22:31,33:48,53:95) # 81 ->99
add_smop<-c(3,7:9,14:17,22:31,316:331,336:378)
# turn all variables except year to 0
us_hiMF_RFA_FA[,c(1,3:31,33:99)]<-0
# nc_hiMF_RFA<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) { # this takes about 20 seconds
  us_hiMF_RFA_FA[,add]<-us_hiMF_RFA_FA[,add]+smop_hiMF_RFA_FA[[3]][[i]][,add_smop]
}
# calculate shares of population by housing type
us_hiMF_RFA_FA[,4:6]<-us_hiMF_RFA_FA[,7:9]/us_hiMF_RFA_FA$Population
# calculate HHS
us_hiMF_RFA_FA[,10:12]<-us_hiMF_RFA_FA[,7:9]/us_hiMF_RFA_FA[,15:17]
us_hiMF_RFA_FA$HH_Size<-us_hiMF_RFA_FA$Population/us_hiMF_RFA_FA$Tot_Hous_Units
# calculate vacancy ratios (TU/OU)
us_hiMF_RFA_FA[,18:21]<-us_hiMF_RFA_FA[,22:25]/us_hiMF_RFA_FA[,14:17]
# calculate m2/cap by type
us_hiMF_RFA_FA[,c("m2cap_SF","m2cap_MF","m2cap_MH","m2cap")]<-us_hiMF_RFA_FA[,c("Occ_m2_SF","Occ_m2_MF","Occ_m2_MH","Occ_m2")]/us_hiMF_RFA_FA[,c("Pop_SF","Pop_MF","Pop_MH","Population")]
# calculate the ratios
us_hiMF_RFA_FA[,96:99]<-us_hiMF_RFA_FA[,c("M_Dem_Con","M_Dem_Wod","M_Dem_Stl","M_Dem")]/us_hiMF_RFA_FA[,c("M_NC_Con","M_NC_Wod","M_NC_Stl","M_NC")]

save(smop_base_FA,smop_hiDR_FA,smop_hiMF_FA,smop_hiDRMF_FA,smop_RFA_FA,smop_hiDR_RFA_FA,smop_hiMF_RFA_FA,file="HSM_results/County_FloorArea_Mat.RData")
save(us_base_FA,us_hiDR_FA,us_hiMF_FA,us_hiDRMF_FA,us_RFA_FA,us_hiDR_RFA_FA,us_hiMF_RFA_FA,file="HSM_results/US_FA_GHG_summaries.RData")
# load("HSM_results/US_FA_GHG_summaries.RData")
# now plot total cumulative emissions in each scenario
# how do total embodied emissions compare for each scenario? 
# these are about 60% of initial estimate. the differences between scenarios are consequently less, bewteen the baseline and hiDR about 700Mt, not 1200Mt
# results are lower due to the archetype specific GHG intensities being quite lower than the originally used average values
sum(us_base_FA$GHG_NC)*1e-9 # 2886 Mt
sum(us_hiDR_FA$GHG_NC)*1e-9 # 3576 Mt
sum(us_hiMF_FA$GHG_NC)*1e-9 # 2559 Mt
sum(us_hiDRMF_FA$GHG_NC)*1e-9 # 3232 Mt
sum(us_RFA_FA$GHG_NC)*1e-9 # 2327 Mt, 2309, 2301
sum(us_hiDR_RFA_FA$GHG_NC)*1e-9 # 2891 Mt, 2868, 2858
sum(us_hiMF_RFA_FA$GHG_NC)*1e-9 # 2138 Mt, 2116, 2111

# calculate and plot cumulative EM GHG
us_base_FA$EmGHG_cum<-0
for (r in 1:41) {us_base_FA$EmGHG_cum[r]<-sum(us_base_FA$GHG_NC[1:r])}
us_hiDR_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiDR_FA$EmGHG_cum[r]<-sum(us_hiDR_FA$GHG_NC[1:r])}
us_hiMF_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiMF_FA$EmGHG_cum[r]<-sum(us_hiMF_FA$GHG_NC[1:r])}
us_hiDRMF_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiDRMF_FA$EmGHG_cum[r]<-sum(us_hiDRMF_FA$GHG_NC[1:r])}
us_RFA_FA$EmGHG_cum<-0
for (r in 1:41) {us_RFA_FA$EmGHG_cum[r]<-sum(us_RFA_FA$GHG_NC[1:r])}
us_hiMF_RFA_FA$EmGHG_cum<-0
for (r in 1:41) {us_hiMF_RFA_FA$EmGHG_cum[r]<-sum(us_hiMF_RFA_FA$GHG_NC[1:r])}

m2c_base<-as.data.frame(cbind(us_base_FA$Year,us_base_FA$HS_Scenario,us_base_FA$m2cap))
m2c_hiDR<-as.data.frame(cbind(us_hiDR_FA$Year,us_hiDR_FA$HS_Scenario,us_hiDR_FA$m2cap))
m2c_hiMF<-as.data.frame(cbind(us_hiMF_FA$Year,us_hiMF_FA$HS_Scenario,us_hiMF_FA$m2cap))
m2c_hiDRMF<-as.data.frame(cbind(us_hiDRMF_FA$Year,us_hiDRMF_FA$HS_Scenario,us_hiDRMF_FA$m2cap))
m2c_RFA<-as.data.frame(cbind(us_RFA_FA$Year,us_RFA_FA$HS_Scenario,us_RFA_FA$m2cap))
m2c_hiMF_RFA<-as.data.frame(cbind(us_hiMF_RFA_FA$Year,us_hiMF_RFA_FA$HS_Scenario,us_hiMF_RFA_FA$m2cap))
m2c_RFA[,2]<-5
m2c_hiMF_RFA[,2]<-6

m2<-rbind(m2c_base,m2c_hiDR,m2c_hiMF,m2c_hiDRMF,m2c_RFA,m2c_hiMF_RFA) # the five scenarios for the HSM paper, now doing six
names(m2)<-c("Year","Scenario","m2/cap")
m2[m2$Scenario==1,]$Scenario<-"1. Baseline"
m2[m2$Scenario==2,]$Scenario<-"2. High Turnover"
m2[m2$Scenario==3,]$Scenario<-"3. High Multifamily"
m2[m2$Scenario==4,]$Scenario<-"4. High TO & MF"
m2[m2$Scenario==5,]$Scenario<-"5. Red. Floor Area"
m2[m2$Scenario==6,]$Scenario<-"6. Hi MF, Red. Floor Area"

cols<-brewer.pal(n = 7, name = "Set1")[c(1:5,7)]
windows(width = 7.8,height = 6)
ggplot(m2,aes(Year,`m2/cap`,group=Scenario)) + geom_line(aes(color=Scenario),size=1)+ ylim(50,73) +
  labs(title ="Floor area per capita by housing stock scenario, 2020-2060") + theme_bw() + scale_color_manual(values = cols)  +  #scale_color_brewer(palette="Set1") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))


m2$EmGHG_cum<-c(us_base_FA$EmGHG_cum,us_hiDR_FA$EmGHG_cum,us_hiMF_FA$EmGHG_cum,us_hiDRMF_FA$EmGHG_cum,us_RFA_FA$EmGHG_cum,us_hiMF_RFA_FA$EmGHG_cum)*1e-9
windows(width = 7.8,height = 6)
ggplot(m2[m2$Year<2060,],aes(Year,EmGHG_cum,group=Scenario)) + geom_line(aes(color=Scenario),size=1)+ scale_y_continuous(labels = scales::comma) + 
  labs(title ="b) Cumulative GHG from new construction, 2020-2060",y="Mton CO2e") + theme_bw() + scale_color_manual(values = cols) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# now m2 inflow and outflow in each scenario
m2f_base<-as.data.frame(cbind(us_base_FA$Year,us_base_FA$HS_Scenario,us_base_FA$Tot_NC_m2,us_base_FA$Tot_Dem_m2))
m2f_hiDR<-as.data.frame(cbind(us_hiDR_FA$Year,us_hiDR_FA$HS_Scenario,us_hiDR_FA$Tot_NC_m2,us_hiDR_FA$Tot_Dem_m2))
m2f_hiMF<-as.data.frame(cbind(us_hiMF_FA$Year,us_hiMF_FA$HS_Scenario,us_hiMF_FA$Tot_NC_m2,us_hiMF_FA$Tot_Dem_m2))
m2f_hiDRMF<-as.data.frame(cbind(us_hiDRMF_FA$Year,us_hiDRMF_FA$HS_Scenario,us_hiDRMF_FA$Tot_NC_m2,us_hiDRMF_FA$Tot_Dem_m2))
m2f_RFA<-as.data.frame(cbind(us_RFA_FA$Year,us_RFA_FA$HS_Scenario,us_RFA_FA$Tot_NC_m2,us_RFA_FA$Tot_Dem_m2))
m2f_hiMF_RFA<-as.data.frame(cbind(us_hiMF_RFA_FA$Year,us_hiMF_RFA_FA$HS_Scenario,us_hiMF_RFA_FA$Tot_NC_m2,us_hiMF_RFA_FA$Tot_Dem_m2))
m2f_RFA[,2]<-5
m2f_hiMF_RFA[,2]<-6
m2f<-rbind(m2f_base,m2f_hiDR,m2f_hiMF,m2f_hiDRMF,m2f_RFA,m2f_hiMF_RFA)
names(m2f)<-c("Year","Scenario","Construction","Demolition")
m2f[m2f$Scenario==1,]$Scenario<-"1. Baseline"
m2f[m2f$Scenario==2,]$Scenario<-"2. High Turnover"
m2f[m2f$Scenario==3,]$Scenario<-"3. High Multifamily"
m2f[m2f$Scenario==4,]$Scenario<-"4. High TO & MF"
m2f[m2f$Scenario==5,]$Scenario<-"5. Red. Floor Area"
m2f[m2f$Scenario==6,]$Scenario<-"6. Hi MF, Red. Floor Area"
mm<-melt(m2f,id.vars = c("Year","Scenario"))
names(mm)[3]<-"Flow"
mm$Scenario_Flow<-paste(mm$Scenario,mm$Flow,sep="_")
windows(width = 7.8,height = 6)
ggplot(mm[mm$Year<2060,],aes(Year,1e-6*value,group=Scenario_Flow)) + geom_line(aes(color=Scenario,linetype=Flow),size=1)+ scale_y_continuous(labels = scales::comma,limits = c(10,550)) + 
  labs(title ="a) Floor area construction and demolition flows, 2020-2060",y="Million m2") + theme_bw() + scale_color_manual(values = cols) + scale_linetype_manual(values=c("solid", "twodash")) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# now show the flows of concrete for four selected counties
# first calculate demolition material outflows, similar to how i calculated inflows

rnm2<-c(1786,2590,1249,2281) # San Juan NM, Harrix TX, Marquette MI, Prov RI
cties<-smop_base_FA[,1:2]
rownames(cties)<-1:nrow(cties)

CtyCon_base<-as.data.frame(smop_base_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")])
CtyCon_hiDR<-as.data.frame(smop_hiDR_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")])
CtyCon_hiMF<-as.data.frame(smop_hiMF_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")])
CtyCon_hiDRMF<-as.data.frame(smop_hiDRMF_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")])
CtyCon_RFA<-as.data.frame(smop_RFA_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")])
CtyCon_hiMF_RFA<-as.data.frame(smop_hiMF_RFA_FA[[3]][[1786]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")])

for (r in rnm2[2:4]) {
  CtyCon_base<-rbind(CtyCon_base,as.data.frame(smop_base_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")]))
  CtyCon_hiDR<-rbind(CtyCon_hiDR,as.data.frame(smop_hiDR_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")]))
  CtyCon_hiMF<-rbind(CtyCon_hiMF,as.data.frame(smop_hiMF_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")]))
  CtyCon_hiDRMF<-rbind(CtyCon_hiDRMF,as.data.frame(smop_hiDRMF_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")]))
  CtyCon_RFA<-rbind(CtyCon_RFA,as.data.frame(smop_RFA_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")]))
  CtyCon_hiMF_RFA<-rbind(CtyCon_hiMF_RFA,as.data.frame(smop_hiMF_RFA_FA[[3]][[r]][,c("Year","GeoID","HS_Scenario","M_NC_Con","M_Dem_Con")]))
  
}
CtyCon_RFA$HS_Scenario<-5
CtyCon_hiMF_RFA$HS_Scenario<-6
CtyCon<-rbind(CtyCon_base,CtyCon_hiDR,CtyCon_hiMF,CtyCon_hiDRMF,CtyCon_RFA,CtyCon_hiMF_RFA)
names(CtyCon)[4:5]<-c("Construction","Demolition")
Con<-melt(CtyCon,id.vars = c("Year","GeoID","HS_Scenario"))
names(Con)[3:4]<-c("Scenario", "Flow")
Con[Con$Scenario==1,]$Scenario<-"1. Baseline"
Con[Con$Scenario==2,]$Scenario<-"2. High Turnover"
Con[Con$Scenario==3,]$Scenario<-"3. High Multifamily"
Con[Con$Scenario==4,]$Scenario<-"4. High TO & MF"
Con[Con$Scenario==5,]$Scenario<-"5. Red. Floor Area"
Con[Con$Scenario==6,]$Scenario<-"6. Hi MF, Red. Floor Area"
Con$value<-1e-6*Con$value # kg -> kton

r=rnm2[3] # 1, San Juan; 3, Marquette; 4,Providence
ConCty<-Con[Con$GeoID==cties[r,]$GeoID,]
location<-ctycode[ctycode$GeoID==cties[r,]$GeoID,]$RS_ID# will fix this to RS_ID
ConCty$Scenario_Flow<-paste(ConCty$Scenario,ConCty$Flow,sep="_")
cols<-c("#E41A1C","#377EB8","#E41A1C","#377EB8","#FF7F00","#FF7F00") # update to 6 colours
# cols<-brewer.pal(n = 7, name = "Set1")[c(1:5,7)]
windows(width = 7,height = 5.3)
ggplot(ConCty[ConCty$Year<2060,],aes(Year,value,group=Scenario_Flow)) + geom_line(aes(color=Scenario,linetype=Flow),size=1) + scale_color_manual(values = cols) +
  labs(title =paste("Concrete inflows and outflows,",location),y="kt Concrete") + theme_bw() + scale_linetype_manual(values=c("solid", "twodash")) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

r=rnm2[2] # 2, Harris
ConCty<-Con[Con$GeoID==cties[r,]$GeoID,]
location<-ctycode[ctycode$GeoID==cties[r,]$GeoID,]$RS_ID# will fix this to RS_ID
ConCty$Scenario_Flow<-paste(ConCty$Scenario,ConCty$Flow,sep="_")
cols<-brewer.pal(n = 7, name = "Set1")[c(1:5,7)]
windows(width = 7,height = 5.3)
ggplot(ConCty[ConCty$Year<2060,],aes(Year,value,group=Scenario_Flow)) + geom_line(aes(color=Scenario,linetype=Flow),size=1)+ scale_y_continuous(labels = scales::comma) + #scale_color_manual(values = cols) +
  labs(title =paste("Concrete inflows and outflows,",location),y="kt Concrete") + theme_bw() + scale_linetype_manual(values=c("solid", "twodash")) +  scale_color_manual(values = cols) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# now make a version of 4b which splits out total ghg by material type.

gm<-us_base_FA[,c(2,56:66)]
gm[41,2:12]<-gm[40,2:12]
gm<-melt(gm,id.vars = "Year")
gm$value<-gm$value*1e-9

gm$Source<-as.character(gm$variable)
gm[substr(gm$variable,8,10)=="Cem",]$Source<-"Cement"
gm[substr(gm$variable,8,10)=="Con",]$Source<-"Concrete"
gm[substr(gm$variable,8,10)=="Stl",]$Source<-"Steel"
gm[substr(gm$variable,8,10)=="Gyp",]$Source<-"Gypsum"
gm[substr(gm$variable,8,10)=="Fbg",]$Source<-"Fibreglass"
gm[substr(gm$variable,8,10)=="Ins",]$Source<-"Insulation"
gm[substr(gm$variable,8,10)=="Gls",]$Source<-"Glass"
gm[substr(gm$variable,8,10)=="Wod",]$Source<-"Wood Products"
gm[substr(gm$variable,8,10)=="SnA",]$Source<-"Sand/Aggregate"
gm[substr(gm$variable,8,10)=="Oth",]$Source<-"Other"
gm[substr(gm$variable,8,10)=="TnC",]$Source<-"Transport, Site Energy"

cols<-colorRampPalette(brewer.pal(11, "Paired"))(11)
cols<-brewer.pal(n = 12, name = "Paired")[-11]
windows(width=7.5,height = 6)
ggplot(gm,aes(x=Year,y=value,fill=Source))+geom_area()+theme_bw()+scale_fill_manual(values = cols) + ylim(0,90) +
  labs(title = "Annual emissions from new construction",subtitle = "Scenario 1. Baseline", y = "Mt CO2e/yr") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

gm2<-us_hiMF_RFA_FA[,c(2,56:66)]
gm2[41,2:12]<-gm2[40,2:12]
gm2<-melt(gm2,id.vars = "Year")
gm2$value<-gm2$value*1e-9

gm2$Source<-as.character(gm2$variable)
gm2[substr(gm2$variable,8,10)=="Cem",]$Source<-"Cement"
gm2[substr(gm2$variable,8,10)=="Con",]$Source<-"Concrete"
gm2[substr(gm2$variable,8,10)=="Stl",]$Source<-"Steel"
gm2[substr(gm2$variable,8,10)=="Gyp",]$Source<-"Gypsum"
gm2[substr(gm2$variable,8,10)=="Fbg",]$Source<-"Fibreglass"
gm2[substr(gm2$variable,8,10)=="Ins",]$Source<-"Insulation"
gm2[substr(gm2$variable,8,10)=="Gls",]$Source<-"Glass"
gm2[substr(gm2$variable,8,10)=="Wod",]$Source<-"Wood Products"
gm2[substr(gm2$variable,8,10)=="SnA",]$Source<-"Sand/Aggregate"
gm2[substr(gm2$variable,8,10)=="Oth",]$Source<-"Other"
gm2[substr(gm2$variable,8,10)=="TnC",]$Source<-"Transport, Site Energy"

cols<-colorRampPalette(brewer.pal(11, "Paired"))(11)
cols<-brewer.pal(n = 12, name = "Paired")[-11]
windows(width=7.5,height = 6)
ggplot(gm2,aes(x=Year,y=value,fill=Source))+geom_area()+theme_bw()+scale_fill_manual(values = cols) + ylim(0,90) +
  labs(title = "Annual emissions from new construction",subtitle = "Scenario 6. Hi MF, Reduced Floor Area", y = "Mt CO2e/yr") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# materials
mm<-us_base_FA[,c(2,71:80)]
mm[41,2:11]<-mm[40,2:11]
mm<-melt(mm,id.vars = "Year")
mm$value<-mm$value*1e-9

mm$Source<-as.character(mm$variable)
mm[substr(mm$variable,6,8)=="Cem",]$Source<-"Cement"
mm[substr(mm$variable,6,8)=="Con",]$Source<-"Concrete"
mm[substr(mm$variable,6,8)=="Stl",]$Source<-"Steel"
mm[substr(mm$variable,6,8)=="Gyp",]$Source<-"Gypsum"
mm[substr(mm$variable,6,8)=="Fbg",]$Source<-"Fibreglass"
mm[substr(mm$variable,6,8)=="Ins",]$Source<-"Insulation"
mm[substr(mm$variable,6,8)=="Gls",]$Source<-"Glass"
mm[substr(mm$variable,6,8)=="Wod",]$Source<-"Wood Products"
mm[substr(mm$variable,6,8)=="SnA",]$Source<-"Sand/Aggregate"
mm[substr(mm$variable,6,8)=="Oth",]$Source<-"Other"

cols<-brewer.pal(n = 12, name = "Paired")[-c(10,11)]
windows(width=7.3,height = 6.2)
ggplot(mm,aes(x=Year,y=value,fill=Source))+geom_area()+theme_bw()+scale_fill_manual(values = cols) + ylim(0,245) +
  labs(title = "Annual material requirements for new construction",subtitle = "Scenario 1. Baseline", y = "Mt yr") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

mm2<-us_hiMF_RFA_FA[,c(2,71:80)]
mm2[41,2:11]<-mm2[40,2:11]
mm2<-melt(mm2,id.vars = "Year")
mm2$value<-mm2$value*1e-9

mm2$Source<-as.character(mm2$variable)
mm2[substr(mm2$variable,6,8)=="Cem",]$Source<-"Cement"
mm2[substr(mm2$variable,6,8)=="Con",]$Source<-"Concrete"
mm2[substr(mm2$variable,6,8)=="Stl",]$Source<-"Steel"
mm2[substr(mm2$variable,6,8)=="Gyp",]$Source<-"Gypsum"
mm2[substr(mm2$variable,6,8)=="Fbg",]$Source<-"Fibreglass"
mm2[substr(mm2$variable,6,8)=="Ins",]$Source<-"Insulation"
mm2[substr(mm2$variable,6,8)=="Gls",]$Source<-"Glass"
mm2[substr(mm2$variable,6,8)=="Wod",]$Source<-"Wood Products"
mm2[substr(mm2$variable,6,8)=="SnA",]$Source<-"Sand/Aggregate"
mm2[substr(mm2$variable,6,8)=="Oth",]$Source<-"Other"

cols<-brewer.pal(n = 12, name = "Paired")[-c(10,11)]
windows(width=7.3,height = 6.2)
ggplot(mm2,aes(x=Year,y=value,fill=Source))+geom_area()+theme_bw()+scale_fill_manual(values = cols) + ylim(0,245) +
  labs(title = "Annual material requirements for new construction",subtitle = "Scenario 6. Hi MF, Reduced Floor Area", y = "Mt yr") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# ratios per year
us_base_FA[41,53:99]<-us_base_FA[40,53:99]

us_base_rat<-melt(us_base_FA[,c(2,96:99)],id.vars = 'Year')
us_base_rat$Material<-as.character(us_base_rat$variable)
us_base_rat[substr(us_base_rat$variable,7,9)=="Con",]$Material<-"Concrete"
us_base_rat[substr(us_base_rat$variable,7,9)=="Stl",]$Material<-"Steel"
us_base_rat[substr(us_base_rat$variable,7,9)=="Wod",]$Material<-"Wood"
us_base_rat[substr(us_base_rat$variable,7,9)=="Tot",]$Material<-"Total"
us_base_rat$Scenario<-"1. Baseline"

us_hiDR_FA[41,53:99]<-us_hiDR_FA[40,53:99]

us_hiDR_rat<-melt(us_hiDR_FA[,c(2,96:99)],id.vars = 'Year')
us_hiDR_rat$Material<-as.character(us_hiDR_rat$variable)
us_hiDR_rat[substr(us_hiDR_rat$variable,7,9)=="Con",]$Material<-"Concrete"
us_hiDR_rat[substr(us_hiDR_rat$variable,7,9)=="Stl",]$Material<-"Steel"
us_hiDR_rat[substr(us_hiDR_rat$variable,7,9)=="Wod",]$Material<-"Wood"
us_hiDR_rat[substr(us_hiDR_rat$variable,7,9)=="Tot",]$Material<-"Total"
us_hiDR_rat$Scenario<-"2. High Turnover"

us_hiMF_FA[41,53:99]<-us_hiMF_FA[40,53:99]

us_hiMF_rat<-melt(us_hiMF_FA[,c(2,96:99)],id.vars = 'Year')
us_hiMF_rat$Material<-as.character(us_hiMF_rat$variable)
us_hiMF_rat[substr(us_hiMF_rat$variable,7,9)=="Con",]$Material<-"Concrete"
us_hiMF_rat[substr(us_hiMF_rat$variable,7,9)=="Stl",]$Material<-"Steel"
us_hiMF_rat[substr(us_hiMF_rat$variable,7,9)=="Wod",]$Material<-"Wood"
us_hiMF_rat[substr(us_hiMF_rat$variable,7,9)=="Tot",]$Material<-"Total"
us_hiMF_rat$Scenario<-"3. High Multifamily"

us_hiDRMF_FA[41,53:99]<-us_hiDRMF_FA[40,53:99]

us_hiDRMF_rat<-melt(us_hiDRMF_FA[,c(2,96:99)],id.vars = 'Year')
us_hiDRMF_rat$Material<-as.character(us_hiDRMF_rat$variable)
us_hiDRMF_rat[substr(us_hiDRMF_rat$variable,7,9)=="Con",]$Material<-"Concrete"
us_hiDRMF_rat[substr(us_hiDRMF_rat$variable,7,9)=="Stl",]$Material<-"Steel"
us_hiDRMF_rat[substr(us_hiDRMF_rat$variable,7,9)=="Wod",]$Material<-"Wood"
us_hiDRMF_rat[substr(us_hiDRMF_rat$variable,7,9)=="Tot",]$Material<-"Total"
us_hiDRMF_rat$Scenario<-"4. High TO & MF"


us_RFA_FA[41,53:99]<-us_RFA_FA[40,53:99]

us_RFA_rat<-melt(us_RFA_FA[,c(2,96:99)],id.vars = 'Year')
us_RFA_rat$Material<-as.character(us_RFA_rat$variable)
us_RFA_rat[substr(us_RFA_rat$variable,7,9)=="Con",]$Material<-"Concrete"
us_RFA_rat[substr(us_RFA_rat$variable,7,9)=="Stl",]$Material<-"Steel"
us_RFA_rat[substr(us_RFA_rat$variable,7,9)=="Wod",]$Material<-"Wood"
us_RFA_rat[substr(us_RFA_rat$variable,7,9)=="Tot",]$Material<-"Total"
us_RFA_rat$Scenario<-"5. Red. Floor Area"

us_hiMF_RFA_FA[41,53:99]<-us_hiMF_RFA_FA[40,53:99]

us_hiMF_RFA_rat<-melt(us_hiMF_RFA_FA[,c(2,96:99)],id.vars = 'Year')
us_hiMF_RFA_rat$Material<-as.character(us_hiMF_RFA_rat$variable)
us_hiMF_RFA_rat[substr(us_hiMF_RFA_rat$variable,7,9)=="Con",]$Material<-"Concrete"
us_hiMF_RFA_rat[substr(us_hiMF_RFA_rat$variable,7,9)=="Stl",]$Material<-"Steel"
us_hiMF_RFA_rat[substr(us_hiMF_RFA_rat$variable,7,9)=="Wod",]$Material<-"Wood"
us_hiMF_RFA_rat[substr(us_hiMF_RFA_rat$variable,7,9)=="Tot",]$Material<-"Total"
us_hiMF_RFA_rat$Scenario<-"6. Hi MF, Red. Floor Area"

rat<-rbind(us_base_rat,us_hiDR_rat,us_hiMF_rat,us_hiDRMF_rat,us_RFA_rat,us_hiMF_RFA_rat)

cols<-brewer.pal(n = 7, name = "Set1")[c(1:5,7)]
windows(width=7.5, height=5.5)
ggplot(rat[rat$Material=='Concrete',],aes(x=Year,y=value)) + geom_line(aes(col=Scenario),size=1) + theme_bw() + ylim(0.2,0.65) +
  labs(title = "a) Ratio of waste concrete generation to concrete demand", y = "") + scale_color_manual(values=cols) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

windows(width=7.5, height=5.5)
ggplot(rat[rat$Material=='Steel',],aes(x=Year,y=value)) + geom_line(aes(col=Scenario),size=1) + theme_bw() + ylim(0.2,0.65) +
  labs(title = "b) Ratio of waste steel generation to steel demand", y = "") + scale_color_manual(values=cols) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

windows(width=7.5, height=5.5)
ggplot(rat[rat$Material=='Wood',],aes(x=Year,y=value)) + geom_line(aes(col=Scenario),size=1) + theme_bw() + ylim(0.2,0.65) +
  labs(title = "c) Ratio of waste wood generation to wood demand", y = "") + scale_color_manual(values=cols) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

windows(width=7.5, height=5.5)
ggplot(rat[rat$Material=='Total',],aes(x=Year,y=value)) + geom_line(aes(col=Scenario),size=1) + theme_bw() + ylim(0.2,0.65) +
  labs(title = "d) Ratio of waste material generation to material demand", y = "") + scale_color_manual(values=cols) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))

# occupied household size #############
hs1<-data.frame(Year=2020:2060,HHS=us_base_FA$Population/us_base_FA$Occ_Hous_Units,Scenario="All other scenarios")
hs3<-data.frame(Year=2020:2060,HHS=us_hiMF_FA$Population/us_hiMF_FA$Occ_Hous_Units,Scenario="High multifamily scenarios")
hs<-rbind(hs1,hs3)

windows(width=7.5, height=4.5)
ggplot(hs,aes(Year,HHS,col=Scenario)) + geom_line(size=1) + theme_bw()+ ylim(2.55,2.7)+
  labs(title = "National Average Household size, 2020-2060", y = "") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))