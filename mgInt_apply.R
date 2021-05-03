# script to calculate Mat. and GHG intensities per house type and cohort and county, for projections
# script to load, analyze, and combine by scenario bs.csv files
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
setwd("~/Yale Courses/Research/Final Paper/HSM_github")
load("Resstock_outputs/bs_base.RData")
load("Resstock_outputs/bs_baseRFA.RData")
load("Material_Intensities/Arch_intensities.RData")
load("Intermediate_results/ctycode.RData")
StDiv<-unique(bs_base[,c("State","Census.Division")])

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

mgi_all_comb<-mgi_all[,c(42,7:39)]

bs_base0<-merge(bs_base,mgi_all_comb,by="arch_div")

bs_base0$Type3<-"MF"
bs_base0[bs_base0$Geometry.Building.Type.RECS %in% c("Single-Family Detached","Single-Family Attached"),]$Type3<-"SF"
bs_base0[bs_base0$Geometry.Building.Type.RECS == "Mobile Home",]$Type3<-"MH"

# ghg int by county
g20_tc<-tapply(bs_base0$gi20_Tot,list(bs_base0$Type3,bs_base0$County),mean)
# until I introduce reductions of ghg intensity of material production over time, it may not be worth using this tcc type, county, cohort version
g20_tcc<-tapply(bs_base0$gi20_Tot,list(bs_base0$Type3,bs_base0$Vintage,bs_base0$County),mean)

# ghg int by state
g20_ts<-tapply(bs_base0$gi20_Tot,list(bs_base0$Type3,bs_base0$State),mean)
g20_tsc<-tapply(bs_base0$gi20_Tot,list(bs_base0$Type3,bs_base0$Vintage,bs_base0$State),mean)

# ghg int by division
g20_td<-tapply(bs_base0$gi20_Tot,list(bs_base0$Type3,bs_base0$Census.Division),mean)

ts_table<-as.data.frame(table(bs_base0$Type3,bs_base0$State))
names(ts_table)<-c("Type3","State","count")

tc_table<-as.data.frame(table(bs_base0$Type3,bs_base0$County))
names(tc_table)<-c("Type3","County","count")

# ghg int by division
g20_td<-round(as.data.frame(tapply(bs_base0$gi20_Tot,list(bs_base0$Census.Division,bs_base0$Type3),mean)),2)
g20_td$Census.Division<-rownames(g20_td)
rownames(g20_td)<-1:nrow(g20_td)
g20_td<-g20_td[,c(4,1:3)]

# ghg int by state
g20_ts<-round(as.data.frame(tapply(bs_base0$gi20_Tot,list(bs_base0$State,bs_base0$Type3),mean)),2)
g20_ts$State<-rownames(g20_ts)
rownames(g20_ts)<-1:nrow(g20_ts)
g20_ts<-g20_ts[,c(4,1:3)]
g20_ts<-merge(g20_ts,StDiv)

# fill in blanks based on division average
for (k in 2:4) { 
  for (r in 1:49) {
    if (is.na(g20_ts[r,k])) {
      g20_ts[r,k]<-g20_td[g20_td$Census.Division==g20_ts[r,"Census.Division"],k]
    } 
  }
}

# ghg int by county
g20_tc<-round(as.data.frame(tapply(bs_base0$gi20_Tot,list(bs_base0$County,bs_base0$Type3),mean)),2)
g20_tc$County<-rownames(g20_tc)
rownames(g20_tc)<-1:nrow(g20_tc)
g20_tc<-g20_tc[,c(4,1:3)]
g20_tc<-merge(g20_tc,ctycode,by.x="County",by.y="RS_ID",all=TRUE)
g20_tc<-g20_tc[order(g20_tc$GeoID),]
g20_tc<-g20_tc[-c(which(substr(g20_tc$GeoID,1,2) %in% c("02","15"))),]
# g20_tc<-merge(g20_tc,ctycode,by.x="County",by.y="RS_ID")
g20_tc$State<-substr(g20_tc$County,1,2)
rownames(g20_tc)<-1:3108
tcs<-tc_table[tc_table$count<4,]

for (j in 1:nrow(tcs)) {
  g20_tc[g20_tc$County==tcs$County[j],as.character(tcs$Type3[j])]<-NA
}

for (k in 2:4) { 
  for (r in 1:3108) {
    if (is.na(g20_tc[r,k])) {
      g20_tc[r,k]<-g20_ts[g20_ts$State==g20_tc[r,"State"],k]
    } 
  }
}

# add construction/transport emissions
g20_tc[,c("SF","MF")]<-g20_tc[,c("SF","MF")]+100
g20_tc[,c("MH")]<-g20_tc[,c("MH")]+70

g30_tc<-g40_tc<-g50_tc<-g20_tc

g30_tc[,2:4]<-0.95*g20_tc[,2:4]
g40_tc[,2:4]<-0.9*g20_tc[,2:4]
g50_tc[,2:4]<-0.85*g20_tc[,2:4]

save(g20_tc,g30_tc,g40_tc,g50_tc,file="Material_Intensities/gi_base.RData")

# GHG intensities for baseline floor area ########
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

ts_table<-as.data.frame(table(bs_baseRFA0$Type3,bs_baseRFA0$State))
names(ts_table)<-c("Type3","State","count")

tc_table<-as.data.frame(table(bs_baseRFA0$Type3,bs_baseRFA0$County))
names(tc_table)<-c("Type3","County","count")

# ghg int by division
g20_td<-round(as.data.frame(tapply(bs_baseRFA0$gi20_Tot,list(bs_baseRFA0$Census.Division,bs_baseRFA0$Type3),mean)),2)
g20_td$Census.Division<-rownames(g20_td)
rownames(g20_td)<-1:nrow(g20_td)
g20_td<-g20_td[,c(4,1:3)]

# ghg int by state
g20_ts<-round(as.data.frame(tapply(bs_baseRFA0$gi20_Tot,list(bs_baseRFA0$State,bs_baseRFA0$Type3),mean)),2)
g20_ts$State<-rownames(g20_ts)
rownames(g20_ts)<-1:nrow(g20_ts)
g20_ts<-g20_ts[,c(4,1:3)]
g20_ts<-merge(g20_ts,StDiv)

# fill in blanks based on division average
for (k in 2:4) { 
  for (r in 1:49) {
    if (is.na(g20_ts[r,k])) {
      g20_ts[r,k]<-g20_td[g20_td$Census.Division==g20_ts[r,"Census.Division"],k]
    } 
  }
}

# ghg int by county
g20_tcRFA<-round(as.data.frame(tapply(bs_baseRFA0$gi20_Tot,list(bs_baseRFA0$County,bs_baseRFA0$Type3),mean)),2)
g20_tcRFA$County<-rownames(g20_tcRFA)
rownames(g20_tcRFA)<-1:nrow(g20_tcRFA)
g20_tcRFA<-g20_tcRFA[,c(4,1:3)]
g20_tcRFA<-merge(g20_tcRFA,ctycode,by.x="County",by.y="RS_ID",all=TRUE)
g20_tcRFA<-g20_tcRFA[order(g20_tcRFA$GeoID),]
g20_tcRFA<-g20_tcRFA[-c(which(substr(g20_tcRFA$GeoID,1,2) %in% c("02","15"))),]

g20_tcRFA$State<-substr(g20_tcRFA$County,1,2)
rownames(g20_tcRFA)<-1:3108
tcs<-tc_table[tc_table$count<4,]

for (j in 1:nrow(tcs)) {
  g20_tcRFA[g20_tcRFA$County==tcs$County[j],as.character(tcs$Type3[j])]<-NA
}

for (k in 2:4) { 
  for (r in 1:3108) {
    if (is.na(g20_tcRFA[r,k])) {
      g20_tcRFA[r,k]<-g20_ts[g20_ts$State==g20_tcRFA[r,"State"],k]
    } 
  }
}

# add construction/transport emissions
g20_tcRFA[,c("SF","MF")]<-g20_tcRFA[,c("SF","MF")]+100
g20_tcRFA[,c("MH")]<-g20_tcRFA[,c("MH")]+70

g30_tcRFA<-g40_tcRFA<-g50_tcRFA<-g20_tcRFA

g30_tcRFA[,2:4]<-0.95*g20_tcRFA[,2:4]
g40_tcRFA[,2:4]<-0.9*g20_tcRFA[,2:4]
g50_tcRFA[,2:4]<-0.85*g20_tcRFA[,2:4]

save(g20_tcRFA,g30_tcRFA,g40_tcRFA,g50_tcRFA,file="Material_Intensities/gi_baseRFA.RData")

# calculate GHG flows #########
load("HSM_results/County_FloorArea.RData")
load("../resstock_projections/ExtData/County_FloorArea.RData")
# load("Material_Intensities/gi_base.RData")
# load("Material_Intensities/gi_RFA.RData")

for (i in 1:3108) { print(i)
  smop_base_FA[[3]][[i]]$GHG_NC_SF<-smop_base_FA[[3]][[i]]$NC_SF_m2*g20_tc$SF[i]
  smop_base_FA[[3]][[i]]$GHG_NC_SF[11:20]<-smop_base_FA[[3]][[i]]$NC_SF_m2[11:20]*g30_tc$SF[i]
  smop_base_FA[[3]][[i]]$GHG_NC_SF[21:30]<-smop_base_FA[[3]][[i]]$NC_SF_m2[21:30]*g40_tc$SF[i]
  smop_base_FA[[3]][[i]]$GHG_NC_SF[31:40]<-smop_base_FA[[3]][[i]]$NC_SF_m2[31:40]*g50_tc$SF[i]
  
  smop_base_FA[[3]][[i]]$GHG_NC_MF<-smop_base_FA[[3]][[i]]$NC_MF_m2*g20_tc$MF[i]
  smop_base_FA[[3]][[i]]$GHG_NC_MF[11:20]<-smop_base_FA[[3]][[i]]$NC_MF_m2[11:20]*g30_tc$MF[i]
  smop_base_FA[[3]][[i]]$GHG_NC_MF[21:30]<-smop_base_FA[[3]][[i]]$NC_MF_m2[21:30]*g40_tc$MF[i]
  smop_base_FA[[3]][[i]]$GHG_NC_MF[31:40]<-smop_base_FA[[3]][[i]]$NC_MF_m2[31:40]*g50_tc$MF[i]
  
  smop_base_FA[[3]][[i]]$GHG_NC_MH<-smop_base_FA[[3]][[i]]$NC_MH_m2*g20_tc$MH[i]
  smop_base_FA[[3]][[i]]$GHG_NC_MH[11:20]<-smop_base_FA[[3]][[i]]$NC_MH_m2[11:20]*g30_tc$MH[i]
  smop_base_FA[[3]][[i]]$GHG_NC_MH[21:30]<-smop_base_FA[[3]][[i]]$NC_MH_m2[21:30]*g40_tc$MH[i]
  smop_base_FA[[3]][[i]]$GHG_NC_MH[31:40]<-smop_base_FA[[3]][[i]]$NC_MH_m2[31:40]*g50_tc$MH[i]
  
  smop_base_FA[[3]][[i]]$GHG_NC<-smop_base_FA[[3]][[i]]$GHG_NC_SF+smop_base_FA[[3]][[i]]$GHG_NC_MF+smop_base_FA[[3]][[i]]$GHG_NC_MH
}
# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type, and construction related GHG
us_base_FA<-as.data.frame(smop_base_FA[[3]][[1]][,c(1:31,315:339)])
# columns to add from the initial base dataframe
add<-c(3,7:9,14:17,22:31,33:48,53:56)
add_smop<-c(3,7:9,14:17,22:31,316:331,336:339)
# turn all variables except year to 0
us_base_FA[,c(1,3:31,33:56)]<-0
# nc_base<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
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

# hiDR
for (i in 1:3108) { print(i)
  smop_hiDR_FA[[3]][[i]]$GHG_NC_SF<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2*g20_tc$SF[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_SF[11:20]<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2[11:20]*g30_tc$SF[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_SF[21:30]<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2[21:30]*g40_tc$SF[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_SF[31:40]<-smop_hiDR_FA[[3]][[i]]$NC_SF_m2[31:40]*g50_tc$SF[i]
  
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MF<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2*g20_tc$MF[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MF[11:20]<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2[11:20]*g30_tc$MF[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MF[21:30]<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2[21:30]*g40_tc$MF[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MF[31:40]<-smop_hiDR_FA[[3]][[i]]$NC_MF_m2[31:40]*g50_tc$MF[i]
  
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MH<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2*g20_tc$MH[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MH[11:20]<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2[11:20]*g30_tc$MH[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MH[21:30]<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2[21:30]*g40_tc$MH[i]
  smop_hiDR_FA[[3]][[i]]$GHG_NC_MH[31:40]<-smop_hiDR_FA[[3]][[i]]$NC_MH_m2[31:40]*g50_tc$MH[i]
  
  smop_hiDR_FA[[3]][[i]]$GHG_NC<-smop_hiDR_FA[[3]][[i]]$GHG_NC_SF+smop_hiDR_FA[[3]][[i]]$GHG_NC_MF+smop_hiDR_FA[[3]][[i]]$GHG_NC_MH
}
# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type, and construction related GHG
us_hiDR_FA<-as.data.frame(smop_hiDR_FA[[3]][[1]][,c(1:31,315:339)])
# columns to add from the initial hiDR dataframe
add<-c(3,7:9,14:17,22:31,33:48,53:56)
add_smop<-c(3,7:9,14:17,22:31,316:331,336:339)
# turn all variables except year to 0
us_hiDR_FA[,c(1,3:31,33:56)]<-0
# nc_hiDR<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
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

# hiMF
for (i in 1:3108) { print(i)
  smop_hiMF_FA[[3]][[i]]$GHG_NC_SF<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2*g20_tc$SF[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_SF[11:20]<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2[11:20]*g30_tc$SF[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_SF[21:30]<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2[21:30]*g40_tc$SF[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_SF[31:40]<-smop_hiMF_FA[[3]][[i]]$NC_SF_m2[31:40]*g50_tc$SF[i]
  
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MF<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2*g20_tc$MF[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MF[11:20]<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2[11:20]*g30_tc$MF[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MF[21:30]<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2[21:30]*g40_tc$MF[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MF[31:40]<-smop_hiMF_FA[[3]][[i]]$NC_MF_m2[31:40]*g50_tc$MF[i]
  
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MH<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2*g20_tc$MH[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MH[11:20]<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2[11:20]*g30_tc$MH[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MH[21:30]<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2[21:30]*g40_tc$MH[i]
  smop_hiMF_FA[[3]][[i]]$GHG_NC_MH[31:40]<-smop_hiMF_FA[[3]][[i]]$NC_MH_m2[31:40]*g50_tc$MH[i]
  
  smop_hiMF_FA[[3]][[i]]$GHG_NC<-smop_hiMF_FA[[3]][[i]]$GHG_NC_SF+smop_hiMF_FA[[3]][[i]]$GHG_NC_MF+smop_hiMF_FA[[3]][[i]]$GHG_NC_MH
}
# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type, and construction related GHG
us_hiMF_FA<-as.data.frame(smop_hiMF_FA[[3]][[1]][,c(1:31,315:339)])
# columns to add from the initial hiMF dataframe
add<-c(3,7:9,14:17,22:31,33:48,53:56)
add_smop<-c(3,7:9,14:17,22:31,316:331,336:339)
# turn all variables except year to 0
us_hiMF_FA[,c(1,3:31,33:56)]<-0
# nc_hiMF<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
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

# base RFA
for (i in 1:3108) { print(i)
  smop_RFA_FA[[3]][[i]]$GHG_NC_SF<-smop_RFA_FA[[3]][[i]]$NC_SF_m2*g20_tcRFA$SF[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_SF[11:20]<-smop_RFA_FA[[3]][[i]]$NC_SF_m2[11:20]*g30_tcRFA$SF[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_SF[21:30]<-smop_RFA_FA[[3]][[i]]$NC_SF_m2[21:30]*g40_tcRFA$SF[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_SF[31:40]<-smop_RFA_FA[[3]][[i]]$NC_SF_m2[31:40]*g50_tcRFA$SF[i]
  
  smop_RFA_FA[[3]][[i]]$GHG_NC_MF<-smop_RFA_FA[[3]][[i]]$NC_MF_m2*g20_tcRFA$MF[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_MF[11:20]<-smop_RFA_FA[[3]][[i]]$NC_MF_m2[11:20]*g30_tcRFA$MF[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_MF[21:30]<-smop_RFA_FA[[3]][[i]]$NC_MF_m2[21:30]*g40_tcRFA$MF[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_MF[31:40]<-smop_RFA_FA[[3]][[i]]$NC_MF_m2[31:40]*g50_tcRFA$MF[i]
  
  smop_RFA_FA[[3]][[i]]$GHG_NC_MH<-smop_RFA_FA[[3]][[i]]$NC_MH_m2*g20_tcRFA$MH[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_MH[11:20]<-smop_RFA_FA[[3]][[i]]$NC_MH_m2[11:20]*g30_tcRFA$MH[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_MH[21:30]<-smop_RFA_FA[[3]][[i]]$NC_MH_m2[21:30]*g40_tcRFA$MH[i]
  smop_RFA_FA[[3]][[i]]$GHG_NC_MH[31:40]<-smop_RFA_FA[[3]][[i]]$NC_MH_m2[31:40]*g50_tcRFA$MH[i]
  
  smop_RFA_FA[[3]][[i]]$GHG_NC<-smop_RFA_FA[[3]][[i]]$GHG_NC_SF+smop_RFA_FA[[3]][[i]]$GHG_NC_MF+smop_RFA_FA[[3]][[i]]$GHG_NC_MH
}
# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type, and construction related GHG
us_RFA_FA<-as.data.frame(smop_RFA_FA[[3]][[1]][,c(1:31,315:339)])
# columns to add from the initial RFA dataframe
add<-c(3,7:9,14:17,22:31,33:48,53:56)
add_smop<-c(3,7:9,14:17,22:31,316:331,336:339)
# turn all variables except year to 0
us_RFA_FA[,c(1,3:31,33:56)]<-0
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

# hiDR RFA
for (i in 1:3108) { print(i)
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2*g20_tcRFA$SF[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF[11:20]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2[11:20]*g30_tcRFA$SF[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF[21:30]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2[21:30]*g40_tcRFA$SF[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF[31:40]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_SF_m2[31:40]*g50_tcRFA$SF[i]
  
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2*g20_tcRFA$MF[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF[11:20]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2[11:20]*g30_tcRFA$MF[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF[21:30]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2[21:30]*g40_tcRFA$MF[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF[31:40]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MF_m2[31:40]*g50_tcRFA$MF[i]
  
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2*g20_tcRFA$MH[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH[11:20]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2[11:20]*g30_tcRFA$MH[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH[21:30]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2[21:30]*g40_tcRFA$MH[i]
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH[31:40]<-smop_hiDR_RFA_FA[[3]][[i]]$NC_MH_m2[31:40]*g50_tcRFA$MH[i]
  
  smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC<-smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_SF+smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MF+smop_hiDR_RFA_FA[[3]][[i]]$GHG_NC_MH
}
# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type, and construction related GHG
us_hiDR_RFA_FA<-as.data.frame(smop_hiDR_RFA_FA[[3]][[1]][,c(1:31,315:339)])
# columns to add from the initial RFA dataframe
add<-c(3,7:9,14:17,22:31,33:48,53:56)
add_smop<-c(3,7:9,14:17,22:31,316:331,336:339)
# turn all variables except year to 0
us_hiDR_RFA_FA[,c(1,3:31,33:56)]<-0
# nc_hiDR_RFA<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
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

# hiMF RFA
smop_hiMF_RFA_FA<-smop_hiMF_RFA # IF necessary
for (i in 1:3108) { print(i)
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2*g20_tcRFA$SF[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF[11:20]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2[11:20]*g30_tcRFA$SF[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF[21:30]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2[21:30]*g40_tcRFA$SF[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF[31:40]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_SF_m2[31:40]*g50_tcRFA$SF[i]
  
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2*g20_tcRFA$MF[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF[11:20]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2[11:20]*g30_tcRFA$MF[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF[21:30]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2[21:30]*g40_tcRFA$MF[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF[31:40]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MF_m2[31:40]*g50_tcRFA$MF[i]
  
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2*g20_tcRFA$MH[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH[11:20]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2[11:20]*g30_tcRFA$MH[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH[21:30]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2[21:30]*g40_tcRFA$MH[i]
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH[31:40]<-smop_hiMF_RFA_FA[[3]][[i]]$NC_MH_m2[31:40]*g50_tcRFA$MH[i]
  
  smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC<-smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_SF+smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MF+smop_hiMF_RFA_FA[[3]][[i]]$GHG_NC_MH
}
# extract template for making US summary results, for each scenario ########## # copied from the smvis3 script. Now applied to 3108 counties
# extract id, year, pop, pop shares, hhs, occ HU, VR, tot HU, tot dem by type, tot con by type, inflows and outflows to m2 stock, total occupied m2 by type, and construction related GHG
us_hiMF_RFA_FA<-as.data.frame(smop_hiMF_RFA_FA[[3]][[1]][,c(1:31,315:339)])
# columns to add from the initial RFA dataframe
add<-c(3,7:9,14:17,22:31,33:48,53:56)
add_smop<-c(3,7:9,14:17,22:31,316:331,336:339)
# turn all variables except year to 0
us_hiMF_RFA_FA[,c(1,3:31,33:56)]<-0
# nc_hiMF_RFA<-rep(0,8) # initialize vector for new construction every 5 years
for (i in 1:3108) {
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

save(us_base_FA,us_hiDR_FA,us_hiMF_FA,us_RFA_FA,us_hiDR_RFA_FA,us_hiMF_RFA_FA,file="HSM_results/US_FA_GHG_summaries.RData")

# comparison of total embodied GHG emissions in the 6 different scenarios
sum(us_base_FA$GHG_NC)*1e-9 # 4,443 Mt
sum(us_hiDR_FA$GHG_NC)*1e-9 # 5,519 Mt
sum(us_hiMF_FA$GHG_NC)*1e-9 # 3,898 Mt
sum(us_RFA_FA$GHG_NC)*1e-9 # 3,575 Mt
sum(us_hiDR_RFA_FA$GHG_NC)*1e-9 # 4,455 Mt
sum(us_hiMF_RFA_FA$GHG_NC)*1e-9 # 3,247 Mt

# plot some results ######
m2c_base<-as.data.frame(cbind(us_base_FA$Year,us_base_FA$HS_Scenario,us_base_FA$m2cap))
m2c_hiDR<-as.data.frame(cbind(us_hiDR_FA$Year,us_hiDR_FA$HS_Scenario,us_hiDR_FA$m2cap))
m2c_hiMF<-as.data.frame(cbind(us_hiMF_FA$Year,us_hiMF_FA$HS_Scenario,us_hiMF_FA$m2cap))

m2c_RFA<-as.data.frame(cbind(us_RFA_FA$Year,us_RFA_FA$HS_Scenario,us_RFA_FA$m2cap))
m2c_hiDR_RFA<-as.data.frame(cbind(us_hiDR_RFA_FA$Year,us_hiDR_RFA_FA$HS_Scenario,us_hiDR_RFA_FA$m2cap))
m2c_hiMF_RFA<-as.data.frame(cbind(us_hiMF_RFA_FA$Year,us_hiMF_RFA_FA$HS_Scenario,us_hiMF_RFA_FA$m2cap))

m2<-rbind(m2c_base,m2c_hiDR,m2c_hiMF,m2c_RFA,m2c_hiDR_RFA,m2c_hiMF_RFA)
names(m2)<-c("Year","HS_Scenario","m2/cap")
m2[m2$HS_Scenario==1,]$HS_Scenario<-"1. Baseline"
m2[m2$HS_Scenario==2,]$HS_Scenario<-"2. High Turnover"
m2[m2$HS_Scenario==3,]$HS_Scenario<-"3. High Multifamily"
m2$FA_Scenario<-"A"
m2$FA_Scenario[124:246]<-"B"
m2$Scenario<-"O"
m2[m2$HS_Scenario=="1. Baseline" & m2$FA_Scenario=="A",]$Scenario<-"1A. Baseline"
m2[m2$HS_Scenario=="2. High Turnover" & m2$FA_Scenario=="A",]$Scenario<-"2A. High Turnover"
m2[m2$HS_Scenario=="3. High Multifamily" & m2$FA_Scenario=="A",]$Scenario<-"3A. High Multifamily"
m2[m2$HS_Scenario=="1. Baseline" & m2$FA_Scenario=="B",]$Scenario<-"1B. Baseline RFA"
m2[m2$HS_Scenario=="2. High Turnover" & m2$FA_Scenario=="B",]$Scenario<-"2B. High Turnover RFA"
m2[m2$HS_Scenario=="3. High Multifamily" & m2$FA_Scenario=="B",]$Scenario<-"3B. High Multifamily RFA"


windows(width = 7.5,height = 6)
ggplot(m2,aes(Year,`m2/cap`,group=Scenario)) + geom_line(aes(color=Scenario),size=1)+ ylim(50,73) +
  labs(title ="Floor area per capita by housing stock scenario, 2020-2060") + theme_bw() + scale_color_brewer(palette="Paired") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face = "bold"),plot.title = element_text(size = 12, face = "bold"))
