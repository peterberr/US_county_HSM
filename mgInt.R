# script to read in and characterize material and GHG intensities of housing archetypes
# Peter Berrill April 2021
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
library(dplyr)
library(reshape2)
library(readxl)
library(ggplot2)
setwd("~/Yale Courses/Research/Final Paper/HSM_github")
conc<-read.csv("Housing Archetypes/MatCon.csv")
conc<-conc[order(conc$Material),]
names(conc)<-gsub('\\.\\.',', ', names(conc))
names(conc)<-gsub('\\.',' ', names(conc))
agg<-read.csv("Housing Archetypes/MatAgg.csv")
names(agg)[c(10,12)]<-c("Sand/Aggregate","Transport/Construction")
gi<-read_excel("Housing Archetypes/MatGHGint.xlsx",sheet = "Data")
# share of new single-family built with wood/masonry framing, based on Survey of Construction microdata https://www.census.gov/construction/chars/microdata.html
load("Data/framing.RData")
MatList<-conc$Material
MatTemp<-data.frame(Material=MatList,Mass = 0)
agg<-agg[order(agg$Material),]

# try in a loop  ####
for (a in c(1:48,59,60)) {
# for (a in c(1:33)) {
  for (b in 1:4) {
fn<-paste("Housing Archetypes/Bill of Materials Report ",a, ".", b, ".xlsx",sep="")
fn2<-paste("Housing Archetypes/LCA ",a, ".", b, ".xlsx",sep="")

bom<-as.data.frame(read_excel(fn))
names(bom)<-bom[5,]
bom<-bom[6:nrow(bom),c(1,11)]

lca<-as.data.frame(read_excel(fn2))

if (a %in% c(1,2,5,6,8,9,12,13,17,18,21,22,24,25,28,29,31,32,35,36,38,39,42,43,45,47)) {
  fa<-3000/10.765 # floor area, m2
}
if (a %in% c(3,4,7,10,11,14,19,20,23,26,27,30,33,34,37,40,41,44,46,48)) {
  fa<-1400/10.765 # floor area, m2
}
if (a==15) {fa<-38880/10.765}
if (a==16) {fa<-20880/10.765}
if (a==59) {fa<-1800/10.765}
if (a==60) {fa<-1000/10.765}
bom$Mat_Int<-as.numeric(bom$`Mass Value`)*1000/fa
bom2<-merge(bom,MatTemp,by="Material",all = TRUE)
bom2[is.na(bom2$`Mass Value`),]$Mat_Int<-0
bom2<-bom2[,c("Material","Mat_Int")]

mc<-t(conc[,2:ncol(conc)])%*%bom2$Mat_Int
mgi<-data.frame(Material=rownames(mc),mi=as.numeric(mc))

mgi$gi20<-mgi$mi*gi$GHGint2020
mgi$gi60<-mgi$mi*gi$GHGint2060

tran<-as.numeric(lca[6,7])
con<-as.numeric(lca[6,6])
tc<-data.frame(Material=c('Transport (A4)','Construction (A5)'),mi=0,gi20=c(tran,con)/fa,gi60=0.7*c(tran,con)/fa)
mgi<-rbind(mgi,tc)

mgi$gi20pc<-round(mgi$gi20/sum(mgi$gi20),3)
mgi$gi60pc<-round(mgi$gi60/sum(mgi$gi60),3)

mgi$arch<-paste(a,".",b,sep="")

assign(paste("mgi",a,".",b,sep=""),mgi)

mgio<-mgi[order(mgi$Material),]

mi_agg<-t(agg[,2:ncol(agg)])%*%mgio$mi

assign(paste("mi_agg",a,".",b,sep=""),mi_agg)
rm(fa)
  }
  assign(paste("mgi",a,sep=""),rbind(get(paste("mgi",a,".1",sep = "")),get(paste("mgi",a,".2",sep = "")),get(paste("mgi",a,".3",sep = "")),get(paste("mgi",a,".4",sep = ""))))
  mgi_arch<-get(paste("mgi",a,sep=""))
  
  mi_avg<-data.frame(Material=mgio$Material,mi= as.numeric(tapply(mgi_arch$mi,mgi_arch$Material,mean)))
  mi_avg_agg<-data.frame(Material=rownames(mi_agg),mi= as.numeric(t(agg[,2:ncol(agg)])%*%mi_avg$mi))
  assign(paste("mi_aa_",a,sep=""),mi_avg_agg)
  
  gi20_avg<-data.frame(Material=mgio$Material,gi20= as.numeric(tapply(mgi_arch$gi20,mgi_arch$Material,mean)))
  gi20_avg_agg<-data.frame(Material=rownames(mi_agg),gi20= as.numeric(t(agg[,2:ncol(agg)])%*%gi20_avg$gi20))
  assign(paste("gi20_aa_",a,sep=""),gi20_avg_agg)
  
  gi60_avg<-data.frame(Material=mgio$Material,gi60= as.numeric(tapply(mgi_arch$gi60,mgi_arch$Material,mean)))
  gi60_avg_agg<-data.frame(Material=rownames(mi_agg),gi60= as.numeric(t(agg[,2:ncol(agg)])%*%gi60_avg$gi60))
  assign(paste("gi60_aa_",a,sep=""),gi60_avg_agg)
  rm(list=ls(pattern='mi_agg'))
}
# calculation for MF hi-rise, for which only one archetype exists ##########
fn<-paste("Housing Archetypes/Bill of Materials Report 61_agg.xlsx",sep="")
fn2<-paste("Housing Archetypes/LCA 61.xlsx",sep="")
bom<-as.data.frame(read_excel(fn))
names(bom)<-bom[5,]
bom<-bom[6:nrow(bom),c(1,11)]
fa=100000/10.765 # 100,000 sfqt of living area divided by sqft->m2 conversion. It seems Reyna et al do not include service and public areas in their modelling of this building

bom$Mat_Int<-as.numeric(bom$`Mass Value`)*1000/fa
bom2<-merge(bom,MatTemp,by="Material",all = TRUE)
bom2[is.na(bom2$`Mass Value`),]$Mat_Int<-0
bom2<-bom2[,c("Material","Mat_Int")]

lca<-as.data.frame(read_excel(fn2))

mc<-t(conc[,2:ncol(conc)])%*%bom2$Mat_Int
mgi<-data.frame(Material=rownames(mc),mi=as.numeric(mc))

mgi$gi20<-mgi$mi*gi$GHGint2020
mgi$gi60<-mgi$mi*gi$GHGint2060

tran<-as.numeric(lca[6,7])
con<-as.numeric(lca[6,6])
tc<-data.frame(Material=c('Transport (A4)','Construction (A5)'),mi=0,gi20=c(tran,con)/fa,gi60=0.7*c(tran,con)/fa)
mgi<-rbind(mgi,tc)

mgi$gi20pc<-round(mgi$gi20/sum(mgi$gi20),3)
mgi$gi60pc<-round(mgi$gi60/sum(mgi$gi60),3)

mgi$arch<-61
mgi61<-mgi

mgio<-mgi[order(mgi$Material),]
mi_agg<-t(agg[,2:ncol(agg)])%*%mgio$mi
mi_agg61<-mi_agg

mgi_arch<-mgi61

mi_avg<-data.frame(Material=mgio$Material,mi= as.numeric(tapply(mgi_arch$mi,mgi_arch$Material,mean)))
mi_avg_agg<-data.frame(Material=rownames(mi_agg),mi= as.numeric(t(agg[,2:ncol(agg)])%*%mi_avg$mi))
mi_aa_61<-mi_avg_agg


gi20_avg<-data.frame(Material=mgio$Material,gi20= as.numeric(tapply(mgi_arch$gi20,mgi_arch$Material,mean)))
gi20_avg_agg<-data.frame(Material=rownames(mi_agg),gi20= as.numeric(t(agg[,2:ncol(agg)])%*%gi20_avg$gi20))
gi20_aa_61<-gi20_avg_agg

gi60_avg<-data.frame(Material=mgio$Material,gi60= as.numeric(tapply(mgi_arch$gi60,mgi_arch$Material,mean)))
gi60_avg_agg<-data.frame(Material=rownames(mi_agg),gi60= as.numeric(t(agg[,2:ncol(agg)])%*%gi60_avg$gi60))
gi60_aa_61<-gi60_avg_agg
rm(list=ls(pattern='mi_agg'))
############

frame_weights<-data.frame(Div=c("NE","MA","ENC","WNC","SA","ESC","WSC","MT","PAC"),Wood=wood_pc_div,Mason=mason_pc_div)
ins_div<-c(1.25,1,1.25,1.25,0.65,0.75,0.5,0.75,0.75) # factor to increase/decrease insulation level by division based on dominant climate 

# SF Slab
# Arch 1, from archs 1 & 8 
mgi_all1<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="Yes",Size="Large",Div=frame_weights$Div)
mgi_all1[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_1$mi,mi_aa_8$mi))
mgi_all1$mi_Insulation<-ins_div*mgi_all1$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all1$mi_Tot<-rowSums(mgi_all1[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all1[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_1$gi20,gi20_aa_8$gi20))
mgi_all1$gi20_Insulation<-ins_div*mgi_all1$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all1$gi20_Tot<-rowSums(mgi_all1[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all1[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_1$gi60,gi60_aa_8$gi60))
mgi_all1$gi60_Insulation<-ins_div*mgi_all1$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all1$gi60_Tot<-rowSums(mgi_all1[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all1$arch<-1
# Arch 2, from archs 2 & 9 
mgi_all2<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all2[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_2$mi,mi_aa_9$mi))
mgi_all2$mi_Insulation<-ins_div*mgi_all2$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all2$mi_Tot<-rowSums(mgi_all2[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all2[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_2$gi20,gi20_aa_9$gi20))
mgi_all2$gi20_Insulation<-ins_div*mgi_all2$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all2$gi20_Tot<-rowSums(mgi_all2[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all2[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_2$gi60,gi60_aa_9$gi60))
mgi_all2$gi60_Insulation<-ins_div*mgi_all2$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all2$gi60_Tot<-rowSums(mgi_all2[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all2$arch<-2

# Arch 3, from archs 3 & 10
mgi_all3<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="Yes",Size="Small",Div=frame_weights$Div)
mgi_all3[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_3$mi,mi_aa_10$mi))
mgi_all3$mi_Insulation<-ins_div*mgi_all3$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all3$mi_Tot<-rowSums(mgi_all3[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all3[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_3$gi20,gi20_aa_10$gi20))
mgi_all3$gi20_Insulation<-ins_div*mgi_all3$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all3$gi20_Tot<-rowSums(mgi_all3[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all3[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_3$gi60,gi60_aa_10$gi60))
mgi_all3$gi60_Insulation<-ins_div*mgi_all3$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all3$gi60_Tot<-rowSums(mgi_all3[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all3$arch<-3

# Arch 4, from archs 4 & 11
mgi_all4<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all4[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_4$mi,mi_aa_11$mi))
mgi_all4$mi_Insulation<-ins_div*mgi_all4$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all4$mi_Tot<-rowSums(mgi_all4[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all4[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_4$gi20,gi20_aa_11$gi20))
mgi_all4$gi20_Insulation<-ins_div*mgi_all4$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all4$gi20_Tot<-rowSums(mgi_all4[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all4[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_4$gi60,gi60_aa_11$gi60))
mgi_all4$gi60_Insulation<-ins_div*mgi_all4$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all4$gi60_Tot<-rowSums(mgi_all4[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all4$arch<-4

# Arch 5, from archs 5 & 12
mgi_all5<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Div=frame_weights$Div)
mgi_all5[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_5$mi,mi_aa_12$mi))
mgi_all5$mi_Insulation<-ins_div*mgi_all5$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all5$mi_Tot<-rowSums(mgi_all5[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all5[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_5$gi20,gi20_aa_12$gi20))
mgi_all5$gi20_Insulation<-ins_div*mgi_all5$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all5$gi20_Tot<-rowSums(mgi_all5[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all5[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_5$gi60,gi60_aa_12$gi60))
mgi_all5$gi60_Insulation<-ins_div*mgi_all5$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all5$gi60_Tot<-rowSums(mgi_all5[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all5$arch<-5

# Arch 6, from archs 6 & 13
mgi_all6<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all6[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_6$mi,mi_aa_13$mi))
mgi_all6$mi_Insulation<-ins_div*mgi_all6$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all6$mi_Tot<-rowSums(mgi_all6[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all6[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_6$gi20,gi20_aa_13$gi20))
mgi_all6$gi20_Insulation<-ins_div*mgi_all6$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all6$gi20_Tot<-rowSums(mgi_all6[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all6[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_6$gi60,gi60_aa_13$gi60))
mgi_all6$gi60_Insulation<-ins_div*mgi_all6$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all6$gi60_Tot<-rowSums(mgi_all6[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all6$arch<-6

# Arch 7, from archs 7 & 14
mgi_all7<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all7[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_7$mi,mi_aa_14$mi))
mgi_all7$mi_Insulation<-ins_div*mgi_all7$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all7$mi_Tot<-rowSums(mgi_all7[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all7[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_7$gi20,gi20_aa_14$gi20))
mgi_all7$gi20_Insulation<-ins_div*mgi_all7$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all7$gi20_Tot<-rowSums(mgi_all7[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all7[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_7$gi60,gi60_aa_14$gi60))
mgi_all7$gi60_Insulation<-ins_div*mgi_all7$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all7$gi60_Tot<-rowSums(mgi_all7[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all7$arch<-7

# SF Basement
# Arch 8, from archs 17 & 24
mgi_all8<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="Yes",Size="Large",Div=frame_weights$Div)
mgi_all8[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_17$mi,mi_aa_24$mi))
mgi_all8$mi_Insulation<-ins_div*mgi_all8$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all8$mi_Tot<-rowSums(mgi_all8[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all8[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_17$gi20,gi20_aa_24$gi20))
mgi_all8$gi20_Insulation<-ins_div*mgi_all8$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all8$gi20_Tot<-rowSums(mgi_all8[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all8[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_17$gi60,gi60_aa_24$gi60))
mgi_all8$gi60_Insulation<-ins_div*mgi_all8$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all8$gi60_Tot<-rowSums(mgi_all8[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all8$arch<-8

# Arch 9, from archs 18 & 25
mgi_all9<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all9[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_18$mi,mi_aa_25$mi))
mgi_all9$mi_Insulation<-ins_div*mgi_all9$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all9$mi_Tot<-rowSums(mgi_all9[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all9[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_18$gi20,gi20_aa_25$gi20))
mgi_all9$gi20_Insulation<-ins_div*mgi_all9$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all9$gi20_Tot<-rowSums(mgi_all9[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all9[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_18$gi60,gi60_aa_25$gi60))
mgi_all9$gi60_Insulation<-ins_div*mgi_all9$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all9$gi60_Tot<-rowSums(mgi_all9[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all9$arch<-9

# Arch 10, from archs 19 & 26
mgi_all10<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="Yes",Size="Small",Div=frame_weights$Div)
mgi_all10[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_19$mi,mi_aa_26$mi))
mgi_all10$mi_Insulation<-ins_div*mgi_all10$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all10$mi_Tot<-rowSums(mgi_all10[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all10[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_19$gi20,gi20_aa_26$gi20))
mgi_all10$gi20_Insulation<-ins_div*mgi_all10$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all10$gi20_Tot<-rowSums(mgi_all10[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all10[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_19$gi60,gi60_aa_26$gi60))
mgi_all10$gi60_Insulation<-ins_div*mgi_all10$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all10$gi60_Tot<-rowSums(mgi_all10[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all10$arch<-10

# Arch 11, from archs 20 & 27
mgi_all11<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all11[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_20$mi,mi_aa_27$mi))
mgi_all11$mi_Insulation<-ins_div*mgi_all11$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all11$mi_Tot<-rowSums(mgi_all11[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all11[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_20$gi20,gi20_aa_27$gi20))
mgi_all11$gi20_Insulation<-ins_div*mgi_all11$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all11$gi20_Tot<-rowSums(mgi_all11[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all11[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_20$gi60,gi60_aa_27$gi60))
mgi_all11$gi60_Insulation<-ins_div*mgi_all11$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all11$gi60_Tot<-rowSums(mgi_all11[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all11$arch<-11

# Arch 12, from archs 21 & 28
mgi_all12<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Div=frame_weights$Div)
mgi_all12[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_21$mi,mi_aa_28$mi))
mgi_all12$mi_Insulation<-ins_div*mgi_all12$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all12$mi_Tot<-rowSums(mgi_all12[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all12[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_21$gi20,gi20_aa_28$gi20))
mgi_all12$gi20_Insulation<-ins_div*mgi_all12$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all12$gi20_Tot<-rowSums(mgi_all12[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all12[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_21$gi60,gi60_aa_28$gi60))
mgi_all12$gi60_Insulation<-ins_div*mgi_all12$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all12$gi60_Tot<-rowSums(mgi_all12[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all12$arch<-12

# Arch 13, from archs 22 & 29
mgi_all13<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all13[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_22$mi,mi_aa_29$mi))
mgi_all13$mi_Insulation<-ins_div*mgi_all13$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all13$mi_Tot<-rowSums(mgi_all13[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all13[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_22$gi20,gi20_aa_29$gi20))
mgi_all13$gi20_Insulation<-ins_div*mgi_all13$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all13$gi20_Tot<-rowSums(mgi_all13[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all13[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_22$gi60,gi60_aa_29$gi60))
mgi_all13$gi60_Insulation<-ins_div*mgi_all13$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all13$gi60_Tot<-rowSums(mgi_all13[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all13$arch<-13

# Arch 14, from archs 23 & 30
mgi_all14<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all14[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_23$mi,mi_aa_30$mi))
mgi_all14$mi_Insulation<-ins_div*mgi_all14$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all14$mi_Tot<-rowSums(mgi_all14[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all14[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_23$gi20,gi20_aa_30$gi20))
mgi_all14$gi20_Insulation<-ins_div*mgi_all14$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all14$gi20_Tot<-rowSums(mgi_all14[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all14[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_23$gi60,gi60_aa_30$gi60))
mgi_all14$gi60_Insulation<-ins_div*mgi_all14$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all14$gi60_Tot<-rowSums(mgi_all14[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all14$arch<-14


# multifamily mid-rise 
# Arch 15, from arch 15
mgi_all15<-data.frame(Foundation="Strip",Type="MF",Stories="Multiple",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all15[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_15$mi,each=9)
mgi_all15$mi_Insulation<-ins_div*mgi_all15$mi_Insulation 
mgi_all15$mi_Tot<-rowSums(mgi_all15[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all15[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_15$gi20,each=9)
mgi_all15$gi20_Insulation<-ins_div*mgi_all15$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all15$gi20_Tot<-rowSums(mgi_all15[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all15[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_15$gi60,each=9)
mgi_all15$gi60_Insulation<-ins_div*mgi_all15$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all15$gi60_Tot<-rowSums(mgi_all15[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all15$arch<-15

mgi_all16<-data.frame(Foundation="Strip",Type="MF",Stories="Multiple",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all16[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_16$mi,each=9)
mgi_all16$mi_Insulation<-ins_div*mgi_all16$mi_Insulation 
mgi_all16$mi_Tot<-rowSums(mgi_all16[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all16[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_16$gi20,each=9)
mgi_all16$gi20_Insulation<-ins_div*mgi_all16$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all16$gi20_Tot<-rowSums(mgi_all16[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all16[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_16$gi60,each=9)
mgi_all16$gi60_Insulation<-ins_div*mgi_all16$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all16$gi60_Tot<-rowSums(mgi_all16[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all16$arch<-16

# single-family crawlspace
# Arch 17, from archs 31 & 38
mgi_all17<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="Yes",Size="Large",Div=frame_weights$Div)
mgi_all17[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_31$mi,mi_aa_38$mi))
mgi_all17$mi_Insulation<-ins_div*mgi_all17$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all17$mi_Tot<-rowSums(mgi_all17[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all17[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_31$gi20,gi20_aa_38$gi20))
mgi_all17$gi20_Insulation<-ins_div*mgi_all17$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all17$gi20_Tot<-rowSums(mgi_all17[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all17[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_31$gi60,gi60_aa_38$gi60))
mgi_all17$gi60_Insulation<-ins_div*mgi_all17$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all17$gi60_Tot<-rowSums(mgi_all17[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all17$arch<-17

# Arch 18, from archs 32 & 39
mgi_all18<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all18[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_32$mi,mi_aa_39$mi))
mgi_all18$mi_Insulation<-ins_div*mgi_all18$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all18$mi_Tot<-rowSums(mgi_all18[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all18[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_32$gi20,gi20_aa_39$gi20))
mgi_all18$gi20_Insulation<-ins_div*mgi_all18$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all18$gi20_Tot<-rowSums(mgi_all18[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all18[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_32$gi60,gi60_aa_39$gi60))
mgi_all18$gi60_Insulation<-ins_div*mgi_all18$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all18$gi60_Tot<-rowSums(mgi_all18[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all18$arch<-18

# Arch 19, from archs 33 & 40
mgi_all19<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="Yes",Size="Small",Div=frame_weights$Div)
mgi_all19[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_33$mi,mi_aa_40$mi))
mgi_all19$mi_Insulation<-ins_div*mgi_all19$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all19$mi_Tot<-rowSums(mgi_all19[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all19[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_33$gi20,gi20_aa_40$gi20))
mgi_all19$gi20_Insulation<-ins_div*mgi_all19$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all19$gi20_Tot<-rowSums(mgi_all19[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all19[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_33$gi60,gi60_aa_40$gi60))
mgi_all19$gi60_Insulation<-ins_div*mgi_all19$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all19$gi60_Tot<-rowSums(mgi_all19[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all19$arch<-19

# Arch 20, from archs 34 & 41
mgi_all20<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all20[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_34$mi,mi_aa_41$mi))
mgi_all20$mi_Insulation<-ins_div*mgi_all20$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all20$mi_Tot<-rowSums(mgi_all20[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all20[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_34$gi20,gi20_aa_41$gi20))
mgi_all20$gi20_Insulation<-ins_div*mgi_all20$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all20$gi20_Tot<-rowSums(mgi_all20[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all20[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_34$gi60,gi60_aa_41$gi60))
mgi_all20$gi60_Insulation<-ins_div*mgi_all20$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all20$gi60_Tot<-rowSums(mgi_all20[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all20$arch<-20

# Arch 21, from archs 35 & 42
mgi_all21<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Div=frame_weights$Div)
mgi_all21[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_35$mi,mi_aa_42$mi))
mgi_all21$mi_Insulation<-ins_div*mgi_all21$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all21$mi_Tot<-rowSums(mgi_all21[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all21[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_35$gi20,gi20_aa_42$gi20))
mgi_all21$gi20_Insulation<-ins_div*mgi_all21$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all21$gi20_Tot<-rowSums(mgi_all21[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all21[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_35$gi60,gi60_aa_42$gi60))
mgi_all21$gi60_Insulation<-ins_div*mgi_all21$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all21$gi60_Tot<-rowSums(mgi_all21[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all21$arch<-21

# Arch 22, from archs 36 & 43
mgi_all22<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all22[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_36$mi,mi_aa_43$mi))
mgi_all22$mi_Insulation<-ins_div*mgi_all22$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all22$mi_Tot<-rowSums(mgi_all22[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all22[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_36$gi20,gi20_aa_43$gi20))
mgi_all22$gi20_Insulation<-ins_div*mgi_all22$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all22$gi20_Tot<-rowSums(mgi_all22[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all22[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_36$gi60,gi60_aa_43$gi60))
mgi_all22$gi60_Insulation<-ins_div*mgi_all22$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all22$gi60_Tot<-rowSums(mgi_all22[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all22$arch<-22

# Arch 23, from archs 37 & 44
mgi_all23<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all23[,paste("mi_", mi_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(mi_aa_37$mi,mi_aa_44$mi))
mgi_all23$mi_Insulation<-ins_div*mgi_all23$mi_Insulation # adjust insulation values based on local climates, by division
mgi_all23$mi_Tot<-rowSums(mgi_all23[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_all23[,paste("gi20_", gi20_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi20_aa_37$gi20,gi20_aa_44$gi20))
mgi_all23$gi20_Insulation<-ins_div*mgi_all23$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all23$gi20_Tot<-rowSums(mgi_all23[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_all23[,paste("gi60_", gi60_aa_1$Material, sep="")]<-as.matrix(frame_weights[,c("Wood","Mason")])%*%t(cbind(gi60_aa_37$gi60,gi60_aa_44$gi60))
mgi_all23$gi60_Insulation<-ins_div*mgi_all23$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all23$gi60_Tot<-rowSums(mgi_all23[,paste("gi60_", gi60_aa_2$Material, sep="")])
mgi_all23$arch<-23

# single-family pier & beam

# Arch 24, from arch 45
mgi_all24<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="One",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all24[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_45$mi,each=9)
mgi_all24$mi_Insulation<-ins_div*mgi_all24$mi_Insulation 
mgi_all24$mi_Tot<-rowSums(mgi_all24[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all24[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_45$gi20,each=9)
mgi_all24$gi20_Insulation<-ins_div*mgi_all24$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all24$gi20_Tot<-rowSums(mgi_all24[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all24[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_45$gi60,each=9)
mgi_all24$gi60_Insulation<-ins_div*mgi_all24$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all24$gi60_Tot<-rowSums(mgi_all24[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all24$arch<-24

# Arch 25, from arch 46
mgi_all25<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="One",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all25[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_46$mi,each=9)
mgi_all25$mi_Insulation<-ins_div*mgi_all25$mi_Insulation 
mgi_all25$mi_Tot<-rowSums(mgi_all25[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all25[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_46$gi20,each=9)
mgi_all25$gi20_Insulation<-ins_div*mgi_all25$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all25$gi20_Tot<-rowSums(mgi_all25[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all25[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_46$gi60,each=9)
mgi_all25$gi60_Insulation<-ins_div*mgi_all25$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all25$gi60_Tot<-rowSums(mgi_all25[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all25$arch<-25

# Arch 26, from arch 47
mgi_all26<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="Multiple",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all26[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_47$mi,each=9)
mgi_all26$mi_Insulation<-ins_div*mgi_all26$mi_Insulation 
mgi_all26$mi_Tot<-rowSums(mgi_all26[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all26[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_47$gi20,each=9)
mgi_all26$gi20_Insulation<-ins_div*mgi_all26$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all26$gi20_Tot<-rowSums(mgi_all26[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all26[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_47$gi60,each=9)
mgi_all26$gi60_Insulation<-ins_div*mgi_all26$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all26$gi60_Tot<-rowSums(mgi_all26[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all26$arch<-26

# Arch 27, from arch 48
mgi_all27<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="Multiple",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all27[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_48$mi,each=9)
mgi_all27$mi_Insulation<-ins_div*mgi_all27$mi_Insulation 
mgi_all27$mi_Tot<-rowSums(mgi_all27[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all27[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_48$gi20,each=9)
mgi_all27$gi20_Insulation<-ins_div*mgi_all27$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all27$gi20_Tot<-rowSums(mgi_all27[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all27[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_48$gi60,each=9)
mgi_all27$gi60_Insulation<-ins_div*mgi_all27$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all27$gi60_Tot<-rowSums(mgi_all27[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all27$arch<-27

# MH
# Arch 28, from arch 59
mgi_all28<-data.frame(Foundation="Pier & Beam",Type="MH",Stories="One",Garage="No",Size="Large",Div=frame_weights$Div)
mgi_all28[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_59$mi,each=9)
mgi_all28$mi_Insulation<-ins_div*mgi_all28$mi_Insulation 
mgi_all28$mi_Tot<-rowSums(mgi_all28[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all28[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_59$gi20,each=9)
mgi_all28$gi20_Insulation<-ins_div*mgi_all28$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all28$gi20_Tot<-rowSums(mgi_all28[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all28[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_59$gi60,each=9)
mgi_all28$gi60_Insulation<-ins_div*mgi_all28$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all28$gi60_Tot<-rowSums(mgi_all28[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all28$arch<-28

# Arch 29, from arch 60
mgi_all29<-data.frame(Foundation="Pier & Beam",Type="MH",Stories="One",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all29[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_60$mi,each=9)
mgi_all29$mi_Insulation<-ins_div*mgi_all29$mi_Insulation 
mgi_all29$mi_Tot<-rowSums(mgi_all29[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all29[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_60$gi20,each=9)
mgi_all29$gi20_Insulation<-ins_div*mgi_all29$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all29$gi20_Tot<-rowSums(mgi_all29[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all29[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_60$gi60,each=9)
mgi_all29$gi60_Insulation<-ins_div*mgi_all29$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all29$gi60_Tot<-rowSums(mgi_all29[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all29$arch<-29

# MF high-rise
# Arch 30, from arch 61
mgi_all30<-data.frame(Foundation="Pile",Type="MF",Stories="Multiple",Garage="No",Size="Small",Div=frame_weights$Div)
mgi_all30[,paste("mi_", mi_aa_1$Material, sep="")]<-rep(mi_aa_61$mi,each=9)
mgi_all30$mi_Insulation<-ins_div*mgi_all30$mi_Insulation 
mgi_all30$mi_Tot<-rowSums(mgi_all30[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_all30[,paste("gi20_", gi20_aa_1$Material, sep="")]<-rep(gi20_aa_61$gi20,each=9)
mgi_all30$gi20_Insulation<-ins_div*mgi_all30$gi20_Insulation # adjust insulation values based on local climates, by division
mgi_all30$gi20_Tot<-rowSums(mgi_all30[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_all30[,paste("gi60_", gi60_aa_1$Material, sep="")]<-rep(gi60_aa_61$gi60,each=9)
mgi_all30$gi60_Insulation<-ins_div*mgi_all30$gi60_Insulation # adjust insulation values based on local climates, by division
mgi_all30$gi60_Tot<-rowSums(mgi_all30[,paste("gi60_", gi60_aa_1$Material, sep="")])
mgi_all30$arch<-30

# paste together
mgi_all<-bind_rows(mgi_all1,mgi_all2,mgi_all3,mgi_all4,mgi_all5,mgi_all6,mgi_all7,mgi_all8,mgi_all9,mgi_all10,
                   mgi_all11,mgi_all12,mgi_all13,mgi_all14,mgi_all15,mgi_all16,mgi_all17,mgi_all18,mgi_all19,mgi_all20,
                   mgi_all21,mgi_all22,mgi_all23,mgi_all24,mgi_all25,mgi_all26,mgi_all27,mgi_all28,mgi_all29,mgi_all30)
save(mgi_all,file="Material_Intensities/Arch_intensities.RData")                   


# now make another dataframe combining all mi and gi for all ~50 archetypes, not including regional differentiation of wood/masonry framing share, or insulation level #########
# SF Slab: 1-14
# Arch 1
mgi_full1<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="Yes",Size="Large",Frame="Wood")
mgi_full1[,paste("mi_", mi_aa_1$Material, sep="")]<-mi_aa_1$mi
mgi_full1$mi_Tot<-sum(mgi_full1[,paste("mi_", mi_aa_1$Material, sep="")])
mgi_full1[,paste("gi20_", gi20_aa_1$Material, sep="")]<-gi20_aa_1$gi20
mgi_full1$gi20_Tot<-sum(mgi_full1[,paste("gi20_", gi20_aa_1$Material, sep="")])
mgi_full1$arch<-1
# Arch 8
mgi_full8<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="Yes",Size="Large",Frame="Masonry")
mgi_full8[,paste("mi_", mi_aa_8$Material, sep="")]<-mi_aa_8$mi
mgi_full8$mi_Tot<-sum(mgi_full8[,paste("mi_", mi_aa_8$Material, sep="")])
mgi_full8[,paste("gi20_", gi20_aa_8$Material, sep="")]<-gi20_aa_8$gi20
mgi_full8$gi20_Tot<-sum(mgi_full8[,paste("gi20_", gi20_aa_8$Material, sep="")])
mgi_full8$arch<-8

# Arch 2
mgi_full2<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Wood")
mgi_full2[,paste("mi_", mi_aa_2$Material, sep="")]<-mi_aa_2$mi
mgi_full2$mi_Tot<-sum(mgi_full2[,paste("mi_", mi_aa_2$Material, sep="")])
mgi_full2[,paste("gi20_", gi20_aa_2$Material, sep="")]<-gi20_aa_2$gi20
mgi_full2$gi20_Tot<-sum(mgi_full2[,paste("gi20_", gi20_aa_2$Material, sep="")])
mgi_full2$arch<-2
# Arch 9
mgi_full9<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Masonry")
mgi_full9[,paste("mi_", mi_aa_9$Material, sep="")]<-mi_aa_9$mi
mgi_full9$mi_Tot<-sum(mgi_full9[,paste("mi_", mi_aa_9$Material, sep="")])
mgi_full9[,paste("gi20_", gi20_aa_9$Material, sep="")]<-gi20_aa_9$gi20
mgi_full9$gi20_Tot<-sum(mgi_full9[,paste("gi20_", gi20_aa_9$Material, sep="")])
mgi_full9$arch<-9

# Arch 3
mgi_full3<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="Yes",Size="Small",Frame="Wood")
mgi_full3[,paste("mi_", mi_aa_3$Material, sep="")]<-mi_aa_3$mi
mgi_full3$mi_Tot<-sum(mgi_full3[,paste("mi_", mi_aa_3$Material, sep="")])
mgi_full3[,paste("gi20_", gi20_aa_3$Material, sep="")]<-gi20_aa_3$gi20
mgi_full3$gi20_Tot<-sum(mgi_full3[,paste("gi20_", gi20_aa_3$Material, sep="")])
mgi_full3$arch<-3
# Arch 10
mgi_full10<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="Yes",Size="Small",Frame="Masonry")
mgi_full10[,paste("mi_", mi_aa_10$Material, sep="")]<-mi_aa_10$mi
mgi_full10$mi_Tot<-sum(mgi_full10[,paste("mi_", mi_aa_10$Material, sep="")])
mgi_full10[,paste("gi20_", gi20_aa_10$Material, sep="")]<-gi20_aa_10$gi20
mgi_full10$gi20_Tot<-sum(mgi_full10[,paste("gi20_", gi20_aa_10$Material, sep="")])
mgi_full10$arch<-10

# Arch 4
mgi_full4<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Wood")
mgi_full4[,paste("mi_", mi_aa_4$Material, sep="")]<-mi_aa_4$mi
mgi_full4$mi_Tot<-sum(mgi_full4[,paste("mi_", mi_aa_4$Material, sep="")])
mgi_full4[,paste("gi20_", gi20_aa_4$Material, sep="")]<-gi20_aa_4$gi20
mgi_full4$gi20_Tot<-sum(mgi_full4[,paste("gi20_", gi20_aa_4$Material, sep="")])
mgi_full4$arch<-4
# Arch 11
mgi_full11<-data.frame(Foundation="Slab",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Masonry")
mgi_full11[,paste("mi_", mi_aa_11$Material, sep="")]<-mi_aa_11$mi
mgi_full11$mi_Tot<-sum(mgi_full11[,paste("mi_", mi_aa_11$Material, sep="")])
mgi_full11[,paste("gi20_", gi20_aa_11$Material, sep="")]<-gi20_aa_11$gi20
mgi_full11$gi20_Tot<-sum(mgi_full11[,paste("gi20_", gi20_aa_11$Material, sep="")])
mgi_full11$arch<-11

# Arch 5
mgi_full5<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Frame="Wood")
mgi_full5[,paste("mi_", mi_aa_5$Material, sep="")]<-mi_aa_5$mi
mgi_full5$mi_Tot<-sum(mgi_full5[,paste("mi_", mi_aa_5$Material, sep="")])
mgi_full5[,paste("gi20_", gi20_aa_5$Material, sep="")]<-gi20_aa_5$gi20
mgi_full5$gi20_Tot<-sum(mgi_full5[,paste("gi20_", gi20_aa_5$Material, sep="")])
mgi_full5$arch<-5
# Arch 12
mgi_full12<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Frame="Masonry")
mgi_full12[,paste("mi_", mi_aa_12$Material, sep="")]<-mi_aa_12$mi
mgi_full12$mi_Tot<-sum(mgi_full12[,paste("mi_", mi_aa_12$Material, sep="")])
mgi_full12[,paste("gi20_", gi20_aa_12$Material, sep="")]<-gi20_aa_12$gi20
mgi_full12$gi20_Tot<-sum(mgi_full12[,paste("gi20_", gi20_aa_12$Material, sep="")])
mgi_full12$arch<-12

# Arch 6
mgi_full6<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Wood")
mgi_full6[,paste("mi_", mi_aa_6$Material, sep="")]<-mi_aa_6$mi
mgi_full6$mi_Tot<-sum(mgi_full6[,paste("mi_", mi_aa_6$Material, sep="")])
mgi_full6[,paste("gi20_", gi20_aa_6$Material, sep="")]<-gi20_aa_6$gi20
mgi_full6$gi20_Tot<-sum(mgi_full6[,paste("gi20_", gi20_aa_6$Material, sep="")])
mgi_full6$arch<-6
# Arch 13
mgi_full13<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Masonry")
mgi_full13[,paste("mi_", mi_aa_13$Material, sep="")]<-mi_aa_13$mi
mgi_full13$mi_Tot<-sum(mgi_full13[,paste("mi_", mi_aa_13$Material, sep="")])
mgi_full13[,paste("gi20_", gi20_aa_13$Material, sep="")]<-gi20_aa_13$gi20
mgi_full13$gi20_Tot<-sum(mgi_full13[,paste("gi20_", gi20_aa_13$Material, sep="")])
mgi_full13$arch<-13

# Arch 7
mgi_full7<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Wood")
mgi_full7[,paste("mi_", mi_aa_7$Material, sep="")]<-mi_aa_7$mi
mgi_full7$mi_Tot<-sum(mgi_full7[,paste("mi_", mi_aa_7$Material, sep="")])
mgi_full7[,paste("gi20_", gi20_aa_7$Material, sep="")]<-gi20_aa_7$gi20
mgi_full7$gi20_Tot<-sum(mgi_full7[,paste("gi20_", gi20_aa_7$Material, sep="")])
mgi_full7$arch<-7
# Arch 14
mgi_full14<-data.frame(Foundation="Slab",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Masonry")
mgi_full14[,paste("mi_", mi_aa_14$Material, sep="")]<-mi_aa_14$mi
mgi_full14$mi_Tot<-sum(mgi_full14[,paste("mi_", mi_aa_14$Material, sep="")])
mgi_full14[,paste("gi20_", gi20_aa_14$Material, sep="")]<-gi20_aa_14$gi20
mgi_full14$gi20_Tot<-sum(mgi_full14[,paste("gi20_", gi20_aa_14$Material, sep="")])
mgi_full14$arch<-14

# MF Mid-Rise: 15-16
mgi_full15<-data.frame(Foundation="Strip",Type="MF",Stories="Multiple",Garage="No",Size="Large",Frame="Podium")
mgi_full15[,paste("mi_", mi_aa_15$Material, sep="")]<-mi_aa_15$mi
mgi_full15$mi_Tot<-sum(mgi_full15[,paste("mi_", mi_aa_15$Material, sep="")])
mgi_full15[,paste("gi20_", gi20_aa_15$Material, sep="")]<-gi20_aa_15$gi20
mgi_full15$gi20_Tot<-sum(mgi_full15[,paste("gi20_", gi20_aa_15$Material, sep="")])
mgi_full15$arch<-15

mgi_full16<-data.frame(Foundation="Strip",Type="MF",Stories="Multiple",Garage="No",Size="Small",Frame="Podium")
mgi_full16[,paste("mi_", mi_aa_16$Material, sep="")]<-mi_aa_16$mi
mgi_full16$mi_Tot<-sum(mgi_full16[,paste("mi_", mi_aa_16$Material, sep="")])
mgi_full16[,paste("gi20_", gi20_aa_16$Material, sep="")]<-gi20_aa_16$gi20
mgi_full16$gi20_Tot<-sum(mgi_full16[,paste("gi20_", gi20_aa_16$Material, sep="")])
mgi_full16$arch<-16

# SF Basement: 17-30
# Arch 17
mgi_full17<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="Yes",Size="Large",Frame="Wood")
mgi_full17[,paste("mi_", mi_aa_17$Material, sep="")]<-mi_aa_17$mi
mgi_full17$mi_Tot<-sum(mgi_full17[,paste("mi_", mi_aa_17$Material, sep="")])
mgi_full17[,paste("gi20_", gi20_aa_17$Material, sep="")]<-gi20_aa_17$gi20
mgi_full17$gi20_Tot<-sum(mgi_full17[,paste("gi20_", gi20_aa_17$Material, sep="")])
mgi_full17$arch<-17
# Arch 24
mgi_full24<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="Yes",Size="Large",Frame="Masonry")
mgi_full24[,paste("mi_", mi_aa_24$Material, sep="")]<-mi_aa_24$mi
mgi_full24$mi_Tot<-sum(mgi_full24[,paste("mi_", mi_aa_24$Material, sep="")])
mgi_full24[,paste("gi20_", gi20_aa_24$Material, sep="")]<-gi20_aa_24$gi20
mgi_full24$gi20_Tot<-sum(mgi_full24[,paste("gi20_", gi20_aa_24$Material, sep="")])
mgi_full24$arch<-24

# Arch 18
mgi_full18<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Wood")
mgi_full18[,paste("mi_", mi_aa_18$Material, sep="")]<-mi_aa_18$mi
mgi_full18$mi_Tot<-sum(mgi_full18[,paste("mi_", mi_aa_18$Material, sep="")])
mgi_full18[,paste("gi20_", gi20_aa_18$Material, sep="")]<-gi20_aa_18$gi20
mgi_full18$gi20_Tot<-sum(mgi_full18[,paste("gi20_", gi20_aa_18$Material, sep="")])
mgi_full18$arch<-18
# Arch 25
mgi_full25<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Masonry")
mgi_full25[,paste("mi_", mi_aa_25$Material, sep="")]<-mi_aa_25$mi
mgi_full25$mi_Tot<-sum(mgi_full25[,paste("mi_", mi_aa_25$Material, sep="")])
mgi_full25[,paste("gi20_", gi20_aa_25$Material, sep="")]<-gi20_aa_25$gi20
mgi_full25$gi20_Tot<-sum(mgi_full25[,paste("gi20_", gi20_aa_25$Material, sep="")])
mgi_full25$arch<-25

# Arch 19
mgi_full19<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="Yes",Size="Small",Frame="Wood")
mgi_full19[,paste("mi_", mi_aa_19$Material, sep="")]<-mi_aa_19$mi
mgi_full19$mi_Tot<-sum(mgi_full19[,paste("mi_", mi_aa_19$Material, sep="")])
mgi_full19[,paste("gi20_", gi20_aa_19$Material, sep="")]<-gi20_aa_19$gi20
mgi_full19$gi20_Tot<-sum(mgi_full19[,paste("gi20_", gi20_aa_19$Material, sep="")])
mgi_full19$arch<-19
# Arch 26
mgi_full26<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="Yes",Size="Small",Frame="Masonry")
mgi_full26[,paste("mi_", mi_aa_26$Material, sep="")]<-mi_aa_26$mi
mgi_full26$mi_Tot<-sum(mgi_full26[,paste("mi_", mi_aa_26$Material, sep="")])
mgi_full26[,paste("gi20_", gi20_aa_26$Material, sep="")]<-gi20_aa_26$gi20
mgi_full26$gi20_Tot<-sum(mgi_full26[,paste("gi20_", gi20_aa_26$Material, sep="")])
mgi_full26$arch<-26

# Arch 20
mgi_full20<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Wood")
mgi_full20[,paste("mi_", mi_aa_20$Material, sep="")]<-mi_aa_20$mi
mgi_full20$mi_Tot<-sum(mgi_full20[,paste("mi_", mi_aa_20$Material, sep="")])
mgi_full20[,paste("gi20_", gi20_aa_20$Material, sep="")]<-gi20_aa_20$gi20
mgi_full20$gi20_Tot<-sum(mgi_full20[,paste("gi20_", gi20_aa_20$Material, sep="")])
mgi_full20$arch<-20
# Arch 27
mgi_full27<-data.frame(Foundation="Basement",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Masonry")
mgi_full27[,paste("mi_", mi_aa_27$Material, sep="")]<-mi_aa_27$mi
mgi_full27$mi_Tot<-sum(mgi_full27[,paste("mi_", mi_aa_27$Material, sep="")])
mgi_full27[,paste("gi20_", gi20_aa_27$Material, sep="")]<-gi20_aa_27$gi20
mgi_full27$gi20_Tot<-sum(mgi_full27[,paste("gi20_", gi20_aa_27$Material, sep="")])
mgi_full27$arch<-27

# Arch 21
mgi_full21<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Frame="Wood")
mgi_full21[,paste("mi_", mi_aa_21$Material, sep="")]<-mi_aa_21$mi
mgi_full21$mi_Tot<-sum(mgi_full21[,paste("mi_", mi_aa_21$Material, sep="")])
mgi_full21[,paste("gi20_", gi20_aa_21$Material, sep="")]<-gi20_aa_21$gi20
mgi_full21$gi20_Tot<-sum(mgi_full21[,paste("gi20_", gi20_aa_21$Material, sep="")])
mgi_full21$arch<-21
# Arch 28
mgi_full28<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Frame="Masonry")
mgi_full28[,paste("mi_", mi_aa_28$Material, sep="")]<-mi_aa_28$mi
mgi_full28$mi_Tot<-sum(mgi_full28[,paste("mi_", mi_aa_28$Material, sep="")])
mgi_full28[,paste("gi20_", gi20_aa_28$Material, sep="")]<-gi20_aa_28$gi20
mgi_full28$gi20_Tot<-sum(mgi_full28[,paste("gi20_", gi20_aa_28$Material, sep="")])
mgi_full28$arch<-28

# Arch 22
mgi_full22<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Wood")
mgi_full22[,paste("mi_", mi_aa_22$Material, sep="")]<-mi_aa_22$mi
mgi_full22$mi_Tot<-sum(mgi_full22[,paste("mi_", mi_aa_22$Material, sep="")])
mgi_full22[,paste("gi20_", gi20_aa_22$Material, sep="")]<-gi20_aa_22$gi20
mgi_full22$gi20_Tot<-sum(mgi_full22[,paste("gi20_", gi20_aa_22$Material, sep="")])
mgi_full22$arch<-22
# Arch 29
mgi_full29<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Masonry")
mgi_full29[,paste("mi_", mi_aa_29$Material, sep="")]<-mi_aa_29$mi
mgi_full29$mi_Tot<-sum(mgi_full29[,paste("mi_", mi_aa_29$Material, sep="")])
mgi_full29[,paste("gi20_", gi20_aa_29$Material, sep="")]<-gi20_aa_29$gi20
mgi_full29$gi20_Tot<-sum(mgi_full29[,paste("gi20_", gi20_aa_29$Material, sep="")])
mgi_full29$arch<-29

# Arch 23
mgi_full23<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Wood")
mgi_full23[,paste("mi_", mi_aa_23$Material, sep="")]<-mi_aa_23$mi
mgi_full23$mi_Tot<-sum(mgi_full23[,paste("mi_", mi_aa_23$Material, sep="")])
mgi_full23[,paste("gi20_", gi20_aa_23$Material, sep="")]<-gi20_aa_23$gi20
mgi_full23$gi20_Tot<-sum(mgi_full23[,paste("gi20_", gi20_aa_23$Material, sep="")])
mgi_full23$arch<-23
# Arch 30
mgi_full30<-data.frame(Foundation="Basement",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Masonry")
mgi_full30[,paste("mi_", mi_aa_30$Material, sep="")]<-mi_aa_30$mi
mgi_full30$mi_Tot<-sum(mgi_full30[,paste("mi_", mi_aa_30$Material, sep="")])
mgi_full30[,paste("gi20_", gi20_aa_30$Material, sep="")]<-gi20_aa_30$gi20
mgi_full30$gi20_Tot<-sum(mgi_full30[,paste("gi20_", gi20_aa_30$Material, sep="")])
mgi_full30$arch<-30

# SF Crawlspace: 31-44
# Arch 31
mgi_full31<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="Yes",Size="Large",Frame="Wood")
mgi_full31[,paste("mi_", mi_aa_31$Material, sep="")]<-mi_aa_31$mi
mgi_full31$mi_Tot<-sum(mgi_full31[,paste("mi_", mi_aa_31$Material, sep="")])
mgi_full31[,paste("gi20_", gi20_aa_31$Material, sep="")]<-gi20_aa_31$gi20
mgi_full31$gi20_Tot<-sum(mgi_full31[,paste("gi20_", gi20_aa_31$Material, sep="")])
mgi_full31$arch<-31
# Arch 38
mgi_full38<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="Yes",Size="Large",Frame="Masonry")
mgi_full38[,paste("mi_", mi_aa_38$Material, sep="")]<-mi_aa_38$mi
mgi_full38$mi_Tot<-sum(mgi_full38[,paste("mi_", mi_aa_38$Material, sep="")])
mgi_full38[,paste("gi20_", gi20_aa_38$Material, sep="")]<-gi20_aa_38$gi20
mgi_full38$gi20_Tot<-sum(mgi_full38[,paste("gi20_", gi20_aa_38$Material, sep="")])
mgi_full38$arch<-38

# Arch 32
mgi_full32<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Wood")
mgi_full32[,paste("mi_", mi_aa_32$Material, sep="")]<-mi_aa_32$mi
mgi_full32$mi_Tot<-sum(mgi_full32[,paste("mi_", mi_aa_32$Material, sep="")])
mgi_full32[,paste("gi20_", gi20_aa_32$Material, sep="")]<-gi20_aa_32$gi20
mgi_full32$gi20_Tot<-sum(mgi_full32[,paste("gi20_", gi20_aa_32$Material, sep="")])
mgi_full32$arch<-32
# Arch 39
mgi_full39<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Masonry")
mgi_full39[,paste("mi_", mi_aa_39$Material, sep="")]<-mi_aa_39$mi
mgi_full39$mi_Tot<-sum(mgi_full39[,paste("mi_", mi_aa_39$Material, sep="")])
mgi_full39[,paste("gi20_", gi20_aa_39$Material, sep="")]<-gi20_aa_39$gi20
mgi_full39$gi20_Tot<-sum(mgi_full39[,paste("gi20_", gi20_aa_39$Material, sep="")])
mgi_full39$arch<-39

# Arch 33
mgi_full33<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="Yes",Size="Small",Frame="Wood")
mgi_full33[,paste("mi_", mi_aa_33$Material, sep="")]<-mi_aa_33$mi
mgi_full33$mi_Tot<-sum(mgi_full33[,paste("mi_", mi_aa_33$Material, sep="")])
mgi_full33[,paste("gi20_", gi20_aa_33$Material, sep="")]<-gi20_aa_33$gi20
mgi_full33$gi20_Tot<-sum(mgi_full33[,paste("gi20_", gi20_aa_33$Material, sep="")])
mgi_full33$arch<-33
# Arch 40
mgi_full40<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="Yes",Size="Small",Frame="Masonry")
mgi_full40[,paste("mi_", mi_aa_40$Material, sep="")]<-mi_aa_40$mi
mgi_full40$mi_Tot<-sum(mgi_full40[,paste("mi_", mi_aa_40$Material, sep="")])
mgi_full40[,paste("gi20_", gi20_aa_40$Material, sep="")]<-gi20_aa_40$gi20
mgi_full40$gi20_Tot<-sum(mgi_full40[,paste("gi20_", gi20_aa_40$Material, sep="")])
mgi_full40$arch<-40

# Arch 34
mgi_full34<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Wood")
mgi_full34[,paste("mi_", mi_aa_34$Material, sep="")]<-mi_aa_34$mi
mgi_full34$mi_Tot<-sum(mgi_full34[,paste("mi_", mi_aa_34$Material, sep="")])
mgi_full34[,paste("gi20_", gi20_aa_34$Material, sep="")]<-gi20_aa_34$gi20
mgi_full34$gi20_Tot<-sum(mgi_full34[,paste("gi20_", gi20_aa_34$Material, sep="")])
mgi_full34$arch<-34
# Arch 41
mgi_full41<-data.frame(Foundation="Crawlspace",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Masonry")
mgi_full41[,paste("mi_", mi_aa_41$Material, sep="")]<-mi_aa_41$mi
mgi_full41$mi_Tot<-sum(mgi_full41[,paste("mi_", mi_aa_41$Material, sep="")])
mgi_full41[,paste("gi20_", gi20_aa_41$Material, sep="")]<-gi20_aa_41$gi20
mgi_full41$gi20_Tot<-sum(mgi_full41[,paste("gi20_", gi20_aa_41$Material, sep="")])
mgi_full41$arch<-41

# Arch 35
mgi_full35<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Frame="Wood")
mgi_full35[,paste("mi_", mi_aa_35$Material, sep="")]<-mi_aa_35$mi
mgi_full35$mi_Tot<-sum(mgi_full35[,paste("mi_", mi_aa_35$Material, sep="")])
mgi_full35[,paste("gi20_", gi20_aa_35$Material, sep="")]<-gi20_aa_35$gi20
mgi_full35$gi20_Tot<-sum(mgi_full35[,paste("gi20_", gi20_aa_35$Material, sep="")])
mgi_full35$arch<-35
# Arch 42
mgi_full42<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="Yes",Size="Large",Frame="Masonry")
mgi_full42[,paste("mi_", mi_aa_42$Material, sep="")]<-mi_aa_42$mi
mgi_full42$mi_Tot<-sum(mgi_full42[,paste("mi_", mi_aa_42$Material, sep="")])
mgi_full42[,paste("gi20_", gi20_aa_42$Material, sep="")]<-gi20_aa_42$gi20
mgi_full42$gi20_Tot<-sum(mgi_full42[,paste("gi20_", gi20_aa_42$Material, sep="")])
mgi_full42$arch<-42

# Arch 36
mgi_full36<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Wood")
mgi_full36[,paste("mi_", mi_aa_36$Material, sep="")]<-mi_aa_36$mi
mgi_full36$mi_Tot<-sum(mgi_full36[,paste("mi_", mi_aa_36$Material, sep="")])
mgi_full36[,paste("gi20_", gi20_aa_36$Material, sep="")]<-gi20_aa_36$gi20
mgi_full36$gi20_Tot<-sum(mgi_full36[,paste("gi20_", gi20_aa_36$Material, sep="")])
mgi_full36$arch<-36
# Arch 43
mgi_full43<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Masonry")
mgi_full43[,paste("mi_", mi_aa_43$Material, sep="")]<-mi_aa_43$mi
mgi_full43$mi_Tot<-sum(mgi_full43[,paste("mi_", mi_aa_43$Material, sep="")])
mgi_full43[,paste("gi20_", gi20_aa_43$Material, sep="")]<-gi20_aa_43$gi20
mgi_full43$gi20_Tot<-sum(mgi_full43[,paste("gi20_", gi20_aa_43$Material, sep="")])
mgi_full43$arch<-43

# Arch 37
mgi_full37<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Wood")
mgi_full37[,paste("mi_", mi_aa_37$Material, sep="")]<-mi_aa_37$mi
mgi_full37$mi_Tot<-sum(mgi_full37[,paste("mi_", mi_aa_37$Material, sep="")])
mgi_full37[,paste("gi20_", gi20_aa_37$Material, sep="")]<-gi20_aa_37$gi20
mgi_full37$gi20_Tot<-sum(mgi_full37[,paste("gi20_", gi20_aa_37$Material, sep="")])
mgi_full37$arch<-37
# Arch 44
mgi_full44<-data.frame(Foundation="Crawlspace",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Masonry")
mgi_full44[,paste("mi_", mi_aa_44$Material, sep="")]<-mi_aa_44$mi
mgi_full44$mi_Tot<-sum(mgi_full44[,paste("mi_", mi_aa_44$Material, sep="")])
mgi_full44[,paste("gi20_", gi20_aa_44$Material, sep="")]<-gi20_aa_44$gi20
mgi_full44$gi20_Tot<-sum(mgi_full44[,paste("gi20_", gi20_aa_44$Material, sep="")])
mgi_full44$arch<-44

# SF Pier and Beam: 45-48 (wood frame only, all excluding garage)
# Arch 45
mgi_full45<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="One",Garage="No",Size="Large",Frame="Wood")
mgi_full45[,paste("mi_", mi_aa_45$Material, sep="")]<-mi_aa_45$mi
mgi_full45$mi_Tot<-sum(mgi_full45[,paste("mi_", mi_aa_45$Material, sep="")])
mgi_full45[,paste("gi20_", gi20_aa_45$Material, sep="")]<-gi20_aa_45$gi20
mgi_full45$gi20_Tot<-sum(mgi_full45[,paste("gi20_", gi20_aa_45$Material, sep="")])
mgi_full45$arch<-45

# Arch 46
mgi_full46<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="One",Garage="No",Size="Small",Frame="Wood")
mgi_full46[,paste("mi_", mi_aa_46$Material, sep="")]<-mi_aa_46$mi
mgi_full46$mi_Tot<-sum(mgi_full46[,paste("mi_", mi_aa_46$Material, sep="")])
mgi_full46[,paste("gi20_", gi20_aa_46$Material, sep="")]<-gi20_aa_46$gi20
mgi_full46$gi20_Tot<-sum(mgi_full46[,paste("gi20_", gi20_aa_46$Material, sep="")])
mgi_full46$arch<-46

# Arch 47
mgi_full47<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="Multiple",Garage="No",Size="Large",Frame="Wood")
mgi_full47[,paste("mi_", mi_aa_47$Material, sep="")]<-mi_aa_47$mi
mgi_full47$mi_Tot<-sum(mgi_full47[,paste("mi_", mi_aa_47$Material, sep="")])
mgi_full47[,paste("gi20_", gi20_aa_47$Material, sep="")]<-gi20_aa_47$gi20
mgi_full47$gi20_Tot<-sum(mgi_full47[,paste("gi20_", gi20_aa_47$Material, sep="")])
mgi_full47$arch<-47

# Arch 48
mgi_full48<-data.frame(Foundation="Pier & Beam",Type="SF",Stories="Multiple",Garage="No",Size="Small",Frame="Wood")
mgi_full48[,paste("mi_", mi_aa_48$Material, sep="")]<-mi_aa_48$mi
mgi_full48$mi_Tot<-sum(mgi_full48[,paste("mi_", mi_aa_48$Material, sep="")])
mgi_full48[,paste("gi20_", gi20_aa_48$Material, sep="")]<-gi20_aa_48$gi20
mgi_full48$gi20_Tot<-sum(mgi_full48[,paste("gi20_", gi20_aa_48$Material, sep="")])
mgi_full48$arch<-48

# MH: 59-60
# Arch 59
mgi_full59<-data.frame(Foundation="Pier & Beam",Type="MH",Stories="One",Garage="No",Size="Large",Frame="Wood")
mgi_full59[,paste("mi_", mi_aa_59$Material, sep="")]<-mi_aa_59$mi
mgi_full59$mi_Tot<-sum(mgi_full59[,paste("mi_", mi_aa_59$Material, sep="")])
mgi_full59[,paste("gi20_", gi20_aa_59$Material, sep="")]<-gi20_aa_59$gi20
mgi_full59$gi20_Tot<-sum(mgi_full59[,paste("gi20_", gi20_aa_59$Material, sep="")])
mgi_full59$arch<-59

# Arch 60
mgi_full60<-data.frame(Foundation="Pier & Beam",Type="MH",Stories="One",Garage="No",Size="Small",Frame="Wood")
mgi_full60[,paste("mi_", mi_aa_60$Material, sep="")]<-mi_aa_60$mi
mgi_full60$mi_Tot<-sum(mgi_full60[,paste("mi_", mi_aa_60$Material, sep="")])
mgi_full60[,paste("gi20_", gi20_aa_60$Material, sep="")]<-gi20_aa_60$gi20
mgi_full60$gi20_Tot<-sum(mgi_full60[,paste("gi20_", gi20_aa_60$Material, sep="")])
mgi_full60$arch<-60

# MF High-rise, 61
mgi_full61<-data.frame(Foundation="Pile",Type="MF",Stories="Multiple",Garage="No",Size="Small",Frame="Masonry")
mgi_full61[,paste("mi_", mi_aa_61$Material, sep="")]<-mi_aa_61$mi
mgi_full61$mi_Tot<-sum(mgi_full61[,paste("mi_", mi_aa_61$Material, sep="")])
mgi_full61[,paste("gi20_", gi20_aa_61$Material, sep="")]<-gi20_aa_61$gi20
mgi_full61$gi20_Tot<-sum(mgi_full61[,paste("gi20_", gi20_aa_61$Material, sep="")])
mgi_full61$arch<-61

# paste together
mgi_full<-bind_rows(mgi_full1,mgi_full2,mgi_full3,mgi_full4,mgi_full5,mgi_full6,mgi_full7,mgi_full8,mgi_full9,mgi_full10,
                   mgi_full11,mgi_full12,mgi_full13,mgi_full14,mgi_full15,mgi_full16,mgi_full17,mgi_full18,mgi_full19,mgi_full20,
                   mgi_full21,mgi_full22,mgi_full23,mgi_full24,mgi_full25,mgi_full26,mgi_full27,mgi_full28,mgi_full29,mgi_full30,
                   mgi_full31,mgi_full32,mgi_full33,mgi_full34,mgi_full35,mgi_full36,mgi_full37,mgi_full38,mgi_full39,mgi_full40,
                   mgi_full41,mgi_full42,mgi_full43,mgi_full44,mgi_full45,mgi_full46,mgi_full47,mgi_full48,
                   mgi_full59,mgi_full60,mgi_full61)
save(mgi_full,file="Material_Intensities/Full_arch_intensities.RData")  
write.csv(mgi_full,file="Material_Intensities/Full_arch_intensities.csv")

mgi_full$Type_Found<-paste(mgi_full$Type,mgi_full$Foundation)
mgi_full[mgi_full$Type_Found=="MF Pile",]$Type_Found<-"MF High-Rise"
mgi_full[mgi_full$Type_Found=="MF Strip",]$Type_Found<-"MF Low-Rise"
mgi_full$`Unit Size`<-mgi_full$Size
# Make SI Figure S9
windows(width=12,height=7)
# breaking out by size and frame type
ggplot(mgi_full,aes(x=Type_Found,y=gi20_Tot)) + geom_point(aes(shape=Frame,col=`Unit Size`),size=2.5) + theme_bw() + ylim(0,500) + 
  scale_color_manual(values=c('blue','red'))+scale_shape_manual(values=c(0, 2, 8)) +
  labs(title = "Embodied GHG Intensities of Archtypes - by Unit Size and Framing Method",y="kgCO2e/m2",x="House and Foundation Type") +
  theme(axis.text=element_text(size=13),axis.title=element_text(size=14,face = "bold"),plot.title = element_text(size = 15, face = "bold"),legend.text=element_text(size=13),legend.title=element_text(size=13)) 

# breaking out by stories and frame type
ggplot(mgi_full,aes(x=Type_Found,y=gi20_Tot)) + geom_point(aes(shape=Frame,col=Stories),size=2.5) + theme_bw() + ylim(0,500) + 
  scale_color_manual(values=c('green4','purple'))+scale_shape_manual(values=c(0, 2, 8)) +
  labs(title = "Embodied GHG Intensities of Archtypes - by Number Stories and Framing Method",y="kgCO2e/m2",x="House and Foundation Type") +
  theme(axis.text=element_text(size=13),axis.title=element_text(size=14,face = "bold"),plot.title = element_text(size = 15, face = "bold"),legend.text=element_text(size=13),legend.title=element_text(size=13))