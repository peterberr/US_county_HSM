# mapping spatial results by counties, states, etc.
# Jan 23 2021

# script to load, analyze, and combine by scenario bs.csv files
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
setwd("~/Yale Courses/Research/Final Paper/HSM_github")
library(dplyr)
library(ggplot2)
library(reshape2)
library(urbnmapr)
library(ggplot2)
library(RColorBrewer)
library(urbnthemes)
library(ggrepel)

set_urbn_defaults(style = "map")

# load("HSM_results/County_FloorArea.RData")
load("HSM_results/County_FloorArea_Mat.RData")
#material flows
MatFl<-smop_base_FA[,1:2]
# MatFl[,c("Dem_SF_m2","Dem_MF_m2", "Dem_MH_m2", "NC_SF_m2", "NC_MF_m2", "NC_MH_m2")]<-0
MatFl[,c("M_Rat_Con20","M_Rat_Stl20","M_Rat_Tot20",
         "M_Rat_Con30","M_Rat_Stl30","M_Rat_Tot30",
         "M_Rat_Con40","M_Rat_Stl40","M_Rat_Tot40",
         "M_Rat_Con50","M_Rat_Stl50","M_Rat_Tot50")]<-0

for (i in 1:nrow(MatFl)) {
  MatFl[i,c("M_Rat_Con20","M_Rat_Stl20","M_Rat_Tot20")]<-smop_base_FA[[3]][[i]][1,c("M_Rat_Con","M_Rat_Stl","M_Rat_Tot")]
  MatFl[i,c("M_Rat_Con30","M_Rat_Stl30","M_Rat_Tot30")]<-smop_base_FA[[3]][[i]][11,c("M_Rat_Con","M_Rat_Stl","M_Rat_Tot")]
  MatFl[i,c("M_Rat_Con40","M_Rat_Stl40","M_Rat_Tot40")]<-smop_base_FA[[3]][[i]][21,c("M_Rat_Con","M_Rat_Stl","M_Rat_Tot")]
  MatFl[i,c("M_Rat_Con50","M_Rat_Stl50","M_Rat_Tot50")]<-smop_base_FA[[3]][[i]][31,c("M_Rat_Con","M_Rat_Stl","M_Rat_Tot")]
}
MatFl[is.infinite(MatFl$M_Rat_Con20),]$M_Rat_Con20<-5
MatFl[is.infinite(MatFl$M_Rat_Con30),]$M_Rat_Con30<-5
MatFl[is.infinite(MatFl$M_Rat_Con40),]$M_Rat_Con40<-5
MatFl[is.infinite(MatFl$M_Rat_Con50),]$M_Rat_Con50<-5

MatFl[is.infinite(MatFl$M_Rat_Stl20),]$M_Rat_Stl20<-5
MatFl[is.infinite(MatFl$M_Rat_Stl30),]$M_Rat_Stl30<-5
MatFl[is.infinite(MatFl$M_Rat_Stl40),]$M_Rat_Stl40<-5
MatFl[is.infinite(MatFl$M_Rat_Stl50),]$M_Rat_Stl50<-5

MatFl[is.infinite(MatFl$M_Rat_Tot20),]$M_Rat_Tot20<-5
MatFl[is.infinite(MatFl$M_Rat_Tot30),]$M_Rat_Tot30<-5
MatFl[is.infinite(MatFl$M_Rat_Tot40),]$M_Rat_Tot40<-5
MatFl[is.infinite(MatFl$M_Rat_Tot50),]$M_Rat_Tot50<-5


# MatFl$lnCon20<-log(MatFl$M_Rat_Con20)
# MatFl[is.infinite(MatFl$lnCon20),]$lnCon20<--2
# MatFl[MatFl$lnCon20>5 ,]$lnCon20<-5
# MatFl[MatFl$lnCon20< -5 ,]$lnCon20<--5
MatFl[MatFl$M_Rat_Con20>6,]$M_Rat_Con20<-6
MatFl[MatFl$M_Rat_Con30>6,]$M_Rat_Con30<-6
MatFl[MatFl$M_Rat_Con40>6,]$M_Rat_Con40<-6
MatFl[MatFl$M_Rat_Con50>6,]$M_Rat_Con50<-6

MatFl[MatFl$M_Rat_Stl20>6,]$M_Rat_Stl20<-6
MatFl[MatFl$M_Rat_Stl30>6,]$M_Rat_Stl30<-6
MatFl[MatFl$M_Rat_Stl40>6,]$M_Rat_Stl40<-6
MatFl[MatFl$M_Rat_Stl50>6,]$M_Rat_Stl50<-6

MatFl[MatFl$M_Rat_Tot20>6,]$M_Rat_Tot20<-6
MatFl[MatFl$M_Rat_Tot30>6,]$M_Rat_Tot30<-6
MatFl[MatFl$M_Rat_Tot40>6,]$M_Rat_Tot40<-6
MatFl[MatFl$M_Rat_Tot50>6,]$M_Rat_Tot50<-6

MatFl[,c("V_Rat20","VF20",
         "V_Rat30","VF30",
         "V_Rat40","VF40",
         "V_Rat50","VF50")]<-0

for (i in 1:nrow(MatFl)) {
  MatFl[i,c("VF20")]<-smop_base_FA[[3]][[i]][1,c("Vacancy_Ratio")]
  MatFl[i,c("VF30")]<-smop_base_FA[[3]][[i]][11,c("Vacancy_Ratio")]
  MatFl[i,c("VF40")]<-smop_base_FA[[3]][[i]][21,c("Vacancy_Ratio")]
  MatFl[i,c("VF50")]<-smop_base_FA[[3]][[i]][31,c("Vacancy_Ratio")]
  
  MatFl$V_Rat20[i]<-(MatFl$VF20[i]-1)/MatFl$VF20[i]
  MatFl$V_Rat30[i]<-(MatFl$VF30[i]-1)/MatFl$VF30[i]
  MatFl$V_Rat40[i]<-(MatFl$VF40[i]-1)/MatFl$VF40[i]
  MatFl$V_Rat50[i]<-(MatFl$VF50[i]-1)/MatFl$VF50[i]
}
MatFl[MatFl$VF20<1,]$VF20<-1

MatFl[MatFl$VF20>6,]$VF20<-6
MatFl[MatFl$VF30>6,]$VF30<-6
MatFl[MatFl$VF40>6,]$VF40<-6
MatFl[MatFl$VF50>6,]$VF50<-6

MatFl[MatFl$V_Rat20<0.05,]$V_Rat20<-0.05
MatFl[MatFl$V_Rat30<0.05,]$V_Rat30<-0.05
MatFl[MatFl$V_Rat40<0.05,]$V_Rat40<-0.05
MatFl[MatFl$V_Rat50<0.05,]$V_Rat50<-0.05

MatFl[MatFl$V_Rat20>0.5,]$V_Rat20<-0.5
MatFl[MatFl$V_Rat30>0.5,]$V_Rat30<-0.5
MatFl[MatFl$V_Rat40>0.5,]$V_Rat40<-0.5
MatFl[MatFl$V_Rat50>0.5,]$V_Rat50<-0.5

MatFl[MatFl$V_Rat20>0.4,]$V_Rat20<-0.4
MatFl[MatFl$V_Rat30>0.4,]$V_Rat30<-0.4
MatFl[MatFl$V_Rat40>0.4,]$V_Rat40<-0.4
MatFl[MatFl$V_Rat50>0.4,]$V_Rat50<-0.4

MatFl[,c("m2c20",
         "m2c30",
         "m2c40",
         "m2c50")]<-0


for (i in 1:nrow(MatFl)) {
  MatFl[i,c("m2c20")]<-smop_base_FA[[3]][[i]][1,c("m2cap")]
  MatFl[i,c("m2c30")]<-smop_base_FA[[3]][[i]][11,c("m2cap")]
  MatFl[i,c("m2c40")]<-smop_base_FA[[3]][[i]][21,c("m2cap")]
  MatFl[i,c("m2c50")]<-smop_base_FA[[3]][[i]][31,c("m2cap")]
}
  
MatFl[MatFl$m2c20<35,]$m2c20<-35
MatFl[MatFl$m2c30<35,]$m2c30<-35
MatFl[MatFl$m2c40<35,]$m2c40<-35
MatFl[MatFl$m2c50<35,]$m2c50<-35

MatFl[MatFl$m2c20>100,]$m2c20<-100
MatFl[MatFl$m2c30>100,]$m2c30<-100
MatFl[MatFl$m2c40>100,]$m2c40<-100
MatFl[MatFl$m2c50>100,]$m2c50<-100
# mapping #####
gg<-get_urbn_map(map = "counties", sf = TRUE)
states_sf <- get_urbn_map(map = "states", sf = TRUE)
spatial_data3 <- right_join(gg,MatFl,by = c("county_fips" ="GeoID"))
MatFl[c(1786,2590,1249,2281),1:2]
sel_cty<-gg[gg$county_fips %in% c('35045','48201','26103','44007'),]
sel_cty$County<-c('Harris, TC','San Juan, NM','Marquette, MI','Providence, RI')

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Con20)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of concrete waste generation to concrete demand, 2020") +
  theme(legend.title = element_text(size = 12),legend.text = element_text(size = 12),legend.key.size = unit(2,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Con20)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
    stat = "sf_coordinates",alpha=0.65,
    min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of concrete waste generation to concrete demand, 2020") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Con30)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
                            # stat = "sf_coordinates",alpha=0.65,
                            # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of concrete waste generation to concrete demand, 2030") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Con40)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
                            # stat = "sf_coordinates",alpha=0.65,
                            # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of concrete waste generation to concrete demand, 2040") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Con50)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
                            # stat = "sf_coordinates",alpha=0.65,
                            # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of concrete waste generation to concrete demand, 2050") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 


# steel ####

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Stl20)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
                            stat = "sf_coordinates",alpha=0.65,
                            min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of steel waste generation to steel demand, 2020") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Stl30)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
  # stat = "sf_coordinates",alpha=0.65,
  # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of steel waste generation to steel demand, 2030") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Stl40)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
  # stat = "sf_coordinates",alpha=0.65,
  # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of steel waste generation to steel demand, 2040") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Stl50)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
  # stat = "sf_coordinates",alpha=0.65,
  # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of steel waste generation to steel demand, 2050") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

# Total ####

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Tot20)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
                            stat = "sf_coordinates",alpha=0.65,
                            min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of total waste material generation to total material demand, 2020") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Tot30)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
  # stat = "sf_coordinates",alpha=0.65,
  # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of total waste material generation to total material demand, 2030") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Tot40)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
  # stat = "sf_coordinates",alpha=0.65,
  # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of total waste material generation to total material demand, 2040") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = M_Rat_Tot50)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  # ggrepel::geom_label_repel(aes(label = sel_cty$County,geometry = sel_cty$geometry),
  # stat = "sf_coordinates",alpha=0.65,
  # min.segment.length = 0,segment.color='red',segment.size=1,label.size = 1) +
  labs(fill = "Ratio") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Ratio of total waste material generation to total material demand, 2050") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 
##############

# vacancy 2020
windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = 100*V_Rat20)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "%   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Vacancy Rate, All House Types, 2020") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 
# vacancy 2030
windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = 100*V_Rat30)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "%   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Vacancy Rate, All House Types, 2030") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = 100*V_Rat40)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "%   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Vacancy Rate, All House Types, 2040") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = 100*V_Rat50)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "%   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Vacancy Rate, All House Types, 2050") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

# m2cap 2020
windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = m2c20)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "m2/cap   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Floor area per capita, 2020") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

# m2cap 2030
windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = m2c30)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "%   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("m2/capita 2030") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 

# m2cap 2040
windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = m2c40)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "%   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("m2/capita 2040") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16))

# m2cap 2050
windows()
ggplot() +
  geom_sf(spatial_data3,mapping = aes(fill = m2c50)) + coord_sf(datum = NA) +
  geom_sf(data = states_sf, fill = NA, color = "#ffffff", size = 0.25) +
  geom_sf(data = sel_cty, fill = NA, color = "#ff0000", size = 0.25) +
  labs(fill = "m2/cap   ") + scale_fill_viridis_c(option = "viridis") + 
  ggtitle("Floor area per capita, 2050") +
  theme(legend.title = element_text(size = 11),legend.text = element_text(size = 10),legend.key.size = unit(1.5,"line"),plot.title = element_text(hjust = 0.5,size=16)) 
