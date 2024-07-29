# 
rm(list=ls())
load("C:/Users/pb637/Documents/Yale Courses/Research/Final Paper/HSM_github/HSM_results/County_FloorArea.RData")

out<-smop_base_FA[c('GeoID','RS_ID')]
out[1,3:9]<-smop_base_FA[[3]][[1]][1,c(328:330,332:335)]
for (k in 2:3108) {out[k,3:9]=smop_base_FA[[3]][[k]][1,c(328:330,332:335)]}

write.csv(out,file='~/Yale Courses/Research/Final Paper/HSM_github/HSM_results/m2cap_county.csv' ,row.names = FALSE)