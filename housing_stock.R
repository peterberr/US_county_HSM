# script to make summaries of total and occupied housing stock in 2019 from ACS tables.
# all tables accessed from data.census.gov
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
setwd("C:/Users/pb637/Documents/Yale Courses/Research/Final Paper/HSM_github")
library(dplyr)

stcd<-read.delim('~/Yale Courses/Research/US Housing/statecodes.txt', header = TRUE, sep = "|")
stcd<-stcd[,1:3] # keep only state abbreviation and state name
colnames(stcd)<-c("STATE_ID","STUSAB","STATE_NAME")

# get data for total housing stock from DP04 1-yr, 5-yr, and 1-yr state tables  ###############
# accessed from data.census.gov
htot2019<-read.csv("~/Yale Courses/Research/US Housing/ACS/DP04_2019/ACSDP5Y2019.DP04_data_with_overlays_2020-12-20T002309.csv") # 5 year version, with data for all counties
htot2019$id<-substr(htot2019$GEO_ID,nchar(htot2019$GEO_ID)-4,nchar(htot2019$GEO_ID))
htot2019$StID<-substr(htot2019$id,1,2)
# remove margins of error and percentages
name<-names(htot2019)
rmmar<-which(substr(name,nchar(name),nchar(name))=="M")
htot2019<-htot2019[,-rmmar]
name<-names(htot2019)
rmmar<-which(substr(name,nchar(name)-1,nchar(name))=="PE")
htot2019<-htot2019[,-rmmar]

htot2019<-htot2019[,c(dim(htot2019)[2]-1,dim(htot2019)[2],2:(dim(htot2019)[2]-2))] # reorder columns, remove long GeoID
n<-as.character(htot2019[1,])
htot2019<-htot2019[,c(1:3,9:29)]
n<-as.character(htot2019[1,])
htot2019[1,]<-sub("Estimate!!","",n)
n<-as.character(htot2019[1,])
htot2019[1,]<-gsub("!!","_",n)
n<-as.character(htot2019[1,])
names(htot2019)<-n
htot2019<-htot2019[-1,]
names(htot2019)[1:3]<-c("GeoID","StID","County,State")
htot2019<-htot2019[1:3142,]
htot2019[htot2019$GeoID==35013,]$`County,State`<-"Dona Ana County, New Mexico"
htot2019[htot2019$GeoID==22059,]$`County,State`<-"La Salle Parish, Louisiana"
# get state abbreviation and county name
htot2019$StID_num<-as.numeric(htot2019$StID)
htot2019<-merge(htot2019,stcd,by.x = "StID_num",by.y = "STATE_ID")
htot2019$County<-sub(",.*","",htot2019$`County,State`)
# add the ResStock compatible county ID
htot2019$RS_ID<-paste(htot2019$STUSAB,", ",htot2019$County,sep="")
ctycode<-htot2019[,c("GeoID","RS_ID")]
htot2019[,5:25]<-as.numeric(unlist(htot2019[,5:25]))
save(ctycode,file="Intermediate_results/ctycode.RData")

# read in up to date data for housing stock in 2019 based on 1yr survey
# data only available for larger population counties
htot2019_1yr<-read.csv("~/Yale Courses/Research/US Housing/ACS/DP04_2019/ACSDP1Y2019.DP04_data_with_overlays_2020-12-20T002309.csv") 
htot2019_1yr$id<-substr(htot2019_1yr$GEO_ID,nchar(htot2019_1yr$GEO_ID)-4,nchar(htot2019_1yr$GEO_ID))
htot2019_1yr$StID<-substr(htot2019_1yr$id,1,2)
# remove margins of error and percentages
name2<-names(htot2019_1yr)
rmmar<-which(substr(name2,nchar(name2),nchar(name2))=="M")
htot2019_1yr<-htot2019_1yr[,-rmmar]
name2<-names(htot2019_1yr)
rmmar<-which(substr(name2,nchar(name2)-1,nchar(name2))=="PE")
htot2019_1yr<-htot2019_1yr[,-rmmar]

htot2019_1yr<-htot2019_1yr[,c(dim(htot2019_1yr)[2]-1,dim(htot2019_1yr)[2],2:(dim(htot2019_1yr)[2]-2))]
n2<-as.character(htot2019_1yr[1,])
htot2019_1yr<-htot2019_1yr[,c(1:3,9:29)]
n2<-as.character(htot2019_1yr[1,])
htot2019_1yr[1,]<-sub("Estimate!!","",n2)
n2<-as.character(htot2019_1yr[1,])
htot2019_1yr[1,]<-gsub("!!","_",n2)
n2<-as.character(htot2019_1yr[1,])
names(htot2019_1yr)<-n2
htot2019_1yr<-htot2019_1yr[-1,]
names(htot2019_1yr)[1:3]<-c("GeoID","StID","County,State")
htot2019_1yr[htot2019_1yr$GeoID==35013,]$`County,State`<-"Dona Ana County, New Mexico"
htot2019_1yr[htot2019_1yr$GeoID==22059,]$`County,State`<-"La Salle Parish, Louisiana" # doesn't exist in 1yr data file
htot2019_1yr<-htot2019_1yr[htot2019_1yr$StID<57,] # remove PR counties
htot2019_1yr[,4:24]<-as.numeric(unlist(htot2019_1yr[,4:24]))
htot2019_1yr<-na.omit(htot2019_1yr)
h19m<-htot2019_1yr[,c(2,4:24)] %>% group_by(StID) %>% summarize_all(funs(sum)) # sum by state, major counties only

# do same for state totals
htot2019_1yrSt<-read.csv("~/Yale Courses/Research/US Housing/ACS/DP04_2019/State/ACSDP1Y2019.DP04_data_with_overlays_2020-12-22T091243.csv") 
htot2019_1yrSt$id<-substr(htot2019_1yrSt$GEO_ID,nchar(htot2019_1yrSt$GEO_ID)-1,nchar(htot2019_1yrSt$GEO_ID))
# remove margins of error and percentages
name<-names(htot2019_1yrSt)
rmmar<-which(substr(name,nchar(name),nchar(name))=="M")
htot2019_1yrSt<-htot2019_1yrSt[,-rmmar]
name<-names(htot2019_1yrSt)
rmmar<-which(substr(name,nchar(name)-1,nchar(name))=="PE")
htot2019_1yrSt<-htot2019_1yrSt[,-rmmar]
n3<-as.character(htot2019_1yrSt[1,])

htot2019_1yrSt<-htot2019_1yrSt[,c(146,2,8:28)]
n3<-as.character(htot2019_1yrSt[1,])
htot2019_1yrSt[1,]<-sub("Estimate!!","",n3)
n3<-as.character(htot2019_1yrSt[1,])
htot2019_1yrSt[1,]<-gsub("!!","_",n3)
n3<-as.character(htot2019_1yrSt[1,])
names(htot2019_1yrSt)<-n3
htot2019_1yrSt<-htot2019_1yrSt[-1,]
names(htot2019_1yrSt)[1:2]<-c("GeoID","State")
htot2019_1yrSt<-htot2019_1yrSt[1:51,] # remove PR
htot2019_1yrSt[,3:23]<-as.numeric(unlist(htot2019_1yrSt[,3:23]))

# calculate sum for 'other' (Smaller) counties by state
h19o<-h19m
h19o[,2:22]<-htot2019_1yrSt[,3:23]-h19m[,2:22] # state totals excluding major counties, based on 1yr calc
m<-unique(htot2019_1yr$GeoID)
h19o_det<-htot2019[!htot2019$GeoID %in% m,] # remove the major counties
# h19o_det<-h19o_det[,-c(6,8,9,seq(10,48,2))] # remove percentage columns
h19o_st_5yr<-h19o_det[,c(3,5:25)] %>% group_by(StID) %>% summarize_all(funs(sum)) # state totals excluding the major counties
h19o_det_pc<-h19o_det

# tot unit pc only per each small county, of small county total
h19o_det_pc[,6:25]<-h19o_det[,6:25]/h19o_det[,5] # percent of total housing units, across row. col 4 is total housing units
for (i in 1:dim(h19o_det_pc)[1]) { for (j in 1:dim(h19o_st_5yr)[1]) {
  if (h19o_det_pc$StID[i]==h19o_st_5yr$StID[j]) {
    h19o_det_pc[i,5]<-h19o_det[i,5]/h19o_st_5yr[j,2]
  }
}
}
h19o_abs<-h19o_det_pc
for (i in 1:dim(h19o_det_pc)[1]) { for (j in 1:dim(h19o)[1]) {
  if (h19o_abs$StID[i]==h19o$StID[j]) {
    h19o_abs[i,5]<-h19o_det_pc[i,5]*h19o[j,2] # scale county totals to 'actual' 1yr total small county.
  }
}
}
h19o_abs[,6:25]<-h19o_abs[,6:25]*h19o_abs[,5] # scale across row based on tot units value

h19t<-bind_rows(h19o_abs[,2:25],htot2019_1yr) # join the small and major counties back together. 
h19t<-h19t[order(h19t$GeoID),] # order by GeoID
h19t[,4:24]<-round(h19t[,4:24]) # remove decimal places
h19t<-merge(h19t,ctycode)


# get data for occupied housing stock from B25127 1-yr, 5-yr, and 1-yr state tables  ###############
hocc2019<-read.csv("~/Yale Courses/Research/US Housing/ACS/B25127_2019/ACSDT5Y2019.B25127_data_with_overlays_2020-12-21T184915.csv")
hocc2019$id<-substr(hocc2019$GEO_ID,nchar(hocc2019$GEO_ID)-4,nchar(hocc2019$GEO_ID))
hocc2019$StID<-substr(hocc2019$id,1,2)
name<-names(hocc2019)
rmmar<-which(substr(name,nchar(name),nchar(name))=="M")
hocc2019<-hocc2019[,-rmmar]
hocc2019<-hocc2019[,c(dim(hocc2019)[2]-1,dim(hocc2019)[2],2:(dim(hocc2019)[2]-2))]
n<-as.character(hocc2019[1,])
# keep<-c(2,3,5,7,9,11.13
# hocc2019$id<-substr(hocc2019$id,nchar(hocc2019$id)-4,nchar(hocc2019$id)-3)
# colnames(hocc2019)[1:3]<-c("GeoID","StID","County,State")
hocc2019[1,6:47]<-substr(n[6:47],19,nchar(n[6:47]))
hocc2019[1,49:90]<-substr(n[49:90],19,nchar(n[49:90]))
n<-as.character(hocc2019[1,])
hocc2019[1,]<-sub("1, detached  or attached","1",n)
n<-as.character(hocc2019[1,])
hocc2019[1,]<-sub("Mobile home, boat, RV, van, etc.","Mobile home, etc.",n)
n<-as.character(hocc2019[1,])
hocc2019[1,]<-gsub(":!!","_",n)
n<-as.character(hocc2019[1,])
hocc2019[1,]<-gsub(":","",n)
n<-as.character(hocc2019[1,])
names(hocc2019)<-n
hocc2019<-hocc2019[-1,]
names(hocc2019)[1:3]<-c("GeoID","StID","County,State")
hocc2019<-hocc2019[1:3142,]
hocc2019[hocc2019$GeoID==35013,]$`County,State`<-"Dona Ana County, New Mexico"
hocc2019[hocc2019$GeoID==22059,]$`County,State`<-"La Salle Parish, Louisiana"
hocc2019[,4:90]<-as.numeric(unlist(hocc2019[,4:90]))

hocc2019_1yr<-read.csv("~/Yale Courses/Research/US Housing/ACS/B25127_2019/ACSDT1Y2019.B25127_data_with_overlays_2020-12-21T184915.csv")
hocc2019_1yr$id<-substr(hocc2019_1yr$GEO_ID,nchar(hocc2019_1yr$GEO_ID)-4,nchar(hocc2019_1yr$GEO_ID))
hocc2019_1yr$StID<-substr(hocc2019_1yr$id,1,2)
name2<-names(hocc2019_1yr)
rmmar<-which(substr(name2,nchar(name2),nchar(name2))=="M")
hocc2019_1yr<-hocc2019_1yr[,-rmmar]
hocc2019_1yr<-hocc2019_1yr[,c(dim(hocc2019_1yr)[2]-1,dim(hocc2019_1yr)[2],2:(dim(hocc2019_1yr)[2]-2))]
n2<-as.character(hocc2019_1yr[1,])
hocc2019_1yr[1,6:47]<-substr(n2[6:47],19,nchar(n2[6:47]))
hocc2019_1yr[1,49:90]<-substr(n2[49:90],19,nchar(n2[49:90]))
n2<-as.character(hocc2019_1yr[1,])
hocc2019_1yr[1,]<-sub("1, detached  or attached","1",n2)
n2<-as.character(hocc2019_1yr[1,])
hocc2019_1yr[1,]<-sub("Mobile home, boat, RV, van, etc.","Mobile home, etc.",n2)
n2<-as.character(hocc2019_1yr[1,])
hocc2019_1yr[1,]<-gsub(":!!","_",n2)
n2<-as.character(hocc2019_1yr[1,])
hocc2019_1yr[1,]<-gsub(":","",n2)
n2<-as.character(hocc2019_1yr[1,])
names(hocc2019_1yr)<-n2
hocc2019_1yr<-hocc2019_1yr[-1,]
names(hocc2019_1yr)[1:3]<-c("GeoID","StID","County,State")
hocc2019_1yr[hocc2019_1yr$GeoID==35013,]$`County,State`<-"Dona Ana County, New Mexico"
hocc2019_1yr[hocc2019_1yr$GeoID==22059,]$`County,State`<-"La Salle Parish, Louisiana" # doen'st exist in 1 year data
# hocc2019_1yr[,3:90]<-gsub('null',NA,hocc2019_1yr[,3:90])
for (k in 3:90) {hocc2019_1yr[,k]<-gsub('null',NA,hocc2019_1yr[,k])}
hocc2019_1yr<-na.omit(hocc2019_1yr) # remove rows without full details, leaving only the counties fully described. this knocks out a lot of rows
hocc2019_1yr[,4:90]<-as.numeric(unlist(hocc2019_1yr[,4:90]))
hocc2019_1yr<-hocc2019_1yr[hocc2019_1yr$StID<57,]

ho19m<-hocc2019_1yr[,c(2,4:90)] %>% group_by(StID) %>% summarize_all(funs(sum)) # sum by state, major counties only
#ho19m2<-hocc2019_1yr[,c(2,4:90)] %>% group_by(StID) %>% summarize_all(list(sum)) # sum by state, major counties only alternative that dplyr prefers
miss<-unique(hocc2019$StID[!hocc2019$StID %in% ho19m$StID]) # which states were completely excluded in the major county count?
ho19m[(nrow(ho19m)+1):(nrow(ho19m)+length(miss)),]<-ho19m[nrow(ho19m),]
ho19m[(nrow(ho19m)-length(miss)+1):nrow(ho19m),1]<-miss # add in the extra states which were missed out, for completeness
ho19m[(nrow(ho19m)-length(miss)+1):nrow(ho19m),2:88]<-0 # their values are zero however
ho19m<-ho19m[order(ho19m$StID),] # order by GeoID

# read in state totals of occupied housing by T-C
hocc2019_1yrSt<-read.csv("~/Yale Courses/Research/US Housing/ACS/B25127_2019/State/ACSDT1Y2019.B25127_data_with_overlays_2020-12-22T070337.csv")
hocc2019_1yrSt$id<-substr(hocc2019_1yrSt$GEO_ID,nchar(hocc2019_1yrSt$GEO_ID)-1,nchar(hocc2019_1yrSt$GEO_ID))
hocc2019_1yrSt<-hocc2019_1yrSt[,c(177,2:176)]

name<-names(hocc2019_1yrSt)
rmmar<-which(substr(name,nchar(name),nchar(name))=="M")
hocc2019_1yrSt<-hocc2019_1yrSt[,-rmmar]
n3<-as.character(hocc2019_1yrSt[1,])
hocc2019_1yrSt[1,5:46]<-substr(n3[5:46],19,nchar(n3[5:46]))
hocc2019_1yrSt[1,48:89]<-substr(n3[48:89],19,nchar(n3[48:89]))
n3<-as.character(hocc2019_1yrSt[1,])

hocc2019_1yrSt[1,]<-sub("1, detached  or attached","1",n3)
n3<-as.character(hocc2019_1yrSt[1,])
hocc2019_1yrSt[1,]<-sub("Mobile home, boat, RV, van, etc.","Mobile home, etc.",n3)
n3<-as.character(hocc2019_1yrSt[1,])
hocc2019_1yrSt[1,]<-gsub(":!!","_",n3)
n3<-as.character(hocc2019_1yrSt[1,])
hocc2019_1yrSt[1,]<-gsub(":","",n3)
n3<-as.character(hocc2019_1yrSt[1,])
names(hocc2019_1yrSt)<-n3
hocc2019_1yrSt<-hocc2019_1yrSt[-1,]
names(hocc2019_1yrSt)[1:2]<-c("GeoID","State")
hocc2019_1yrSt<-hocc2019_1yrSt[1:51,] # remove PR

# calculate sum for 'other' (Smaller) counties by state
ho19o<-ho19m
all.equal(names(hocc2019_1yrSt[3:89]),names(ho19m)[2:88])
hocc2019_1yrSt[,3:89]<-as.numeric(unlist(hocc2019_1yrSt[,3:89]))
ho19o[,2:88]<-hocc2019_1yrSt[,3:89]-ho19m[,2:88] # state totals excluding major counties, based on 1yr calc
m<-unique(hocc2019_1yr$GeoID) # which are the 'major' counties included in the 1yr data?
ho19o_det<-hocc2019[!hocc2019$GeoID %in% m,] # extract the 5yr data for the 'other' (non-major) counties
ho19o_st_5yr<-ho19o_det[,c(2,4:90)] %>% group_by(StID) %>% summarize_all(funs(sum)) # sum by state, other counties
ho19o_det_pc<-ho19o_det

ho19o_det_pc<-ho19o_det

# tot unit pc only per each small county, of small county total
ho19o_det_pc[,5:90]<-ho19o_det[,5:90]/ho19o_det[,4] # percent of total housing units, across row, other counties
# loop to calculate the pc population of each other county, of the other county state totals
for (i in 1:dim(ho19o_det_pc)[1]) { for (j in 1:dim(ho19o_st_5yr)[1]) {
  if (ho19o_det_pc$StID[i]==ho19o_st_5yr$StID[j]) {
    ho19o_det_pc[i,4]<-ho19o_det[i,4]/ho19o_st_5yr[j,2]
  }
}
}  
ho19o_abs<-ho19o_det_pc
# scale county totals to 'actual' 1yr total small county, based on state other county totals in 2019 1-yr
for (i in 1:dim(ho19o_abs)[1]) { for (j in 1:dim(ho19o)[1]) {
    if (ho19o_abs$StID[i]==ho19o$StID[j]) {
      ho19o_abs[i,4]<-ho19o_det_pc[i,4]*ho19o[j,2] 
    }
  }
}


ho19o_abs[,5:90]<-ho19o_abs[,5:90]*ho19o_abs[,4] # scale across row based on tot units value
ho19t<-bind_rows(ho19o_abs,hocc2019_1yr) # join the small and major counties back together
ho19t<-ho19t[order(ho19t$GeoID),] # order by GeoID
ho19t[,4:90]<-round(ho19t[,4:90]) # remove decimal places. total matrix of occupied housing by T-C, split by tenure (owned/rented)

# now add together owned/rented ############
# types
h19t$SF<-h19t$`UNITS IN STRUCTURE_Total housing units_1-unit, detached`+h19t$`UNITS IN STRUCTURE_Total housing units_1-unit, attached`
# I think I can just add up all MF, because previously, they were all added up anyway, in pch18, and HSMtoRS
h19t$MF<-h19t$`UNITS IN STRUCTURE_Total housing units_2 units`+h19t$`UNITS IN STRUCTURE_Total housing units_3 or 4 units`+
  h19t$`UNITS IN STRUCTURE_Total housing units_5 to 9 units`+h19t$`UNITS IN STRUCTURE_Total housing units_10 to 19 units`+
  h19t$`UNITS IN STRUCTURE_Total housing units_20 or more units`
h19t$MH<-h19t$`UNITS IN STRUCTURE_Total housing units_Mobile home`+h19t$`UNITS IN STRUCTURE_Total housing units_Boat, RV, van, etc.`
# cohorts
h19t$Cpre40<-h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1939 or earlier`
h19t$C40_59<-h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1940 to 1949`+h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1950 to 1959`
h19t$C60_79<-h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1960 to 1969`+h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1970 to 1979`
h19t$C80_99<-h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1980 to 1989`+h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 1990 to 1999`
h19t$C2000<-h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 2000 to 2009`
h19t$C2010<-h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 2010 to 2013`+h19t$`YEAR STRUCTURE BUILT_Total housing units_Built 2014 or later`

h19<-h19t[,-c(5:24)] # extract only the variables of total housing units we need
h19<-h19[,c(1:3,5,4,6:14)]
names(h19)[5]<-"Total_HU"
# add together owned and rented occupied housing data to get total occupied, nested T-C, from b25127
ho19t$SF_pre40<-ho19t$`Owner occupied_Built 1939 or earlier_1`+ho19t$`Renter occupied_Built 1939 or earlier_1`
ho19t$SF_4059<-ho19t$`Owner occupied_Built 1940 to 1959_1`+ho19t$`Renter occupied_Built 1940 to 1959_1`
ho19t$SF_6079<-ho19t$`Owner occupied_Built 1960 to 1979_1`+ho19t$`Renter occupied_Built 1960 to 1979_1`
ho19t$SF_8099<-ho19t$`Owner occupied_Built 1980 to 1999_1`+ho19t$`Renter occupied_Built 1980 to 1999_1`
ho19t$SF_2000<-ho19t$`Owner occupied_Built 2000 to 2009_1`+ho19t$`Renter occupied_Built 2000 to 2009_1`
ho19t$SF_2010<-ho19t$`Owner occupied_Built 2010 or later_1`+ho19t$`Renter occupied_Built 2010 or later_1`

ho19t$MF_pre40<-ho19t$`Owner occupied_Built 1939 or earlier_2 to 4`+ho19t$`Owner occupied_Built 1939 or earlier_5 to 19`+
  ho19t$`Owner occupied_Built 1939 or earlier_20 to 49`+ho19t$`Owner occupied_Built 1939 or earlier_50 or more`+
  ho19t$`Renter occupied_Built 1939 or earlier_2 to 4`+ho19t$`Renter occupied_Built 1939 or earlier_5 to 19`+
  ho19t$`Renter occupied_Built 1939 or earlier_20 to 49`+ho19t$`Renter occupied_Built 1939 or earlier_50 or more`
ho19t$MF_4059<-ho19t$`Owner occupied_Built 1940 to 1959_2 to 4`+ho19t$`Owner occupied_Built 1940 to 1959_5 to 19`+
  ho19t$`Owner occupied_Built 1940 to 1959_20 to 49`+ho19t$`Owner occupied_Built 1940 to 1959_50 or more`+
  ho19t$`Renter occupied_Built 1940 to 1959_2 to 4`+ho19t$`Renter occupied_Built 1940 to 1959_5 to 19`+
  ho19t$`Renter occupied_Built 1940 to 1959_20 to 49`+ho19t$`Renter occupied_Built 1940 to 1959_50 or more`
ho19t$MF_6079<-ho19t$`Owner occupied_Built 1960 to 1979_2 to 4`+ho19t$`Owner occupied_Built 1960 to 1979_5 to 19`+
  ho19t$`Owner occupied_Built 1960 to 1979_20 to 49`+ho19t$`Owner occupied_Built 1960 to 1979_50 or more`+
  ho19t$`Renter occupied_Built 1960 to 1979_2 to 4`+ho19t$`Renter occupied_Built 1960 to 1979_5 to 19`+
  ho19t$`Renter occupied_Built 1960 to 1979_20 to 49`+ho19t$`Renter occupied_Built 1960 to 1979_50 or more`
ho19t$MF_8099<-ho19t$`Owner occupied_Built 1980 to 1999_2 to 4`+ho19t$`Owner occupied_Built 1980 to 1999_5 to 19`+
  ho19t$`Owner occupied_Built 1980 to 1999_20 to 49`+ho19t$`Owner occupied_Built 1980 to 1999_50 or more`+
  ho19t$`Renter occupied_Built 1980 to 1999_2 to 4`+ho19t$`Renter occupied_Built 1980 to 1999_5 to 19`+
  ho19t$`Renter occupied_Built 1980 to 1999_20 to 49`+ho19t$`Renter occupied_Built 1980 to 1999_50 or more`
ho19t$MF_2000<-ho19t$`Owner occupied_Built 2000 to 2009_2 to 4`+ho19t$`Owner occupied_Built 2000 to 2009_5 to 19`+
  ho19t$`Owner occupied_Built 2000 to 2009_20 to 49`+ho19t$`Owner occupied_Built 2000 to 2009_50 or more`+
  ho19t$`Renter occupied_Built 2000 to 2009_2 to 4`+ho19t$`Renter occupied_Built 2000 to 2009_5 to 19`+
  ho19t$`Renter occupied_Built 2000 to 2009_20 to 49`+ho19t$`Renter occupied_Built 2000 to 2009_50 or more`
ho19t$MF_2010<-ho19t$`Owner occupied_Built 2010 or later_2 to 4`+ho19t$`Owner occupied_Built 2010 or later_5 to 19`+
  ho19t$`Owner occupied_Built 2010 or later_20 to 49`+ho19t$`Owner occupied_Built 2010 or later_50 or more`+
  ho19t$`Renter occupied_Built 2010 or later_2 to 4`+ho19t$`Renter occupied_Built 2010 or later_5 to 19`+
  ho19t$`Renter occupied_Built 2010 or later_20 to 49`+ho19t$`Renter occupied_Built 2010 or later_50 or more`

ho19t$MH_pre40<-ho19t$`Owner occupied_Built 1939 or earlier_Mobile home, etc.`+ho19t$`Renter occupied_Built 1939 or earlier_Mobile home, etc.`
ho19t$MH_4059<-ho19t$`Owner occupied_Built 1940 to 1959_Mobile home, etc.`+ho19t$`Renter occupied_Built 1940 to 1959_Mobile home, etc.`
ho19t$MH_6079<-ho19t$`Owner occupied_Built 1960 to 1979_Mobile home, etc.`+ho19t$`Renter occupied_Built 1960 to 1979_Mobile home, etc.`
ho19t$MH_8099<-ho19t$`Owner occupied_Built 1980 to 1999_Mobile home, etc.`+ho19t$`Renter occupied_Built 1980 to 1999_Mobile home, etc.`
ho19t$MH_2000<-ho19t$`Owner occupied_Built 2000 to 2009_Mobile home, etc.`+ho19t$`Renter occupied_Built 2000 to 2009_Mobile home, etc.`
ho19t$MH_2010<-ho19t$`Owner occupied_Built 2010 or later_Mobile home, etc.`+ho19t$`Renter occupied_Built 2010 or later_Mobile home, etc.`


# sum up types over all cohorts
ho19t$SF<-ho19t$SF_pre40+ho19t$SF_4059+ho19t$SF_6079+ho19t$SF_8099+ho19t$SF_2000+ho19t$SF_2010
ho19t$MF<-ho19t$MF_pre40+ho19t$MF_4059+ho19t$MF_6079+ho19t$MF_8099+ho19t$MF_2000+ho19t$MF_2010
ho19t$MH<-ho19t$MH_pre40+ho19t$MH_4059+ho19t$MH_6079+ho19t$MH_8099+ho19t$MH_2000+ho19t$MH_2010

# balance based on 2019 type totals in table s2504
ho19t$SF<-round(ho19t$SF*(84051726/sum(ho19t$SF))) # sum for SF
ho19t$MF<-round(ho19t$MF*(31952644/sum(ho19t$MF)))# sum for MF
ho19t$MH<-round(ho19t$MH*(6798482/sum(ho19t$MH))) # sum for MH

ho19t<-ho19t[,c(1:4,91:111)]
names(ho19t)[4]<-"Tot_OU"
# save occupied housing stock df
save(ho19t,file= 'Intermediate_results/OccHousing2019.RData')
# write.csv(ho19t,'OccHousStock2019new.csv')

# finally add the data on occupied homes by T-C nested to the df (h18) of total homes by type and cohort separately
h19[,15:33]<-ho19t[,4:22]

# Now, define total balanced housing stock by tc for 2019, h19tb, using RAS procedure ##################
h19tb<-h19
# calculate sums of occupied houses by type, to be RASsed to total houses by typa
h19tb$SFSum<-h19tb$SF_2010+h19tb$SF_2000+h19tb$SF_8099+h19tb$SF_6079+h19tb$SF_4059+h19tb$SF_pre40
h19tb$MFSum<-h19tb$MF_2010+h19tb$MF_2000+h19tb$MF_8099+h19tb$MF_6079+h19tb$MF_4059+h19tb$MF_pre40
h19tb$MHSum<-h19tb$MH_2010+h19tb$MH_2000+h19tb$MH_8099+h19tb$MH_6079+h19tb$MH_4059+h19tb$MH_pre40

# calculate sums of occupied houses by cohort, to be RASsed to total houses by cohort
h19tb$Cpre40Sum<-h19tb$SF_pre40+h19tb$MF_pre40+h19tb$MH_pre40
h19tb$C4059Sum<-h19tb$SF_4059+h19tb$MF_4059+h19tb$MH_4059
h19tb$C6079Sum<-h19tb$SF_6079+h19tb$MF_6079+h19tb$MH_6079
h19tb$C8099Sum<-h19tb$SF_8099+h19tb$MF_8099+h19tb$MH_8099
h19tb$C2000Sum<-h19tb$SF_2000+h19tb$MF_2000+h19tb$MH_2000
h19tb$C2010Sum<-h19tb$SF_2010+h19tb$MF_2010+h19tb$MH_2010
W<-rep(0,3142)
# make adjustment for case where 0 total housing and non-zero occupied housing. only for types, not cohorts
for (y in 6:8) { for (x in 1:3142) { 
  if(h19tb[x,y]==0 & !h19tb[x,y+28]==0) {
    W[x]<-1; # only an issue in Sierra Cty CA
    h19tb[x,y]<-h19tb[x,y+28];
    h19tb[x,9:14]<-h19tb[x,9:14]+h19tb[x, 6*(y-0)+(-20:-15)] # distribute type difference based on cohort-type distribution
  }
}
}
W<-rep(0,3142)
# adjustment for the case of non zero total units and zero occupied units, which will produce infinite ratios
for (y in 6:8) { for (x in 1:3142) { 
  if(!h19tb[x,y]==0 & h19tb[x,y+28]==0) {
    W[x]<-1;
    h19tb[x,y+28]<-1; # 19 counties affected by this adjustment
    h19tb[x,6*(y-0)+(-17)]<-1 # allocate the new 1 unit to the 80-99 cohort
  }
}
}
W<-rep(0,3142)
# adjustment for the case of non zero total and zero occupied, which will produce infinite ratios. need to do for cohorts too
for (y in 9:14) { for (x in 1:3142) { 
  if(!h19tb[x,y]==0 & h19tb[x,y+28]==0) {
    W[x]<-1; # 22 counties affected here
    h19tb[x,y+28]<-1;
    h19tb[x,y+7]<-1 # allocate the new 1 unit to SF
  }
}
}

# Calculate balanced total housing units, defined by the greater of type/cohort sums
if (sum(h19tb[,9:14])>=sum(h19tb[,6:8])) {
  h19tb$Total_HU<-h19tb$Cpre40+h19tb$C40_59+h19tb$C60_79+h19tb$C80_99+h19tb$C2000+h19tb$C2010
} else {
  h19tb$Total_HU<-h19tb$SF+h19tb$MF+h19tb$MH
}
# check that these totals are similar to DP04 totals
sum(h19tb$Tot_OU)
sum(h19tb$Total_HU)

# calculate ratio of target sum by type to actual sum by type
h19tb$SF_r<-h19tb$SF/h19tb$SFSum
h19tb$MF_r<-h19tb$MF/h19tb$MFSum
h19tb$MH_r<-h19tb$MH/h19tb$MHSum
# turn any NaN (e.g. from 0/0) into 1
h19tb$SF_r[which(is.na(h19tb$SF_r))]<-1
h19tb$MF_r[which(is.na(h19tb$MF_r))]<-1
h19tb$MH_r[which(is.na(h19tb$MH_r))]<-1
# calculate ratio of target sum by cohort to actual sum by cohort
h19tb$rp40<-h19tb$Cpre40/h19tb$Cpre40Sum
h19tb$r4059<-h19tb$C40_59/h19tb$C4059Sum
h19tb$r6079<-h19tb$C60_79/h19tb$C6079Sum
h19tb$r8099<-h19tb$C80_99/h19tb$C8099Sum
h19tb$r2000<-h19tb$C2000/h19tb$C2000Sum
h19tb$r2010<-h19tb$C2010/h19tb$C2010Sum
# turn any NaN (e.g. from 0/0) into 1
h19tb$rp40[which(is.na(h19tb$rp40))]<-1
h19tb$r4059[which(is.na(h19tb$r4059))]<-1
h19tb$r6079[which(is.na(h19tb$r6079))]<-1
h19tb$r8099[which(is.na(h19tb$r8099))]<-1
h19tb$r2000[which(is.na(h19tb$r2000))]<-1
h19tb$r2010[which(is.na(h19tb$r2010))]<-1
rownames(h19tb)<-1:3142

# Define new data frame in which to store the balanced RAS data
h19tb_new<-h19tb 
# first define columns for each type group 
colSF<-c(11:16)
colMF<-c(17:22)
colMH<-c(23:28)
# now define columns for each cohort group 
colpre40<-c(11,17,23)
col4059<-colpre40+1
col6079<-col4059+1
col8099<-col6079+1
col2000<-col8099+1
col2010<-col2000+1

# function to RASS ##############
for (n in 1:3142) { print(n) # for each county
# n=1
a<-h19tb[n,6:51] # numeric data, excl Total HU
for (q in 1:10) {  #q is number of times to balance.
# q=1
  # add new row [i] on which to do balancing
  i=2*q
  a[i,]<-a[i-1,]
  a[i,colSF]<-a$SF_r[i]*a[i-1,colSF]
  a[i,colMF]<-a$MF_r[i]*a[i-1,colMF]
  a[i,colMH]<-a$MH_r[i]*a[i-1,colMH]
  # now recalc cohort sums 
  a$Cpre40Sum[i]<-sum(a[i,colpre40])
  a$C4059Sum[i]<-sum(a[i,col4059])
  a$C6079Sum[i]<-sum(a[i,col6079])
  a$C8099Sum[i]<-sum(a[i,col8099])
  a$C2000Sum[i]<-sum(a[i,col2000])
  a$C2010Sum[i]<-sum(a[i,col2010])
  # calculate new cohort ratios
  a$rp40[i]<-a$Cpre40[i]/a$Cpre40Sum[i]
  a$r4059[i]<-a$C40_59[i]/a$C4059Sum[i]
  a$r6079[i]<-a$C60_79[i]/a$C6079Sum[i]
  a$r8099[i]<-a$C80_99[i]/a$C8099Sum[i]
  a$r2000[i]<-a$C2000[i]/a$C2000Sum[i]
  a$r2010[i]<-a$C2010[i]/a$C2010Sum[i]
  a[i,41:46][which(is.na(a[i,41:46]))]<-1
  
  # new row, i
  i=2*q+1
  a[i,]<-a[i-1,]
  # now multiply each t-c combo by the cohort ratio
  a[i,colpre40]<-a$rp40[i]*a[i-1,colpre40]
  a[i,col4059]<-a$r4059[i]*a[i-1,col4059]
  a[i,col6079]<-a$r6079[i]*a[i-1,col6079]
  a[i,col8099]<-a$r8099[i]*a[i-1,col8099]
  a[i,col2000]<-a$r2000[i]*a[i-1,col2000]
  a[i,col2010]<-a$r2010[i]*a[i-1,col2010]

  # now recalc type sums 
  a$SFSum[i]<-sum(a[i,colSF])
  a$MFSum[i]<-sum(a[i,colMF])
  a$MHSum[i]<-sum(a[i,colMH])
  
  # and recalc type sum ratios
  a$SF_r[i]<-a$SF[i]/a$SFSum[i]
  a$MF_r[i]<-a$MF[i]/a$MFSum[i]
  a$MH_r[i]<-a$MH[i]/a$MHSum[i]
  a[i,38:40][which(is.na(a[i,38:40]))]<-1 # turn any NaN (e.g. from 0/0) into 1
  # and start all over again! 
}
h19tb_new[n,6:51]<-a[dim(a)[1],1:46] # add data for balanced county n into balanced data frame
} # end of loop

h19tb_new[,16:42]<-round(h19tb_new[,16:42]) # get rid of decimal places
# # sum, this is for later when I am finalzing the number of occupied houses. If occupied>total, occupied <- total
h19tb_new$TotSumUnits<-h19tb_new$Cpre40Sum+h19tb_new$C4059Sum+h19tb_new$C6079Sum+h19tb_new$C8099Sum+h19tb_new$C2000Sum+h19tb_new$C2010Sum
h19tb_new$CheckSum<-h19tb_new$TotSumUnits/h19tb_new$Total_HU # compare the cohort sum with the reported total sum
# just to make sure, but should already be dealt with above
h19tb_new[h19tb_new$GeoID==35013,]$`County,State`<-"Dona Ana County, New Mexico"
h19tb_new[h19tb_new$GeoID==22059,]$`County,State`<-"La Salle Parish, Louisiana"
save(h19tb_new,file= "Intermediate_results/TotalHousing2019.RData")
# write.csv(h19tb_new,'TotHousStock2019new.csv')
