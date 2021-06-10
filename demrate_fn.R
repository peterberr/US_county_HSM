# Calculate demolition rates for cohorts based on demolition rates for age ranges
# Peter Berrill Jan 2021

# this function takes an input of a data frame with stock by cohorts in columns, with a given year, and returns the same data frame
# with additional columns indicating the demolition rate of each cohort, based on exogenous demolition rates by age ranges.

demrate_fn <- function(yr0,stock,dem_rates) {
  library(reshape2)
  s2<-data.frame("Cohort"=names(stock),"Units"=as.numeric(stock))
  s2$Year<-yr0+1
  s2$Cohort<-as.character(s2$Cohort)

  s2$ar0.19<-s2$ar20.59<-s2$ar60<-s2$pc0.19<-s2$pc20.59<-s2$pc60<-s2$dem_rate_wtd<-0

  s<-s2
  cohorts<-unique(s$Cohort)
  
  # looped condensed code
  for (k in 1:length(cohorts)) {
    s[s$Cohort==cohorts[k],]$ar0.19<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(s[s$Cohort==cohorts[k],]$Year-19,s[s$Cohort==cohorts[k],]$Year)))
    s[s$Cohort==cohorts[k],]$ar20.59<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(s[s$Cohort==cohorts[k],]$Year-59,s[s$Cohort==cohorts[k],]$Year-20)))
    s[s$Cohort==cohorts[k],]$ar60<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(1890,s[s$Cohort==cohorts[k],]$Year-60)))
    
    s[s$Cohort==cohorts[k],c('pc0.19','pc20.59','pc60')]<-s[s$Cohort==cohorts[k],c('ar0.19','ar20.59','ar60')]/sum(s[s$Cohort==cohorts[k],c('ar0.19','ar20.59','ar60')])
    
    s[s$Cohort==cohorts[k],]$dem_rate_wtd<-sum(as.numeric(s[s$Cohort==cohorts[k],c('pc0.19','pc20.59','pc60')])*dem_rates,na.rm = TRUE)
  }
  # demolition rates by cohort group
  op<-as.data.frame(matrix(s$dem_rate_wtd,1,length(cohorts)))
  names(op)<-paste('dr',cohorts,sep = '_')
  # op$Year<-yr0
  
  op

}
