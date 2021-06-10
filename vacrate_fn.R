# Function to calculate vacancy rates for cohorts based on vacancy rates for age ranges
# Peter Berrill Jan 2021

# this function takes an input of a data frame with stock by cohorts in columns, with a given year, and returns the same data frame
# with additional columns indicating the vacancy rate of each cohort, based on exogenous vacancy rates by age ranges.

vacrate_fn <- function(yr1,vacstock,dem_adj,isMH) {
  library(reshape2)

  s2<-vacstock
  s2$Year<-yr1

  s2$pcvac_adj<-s2$vac_rate_adj<-s2$pc61<-s2$pc31.60<-s2$pc11.30<-s2$pc0.10<-s2$ar61<-s2$ar31.60<-s2$ar11.30<-s2$ar0.10<-0
  s<-s2
  cohorts<-unique(s$Cohort)
  
  # looped condensed code
  for (k in 1:length(cohorts)) {
    s[s$Cohort==cohorts[k],]$ar0.10<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(s[s$Cohort==cohorts[k],]$Year-10,s[s$Cohort==cohorts[k],]$Year)))
    s[s$Cohort==cohorts[k],]$ar11.30<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(s[s$Cohort==cohorts[k],]$Year-30,s[s$Cohort==cohorts[k],]$Year-11)))
    s[s$Cohort==cohorts[k],]$ar31.60<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(s[s$Cohort==cohorts[k],]$Year-60,s[s$Cohort==cohorts[k],]$Year-31)))
    s[s$Cohort==cohorts[k],]$ar61<-length(intersect(seq(as.numeric(substr(cohorts[k],1,4)),as.numeric(substr(cohorts[k],6,9))),seq(1890,s[s$Cohort==cohorts[k],]$Year-61)))

  }
  if (isMH==1) {s[1,"ar31.60"]<-0}
  s[,c('pc0.10','pc11.30','pc31.60','pc61')]<-s[,4:7]/rep(colSums(s[,4:7]),each=dim(s)[1]) # calculate what percent of each age range fall into each age cohort
  s$vac_rate_adj<-rowSums(s[,c('pc0.10','pc11.30','pc31.60','pc61')]*rep(dem_adj,each=dim(s)[1])) # calculate the adjustment for each age cohort
  s$pcvac_adj<-s$pcvac+s$vac_rate_adj # calculate the adjusted vacancy rates
  vacstock_ret<-vacstock
  if (!min(s$pcvac_adj)<0) { # clause to avoid implementing the calculated changes if it caused any of the cohorts to have negative vacancy rates
        vacstock_ret$pcvac<-s$pcvac_adj
  }
  vacstock_ret
}
