run_sm <- function(stockModel) { # function to run stock model
  for (i in 1:(dim(stockModel)[1]-1)) {
    # within loop
    ## step 2. calculate demolition rates by age cohort, remember these are demolition over two years, as we converted the demolition rates as such
    # first get the total stock by type and occupancy and age cohort
    sfocc<-stockModel[i,85:90] ; names(sfocc)<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
    sfvac<-stockModel[i,91:96] ; names(sfvac)<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
    mfocc<-stockModel[i,97:102] ; names(mfocc)<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
    mfvac<-stockModel[i,103:108] ; names(mfvac)<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
    mhocc<-stockModel[i,109:114] ; names(mhocc)<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
    mhvac<-stockModel[i,115:120] ; names(mhvac)<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019")
    
    # sf occ dem rates
    stockModel[i,c("Dem_Rate_SF_Occ_p1940", "Dem_Rate_SF_Occ_1940_59", "Dem_Rate_SF_Occ_1960_79", "Dem_Rate_SF_Occ_1980_99","Dem_Rate_SF_Occ_2000_09",
                   "Dem_Rate_SF_Occ_2010_19")]<-demrate_fn(stockModel$Year[i], sfocc,stockModel[i,c("Dem_Rate_SF_0-19Occ","Dem_Rate_SF_20-59Occ","Dem_Rate_SF_60+Occ")])
    # sf vac dem rates
    stockModel[i,c("Dem_Rate_SF_Vac_p1940", "Dem_Rate_SF_Vac_1940_59", "Dem_Rate_SF_Vac_1960_79", "Dem_Rate_SF_Vac_1980_99","Dem_Rate_SF_Vac_2000_09",
                   "Dem_Rate_SF_Vac_2010_19")]<-demrate_fn(stockModel$Year[i], sfvac,stockModel[i,c("Dem_Rate_SF_0-19Vac","Dem_Rate_SF_20-59Vac","Dem_Rate_SF_60+Vac")])
    # mf occ dem rates
    stockModel[i,c("Dem_Rate_MF_Occ_p1940", "Dem_Rate_MF_Occ_1940_59", "Dem_Rate_MF_Occ_1960_79", "Dem_Rate_MF_Occ_1980_99","Dem_Rate_MF_Occ_2000_09",
                   "Dem_Rate_MF_Occ_2010_19")]<-demrate_fn(stockModel$Year[i], mfocc,stockModel[i,c("Dem_Rate_MF_0-19Occ","Dem_Rate_MF_20-59Occ","Dem_Rate_MF_60+Occ")])
    # mf vac dem rates
    stockModel[i,c("Dem_Rate_MF_Vac_p1940", "Dem_Rate_MF_Vac_1940_59", "Dem_Rate_MF_Vac_1960_79", "Dem_Rate_MF_Vac_1980_99","Dem_Rate_MF_Vac_2000_09",
                   "Dem_Rate_MF_Vac_2010_19")]<-demrate_fn(stockModel$Year[i], mfvac,stockModel[i,c("Dem_Rate_MF_0-19Vac","Dem_Rate_MF_20-59Vac","Dem_Rate_MF_60+Vac")])
    # mh occ dem rates
    stockModel[i,c("Dem_Rate_MH_Occ_p1940", "Dem_Rate_MH_Occ_1940_59", "Dem_Rate_MH_Occ_1960_79", "Dem_Rate_MH_Occ_1980_99","Dem_Rate_MH_Occ_2000_09",
                   "Dem_Rate_MH_Occ_2010_19")]<-demrate_fn(stockModel$Year[i], mhocc,stockModel[i,c("Dem_Rate_MH_0-19Occ","Dem_Rate_MH_20-59Occ","Dem_Rate_MH_60+Occ")])
    # mh vac dem rates
    stockModel[i,c("Dem_Rate_MH_Vac_p1940", "Dem_Rate_MH_Vac_1940_59", "Dem_Rate_MH_Vac_1960_79", "Dem_Rate_MH_Vac_1980_99","Dem_Rate_MH_Vac_2000_09",
                   "Dem_Rate_MH_Vac_2010_19")]<-demrate_fn(stockModel$Year[i], mhvac,stockModel[i,c("Dem_Rate_MH_0-19Vac","Dem_Rate_MH_20-59Vac","Dem_Rate_MH_60+Vac")])
    
    ## step 3. calculate absolute demolition values based on rates and existing stock
    stockModel[i,175:186]<-stockModel[i,139:150]*stockModel[i,49:60]*stockModel$Tot_HU_SF[i] # SF dem = SF demrates * SF stock percentages * SF stock
    stockModel[i,187:198]<-stockModel[i,151:162]*stockModel[i,61:72]*stockModel$Tot_HU_MF[i] # MF dem = MF demrates * MF stock percentages * MF stock
    stockModel[i,199:210]<-stockModel[i,163:174]*stockModel[i,73:84]*stockModel$Tot_HU_MH[i] # MH dem = MH demrates * MH stock percentages * MH stock
    # sum to calculate total demolitions by type
    stockModel$Dem_SF[i]<-sum(stockModel[i,175:186])
    stockModel$Dem_MF[i]<-sum(stockModel[i,187:198])
    stockModel$Dem_MH[i]<-sum(stockModel[i,199:210]) 
    # estimate annualized demolition rates
    stockModel$Dem_Rate_SF[i]<-0.5*stockModel$Dem_SF[i]/stockModel$Tot_HU_SF[i] # annualized construction rate, (CON_SF is now the 2-year total construction)
    stockModel$Dem_Rate_MF[i]<-0.5*stockModel$Dem_MF[i]/stockModel$Tot_HU_MF[i] # annualized construction rate, (CON_MF is now the 2-year total construction)
    stockModel$Dem_Rate_MH[i]<-0.5*stockModel$Dem_MH[i]/stockModel$Tot_HU_MH[i] # annualized construction rate, (CON_MH is now the 2-year total construction)
    
    ## step 4. estimate new construction, as a function of occupied housing stock growth, vacancy rate, and demolition rate
    # vacancy-rate adjusted occupied stock growth,, a first estimate for total stock growth
    # SF stock growth
    stockModel$OSG_SF[i]<-stockModel$Occ_HU_SF[i+1]-stockModel$Occ_HU_SF[i]
    stockModel$VOSG_SF[i]<-(stockModel$Occ_HU_SF[i+1]-stockModel$Occ_HU_SF[i])*Vn_SF
    # MF stock growth
    stockModel$OSG_MF[i]<-stockModel$Occ_HU_MF[i+1]-stockModel$Occ_HU_MF[i]
    stockModel$VOSG_MF[i]<-(stockModel$Occ_HU_MF[i+1]-stockModel$Occ_HU_MF[i])*Vn_MF
    # MH stock growth
    stockModel$OSG_MH[i]<-stockModel$Occ_HU_MH[i+1]-stockModel$Occ_HU_MH[i]
    stockModel$VOSG_MH[i]<-(stockModel$Occ_HU_MH[i+1]-stockModel$Occ_HU_MH[i])*Vn_MH
    
    # SF Construction
    if (stockModel$Occ_HU_SF[i+1]<stockModel$Occ_HU_SF[i]) { # if negative growth of occupied housing
      # calculate construction based on linear model of construction. factor two used to convert annual construction to construction over two years
      stockModel$Con_SF[i]<-predict(lmSFc,data.frame(OSG_Rate_SF=0.5*stockModel$OSG_SF[i]/stockModel$Tot_HU_SF[i]))*stockModel$Tot_HU_SF[i]*2
        if (stockModel$Con_SF[i]<0) {stockModel$Con_SF[i]<-0}
      stockModel$Con_Scenario_SF[i]<-"A"
    } else { # if zero or positive growth of occupied housing
      stockModel$H_SF[i]<-predict(lmSF,data.frame(dVR_SF=0.5*(Vn_SF-stockModel$VR_SF[i]))) # optionally reduce the effect of the change in vacancy rate by 0.5 so gap with Vn isn't closed immediately
        if (stockModel$H_SF[i]<0.2) {stockModel$H_SF[i]<-0.2} #  limit H from getting below 0.2 
        if (stockModel$H_SF[i]>1.3) {stockModel$H_SF[i]<-1.3} # also limit H from getting above 1.3
      stockModel$Con_SF[i]<-stockModel$H_SF[i]*stockModel$VOSG_SF[i]+stockModel$Dem_SF[i]
      stockModel$Con_Scenario_SF[i]<-"B"
    }
    stockModel$Con_Rate_SF[i]<-0.5*stockModel$Con_SF[i]/stockModel$Tot_HU_SF[i] # annualized construction rate, (CON_SF is now the 2-year total construction)
    # MF Construction
    if (stockModel$Occ_HU_MF[i+1]<stockModel$Occ_HU_MF[i]) { # if negative growth of occupied housing
      # calculate construction based on linear model of construction. factor two used to convert annual construction to construction over two years
      stockModel$Con_MF[i]<-predict(lmMFc,data.frame(OSG_Rate_MF=0.5*stockModel$OSG_MF[i]/stockModel$Tot_HU_MF[i]))*stockModel$Tot_HU_MF[i]*2
        if (stockModel$Con_MF[i]<0) {stockModel$Con_MF[i]<-0}
      stockModel$Con_Scenario_MF[i]<-"A"
    } else { # if zero or positive growth of occupied housing
      stockModel$H_MF[i]<-predict(lmMF,data.frame(dVR_MF=0.5*(Vn_MF-stockModel$VR_MF[i]))) # optionally reduce the effect of the change in vacancy rate by 0.5 so gap with Vn isn't closed immediately
        if (stockModel$H_MF[i]<0.2) {stockModel$H_MF[i]<-0.2} #  limit H from getting below 0.2 
        if (stockModel$H_MF[i]>1.3) {stockModel$H_MF[i]<-1.3} # also limit H from getting above 1.3 
      stockModel$Con_MF[i]<-stockModel$H_MF[i]*stockModel$VOSG_MF[i]+stockModel$Dem_MF[i]
      stockModel$Con_Scenario_MF[i]<-"B"
    }
    stockModel$Con_Rate_MF[i]<-0.5*stockModel$Con_MF[i]/stockModel$Tot_HU_MF[i] # annualized construction rate, (CON_MF is now the 2-year total construction)
    # MH Construction
    if (stockModel$Occ_HU_MH[i+1]<stockModel$Occ_HU_MH[i]) { # if negative growth of occupied housing
      # calculate construction based on linear model of construction. factor two used to convert annual construction to construction over two years
      stockModel$Con_MH[i]<-predict(lmMHc,data.frame(OSG_Rate_MH=0.5*stockModel$OSG_MH[i]/stockModel$Tot_HU_MH[i]))*stockModel$Tot_HU_MH[i]*2
        if (stockModel$Con_MH[i]<0) {stockModel$Con_MH[i]<-0}
      stockModel$Con_Scenario_MH[i]<-"A"
    } else { # if zero or positive growth of occupied housing
      stockModel$H_MH[i]<-predict(lmMH,data.frame(dVR_MH=0.5*(Vn_MH-stockModel$VR_MH[i]))) # optionally reduce the effect of the change in vacancy rate by 0.5 so gap with Vn isn't closed immediately
        if (stockModel$H_MH[i]<0.2) {stockModel$H_MH[i]<-0.2} #  limit H from getting below 0.2
        if (stockModel$H_MH[i]>1.3) {stockModel$H_MH[i]<-1.3} # also limit H from getting above 1.3
      stockModel$Con_MH[i]<-stockModel$H_MH[i]*stockModel$VOSG_MH[i]+stockModel$Dem_MH[i]
      stockModel$Con_Scenario_MH[i]<-"B"
    }
    stockModel$Con_Rate_MH[i]<-0.5*stockModel$Con_MH[i]/stockModel$Tot_HU_MH[i] # annualized construction rate, (CON_MH is now the 2-year total construction)
    # step 5. calculate total housing units in the next year
    stockModel$Tot_HU_SF[i+1]<-stockModel$Tot_HU_SF[i]+stockModel$Con_SF[i]-stockModel$Dem_SF[i]
    stockModel$Tot_HU_MF[i+1]<-stockModel$Tot_HU_MF[i]+stockModel$Con_MF[i]-stockModel$Dem_MF[i]
    stockModel$Tot_HU_MH[i+1]<-stockModel$Tot_HU_MH[i]+stockModel$Con_MH[i]-stockModel$Dem_MH[i]
    # step 6. calculate new vacancy rate
    stockModel$VR_SF[i+1]<-stockModel$Tot_HU_SF[i+1]/stockModel$Occ_HU_SF[i+1]
    stockModel$VR_MF[i+1]<-stockModel$Tot_HU_MF[i+1]/stockModel$Occ_HU_MF[i+1]
    stockModel$VR_MH[i+1]<-stockModel$Tot_HU_MH[i+1]/stockModel$Occ_HU_MH[i+1]
    # step 7. split total, occ, and vacant stock in subsequent timestep for each type into age cohorts
    # first need to calculate stock, construction, and demolition by age cohort in current timestep
    # Stock
    sfp40<-stockModel$Tot_HU_SF_Occ_p1940[i]+stockModel$Tot_HU_SF_Vac_p1940[i]
    sf4059<-stockModel$Tot_HU_SF_Occ_1940_59[i]+stockModel$Tot_HU_SF_Vac_1940_59[i]
    sf6079<-stockModel$Tot_HU_SF_Occ_1960_79[i]+stockModel$Tot_HU_SF_Vac_1960_79[i]
    sf8099<-stockModel$Tot_HU_SF_Occ_1980_99[i]+stockModel$Tot_HU_SF_Vac_1980_99[i]
    sf0009<-stockModel$Tot_HU_SF_Occ_2000_09[i]+stockModel$Tot_HU_SF_Vac_2000_09[i]
    sf1019<-stockModel$Tot_HU_SF_Occ_2010_19[i]+stockModel$Tot_HU_SF_Vac_2010_19[i]
    
    mfp40<-stockModel$Tot_HU_MF_Occ_p1940[i]+stockModel$Tot_HU_MF_Vac_p1940[i]
    mf4059<-stockModel$Tot_HU_MF_Occ_1940_59[i]+stockModel$Tot_HU_MF_Vac_1940_59[i]
    mf6079<-stockModel$Tot_HU_MF_Occ_1960_79[i]+stockModel$Tot_HU_MF_Vac_1960_79[i]
    mf8099<-stockModel$Tot_HU_MF_Occ_1980_99[i]+stockModel$Tot_HU_MF_Vac_1980_99[i]
    mf0009<-stockModel$Tot_HU_MF_Occ_2000_09[i]+stockModel$Tot_HU_MF_Vac_2000_09[i]
    mf1019<-stockModel$Tot_HU_MF_Occ_2010_19[i]+stockModel$Tot_HU_MF_Vac_2010_19[i]
    
    mhp40<-stockModel$Tot_HU_MH_Occ_p1940[i]+stockModel$Tot_HU_MH_Vac_p1940[i]
    mh4059<-stockModel$Tot_HU_MH_Occ_1940_59[i]+stockModel$Tot_HU_MH_Vac_1940_59[i]
    mh6079<-stockModel$Tot_HU_MH_Occ_1960_79[i]+stockModel$Tot_HU_MH_Vac_1960_79[i]
    mh8099<-stockModel$Tot_HU_MH_Occ_1980_99[i]+stockModel$Tot_HU_MH_Vac_1980_99[i]
    mh0009<-stockModel$Tot_HU_MH_Occ_2000_09[i]+stockModel$Tot_HU_MH_Vac_2000_09[i]
    mh1019<-stockModel$Tot_HU_MH_Occ_2010_19[i]+stockModel$Tot_HU_MH_Vac_2010_19[i]
    # Demolition 
    sfdemp40<-stockModel$Dem_SF_Occ_p1940[i]+stockModel$Dem_SF_Vac_p1940[i]
    sfdem4059<-stockModel$Dem_SF_Occ_1940_59[i]+stockModel$Dem_SF_Vac_1940_59[i]
    sfdem6079<-stockModel$Dem_SF_Occ_1960_79[i]+stockModel$Dem_SF_Vac_1960_79[i]
    sfdem8099<-stockModel$Dem_SF_Occ_1980_99[i]+stockModel$Dem_SF_Vac_1980_99[i]
    sfdem0009<-stockModel$Dem_SF_Occ_2000_09[i]+stockModel$Dem_SF_Vac_2000_09[i]
    sfdem1019<-stockModel$Dem_SF_Occ_2010_19[i]+stockModel$Dem_SF_Vac_2010_19[i]
    
    mfdemp40<-stockModel$Dem_MF_Occ_p1940[i]+stockModel$Dem_MF_Vac_p1940[i]
    mfdem4059<-stockModel$Dem_MF_Occ_1940_59[i]+stockModel$Dem_MF_Vac_1940_59[i]
    mfdem6079<-stockModel$Dem_MF_Occ_1960_79[i]+stockModel$Dem_MF_Vac_1960_79[i]
    mfdem8099<-stockModel$Dem_MF_Occ_1980_99[i]+stockModel$Dem_MF_Vac_1980_99[i]
    mfdem0009<-stockModel$Dem_MF_Occ_2000_09[i]+stockModel$Dem_MF_Vac_2000_09[i]
    mfdem1019<-stockModel$Dem_MF_Occ_2010_19[i]+stockModel$Dem_MF_Vac_2010_19[i]
    
    mhdemp40<-stockModel$Dem_MH_Occ_p1940[i]+stockModel$Dem_MH_Vac_p1940[i]
    mhdem4059<-stockModel$Dem_MH_Occ_1940_59[i]+stockModel$Dem_MH_Vac_1940_59[i]
    mhdem6079<-stockModel$Dem_MH_Occ_1960_79[i]+stockModel$Dem_MH_Vac_1960_79[i]
    mhdem8099<-stockModel$Dem_MH_Occ_1980_99[i]+stockModel$Dem_MH_Vac_1980_99[i]
    mhdem0009<-stockModel$Dem_MH_Occ_2000_09[i]+stockModel$Dem_MH_Vac_2000_09[i]
    mhdem1019<-stockModel$Dem_MH_Occ_2010_19[i]+stockModel$Dem_MH_Vac_2010_19[i]
    # Construction
    # First construction which isn't construction, but returns to housing stock of residential units which had been unfit, or used for business, or MH moving in, etc.
    sfconp40<-(sfp40/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon4059<-(sf4059/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon6079<-(sf6079/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon8099<-(sf8099/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon0009<-(sf0009/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon1019<-(sf1019/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    
    mfconp40<-(mfp40/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon4059<-(mf4059/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon6079<-(mf6079/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon8099<-(mf8099/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon0009<-(mf0009/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon1019<-(mf1019/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    
    mhconp40<-(mhp40/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon4059<-(mh4059/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon6079<-(mh6079/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon8099<-(mh8099/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon0009<-(mh0009/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon1019<-(mh1019/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    
    # half of new construction allocated to the cohort which year fits into. The other half will be allocated to the cohort which year+1 fits into
    # 1980-1999
    if (stockModel$Year[i]>1979 & stockModel$Year[i]<2000) {
      sfcon8099<-sfcon8099+stockModel$Con_SF[i]*0.5*(1-SF_growth_returns_pc) 
      mfcon8099<-mfcon8099+stockModel$Con_MF[i]*0.5*(1-MF_growth_returns_pc)  
      mhcon8099<-mhcon8099+stockModel$Con_MH[i]*0.5*(1-MH_growth_returns_pc)   
    }
    if (stockModel$Year[i]+1>1979 & stockModel$Year[i]+1<2000) {
      sfcon8099<-sfcon8099+stockModel$Con_SF[i]*0.5*(1-SF_growth_returns_pc)  
      mfcon8099<-mfcon8099+stockModel$Con_MF[i]*0.5*(1-MF_growth_returns_pc)   
      mhcon8099<-mhcon8099+stockModel$Con_MH[i]*0.5*(1-MH_growth_returns_pc)    
    }
    # 2000-2009
    if (stockModel$Year[i]>1999 & stockModel$Year[i]<2010) {
      sfcon0009<-sfcon0009+stockModel$Con_SF[i]*0.5*(1-SF_growth_returns_pc)  
      mfcon0009<-mfcon0009+stockModel$Con_MF[i]*0.5*(1-MF_growth_returns_pc)   
      mhcon0009<-mhcon0009+stockModel$Con_MH[i]*0.5*(1-MH_growth_returns_pc)    
    }
    if (stockModel$Year[i]+1>1999 & stockModel$Year[i]+1<2010) {
      sfcon0009<-sfcon0009+stockModel$Con_SF[i]*0.5*(1-SF_growth_returns_pc)  
      mfcon0009<-mfcon0009+stockModel$Con_MF[i]*0.5*(1-MF_growth_returns_pc)   
      mhcon0009<-mhcon0009+stockModel$Con_MH[i]*0.5*(1-MH_growth_returns_pc)    
    }
    # 2010-2019
    if (stockModel$Year[i]>2009 & stockModel$Year[i]<2020) {
      sfcon1019<-sfcon1019+stockModel$Con_SF[i]*0.5*(1-SF_growth_returns_pc)  
      mfcon1019<-mfcon1019+stockModel$Con_MF[i]*0.5*(1-MF_growth_returns_pc)   
      mhcon1019<-mhcon1019+stockModel$Con_MH[i]*0.5*(1-MH_growth_returns_pc)    
    }
    if (stockModel$Year[i]+1>2009 & stockModel$Year[i]+1<2020) {
      sfcon1019<-sfcon1019+stockModel$Con_SF[i]*0.5*(1-SF_growth_returns_pc)  
      mfcon1019<-mfcon1019+stockModel$Con_MF[i]*0.5*(1-MF_growth_returns_pc)   
      mhcon1019<-mhcon1019+stockModel$Con_MH[i]*0.5*(1-MH_growth_returns_pc)    
    }
    
    sfp40_next<-sfp40+sfconp40-sfdemp40
    sf4059_next<-sf4059+sfcon4059-sfdem4059
    sf6079_next<-sf6079+sfcon6079-sfdem6079
    sf8099_next<-sf8099+sfcon8099-sfdem8099
    sf0009_next<-sf0009+sfcon0009-sfdem0009
    sf1019_next<-sf1019+sfcon1019-sfdem1019
    
    mfp40_next<-mfp40+mfconp40-mfdemp40
    mf4059_next<-mf4059+mfcon4059-mfdem4059
    mf6079_next<-mf6079+mfcon6079-mfdem6079
    mf8099_next<-mf8099+mfcon8099-mfdem8099
    mf0009_next<-mf0009+mfcon0009-mfdem0009
    mf1019_next<-mf1019+mfcon1019-mfdem1019
    
    mhp40_next<-mhp40+mhconp40-mhdemp40
    mh4059_next<-mh4059+mhcon4059-mhdem4059
    mh6079_next<-mh6079+mhcon6079-mhdem6079
    mh8099_next<-mh8099+mhcon8099-mhdem8099
    mh0009_next<-mh0009+mhcon0009-mhdem0009
    mh1019_next<-mh1019+mhcon1019-mhdem1019
    
    # calculate SF units by occupancy and age cohort in next timestep
    # generate first estimate of pc of total SF units that are vacant by age cohort, by applying stock average vacancy rate to the total SF units by age cohort
    sf_pcvacest_ac<-data.frame(Cohort=names(sfocc),
                               pcvac=c(sfp40_next,sf4059_next,sf6079_next,sf8099_next,sf0009_next,sf1019_next)*((stockModel$VR_SF[i+1]-1)/stockModel$VR_SF[i+1])/stockModel$Tot_HU_SF[i+1])
    # adjust vacancies by age cohort based on differences in vacancies by age ranges
    sf_pcvac_ac<-vacrate_fn(stockModel$Year[i+1],sf_pcvacest_ac,dem_adj_SF,0)
    # total units by age cohort
    sf_pcTU_ac<-data.frame(Cohort=names(sfocc),pcTU=c(sfp40_next,sf4059_next,sf6079_next,sf8099_next,sf0009_next,sf1019_next)/stockModel$Tot_HU_SF[i+1])
    # occupied units by age cohort
    sf_pcocc_ac<-data.frame(Cohort=names(sfocc),pcocc=sf_pcTU_ac$pcTU-sf_pcvac_ac$pcvac)
    # put calculated values of pc occ and vac by age cohort into the model data frame
    stockModel[i+1,49:54]<-sf_pcocc_ac$pcocc
    stockModel[i+1,55:60]<-sf_pcvac_ac$pcvac
    
    # calculate MF units by occupancy and age cohort in next timestep
    # generate first estimate of pc of total MF units that are vacant by age cohort, by applying stock average vacancy rate to the total MF units by age cohort
    mf_pcvacest_ac<-data.frame(Cohort=names(mfocc),
                               pcvac=c(mfp40_next,mf4059_next,mf6079_next,mf8099_next,mf0009_next,mf1019_next)*((stockModel$VR_MF[i+1]-1)/stockModel$VR_MF[i+1])/stockModel$Tot_HU_MF[i+1])
    # adjust vacancies by age cohort based on differences in vacancies by age ranges
    mf_pcvac_ac<-vacrate_fn(stockModel$Year[i+1],mf_pcvacest_ac,dem_adj_MF,0)
    # total units by age cohort
    mf_pcTU_ac<-data.frame(Cohort=names(mfocc),pcTU=c(mfp40_next,mf4059_next,mf6079_next,mf8099_next,mf0009_next,mf1019_next)/stockModel$Tot_HU_MF[i+1])
    # occupied units by age cohort
    mf_pcocc_ac<-data.frame(Cohort=names(mfocc),pcocc=mf_pcTU_ac$pcTU-mf_pcvac_ac$pcvac)
    # put calculated values of pc occ and vac by age cohort into the model data frame
    stockModel[i+1,61:66]<-mf_pcocc_ac$pcocc
    stockModel[i+1,67:72]<-mf_pcvac_ac$pcvac
    
    # calculate MH units by occupancy and age cohort in next timestep
    # generate first estimate of pc of total MH units that are vacant by age cohort, by applying stock average vacancy rate to the total MH units by age cohort
    mh_pcvacest_ac<-data.frame(Cohort=names(mhocc),
                               pcvac=c(mhp40_next,mh4059_next,mh6079_next,mh8099_next,mh0009_next,mh1019_next)*((stockModel$VR_MH[i+1]-1)/stockModel$VR_MH[i+1])/stockModel$Tot_HU_MH[i+1])
    # adjust vacancies by age cohort based on differences in vacancies by age ranges
    mh_pcvac_ac<-vacrate_fn(stockModel$Year[i+1],mh_pcvacest_ac,dem_adj_MH,1)
    # total units by age cohort
    mh_pcTU_ac<-data.frame(Cohort=names(mhocc),pcTU=c(mhp40_next,mh4059_next,mh6079_next,mh8099_next,mh0009_next,mh1019_next)/stockModel$Tot_HU_MH[i+1])
    # occupied units by age cohort
    mh_pcocc_ac<-data.frame(Cohort=names(mhocc),pcocc=mh_pcTU_ac$pcTU-mh_pcvac_ac$pcvac)
    # put calculated values of pc occ and vac by age cohort into the model data frame
    stockModel[i+1,73:78]<-mh_pcocc_ac$pcocc
    stockModel[i+1,79:84]<-mh_pcvac_ac$pcvac
    
    # finally calculate occ and vac units by age cohort for each type in the model data frame
    stockModel[i+1,85:96]<-stockModel$Tot_HU_SF[i+1]*stockModel[i+1,49:60] # for SF homes
    stockModel[i+1,97:108]<-stockModel$Tot_HU_MF[i+1]*stockModel[i+1,61:72] # for MF homes
    stockModel[i+1,109:120]<-stockModel$Tot_HU_MH[i+1]*stockModel[i+1,73:84] # for MH homes
    
    stockModel$Tot_Hous_Units[i+1]<-sum(stockModel[i+1,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")])
    stockModel$Vacancy_Ratio[i+1]<- stockModel$Tot_Hous_Units[i+1]/ stockModel$Occ_Hous_Units[i+1]
    
    
  } 
  stockModel
} # end of stock model function