run_sm_cty <- function(stockModel) { # begin large function of stock model
  for (i in 1:(dim(stockModel)[1]-1)) {
  # for (i in 1:15) {
    # within loop
    ## step 2. calculate demolition rates by age cohort, remember these are demolition over two years, as we converted the demolition rates as such
    # first get the total stock by type and occupancy and age cohort
    cohort_yrs<-c("1890.1939","1940.1959", "1960.1979" ,"1980.1999","2000.2009","2010.2019","2020.2029","2030.2039","2040.2049","2050.2059")
    sfocc<-stockModel[i,110:119] ; names(sfocc)<-cohort_yrs
    sfvac<-stockModel[i,120:129] ; names(sfvac)<-cohort_yrs
    mfocc<-stockModel[i,130:139] ; names(mfocc)<-cohort_yrs
    mfvac<-stockModel[i,140:149] ; names(mfvac)<-cohort_yrs
    mhocc<-stockModel[i,150:159] ; names(mhocc)<-cohort_yrs
    mhvac<-stockModel[i,160:169] ; names(mhvac)<-cohort_yrs
    
    # sf occ dem rates
    stockModel[i,c("Dem_Rate_SF_Occ_p1940", "Dem_Rate_SF_Occ_1940_59", "Dem_Rate_SF_Occ_1960_79", "Dem_Rate_SF_Occ_1980_99","Dem_Rate_SF_Occ_2000_09",
                   "Dem_Rate_SF_Occ_2010_19","Dem_Rate_SF_Occ_2020_29","Dem_Rate_SF_Occ_2030_39","Dem_Rate_SF_Occ_2040_49","Dem_Rate_SF_Occ_2050_60")]<-
      demrate_fn(stockModel$Year[i], sfocc,stockModel[i,c("Dem_Rate_SF_0-19Occ","Dem_Rate_SF_20-59Occ","Dem_Rate_SF_60+Occ")])
    # sf vac dem rates
    stockModel[i,c("Dem_Rate_SF_Vac_p1940", "Dem_Rate_SF_Vac_1940_59", "Dem_Rate_SF_Vac_1960_79", "Dem_Rate_SF_Vac_1980_99","Dem_Rate_SF_Vac_2000_09",
                   "Dem_Rate_SF_Vac_2010_19","Dem_Rate_SF_Vac_2020_29","Dem_Rate_SF_Vac_2030_39","Dem_Rate_SF_Vac_2040_49","Dem_Rate_SF_Vac_2050_60")]<-
      demrate_fn(stockModel$Year[i], sfvac,stockModel[i,c("Dem_Rate_SF_0-19Vac","Dem_Rate_SF_20-59Vac","Dem_Rate_SF_60+Vac")])
    # mf occ dem rates
    stockModel[i,c("Dem_Rate_MF_Occ_p1940", "Dem_Rate_MF_Occ_1940_59", "Dem_Rate_MF_Occ_1960_79", "Dem_Rate_MF_Occ_1980_99","Dem_Rate_MF_Occ_2000_09",
                   "Dem_Rate_MF_Occ_2010_19","Dem_Rate_MF_Occ_2020_29","Dem_Rate_MF_Occ_2030_39","Dem_Rate_MF_Occ_2040_49","Dem_Rate_MF_Occ_2050_60")]<-
      demrate_fn(stockModel$Year[i], mfocc,stockModel[i,c("Dem_Rate_MF_0-19Occ","Dem_Rate_MF_20-59Occ","Dem_Rate_MF_60+Occ")])
    # mf vac dem rates
    stockModel[i,c("Dem_Rate_MF_Vac_p1940", "Dem_Rate_MF_Vac_1940_59", "Dem_Rate_MF_Vac_1960_79", "Dem_Rate_MF_Vac_1980_99","Dem_Rate_MF_Vac_2000_09",
                   "Dem_Rate_MF_Vac_2010_19","Dem_Rate_MF_Vac_2020_29","Dem_Rate_MF_Vac_2030_39","Dem_Rate_MF_Vac_2040_49","Dem_Rate_MF_Vac_2050_60")]<-
      demrate_fn(stockModel$Year[i], mfvac,stockModel[i,c("Dem_Rate_MF_0-19Vac","Dem_Rate_MF_20-59Vac","Dem_Rate_MF_60+Vac")])
    # mh occ dem rates
    stockModel[i,c("Dem_Rate_MH_Occ_p1940", "Dem_Rate_MH_Occ_1940_59", "Dem_Rate_MH_Occ_1960_79", "Dem_Rate_MH_Occ_1980_99","Dem_Rate_MH_Occ_2000_09",
                   "Dem_Rate_MH_Occ_2010_19","Dem_Rate_MH_Occ_2020_29","Dem_Rate_MH_Occ_2030_39","Dem_Rate_MH_Occ_2040_49","Dem_Rate_MH_Occ_2050_60")]<-
      demrate_fn(stockModel$Year[i], mhocc,stockModel[i,c("Dem_Rate_MH_0-19Occ","Dem_Rate_MH_20-59Occ","Dem_Rate_MH_60+Occ")])
    # mh vac dem rates
    stockModel[i,c("Dem_Rate_MH_Vac_p1940", "Dem_Rate_MH_Vac_1940_59", "Dem_Rate_MH_Vac_1960_79", "Dem_Rate_MH_Vac_1980_99","Dem_Rate_MH_Vac_2000_09",
                   "Dem_Rate_MH_Vac_2010_19","Dem_Rate_MH_Vac_2020_29","Dem_Rate_MH_Vac_2030_39","Dem_Rate_MH_Vac_2040_49","Dem_Rate_MH_Vac_2050_60")]<-
      demrate_fn(stockModel$Year[i], mhvac,stockModel[i,c("Dem_Rate_MH_0-19Vac","Dem_Rate_MH_20-59Vac","Dem_Rate_MH_60+Vac")])
  
    
    ## step 3. calculate absolute demolition values based on rates and existing stock. 
    Vn_SF<-stockModel$Vn_SF[1]
    Vn_MF<-stockModel$Vn_MF[1]
    Vn_MH<-stockModel$Vn_MH[1]
    # Add a dependency on VR here in case of neg OSG? With neg OSG and VR<0.94Vn, reduce demolition rates so that demolition doesn't continue to outpace construction, leading to an ever declining VR.
    # This ends up being too severe for some locations, like LA (MH only) where when stock growth turns negative instead of positive, demolition rates suddenly decline a lot, which is not desirable. Thus I lowered the cutoff to 0.93*Vn, and made the reductino 0.75 instead of 0.66
    if (stockModel$Occ_HU_SF[i+1]<stockModel$Occ_HU_SF[i] & stockModel$VR_SF[i]<0.975*Vn_SF) {stockModel[i,188:207]<-0.9*stockModel[i,188:207];stockModel$TSG_SF[i]<-"D1"} # reduce SF dem rates if necessary
    if (stockModel$Occ_HU_MF[i+1]<stockModel$Occ_HU_MF[i] & stockModel$VR_MF[i]<0.975*Vn_MF) {stockModel[i,208:227]<-0.9*stockModel[i,208:227];stockModel$TSG_MF[i]<-"D1"} # reduce MF dem rates if necessary
    if (stockModel$Occ_HU_MH[i+1]<stockModel$Occ_HU_MH[i] & stockModel$VR_MH[i]<0.975*Vn_MH) {stockModel[i,228:247]<-0.9*stockModel[i,228:247];stockModel$TSG_MH[i]<-"D1"} # reduce MH dem rates if necessary. should be less severe for MH, e.g. LA, but perhaps more severe for e.g. Barbour AL. Need to balance 
    
    if (stockModel$Occ_HU_SF[i+1]<stockModel$Occ_HU_SF[i] & stockModel$VR_SF[i]<0.955*Vn_SF) {stockModel[i,188:207]<-0.78*stockModel[i,188:207];stockModel$TSG_SF[i]<-"D2"} # reduce SF dem rates if necessary
    if (stockModel$Occ_HU_MF[i+1]<stockModel$Occ_HU_MF[i] & stockModel$VR_MF[i]<0.955*Vn_MF) {stockModel[i,208:227]<-0.78*stockModel[i,208:227];stockModel$TSG_MF[i]<-"D2"} # reduce MF dem rates if necessary
    if (stockModel$Occ_HU_MH[i+1]<stockModel$Occ_HU_MH[i] & stockModel$VR_MH[i]<0.955*Vn_MH) {stockModel[i,228:247]<-0.78*stockModel[i,228:247];stockModel$TSG_MH[i]<-"D2"} # reduce MH dem rates if necessary. should be less severe for MH, e.g. LA, but perhaps more severe for e.g. Barbour AL. Need to balance
    
    # e.g. method to avoid VR<1. a bit messy and extreme, the construction should really taper down at higher levels of VR?
    if (stockModel$VR_SF[i]<1.025) {stockModel[i,188:207]<-0;stockModel$TSG_SF[i]<-"D3"} # the TSG column is now a altered demolition scenario column
    if (stockModel$VR_MF[i]<1.035) {stockModel[i,208:227]<-0;stockModel$TSG_MF[i]<-"D3"} 
    if (stockModel$VR_MH[i]<1.035) {stockModel[i,228:247]<-0;stockModel$TSG_MH[i]<-"D3"} 
    
    stockModel[i,248:267]<-stockModel[i,188:207]*stockModel[i,50:69]*stockModel$Tot_HU_SF[i] # SF dem = SF demrates * SF stock percentages * SF stock
    stockModel[i,268:287]<-stockModel[i,208:227]*stockModel[i,70:89]*stockModel$Tot_HU_MF[i] # MF dem = MF demrates * MF stock percentages * MF stock
    stockModel[i,288:307]<-stockModel[i,228:247]*stockModel[i,90:109]*stockModel$Tot_HU_MH[i] # MH dem = MH demrates * MH stock percentages * MH stock
    # sum to calculate total demolitions by type
    stockModel$Dem_SF[i]<-sum(stockModel[i,248:267])
    stockModel$Dem_MF[i]<-sum(stockModel[i,268:287])
    stockModel$Dem_MH[i]<-sum(stockModel[i,288:307]) 
    
    if (stockModel$Dem_SF[i]>(stockModel$Tot_HU_SF[i]-stockModel$Occ_HU_SF[i])) {stockModel$Dem_SF[i]<-stockModel[i,248:267]<-stockModel[i,188:207]<-0 ; stockModel$TSG_SF[i]<-"D4"} # avoid demolition being greater than all vacant units by sett
    if (stockModel$Dem_MF[i]>(stockModel$Tot_HU_MF[i]-stockModel$Occ_HU_MF[i])) {stockModel$Dem_MF[i]<-stockModel[i,268:287]<-stockModel[i,208:227]<-0 ; stockModel$TSG_MF[i]<-"D4"} # avoid demolition being greater than all vacant units.
    if (stockModel$Dem_MH[i]>(stockModel$Tot_HU_MH[i]-stockModel$Occ_HU_MH[i])) {stockModel$Dem_MH[i]<-stockModel[i,288:307]<-stockModel[i,228:247]<-0 ; stockModel$TSG_MH[i]<-"D4"} # avoid demolition being greater than all vacant units.
    
    # adjust for potential NaNs due to zero total stock
    MHNAN<-MFNAN<-SFNAN<-0
    if (stockModel$Tot_HU_SF[i]==0) {SFNAN <- 1; stockModel$Tot_HU_SF[i]<-1; stockModel$Tot_HU_SF[i+1]<-1; stockModel$Occ_HU_SF[i]<-1;stockModel$Occ_HU_SF[i+1]<-1}
    if (stockModel$Tot_HU_MF[i]==0) {MFNAN <- 1; stockModel$Tot_HU_MF[i]<-1; stockModel$Tot_HU_MF[i+1]<-1; stockModel$Occ_HU_MF[i]<-1;stockModel$Occ_HU_MF[i+1]<-1}
    if (stockModel$Tot_HU_MH[i]==0) {MHNAN <- 1; stockModel$Tot_HU_MH[i]<-1; stockModel$Tot_HU_MH[i+1]<-1; stockModel$Occ_HU_MH[i]<-1;stockModel$Occ_HU_MH[i+1]<-1}
    
    # estimate annualized demolition rates
    stockModel$Dem_Rate_SF[i]<-stockModel$Dem_SF[i]/stockModel$Tot_HU_SF[i] # annualized demolition rate, 
    stockModel$Dem_Rate_MF[i]<-stockModel$Dem_MF[i]/stockModel$Tot_HU_MF[i] # annualized demolition rate, this can produce NaN if tot stock is 0
    stockModel$Dem_Rate_MH[i]<-stockModel$Dem_MH[i]/stockModel$Tot_HU_MH[i] # annualized demolition rate, 
    
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
    ConFac<-1
    if (stockModel$HS_Scenario[i]%%2==0) {ConFac<-1.5} # increase con rate with negative OSG in case of high DR scenario
    # SF Construction
    if (stockModel$Occ_HU_SF[i+1]<stockModel$Occ_HU_SF[i]) { # if negative growth of occupied housing
      # calculate construction based on linear model of construction rate. 
      stockModel$Con_SF[i]<-ConFac*predict(lmSFc,data.frame(OSG_Rate_SF=stockModel$OSG_SF[i]/stockModel$Tot_HU_SF[i]))*stockModel$Tot_HU_SF[i]
      # put some extra clauses in to avoid unusual results in times of negative occ stock growth. Basically to avoid excessive growth of vacancy rates
      # construction under negative stock growth should be lower than construction with very small positive stock growth
      stockModel$Con_Scenario_SF[i]<-"A"
      low_OSG<-0.00015*stockModel$Tot_HU_SF[i]
      Con_SF_hyp<-Vn_SF*low_OSG+stockModel$Dem_SF[i]
      if (stockModel$Con_SF[i]>Con_SF_hyp) {stockModel$Con_SF[i]<-0.98*Con_SF_hyp;stockModel$Con_Scenario_SF[i]<-"A1"} # make sure that construction is not greater under negative OSG than it would be with 'very low' positive OSG
      if (stockModel$Con_SF[i]<0) {stockModel$Con_SF[i]<-0;stockModel$Con_Scenario_SF[i]<-"A2"} # make sure no sub-zero construction
      # if (stockModel$Con_SF[i]>1.25*stockModel$Dem_SF[i] & stockModel$VR_SF[i]<Vn_SF) {stockModel$Con_SF[i]<-1.25*stockModel$Dem_SF[i];stockModel$Con_Scenario_SF[i]<-"A3"} # limit construction to 1.25 times demolition, if vacancy rate is still too low
      if (stockModel$Con_SF[i]>stockModel$Dem_SF[i] & stockModel$VR_SF[i]>1.05*Vn_SF) {stockModel$Con_SF[i]<-0.8*stockModel$Dem_SF[i];stockModel$Con_Scenario_SF[i]<-"A4"} # limit construction to less than demolition, if vacancy rate is getting much too high 
      if (stockModel$VR_SF[i]>1.2*Vn_SF & stockModel$Con_SF[i]>0.5*stockModel$Dem_SF[i]) {stockModel$Con_SF[i]<-0.5*stockModel$Dem_SF[i];stockModel$Con_Scenario_SF[i]<-"A5"} # limit construction to no more than half demolition, if vacancy rate is getting very much too high 
      
    } else { # if zero or positive growth of occupied housing
      stockModel$H_SF[i]<-predict(lmSF,data.frame(dVR_SF=0.5*(Vn_SF-stockModel$VR_SF[i]))) # optionally reduce the effect of the change in vacancy rate by 0.5 so gap with Vn isn't closed immediately
        if (stockModel$H_SF[i]<0.2) {stockModel$H_SF[i]<-0.2} #  limit H from getting below 0.2 
        if (stockModel$H_SF[i]>1.3) {stockModel$H_SF[i]<-1.3} # also limit H from getting above 1.3
      stockModel$Con_SF[i]<-stockModel$H_SF[i]*stockModel$VOSG_SF[i]+stockModel$Dem_SF[i]
      stockModel$Con_Scenario_SF[i]<-"B"
    }
    stockModel$Con_Rate_SF[i]<-stockModel$Con_SF[i]/stockModel$Tot_HU_SF[i] # annualized construction rate
    # MF Construction
    if (stockModel$Occ_HU_MF[i+1]<stockModel$Occ_HU_MF[i]) { # if negative growth of occupied housing
      # calculate construction based on linear model of construction. 
      stockModel$Con_MF[i]<-ConFac*predict(lmMFc,data.frame(OSG_Rate_MF=stockModel$OSG_MF[i]/stockModel$Tot_HU_MF[i]))*stockModel$Tot_HU_MF[i]
      # put some extra clauses in to avoid unusual results in times of negative occ stock growth. Basically to avoid excessive growht of vacancy rates
      stockModel$Con_Scenario_MF[i]<-"A"
      low_OSG<-0.0002*stockModel$Tot_HU_MF[i]
      Con_MF_hyp<-Vn_MF*low_OSG+stockModel$Dem_MF[i]
      if (stockModel$Con_MF[i]>Con_MF_hyp) {stockModel$Con_MF[i]<-0.98*Con_MF_hyp;stockModel$Con_Scenario_MF[i]<-"A1"} # make sure that construction is lower under negative OSG than it would be with 'very low' positive OSG
      if (stockModel$Con_MF[i]<0) {stockModel$Con_MF[i]<-0;stockModel$Con_Scenario_MF[i]<-"A2"} # make sure no zero construction
      # if (stockModel$Con_MF[i]>1.25*stockModel$Dem_MF[i] & stockModel$VR_MF[i]<Vn_MF) {stockModel$Con_MF[i]<-1.25*stockModel$Dem_MF[i];stockModel$Con_Scenario_MF[i]<-"A3"} # limit construction to 1.25 times demolition, if vacancy rate is still too low
      if (stockModel$Con_MF[i]>stockModel$Dem_MF[i] & stockModel$VR_MF[i]>1.05*Vn_MF) {stockModel$Con_MF[i]<-0.8*stockModel$Dem_MF[i];stockModel$Con_Scenario_MF[i]<-"A4"} # limit construction to less than demolition, if vacancy rate is getting much too high 
      if (stockModel$VR_MF[i]>1.2*Vn_MF & stockModel$Con_MF[i]>0.5*stockModel$Dem_MF[i]) {stockModel$Con_MF[i]<-0.5*stockModel$Dem_MF[i];stockModel$Con_Scenario_MF[i]<-"A5"} # limit construction to no more than half demolition, if vacancy rate is getting very much too high 
      
    } else { # if zero or positive growth of occupied housing
      stockModel$H_MF[i]<-predict(lmMF,data.frame(dVR_MF=0.5*(Vn_MF-stockModel$VR_MF[i]))) # optionally reduce the effect of the change in vacancy rate by 0.5 so gap with Vn isn't closed immediately
        if (stockModel$H_MF[i]<0.2) {stockModel$H_MF[i]<-0.2} #  limit H from getting below 0.2 
        if (stockModel$H_MF[i]>1.3) {stockModel$H_MF[i]<-1.3} # also limit H from getting above 1.3 
      stockModel$Con_MF[i]<-stockModel$H_MF[i]*stockModel$VOSG_MF[i]+stockModel$Dem_MF[i]
      stockModel$Con_Scenario_MF[i]<-"B"
    }
    stockModel$Con_Rate_MF[i]<-stockModel$Con_MF[i]/stockModel$Tot_HU_MF[i] # annualized construction rate, this can also be nan if stock is zero
    # MH Construction
    if (stockModel$Occ_HU_MH[i+1]<stockModel$Occ_HU_MH[i]) { # if negative growth of occupied housing
      # calculate construction based on linear model of construction. 
      stockModel$Con_MH[i]<-ConFac*predict(lmMHc,data.frame(OSG_Rate_MH=stockModel$OSG_MH[i]/stockModel$Tot_HU_MH[i]))*stockModel$Tot_HU_MH[i]
      # put some extra clauses in to avoid unusual results in times of negative occ stock growth. Basically to avoid excessive growht of vacancy rates
      stockModel$Con_Scenario_MH[i]<-"A"
      low_OSG<-0.0004*stockModel$Tot_HU_MH[i]
      Con_MH_hyp<-Vn_MH*low_OSG+stockModel$Dem_MH[i]
      if (stockModel$Con_MH[i]>Con_MH_hyp) {stockModel$Con_MH[i]<-0.98*Con_MH_hyp;stockModel$Con_Scenario_MH[i]<-"A1"} # make sure that construction is lower under negative OSG than it would be with 'very low' positive OSG
      if (stockModel$Con_MH[i]<0) {stockModel$Con_MH[i]<-0;stockModel$Con_Scenario_MH[i]<-"A2"} # make sure no zero construction
      # if (stockModel$Con_MH[i]>1.25*stockModel$Dem_MH[i] & stockModel$VR_MH[i]<Vn_MH) {stockModel$Con_MH[i]<-1.25*stockModel$Dem_MH[i];stockModel$Con_Scenario_MH[i]<-"A3"} # limit construction to 1.25 times demolition, if vacancy rate is still too low
      if (stockModel$Con_MH[i]>stockModel$Dem_MH[i] & stockModel$VR_MH[i]>1.05*Vn_MH) {stockModel$Con_MH[i]<-0.8*stockModel$Dem_MH[i];stockModel$Con_Scenario_MH[i]<-"A4"} # limit construction to less than demolition, if vacancy rate is getting much too high 
      if (stockModel$VR_MH[i]>1.2*Vn_MH & stockModel$Con_MH[i]>0.5*stockModel$Dem_MH[i]) {stockModel$Con_MH[i]<-0.5*stockModel$Dem_MH[i];stockModel$Con_Scenario_MH[i]<-"A5"} # limit construction to no more than half demolition, if vacancy rate is getting very much too high 
      
    } else { # if zero or positive growth of occupied housing
      stockModel$H_MH[i]<-predict(lmMH,data.frame(dVR_MH=0.5*(Vn_MH-stockModel$VR_MH[i]))) # optionally reduce the effect of the change in vacancy rate by 0.5 so gap with Vn isn't closed immediately
        if (stockModel$H_MH[i]<0.2) {stockModel$H_MH[i]<-0.2} #  limit H from getting below 0.2
        if (stockModel$H_MH[i]>1.4) {stockModel$H_MH[i]<-1.4} # also limit H from getting above 1.3. Consider changing this to allow swifter assimilation of VR in sluggish-growing counties, e.g. LA.
      stockModel$Con_MH[i]<-stockModel$H_MH[i]*stockModel$VOSG_MH[i]+stockModel$Dem_MH[i]
      stockModel$Con_Scenario_MH[i]<-"B"
    }
    stockModel$Con_Rate_MH[i]<-stockModel$Con_MH[i]/stockModel$Tot_HU_MH[i] # annualized construction rate
    # step 5. calculate total housing units in the next year
    stockModel$Tot_HU_SF[i+1]<-stockModel$Tot_HU_SF[i]+stockModel$Con_SF[i]-stockModel$Dem_SF[i]
    stockModel$Tot_HU_MF[i+1]<-stockModel$Tot_HU_MF[i]+stockModel$Con_MF[i]-stockModel$Dem_MF[i]
    stockModel$Tot_HU_MH[i+1]<-stockModel$Tot_HU_MH[i]+stockModel$Con_MH[i]-stockModel$Dem_MH[i]
    # step 6. calculate new vacancy rate
    stockModel$VR_SF[i+1]<-stockModel$Tot_HU_SF[i+1]/stockModel$Occ_HU_SF[i+1]
    stockModel$VR_MF[i+1]<-stockModel$Tot_HU_MF[i+1]/stockModel$Occ_HU_MF[i+1] # this may be nan if stock is zero
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
    sf2029<-stockModel$Tot_HU_SF_Occ_2020_29[i]+stockModel$Tot_HU_SF_Vac_2020_29[i]
    sf3039<-stockModel$Tot_HU_SF_Occ_2030_39[i]+stockModel$Tot_HU_SF_Vac_2030_39[i]
    sf4049<-stockModel$Tot_HU_SF_Occ_2040_49[i]+stockModel$Tot_HU_SF_Vac_2040_49[i]
    sf5059<-stockModel$Tot_HU_SF_Occ_2050_60[i]+stockModel$Tot_HU_SF_Vac_2050_60[i]
    
    mfp40<-stockModel$Tot_HU_MF_Occ_p1940[i]+stockModel$Tot_HU_MF_Vac_p1940[i]
    mf4059<-stockModel$Tot_HU_MF_Occ_1940_59[i]+stockModel$Tot_HU_MF_Vac_1940_59[i]
    mf6079<-stockModel$Tot_HU_MF_Occ_1960_79[i]+stockModel$Tot_HU_MF_Vac_1960_79[i]
    mf8099<-stockModel$Tot_HU_MF_Occ_1980_99[i]+stockModel$Tot_HU_MF_Vac_1980_99[i]
    mf0009<-stockModel$Tot_HU_MF_Occ_2000_09[i]+stockModel$Tot_HU_MF_Vac_2000_09[i]
    mf1019<-stockModel$Tot_HU_MF_Occ_2010_19[i]+stockModel$Tot_HU_MF_Vac_2010_19[i]
    mf2029<-stockModel$Tot_HU_MF_Occ_2020_29[i]+stockModel$Tot_HU_MF_Vac_2020_29[i]
    mf3039<-stockModel$Tot_HU_MF_Occ_2030_39[i]+stockModel$Tot_HU_MF_Vac_2030_39[i]
    mf4049<-stockModel$Tot_HU_MF_Occ_2040_49[i]+stockModel$Tot_HU_MF_Vac_2040_49[i]
    mf5059<-stockModel$Tot_HU_MF_Occ_2050_60[i]+stockModel$Tot_HU_MF_Vac_2050_60[i]
    
    mhp40<-stockModel$Tot_HU_MH_Occ_p1940[i]+stockModel$Tot_HU_MH_Vac_p1940[i]
    mh4059<-stockModel$Tot_HU_MH_Occ_1940_59[i]+stockModel$Tot_HU_MH_Vac_1940_59[i]
    mh6079<-stockModel$Tot_HU_MH_Occ_1960_79[i]+stockModel$Tot_HU_MH_Vac_1960_79[i]
    mh8099<-stockModel$Tot_HU_MH_Occ_1980_99[i]+stockModel$Tot_HU_MH_Vac_1980_99[i]
    mh0009<-stockModel$Tot_HU_MH_Occ_2000_09[i]+stockModel$Tot_HU_MH_Vac_2000_09[i]
    mh1019<-stockModel$Tot_HU_MH_Occ_2010_19[i]+stockModel$Tot_HU_MH_Vac_2010_19[i]
    mh2029<-stockModel$Tot_HU_MH_Occ_2020_29[i]+stockModel$Tot_HU_MH_Vac_2020_29[i]
    mh3039<-stockModel$Tot_HU_MH_Occ_2030_39[i]+stockModel$Tot_HU_MH_Vac_2030_39[i]
    mh4049<-stockModel$Tot_HU_MH_Occ_2040_49[i]+stockModel$Tot_HU_MH_Vac_2040_49[i]
    mh5059<-stockModel$Tot_HU_MH_Occ_2050_60[i]+stockModel$Tot_HU_MH_Vac_2050_60[i]
    
    # Demolition 
    sfdemp40<-stockModel$Dem_SF_Occ_p1940[i]+stockModel$Dem_SF_Vac_p1940[i]
    sfdem4059<-stockModel$Dem_SF_Occ_1940_59[i]+stockModel$Dem_SF_Vac_1940_59[i]
    sfdem6079<-stockModel$Dem_SF_Occ_1960_79[i]+stockModel$Dem_SF_Vac_1960_79[i]
    sfdem8099<-stockModel$Dem_SF_Occ_1980_99[i]+stockModel$Dem_SF_Vac_1980_99[i]
    sfdem0009<-stockModel$Dem_SF_Occ_2000_09[i]+stockModel$Dem_SF_Vac_2000_09[i]
    sfdem1019<-stockModel$Dem_SF_Occ_2010_19[i]+stockModel$Dem_SF_Vac_2010_19[i]
    sfdem2029<-stockModel$Dem_SF_Occ_2020_29[i]+stockModel$Dem_SF_Vac_2020_29[i]
    sfdem3039<-stockModel$Dem_SF_Occ_2030_39[i]+stockModel$Dem_SF_Vac_2030_39[i]
    sfdem4049<-stockModel$Dem_SF_Occ_2040_49[i]+stockModel$Dem_SF_Vac_2040_49[i]
    sfdem5059<-stockModel$Dem_SF_Occ_2050_60[i]+stockModel$Dem_SF_Vac_2050_60[i]
    # MF
    mfdemp40<-stockModel$Dem_MF_Occ_p1940[i]+stockModel$Dem_MF_Vac_p1940[i]
    mfdem4059<-stockModel$Dem_MF_Occ_1940_59[i]+stockModel$Dem_MF_Vac_1940_59[i]
    mfdem6079<-stockModel$Dem_MF_Occ_1960_79[i]+stockModel$Dem_MF_Vac_1960_79[i]
    mfdem8099<-stockModel$Dem_MF_Occ_1980_99[i]+stockModel$Dem_MF_Vac_1980_99[i]
    mfdem0009<-stockModel$Dem_MF_Occ_2000_09[i]+stockModel$Dem_MF_Vac_2000_09[i]
    mfdem1019<-stockModel$Dem_MF_Occ_2010_19[i]+stockModel$Dem_MF_Vac_2010_19[i]
    mfdem2029<-stockModel$Dem_MF_Occ_2020_29[i]+stockModel$Dem_MF_Vac_2020_29[i]
    mfdem3039<-stockModel$Dem_MF_Occ_2030_39[i]+stockModel$Dem_MF_Vac_2030_39[i]
    mfdem4049<-stockModel$Dem_MF_Occ_2040_49[i]+stockModel$Dem_MF_Vac_2040_49[i]
    mfdem5059<-stockModel$Dem_MF_Occ_2050_60[i]+stockModel$Dem_MF_Vac_2050_60[i]
    # MH
    mhdemp40<-stockModel$Dem_MH_Occ_p1940[i]+stockModel$Dem_MH_Vac_p1940[i]
    mhdem4059<-stockModel$Dem_MH_Occ_1940_59[i]+stockModel$Dem_MH_Vac_1940_59[i]
    mhdem6079<-stockModel$Dem_MH_Occ_1960_79[i]+stockModel$Dem_MH_Vac_1960_79[i]
    mhdem8099<-stockModel$Dem_MH_Occ_1980_99[i]+stockModel$Dem_MH_Vac_1980_99[i]
    mhdem0009<-stockModel$Dem_MH_Occ_2000_09[i]+stockModel$Dem_MH_Vac_2000_09[i]
    mhdem1019<-stockModel$Dem_MH_Occ_2010_19[i]+stockModel$Dem_MH_Vac_2010_19[i]
    mhdem2029<-stockModel$Dem_MH_Occ_2020_29[i]+stockModel$Dem_MH_Vac_2020_29[i]
    mhdem3039<-stockModel$Dem_MH_Occ_2030_39[i]+stockModel$Dem_MH_Vac_2030_39[i]
    mhdem4049<-stockModel$Dem_MH_Occ_2040_49[i]+stockModel$Dem_MH_Vac_2040_49[i]
    mhdem5059<-stockModel$Dem_MH_Occ_2050_60[i]+stockModel$Dem_MH_Vac_2050_60[i]
    
    # Construction
    # define which region we use for basing assumptions of how much of stock growth is from new construction
    if (stockModel$Region[i]==1) {
      SF_growth_returns_pc<-SF_growth_returns_pc_NE
      MF_growth_returns_pc<-MF_growth_returns_pc_NE
      MH_growth_returns_pc<-MH_growth_returns_pc_NE
    }
    if (stockModel$Region[i]==2) {
      SF_growth_returns_pc<-SF_growth_returns_pc_MW
      MF_growth_returns_pc<-MF_growth_returns_pc_MW
      MH_growth_returns_pc<-MH_growth_returns_pc_MW
    }
    if (stockModel$Region[i]==3) {
      SF_growth_returns_pc<-SF_growth_returns_pc_S
      MF_growth_returns_pc<-MF_growth_returns_pc_S
      MH_growth_returns_pc<-MH_growth_returns_pc_S
    }
    if (stockModel$Region[i]==4) {
      SF_growth_returns_pc<-SF_growth_returns_pc_W
      MF_growth_returns_pc<-MF_growth_returns_pc_W
      MH_growth_returns_pc<-MH_growth_returns_pc_W
    }
    
    # First construction which isn't construction, but returns to housing stock of residential units which had been unfit, or used for business, or MH moving in, etc.
    sfconp40<-(sfp40/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon4059<-(sf4059/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon6079<-(sf6079/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon8099<-(sf8099/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon0009<-(sf0009/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon1019<-(sf1019/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon2029<-(sf2029/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon3039<-(sf3039/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon4049<-(sf4049/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    sfcon5059<-(sf5059/stockModel$Tot_HU_SF[i])*SF_growth_returns_pc*stockModel$Con_SF[i]
    
    mfconp40<-(mfp40/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i] # this can be NaN
    mfcon4059<-(mf4059/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon6079<-(mf6079/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon8099<-(mf8099/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon0009<-(mf0009/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon1019<-(mf1019/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon2029<-(mf2029/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon3039<-(mf3039/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon4049<-(mf4049/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    mfcon5059<-(mf5059/stockModel$Tot_HU_MF[i])*MF_growth_returns_pc*stockModel$Con_MF[i]
    
    mhconp40<-(mhp40/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon4059<-(mh4059/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon6079<-(mh6079/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon8099<-(mh8099/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon0009<-(mh0009/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon1019<-(mh1019/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon2029<-(mh2029/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon3039<-(mh3039/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon4049<-(mh4049/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    mhcon5059<-(mh5059/stockModel$Tot_HU_MH[i])*MH_growth_returns_pc*stockModel$Con_MH[i]
    
    # allocate new construction to the cohort which year fits into.
    # 2020-2029
    if (stockModel$Year[i]>2019 & stockModel$Year[i]<2030) {
      sfcon2029<-sfcon2029+stockModel$Con_SF[i]*(1-SF_growth_returns_pc) 
      mfcon2029<-mfcon2029+stockModel$Con_MF[i]*(1-MF_growth_returns_pc)  
      mhcon2029<-mhcon2029+stockModel$Con_MH[i]*(1-MH_growth_returns_pc)   
    }
    # 2030-2039
    if (stockModel$Year[i]>2029 & stockModel$Year[i]<2040) {
      sfcon3039<-sfcon3039+stockModel$Con_SF[i]*(1-SF_growth_returns_pc)  
      mfcon3039<-mfcon3039+stockModel$Con_MF[i]*(1-MF_growth_returns_pc)   
      mhcon3039<-mhcon3039+stockModel$Con_MH[i]*(1-MH_growth_returns_pc)    
    }
    # 2040-2049
    if (stockModel$Year[i]>2039 & stockModel$Year[i]<2050) {
      sfcon4049<-sfcon4049+stockModel$Con_SF[i]*(1-SF_growth_returns_pc)  
      mfcon4049<-mfcon4049+stockModel$Con_MF[i]*(1-MF_growth_returns_pc)   
      mhcon4049<-mhcon4049+stockModel$Con_MH[i]*(1-MH_growth_returns_pc)    
    }
    # 2050-2059
    if (stockModel$Year[i]>2049 & stockModel$Year[i]<2060) {
      sfcon5059<-sfcon5059+stockModel$Con_SF[i]*(1-SF_growth_returns_pc)  
      mfcon5059<-mfcon5059+stockModel$Con_MF[i]*(1-MF_growth_returns_pc)   
      mhcon5059<-mhcon5059+stockModel$Con_MH[i]*(1-MH_growth_returns_pc)    
    }
    # calculate changes in housing stock by type and cohort, and estimate housing stock in next timestep. 
    sfp40_next<-sfp40+sfconp40-sfdemp40
    sf4059_next<-sf4059+sfcon4059-sfdem4059
    sf6079_next<-sf6079+sfcon6079-sfdem6079
    sf8099_next<-sf8099+sfcon8099-sfdem8099
    sf0009_next<-sf0009+sfcon0009-sfdem0009
    sf1019_next<-sf1019+sfcon1019-sfdem1019
    sf2029_next<-sf2029+sfcon2029-sfdem2029
    sf3039_next<-sf3039+sfcon3039-sfdem3039
    sf4049_next<-sf4049+sfcon4049-sfdem4049
    sf5059_next<-sf5059+sfcon5059-sfdem5059
    
    mfp40_next<-mfp40+mfconp40-mfdemp40
    mf4059_next<-mf4059+mfcon4059-mfdem4059
    mf6079_next<-mf6079+mfcon6079-mfdem6079
    mf8099_next<-mf8099+mfcon8099-mfdem8099
    mf0009_next<-mf0009+mfcon0009-mfdem0009
    mf1019_next<-mf1019+mfcon1019-mfdem1019
    mf2029_next<-mf2029+mfcon2029-mfdem2029
    mf3039_next<-mf3039+mfcon3039-mfdem3039
    mf4049_next<-mf4049+mfcon4049-mfdem4049
    mf5059_next<-mf5059+mfcon5059-mfdem5059
    
    mhp40_next<-mhp40+mhconp40-mhdemp40
    mh4059_next<-mh4059+mhcon4059-mhdem4059
    mh6079_next<-mh6079+mhcon6079-mhdem6079
    mh8099_next<-mh8099+mhcon8099-mhdem8099
    mh0009_next<-mh0009+mhcon0009-mhdem0009
    mh1019_next<-mh1019+mhcon1019-mhdem1019
    mh2029_next<-mh2029+mhcon2029-mhdem2029
    mh3039_next<-mh3039+mhcon3039-mhdem3039
    mh4049_next<-mh4049+mhcon4049-mhdem4049
    mh5059_next<-mh5059+mhcon5059-mhdem5059
    
    # calculate SF units by occupancy and age cohort in next timestep
    # total units by age cohort
    sf_pcTU_ac<-data.frame(Cohort=names(sfocc),pcTU=c(sfp40_next,sf4059_next,sf6079_next,sf8099_next,sf0009_next,sf1019_next,
                                                      sf2029_next,sf3039_next,sf4049_next,sf5059_next)/stockModel$Tot_HU_SF[i+1])
    # generate first estimate of pc of total SF units that are vacant by age cohort, by applying stock average vacancy rate to the total SF units by age cohort
    # calculation is pc vac by cohort = stock_by_cohort * avg_vac_rate / tot_stock = vac_stock_by_cohort / tot_stock
    sf_pcvacest_ac<-data.frame(Cohort=names(sfocc),
                               pcvac=c(sfp40_next,sf4059_next,sf6079_next,sf8099_next,sf0009_next,sf1019_next,sf2029_next,sf3039_next,sf4049_next,sf5059_next)*
                                 ((stockModel$VR_SF[i+1]-1)/stockModel$VR_SF[i+1])/stockModel$Tot_HU_SF[i+1])
    # then adjust vacancies by age cohort based on differences in vacancies by age ranges. after spotting trouble with MF occupied units being negative, I change the year here from i+1 to i
    sf_pcvac_ac<-vacrate_fn(stockModel$Year[i],sf_pcvacest_ac,dem_adj_SF,0) # this has the potential to make the vacant stock higher than the total stock for the oldest cohort(s), generally <1940, which will end up with negative occupied units.
    if (any(sf_pcTU_ac$pcTU<sf_pcvac_ac$pcvac)) { # if any of the vacancy percentages are greater than the total percentages, we're in trouble. Try to define sf_pcvac_ac again with the smaller changes in dem_adj_SF2
      sf_pcvac_ac<-vacrate_fn(stockModel$Year[i],sf_pcvacest_ac,dem_adj_SF2,0)
    }
    if (any(sf_pcTU_ac$pcTU<sf_pcvac_ac$pcvac)) { # if any of the vacancy percentages are still greater than the total percentages, revert to the original vacancy estimate
      sf_pcvac_ac<-sf_pcvacest_ac
    }

    # occupied units by age cohort
    sf_pcocc_ac<-data.frame(Cohort=names(sfocc),pcocc=sf_pcTU_ac$pcTU-sf_pcvac_ac$pcvac)
    # put calculated values of pc occ and vac by age cohort into the model data frame
    stockModel[i+1,50:59]<-sf_pcocc_ac$pcocc
    stockModel[i+1,60:69]<-sf_pcvac_ac$pcvac
    
    # calculate MF units by occupancy and age cohort in next timestep
    # total units by age cohort
    mf_pcTU_ac<-data.frame(Cohort=names(mfocc),pcTU=c(mfp40_next,mf4059_next,mf6079_next,mf8099_next,mf0009_next,mf1019_next,
                                                      mf2029_next,mf3039_next,mf4049_next,mf5059_next)/stockModel$Tot_HU_MF[i+1])
    # generate first estimate of pc of total MF units that are vacant by age cohort, by applying stock average vacancy rate to the total MF units by age cohort
    mf_pcvacest_ac<-data.frame(Cohort=names(mfocc),
                               pcvac=c(mfp40_next,mf4059_next,mf6079_next,mf8099_next,mf0009_next,mf1019_next,mf2029_next,mf3039_next,mf4049_next,mf5059_next)*
                                 ((stockModel$VR_MF[i+1]-1)/stockModel$VR_MF[i+1])/stockModel$Tot_HU_MF[i+1])
    # adjust vacancies by age cohort based on differences in vacancies by age ranges. after spotting trouble with MF occupied units being negative, I change the year here from i+1 to i
    mf_pcvac_ac<-vacrate_fn(stockModel$Year[i],mf_pcvacest_ac,dem_adj_MF,0)
    # with the following two if statements, I attempt to resolve problems of negative MF in new cohorts
    if (any(mf_pcTU_ac$pcTU<mf_pcvac_ac$pcvac)) { # if any of the vacancy percentages are greater than the total percentages, we're in trouble. Try to define mf_pcvac_ac again with the smaller changes in dem_adj_MF2
      mf_pcvac_ac<-vacrate_fn(stockModel$Year[i],mf_pcvacest_ac,dem_adj_MF2,0)
    }
    if (any(mf_pcTU_ac$pcTU<mf_pcvac_ac$pcvac)) { # if any of the vacancy percentages are still greater than the total percentages, revert to the original vacancy estimate
        mf_pcvac_ac<-mf_pcvacest_ac
      }

    # occupied units by age cohort
    mf_pcocc_ac<-data.frame(Cohort=names(mfocc),pcocc=mf_pcTU_ac$pcTU-mf_pcvac_ac$pcvac) # this still in some cases causes negative occupied units, due to 0 total and non-zero positve vacant units
    # put calculated values of pc occ and vac by age cohort into the model data frame
    stockModel[i+1,70:79]<-mf_pcocc_ac$pcocc
    stockModel[i+1,80:89]<-mf_pcvac_ac$pcvac
    
    # calculate MH units by occupancy and age cohort in next timestep
    # total units by age cohort
    mh_pcTU_ac<-data.frame(Cohort=names(mhocc),pcTU=c(mhp40_next,mh4059_next,mh6079_next,mh8099_next,mh0009_next,mh1019_next,
                                                      mh2029_next,mh3039_next,mh4049_next,mh5059_next)/stockModel$Tot_HU_MH[i+1])
    # generate first estimate of pc of total MH units that are vacant by age cohort, by applying stock average vacancy rate to the total MH units by age cohort
    mh_pcvacest_ac<-data.frame(Cohort=names(mhocc),
                               pcvac=c(mhp40_next,mh4059_next,mh6079_next,mh8099_next,mh0009_next,mh1019_next,mh2029_next,mh3039_next,mh4049_next,mh5059_next)*
                                 ((stockModel$VR_MH[i+1]-1)/stockModel$VR_MH[i+1])/stockModel$Tot_HU_MH[i+1])
    # adjust vacancies by age cohort based on differences in vacancies by age ranges. after spotting trouble with MF occupied units being negative, I change the year here from i+1 to i
    mh_pcvac_ac<-vacrate_fn(stockModel$Year[i],mh_pcvacest_ac,dem_adj_MH,1)
    if (any(mh_pcTU_ac$pcTU<mh_pcvac_ac$pcvac)) { # if any of the vacancy percentages are greater than the total percentages, revert to the original vacancy estimate
      mh_pcvac_ac<-mh_pcvacest_ac
    }
   
    # occupied units by age cohort
    mh_pcocc_ac<-data.frame(Cohort=names(mhocc),pcocc=mh_pcTU_ac$pcTU-mh_pcvac_ac$pcvac)
    # put calculated values of pc occ and vac by age cohort into the model data frame
    stockModel[i+1,90:99]<-mh_pcocc_ac$pcocc
    stockModel[i+1,100:109]<-mh_pcvac_ac$pcvac
    
    # finally calculate occ and vac units by age cohort for each type in the model data frame
    stockModel[i+1,110:129]<-stockModel$Tot_HU_SF[i+1]*stockModel[i+1,50:69] # for SF homes
    stockModel[i+1,130:149]<-stockModel$Tot_HU_MF[i+1]*stockModel[i+1,70:89] # for MF homes
    stockModel[i+1,150:169]<-stockModel$Tot_HU_MH[i+1]*stockModel[i+1,90:109] # for MH homes
    
    stockModel$Tot_Hous_Units[i+1]<-sum(stockModel[i+1,c("Tot_HU_SF","Tot_HU_MF","Tot_HU_MH")])
    stockModel$Vacancy_Ratio[i+1]<- stockModel$Tot_Hous_Units[i+1]/ stockModel$Occ_Hous_Units[i+1]
    
    # revert adjustment to avoid NaNs
    if (SFNAN==1) {SFNAN <- 0; stockModel$Tot_HU_SF[i]<-0; stockModel$Tot_HU_SF[i+1]<-0; stockModel$Occ_HU_SF[i]<-0;stockModel$Occ_HU_SF[i+1]<-0}
    if (MFNAN==1) {MFNAN <- 0; stockModel$Tot_HU_MF[i]<-0; stockModel$Tot_HU_MF[i+1]<-0; stockModel$Occ_HU_MF[i]<-0;stockModel$Occ_HU_MF[i+1]<-0}
    if (MHNAN==1) {MHNAN <- 0; stockModel$Tot_HU_MH[i]<-0; stockModel$Tot_HU_MH[i+1]<-0; stockModel$Occ_HU_MH[i]<-0;stockModel$Occ_HU_MH[i+1]<-0}
    
  } 
  stockModel
} # end of large stock model function