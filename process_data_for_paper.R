#  Copyright 2016, INSEAD
#  by T. Evgeniou, Theo Vermaelen
#  Dual licensed under the MIT or GPL Version 2 licenses.

##########################################################################################
# Creates the data used in the .Rnw file
##########################################################################################

if (!exists("generate_all_appendices")){
  rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
  project_var_name = "women" 
}

source("~/OneDrive - INSEAD/FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("~/OneDrive - INSEAD/FinanceData/rawdata_indices_and_factors/ff_industries_sic.R")
source("Paper_global_parameters.R")
library("sampleSelection")
library("AER")
winsorize <- function(r) {r0 = r[!is.na(r)]; minval = quantile(r[!is.na(r)],0.01); maxval= quantile(r[!is.na(r)],0.99); r[!is.na(r)] <- ifelse(r[!is.na(r)] < minval | r[!is.na(r)] > maxval, NA, r[!is.na(r)]); r}
thecor <- function(x) { tmp = Reduce(cbind,lapply(1:ncol(x), function(i) sapply(1:ncol(x), function(j) { useonly = which(!is.na(x[,i]) & !is.na(x[,j])); cor(x[useonly,i],x[useonly,j])}))); colnames(tmp)<-rownames(tmp)<-colnames(x); tmp}

initial_vars = ls(all = TRUE) # takes time to save and load, so we save only what is needed at the end. 
# Project specific parameters
quantile_simple = 0.2 # Used only for the EU-calculation
quantile_simple = 0.2 # Quantile for defining high/low U-index etc simple categories
quantile_doublesort = 0.5 # NOTE:if this is not 0.5 we need to change the code for project_relations below
projectwindow = 365
project_var_name = project_var_name

#####
load("~/OneDrive - INSEAD/FinanceData/created_projects_datasets/bb_diversity.Rdata") # PROJECT SPECIFIC DATA FILE TO LOAD
Risk_Factors_Monthly = BUYBACK_DATA_PROJECT$Risk_Factors_Monthly
Market_Monthly = BUYBACK_DATA_PROJECT$Market_Monthly

###### PERMNO vs PERMCO
BUYBACK_DATA_PROJECT$DATASET$boardex$other_female_boards_sum = BUYBACK_DATA_PROJECT$DATASET$boardex$other_female_boards_permno_sum
#BUYBACK_DATA_PROJECT$DATASET$boardex$other_female_boards_sum = BUYBACK_DATA_PROJECT$DATASET$boardex$other_female_boards_permco_sum
######

value.weights_bb = rep(1,length(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
value.weights_iss = rep(1,length(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method

BUYBACK_DATA_PROJECT$DATASET$DatesMonth <- create_dates_month(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, rownames(BUYBACK_DATA_PROJECT$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(BUYBACK_DATA_PROJECT$DATASET$DatesMonth) <- BUYBACK_DATA_PROJECT$DATASET$SDC$permno

ISSUERS_DATA_PROJECT$DATASET$DatesMonth <- create_dates_month(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date, rownames(ISSUERS_DATA_PROJECT$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(ISSUERS_DATA_PROJECT$DATASET$DatesMonth) <- ISSUERS_DATA_PROJECT$DATASET$SDC$permno

###############################################################################################
# SOME PROJECT SPECIFIC DATA CLEANUP
###############################################################################################
# If we want to use this instead (default is no)

to_remove = which(is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$gender))
to_remove2 = which(scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$women) + scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$men) != scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$total_directors))
to_remove3 = which(BUYBACK_DATA_PROJECT$DATASET$boardex$other_female_boards_sum > BUYBACK_DATA_PROJECT$DATASET$boardex$men)
to_remove = union(union(to_remove,to_remove2),to_remove3)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  BUYBACK_DATA_PROJECT$BEME_used <- BUYBACK_DATA_PROJECT$BEME_used[-to_remove]
  BUYBACK_DATA_PROJECT$Performance_used <- BUYBACK_DATA_PROJECT$Performance_used[-to_remove]
  BUYBACK_DATA_PROJECT$Size_used <- BUYBACK_DATA_PROJECT$Size_used[-to_remove]
  BUYBACK_DATA_PROJECT$Valuation_Index <- BUYBACK_DATA_PROJECT$Valuation_Index[-to_remove]
  
  BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly <- BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,-to_remove]
  BUYBACK_DATA_PROJECT$DATASET$SDC <- BUYBACK_DATA_PROJECT$DATASET$SDC[-to_remove,]
  for(field in ls(BUYBACK_DATA_PROJECT$DATASET$CRSP))  BUYBACK_DATA_PROJECT$DATASET$CRSP[[field]] <- BUYBACK_DATA_PROJECT$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_PROJECT$DATASET$boardex))  BUYBACK_DATA_PROJECT$DATASET$boardex[[field]] <- BUYBACK_DATA_PROJECT$DATASET$boardex[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_PROJECT$DATASET$institutional))  BUYBACK_DATA_PROJECT$DATASET$institutional[[field]] <- BUYBACK_DATA_PROJECT$DATASET$institutional[[field]][-to_remove]
  for(field1 in ls(BUYBACK_DATA_PROJECT$DATASET$ibes))  
    for (field in ls(BUYBACK_DATA_PROJECT$DATASET$ibes[[field1]])) 
      BUYBACK_DATA_PROJECT$DATASET$ibes[[field1]][[field]]<- BUYBACK_DATA_PROJECT$DATASET$ibes[[field1]][[field]][-to_remove]
  rm("field","field1")
}
BUYBACK_DATA_PROJECT$cleanupMissingSomeValues = length(to_remove)

to_remove = which(is.na(ISSUERS_DATA_PROJECT$DATASET$boardex$gender))
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  ISSUERS_DATA_PROJECT$BEME_used <- ISSUERS_DATA_PROJECT$BEME_used[-to_remove]
  ISSUERS_DATA_PROJECT$Performance_used <- ISSUERS_DATA_PROJECT$Performance_used[-to_remove]
  ISSUERS_DATA_PROJECT$Size_used <- ISSUERS_DATA_PROJECT$Size_used[-to_remove]
  ISSUERS_DATA_PROJECT$Valuation_Index <- ISSUERS_DATA_PROJECT$Valuation_Index[-to_remove]
  
  ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly <- ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,-to_remove]
  ISSUERS_DATA_PROJECT$DATASET$SDC <- ISSUERS_DATA_PROJECT$DATASET$SDC[-to_remove,]
  for(field in ls(ISSUERS_DATA_PROJECT$DATASET$CRSP))  ISSUERS_DATA_PROJECT$DATASET$CRSP[[field]] <- ISSUERS_DATA_PROJECT$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(ISSUERS_DATA_PROJECT$DATASET$boardex))  ISSUERS_DATA_PROJECT$DATASET$boardex[[field]] <- ISSUERS_DATA_PROJECT$DATASET$boardex[[field]][-to_remove]
  for(field in ls(ISSUERS_DATA_PROJECT$DATASET$institutional))  ISSUERS_DATA_PROJECT$DATASET$institutional[[field]] <- ISSUERS_DATA_PROJECT$DATASET$institutional[[field]][-to_remove]
  for(field1 in ls(ISSUERS_DATA_PROJECT$DATASET$ibes))  
    for (field in ls(ISSUERS_DATA_PROJECT$DATASET$ibes[[field1]])) 
      ISSUERS_DATA_PROJECT$DATASET$ibes[[field1]][[field]]<- ISSUERS_DATA_PROJECT$DATASET$ibes[[field1]][[field]][-to_remove]
  rm("field","field1")
}
ISSUERS_DATA_PROJECT$cleanupMissingSomeValues = length(to_remove)

# ALL (ABSOLUTE) RETURNS ARE e.g. 0-100 NOT 0-1 ranges... 
BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly <- 100*BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly
ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly <- 100*ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly

####################################################################################
# REMOVE THE CEO FROM THE WOMEN
BUYBACK_DATA_PROJECT$DATASET$boardex$women <- BUYBACK_DATA_PROJECT$DATASET$boardex$women - (BUYBACK_DATA_PROJECT$DATASET$boardex$CEO_female!=0)*1
ISSUERS_DATA_PROJECT$DATASET$boardex$women <- ISSUERS_DATA_PROJECT$DATASET$boardex$women - (ISSUERS_DATA_PROJECT$DATASET$boardex$CEO_female!=0)*1
####################################################################################

####################################################################################
# G-index
#system.time(gindexWRDS<- fread("~/OneDrive - INSEAD/FinanceData/rawdata_governance/Gindex.csv"))
# from http://faculty.som.yale.edu/andrewmetrick/data.html  HAS ALSO OLDER DATA BUT NO CUSIP
#system.time(gindexWebsite<- fread("~/OneDrive - INSEAD/FinanceData/rawdata_governance/Governance.csv"))

# E index
system.time(eindexWebsite<- fread("~/OneDrive - INSEAD/FinanceData/rawdata_governance/E Index 1990-2006.csv"))
system.time(eindexWRDSall<- fread("~/OneDrive - INSEAD/FinanceData/rawdata_governance/iSSgindexAll.csv"))
#http://www.law.harvard.edu/faculty/bebchuk/data.shtml
Eindex_created = 
  ifelse(!is.na(eindexWRDSall$CBOARD), eindexWRDSall$CBOARD == "YES", NA) + 
  ifelse(!is.na(eindexWRDSall$LABYLW), eindexWRDSall$LABYLW == "YES", NA) + 
  ifelse(!is.na(eindexWRDSall$LACHTR), eindexWRDSall$LACHTR == "NO", NA) + 
  ifelse(!is.na(eindexWRDSall$SUPERMAJOR_PCNT), eindexWRDSall$SUPERMAJOR_PCNT > 66.7, NA) + 
  ifelse(!is.na(eindexWRDSall$GPARACHUTE), eindexWRDSall$GPARACHUTE == "YES", NA) + 
  ifelse(!is.na(eindexWRDSall$PPILL), eindexWRDSall$PPILL == "YES", NA) 
Eindex_created = ifelse(!is.na(eindexWRDSall$DUALCLASS), ifelse(eindexWRDSall$DUALCLASS == "YES", NA, Eindex_created))

eindex_new = data.frame(
  year = eindexWRDSall$YEAR,
  cusip = str_sub(eindexWRDSall$CUSIP,start=1,end=8),
  eindex = Eindex_created
)

eindex_old = data.frame(
  year = eindexWebsite$Year,
  cusip = eindexWebsite$CUSIP,
  eindex = eindexWebsite$`E Index`
)

if (0){
  year_cusip = paste(eindex_new$year, eindex_new$cusip)
  eindex_new = Reduce(rbind, lapply(unique(year_cusip), function(i)
    c(eindex_new$year[which(year_cusip == i)[1]], eindex_new$cusip[which(year_cusip == i)[1]], non_na_mean(eindex_new$eindex[which(year_cusip == i)]))
  ))
  eindex_new = data.frame(
    year = as.integer(eindex_new[,1]),
    cusip = eindex_new[,2],
    eindex = as.numeric(eindex_new[,3])
  )
  rownames(eindex_new) <- paste(eindex_new$year, eindex_new$cusip)
  
  
  year_cusip_old = paste(eindex_old$year, eindex_old$cusip)
  eindex_old = Reduce(rbind, lapply(unique(year_cusip_old), function(i)
    c(eindex_old$year[which(year_cusip_old == i)[1]], eindex_old$cusip[which(year_cusip_old == i)[1]], non_na_mean(eindex_old$eindex[which(year_cusip_old == i)]))
  ))
  eindex_old = data.frame(
    year = as.integer(eindex_old[,1]),
    cusip = eindex_old[,2],
    eindex = as.numeric(eindex_old[,3])
  )
  rownames(eindex_old) <- paste(eindex_old$year, eindex_old$cusip)
  
  eindex_new = eindex_new[setdiff(rownames(eindex_new),rownames(eindex_old)),]
}

eindex_all = rbind(eindex_old,eindex_new)
#eindex_all = eindex_old
cusip_used= BUYBACK_DATA_PROJECT$DATASET$SDC$CUSIP
bbyear = as.numeric(format(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,"%Y"))
tmp = sapply(1:length(BUYBACK_DATA_PROJECT$DATASET$SDC$CUSIP), function(i){
  useonly = which(eindex_all$cusip == cusip_used[i] & eindex_all$year <=  bbyear[i]) # OLD WAY
  #useonly = which(eindex_all$cusip == cusip_used[i] & eindex_all$year <=  bbyear[i] & !is.na(eindex_all$eindex))
  ifelse(length(useonly)!=0,non_na_mean(eindex_all$eindex[useonly][eindex_all$year[useonly] == max(eindex_all$year[useonly])]),NA)
})
BUYBACK_DATA_PROJECT$DATASET$boardex$eindex = tmp
# Fix the e-index vs g index 
rm("cusip_used","bbyear","eindex_old","eindex_new","eindexWebsite","eindexWRDSall","Eindex_created","tmp")

####################################################################################
# These are one by one the tables in the .Rnw. 
####################################################################################

# THIS IS WHAT IS USED FROM NOW ON AS DEFAULT
project_data_subset = BUYBACK_DATA_PROJECT$DATASET$boardex
project_data_subset_iss = ISSUERS_DATA_PROJECT$DATASET$boardex

############################################################################
# PREPARE ALL VARIABLES
############################################################################

# First the EU-index with the quantiles are in Evgeniou et al (2016)
company_subset_undervalued_bb = BUYBACK_DATA_PROJECT$Valuation_Index > quantile(BUYBACK_DATA_PROJECT$Valuation_Index, 1-quantile_simple)
company_subset_overvalued_bb = BUYBACK_DATA_PROJECT$Valuation_Index < quantile(BUYBACK_DATA_PROJECT$Valuation_Index,quantile_simple)
High_Idiosyncr_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score, quantile_simple)
Low_Idiosyncr_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score, 1-quantile_simple)
High_IVOL_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score, 1-quantile_simple)
Low_IVOL_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score, quantile_simple)
High_VOL_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score, 1-quantile_simple)
Low_VOL_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score, quantile_simple)

BUYBACK_DATA_PROJECT$EU_index = sapply(1:length(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
    ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
})
rm("company_subset_undervalued_bb","company_subset_overvalued_bb","High_Idiosyncr_eventsBB",
   "Low_Idiosyncr_eventsBB","High_IVOL_eventsBB","Low_IVOL_eventsBB","High_VOL_eventsBB","Low_VOL_eventsBB")

#####
buybacks.events.past2years = 1*(BUYBACK_DATA_PROJECT$DATASET$CRSP$buybacks_events_past2years !=0)
#Recommendation score: 1. Strong Buy, 2. Buy, 3. Hold, 4. Underperform, 5. Sell
downgraded_events = !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) > scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
not_downgraded_events = !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) <= scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
upgraded_events = !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) < scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
#### Continuous variables now
# First the pairs with their scores
Firm_size = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap
Firm_size_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score
Prior_R = 100*BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance
Prior_R_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score
BEME = BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME
BEME_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score
U_index = BUYBACK_DATA_PROJECT$Valuation_Index
EU_index = BUYBACK_DATA_PROJECT$EU_index
Vol_raw = 100*BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol
Vol_raw_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score
Idiosyncratic = 100*BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL
Idiosyncratic_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score
One_m_Rsqr = 1-BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq
One_m_Rsqr_score = 1-BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score
market_beta = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta
market_beta_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score
Analyst_recommendation = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec)
Analyst_recommendation_score = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec_score)
Analyst_disagreement = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_disagreement)
Analyst_disagreement_score = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_disagreement_score)
Analyst_coverage = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_coverage)
Analyst_coverage_score = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_coverage_score)
IndependentDirectorsPercent = ifelse(BUYBACK_DATA_PROJECT$DATASET$boardex$total_directors!=0,BUYBACK_DATA_PROJECT$DATASET$boardex$number_of_independent_NED/BUYBACK_DATA_PROJECT$DATASET$boardex$total_directors, NA )

# No scores here
Event.Size = BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size
buybacks.events.past2years = 1*(BUYBACK_DATA_PROJECT$DATASET$CRSP$buybacks_events_past2years !=0)
Total.Payout = (BUYBACK_DATA_PROJECT$DATASET$CRSP$Total_Payout)
lagged.dividend.payout.ratio = BUYBACK_DATA_PROJECT$DATASET$CRSP$divident_payout_ratio
lagged.dividend.payout.ratio[scrub(lagged.dividend.payout.ratio) < 0 | scrub(lagged.dividend.payout.ratio) > 100] <- NA
lagged.dividend.payout.ratio = (lagged.dividend.payout.ratio) 
Leverage = (BUYBACK_DATA_PROJECT$DATASET$CRSP$leverage_d_over_d_plus_e)
operating.income = (BUYBACK_DATA_PROJECT$DATASET$CRSP$operating_income)
std.operating.income = (BUYBACK_DATA_PROJECT$DATASET$CRSP$std_operating_income)
non.operating.income = (BUYBACK_DATA_PROJECT$DATASET$CRSP$non_operating_income)
liquid.assets = (BUYBACK_DATA_PROJECT$DATASET$CRSP$liquid_assets)
price.earnings.ratio = (BUYBACK_DATA_PROJECT$DATASET$CRSP$price_earnings_ratio)
capital.expenditures = (BUYBACK_DATA_PROJECT$DATASET$CRSP$capital_expenditures)
profitability = (BUYBACK_DATA_PROJECT$DATASET$CRSP$profitability)
net_debt = (BUYBACK_DATA_PROJECT$DATASET$CRSP$net_debt)
tax_rate = abs(BUYBACK_DATA_PROJECT$DATASET$CRSP$tax_rate)
tax_rate = ifelse(tax_rate > 60, NA, tax_rate)
eindex= BUYBACK_DATA_PROJECT$DATASET$boardex$eindex
gender = 100*BUYBACK_DATA_PROJECT$DATASET$boardex$gender
women = BUYBACK_DATA_PROJECT$DATASET$boardex$women
board_size = BUYBACK_DATA_PROJECT$DATASET$boardex$total_directors
# Institutional - still not standard across projects
Institutional = (BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1)
Institutional[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
Institutional_score = (BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1_score)
Institutional_score[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
Institutional.number = (BUYBACK_DATA_PROJECT$DATASET$institutional$num.institutional.investors)
Institutional.number[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
Institutional.number_score = (BUYBACK_DATA_PROJECT$DATASET$institutional$num.institutional.investors_score)
Institutional.number_score[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA

########################
# WOMEN DATA HERE
missing_data_value = NA
womenboards = ifelse(BUYBACK_DATA_PROJECT$DATASET$boardex$women,BUYBACK_DATA_PROJECT$DATASET$boardex$womenboards_sum/BUYBACK_DATA_PROJECT$DATASET$boardex$women, missing_data_value)
menboards = ifelse(BUYBACK_DATA_PROJECT$DATASET$boardex$men,BUYBACK_DATA_PROJECT$DATASET$boardex$menboards_sum/BUYBACK_DATA_PROJECT$DATASET$boardex$men, missing_data_value)

# Percent of women with social ties (been on other boards with) with men on the focal firm's board
womenboards_sociallinks = 100*ifelse(BUYBACK_DATA_PROJECT$DATASET$boardex$women,BUYBACK_DATA_PROJECT$DATASET$boardex$number_women_menboard_social_ties/BUYBACK_DATA_PROJECT$DATASET$boardex$women, missing_data_value)
womenboards_jobslinks = 100*ifelse(BUYBACK_DATA_PROJECT$DATASET$boardex$women,BUYBACK_DATA_PROJECT$DATASET$boardex$number_women_board_job_ties/BUYBACK_DATA_PROJECT$DATASET$boardex$women, missing_data_value)
womenboards_sociallinks = ifelse(scrub(womenboards_sociallinks) > 100, missing_data_value, womenboards_sociallinks) # Problematic cases
womenboards_jobslinks = ifelse(scrub(womenboards_jobslinks) > 100, missing_data_value, womenboards_jobslinks) # Problematic cases

## our instruments
men_with_other_boards_women_percent = 100*pmin(1,ifelse(BUYBACK_DATA_PROJECT$DATASET$boardex$men!=0, BUYBACK_DATA_PROJECT$DATASET$boardex$other_female_boards_sum/BUYBACK_DATA_PROJECT$DATASET$boardex$men,missing_data_value))##
## Get the second instrument (for robustness)
system.time(counties <- fread("~/OneDrive - INSEAD/FinanceData/rawdata_compustat/yearly_data_compustat.csv"))
#counties <- read.csv("counties.csv", header=TRUE, sep=",", dec=".")
female_counties <- read.csv("FemaleCounties.csv", header=TRUE, sep=";", dec=",")
library(foreign)
counties_matches  <- read.dta("fips_census.dta")
zip_matches  <- read.dta("zipfips.dta")
###
# Get the female percent per fipcode
tmp1 = paste(toupper(female_counties$State_download), toupper(str_sub(female_counties$County_download,start=1,end=3)))
tmp2 = paste(toupper(counties_matches$statename), toupper(str_sub(counties_matches$countyname,start=1,end=3)))
counties_matches$female_percent = female_counties$Civilian.Labor.Force..Female.Participation.Rate.1990.Rte[match(tmp2,tmp1)]
rm("female_counties", "tmp1","tmp2")
# get the firm zipcodes
counties_gvkey_yearmonth = paste(counties$GVKEY, str_sub(counties$datadate,start=1,end=4), sep="")
bb_gvkey_yearmonth = paste(as.numeric(BUYBACK_DATA_PROJECT$DATASET$SDC$GVKEY), str_sub(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,start=1,end=4), sep="") 
bb_zipcode = counties$addzip[match(bb_gvkey_yearmonth,counties_gvkey_yearmonth)]
# Now from the zip codes of the firms get the female participation in the county via matching the zipcode with the fipscode. 
bb_fipcodes = zip_matches$fips[match(bb_zipcode,zip_matches$zipcode)]
BUYBACK_DATA_PROJECT$DATASET$boardex$county_gender = counties_matches[match(bb_fipcodes,counties_matches$FIPS),"female_percent"]
county_gender = BUYBACK_DATA_PROJECT$DATASET$boardex$county_gender 

########################

# THESE ARE THE VARIABLES WE USE IN THE DATA SUMMARY STATS
all_characteristics_continuous_summary = cbind(
  buybacks.events.past2years,
  Firm_size,
  Prior_R,
  BEME,
  U_index,
  EU_index,
  Vol_raw,
  One_m_Rsqr,
  Event.Size,
  Analyst_coverage,
  gender,
  women,
  womenboards,
  menboards,
  womenboards_sociallinks,
  board_size,
  IndependentDirectorsPercent,
  men_with_other_boards_women_percent,
  Total.Payout,
  lagged.dividend.payout.ratio,
  Leverage,
  profitability,
  net_debt,
  tax_rate,
  operating.income,
  std.operating.income,
  non.operating.income,
  liquid.assets,
  price.earnings.ratio,
  capital.expenditures,
  Institutional,
  Institutional.number,
  eindex
)
colnames(all_characteristics_continuous_summary) <- c(
  "Announced Repurchace in Previous 2 Years (0/1)",
  "Market Cap. (Mil.)", 
  "Prior Returns",
  "BE/ME", 
  "U-index",
  "EU-index",
  "Volatility (Percent)", 
  "One minus Rsq",
  "Percent Shares",
  "Analyst Coverage",
  "Female Percent on Board",
  "Female Number on Board",
  "Numb. of Other Boards for Women",
  "Numb. of Other Boards for Men", 
  "Percent of Women School Links",
  "Board Size",
  "Percent Independent Directors",
  "Percent of Men on Other Boards with Women",
  "Total Payout in Event Year before Event",
  "Lag Dividend Payout Ratio",
  "Leverage",
  "Profitability (ROA)",
  "Net Debt",
  "Tax Rate",
  "Operating Income (Percent assets)", 
  "std Operating Income", 
  "Non-Operating Income (Percent assets)",
  "Liquid Assets (Percent assets)",
  "Price/Earnings Ratio",
  "Capital Expenditures (Percent assets)",
  "Institutional Holdings",
  "Number of Institutions",
  "E-index"
)

# THESE ARE THE VARIABLES WE USE IN ALL REGRESSIONS
all_characteristics_continuous = cbind(
  buybacks.events.past2years,
  Firm_size,
  Prior_R,BEME,
  U_index,
  EU_index,
  Vol_raw,
  One_m_Rsqr,
  Event.Size,
  Analyst_coverage,
  womenboards,
  menboards,
  womenboards_sociallinks,
  board_size,
  IndependentDirectorsPercent,
  Total.Payout,
  lagged.dividend.payout.ratio,
  Leverage,
  profitability,
  net_debt,
  tax_rate,
  operating.income,
  std.operating.income,
  non.operating.income,
  liquid.assets,
  price.earnings.ratio,
  capital.expenditures,
  Institutional,
  Institutional.number,
  eindex
)
colnames(all_characteristics_continuous) <- c(
  "Announced Repurchace in Previous 2 Years (0/1)",
  "Market Cap.", 
  "Prior Returns",
  "BE/ME", 
  "U-index",
  "EU-index",
  "Volatility", 
  "One minus Rsq",
  "Percent Shares",
  "Analyst Coverage",
  "Numb. of Other Boards for Women",
  "Numb. of Other Boards for Men",
  "Percent of Women School Links",
  "Board Size",
  "Percent Independent Directors",
  "Total Payout in Event Year before Event",
  "Lag Dividend Payout Ratio",
  "Leverage",
  "Profitability (ROA)",
  "Net Debt",
  "Tax Rate",
  "Operating Income (Percent assets)", 
  "std Operating Income", 
  "Non-Operating Income (Percent assets)",
  "Liquid Assets (Percent assets)",
  "Price/Earnings Ratio",
  "Capital Expenditures (Percent assets)",
  "Institutional Holdings",
  "Number of Institutions",
  "E-index"
)

##################################################################################################################
## ADD ANY PROJECT SPECIFIC VARIABLES TO all_characteristics_continuous
##################################################################################################################

###############################################################################################
# Histograms
###############################################################################################

project_dates = str_sub(rownames(Risk_Factors_Monthly)[which(as.Date(rownames(Risk_Factors_Monthly)) >= min(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date) & as.Date(rownames(Risk_Factors_Monthly)) <= max(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date))])
yearlag = 0
bb_years = format(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, "%Y")
hist_data = gender
###
yearly_features_count <- sapply(unique(format(as.Date(project_dates), "%Y")), function(i){
  tmp2 = hist_data[bb_years==as.character(as.numeric(i)+yearlag)]
  length(tmp2[!is.na(tmp2)])
})
names(yearly_features_count)<- unique(format(as.Date(project_dates), "%Y"))
###

yearly_features_mean <- sapply(unique(format(as.Date(project_dates), "%Y")), function(i){
  non_na_mean <- function(x) { mean(x[!is.na(x) & x != 0]) }
  tmp2 = hist_data[bb_years==as.character(as.numeric(i)+yearlag)]
  non_na_mean(tmp2)
})
names(yearly_features_mean)<- unique(format(as.Date(project_dates), "%Y"))
###
hist_data = project_data_subset$women
yearly_features_mean_number <- sapply(unique(format(as.Date(project_dates), "%Y")), function(i){
  non_na_mean <- function(x) { mean(x[!is.na(x) & x != 0]) }
  tmp2 = hist_data[bb_years==as.character(as.numeric(i)+yearlag)]
  non_na_mean(tmp2)
})
names(yearly_features_mean_number)<- unique(format(as.Date(project_dates), "%Y"))

###
datasummaryBB = rbind(
  round(c(summary(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size) & BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size) & BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size!=0]),sum(is.na(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size) | BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap) & BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap) & BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0]),sum(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap) | BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME[BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME) & BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME[BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME) & BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME!=0]),sum(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME >= 1e20) | BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME ==0 | BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryBB) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryBB)[ncol(datasummaryBB)-1] <- "std"
colnames(datasummaryBB)[ncol(datasummaryBB)] <- "Missing"

datasummaryISS = rbind(
  round(c(summary(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size) & ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size) & ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size!=0]),sum(is.na(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size) | ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap) & ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap) & ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0]),sum(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap) | ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME[ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME) & ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME[ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME) & ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME!=0]),sum(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME >= 1e20) | ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME ==0 | ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryISS) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryISS)[ncol(datasummaryISS)-1] <- "std"
colnames(datasummaryISS)[ncol(datasummaryISS)] <- "Missing"

########################################################################################################
# SINGLE SORT IRATS/CALENDAR
########################################################################################################
# WE USE THIS IN THE SINGLE SORTS FIRST
thefeature_single_sort = BUYBACK_DATA_PROJECT$DATASET$boardex$women # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 

useonly_no_project_var = which(scrub(thefeature_single_sort) == 0 & !is.na(thefeature_single_sort))
no_project_var_all = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var], Risk_Factors_Monthly)
no_project_var = no_project_var_all$results
no_project_var_cal_all = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_no_project_var])
no_project_var_cal = no_project_var_cal_all$results

useonly_some_project_var = which(scrub(thefeature_single_sort) !=0 & !is.na(thefeature_single_sort))
some_project_var_all = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var], Risk_Factors_Monthly)
some_project_var = some_project_var_all$results
some_project_var_cal_all = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_some_project_var])
some_project_var_cal = some_project_var_cal_all$results

lowstd = no_project_var_all$results[,1]/no_project_var_all$results[,2]
highstd = some_project_var_all$results[,1]/some_project_var_all$results[,2]
no_project_var_minus_some_project_var = (no_project_var_all$results[,1] - some_project_var_all$results[,1])
tvalHL = no_project_var_minus_some_project_var/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(no_project_var_all$dfs-1, some_project_var_all$dfs -1))
no_project_var_minus_some_project_var = cbind(no_project_var_minus_some_project_var, tvalHL,pvalHL)
no_project_var_minus_some_project_var[nrow(no_project_var_minus_some_project_var),] <- 0

lowstd = no_project_var_cal_all$results[,1]/no_project_var_cal_all$results[,2]
highstd = some_project_var_cal_all$results[,1]/some_project_var_cal_all$results[,2]
no_project_var_minus_some_project_var_cal = (no_project_var_cal_all$results[,1] - some_project_var_cal_all$results[,1])
tvalHL = no_project_var_minus_some_project_var_cal/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(no_project_var_cal_all$dfs-1, some_project_var_cal_all$dfs -1))
no_project_var_minus_some_project_var_cal = cbind(no_project_var_minus_some_project_var_cal, tvalHL,pvalHL)
no_project_var_minus_some_project_var_cal[nrow(no_project_var_minus_some_project_var_cal),] <- 0

# WE USE THIS NOW IN THE SINGLE SORTS 
thefeature_single_sort = BUYBACK_DATA_PROJECT$DATASET$boardex$gender # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 
# Also make this into a T/F vector
High_project_eventsBB = thefeature_single_sort >= quantile(thefeature_single_sort, 1-quantile_simple)
Low_project_eventsBB = thefeature_single_sort <= quantile(thefeature_single_sort, quantile_simple)

useonly_high_project_var = which(thefeature_single_sort >= quantile(thefeature_single_sort, 1-quantile_simple))
high_project_var_all = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_high_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_high_project_var], Risk_Factors_Monthly)
high_project_var = high_project_var_all$results
high_project_var_cal_all = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_high_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_high_project_var], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_high_project_var])
high_project_var_cal = high_project_var_cal_all$results

lowstd = no_project_var_all$results[,1]/no_project_var_all$results[,2]
highstd = high_project_var_all$results[,1]/high_project_var_all$results[,2]
no_project_var_minus_high_project_var = (no_project_var_all$results[,1] - high_project_var_all$results[,1])
tvalHL = no_project_var_minus_high_project_var/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(no_project_var_all$dfs-1, high_project_var_all$dfs -1))
no_project_var_minus_high_project_var = cbind(no_project_var_minus_high_project_var, tvalHL,pvalHL)
no_project_var_minus_high_project_var[nrow(no_project_var_minus_high_project_var),] <- 0

rm("no_project_var_all","no_project_var_cal_all","some_project_var_all","some_project_var_cal_all",
   "high_project_var_all")

#######################################
#### SOME PROJECT SPECIFIC SINGLE SORTS

## eindex
useonly = scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex) >= quantile(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex[!is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)], 1-quantile_simple) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)
eindex_BB0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
eindex_BB0_df = eindex_BB0$dfs
eindex_BB0 = eindex_BB0$results
eindex_BB0_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
eindex_BB0_cal_df = eindex_BB0_cal$dfs
eindex_BB0_cal = eindex_BB0_cal$results

useonly = scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex) < quantile(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex[!is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)], 1-quantile_simple) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)
eindex_BB1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
eindex_BB1_df = eindex_BB1$dfs
eindex_BB1 = eindex_BB1$results
eindex_BB1_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
eindex_BB1_cal_df = eindex_BB1_cal$dfs
eindex_BB1_cal = eindex_BB1_cal$results

tmp1=eindex_BB0; tmp2=eindex_BB1
df1=eindex_BB0_df; df2=eindex_BB1_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
eindex_BB0_minus_eindex_BB1 = tmp

tmp1=eindex_BB0_cal; tmp2=eindex_BB1_cal
df1=eindex_BB0_cal_df; df2=eindex_BB1_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
eindex_BB0_minus_eindex_BB1_cal = tmp


## Number of womenboard
useonly  = which((scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$women) != 0) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$women) &
                   womenboards ==0 & !is.na(womenboards))
womenboards_BB0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenboards_BB0_df = womenboards_BB0$dfs
womenboards_BB0 = womenboards_BB0$results
womenboards_BB0_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenboards_BB0_cal_df = womenboards_BB0_cal$dfs
womenboards_BB0_cal = womenboards_BB0_cal$results

useonly  = which((scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$women) != 0) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$women) &
                   womenboards >= quantile(womenboards[!is.na(womenboards)], quantile_simple) & !is.na(womenboards) & scrub(womenboards)!=0)
womenboards_BB1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenboards_BB1_df = womenboards_BB1$dfs
womenboards_BB1 = womenboards_BB1$results
womenboards_BB1_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenboards_BB1_cal_df = womenboards_BB1_cal$dfs
womenboards_BB1_cal = womenboards_BB1_cal$results

tmp1=womenboards_BB0; tmp2=womenboards_BB1
df1=womenboards_BB0_df; df2=womenboards_BB1_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenboards_BB0_minus_womenboards_BB1 = tmp

tmp1=womenboards_BB0_cal; tmp2=womenboards_BB1_cal
df1=womenboards_BB0_cal_df; df2=womenboards_BB1_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenboards_BB0_minus_womenboards_BB1_cal = tmp


## Number of womenboard Social Links
useonly  = which(scrub(womenboards_sociallinks) < quantile(scrub(womenboards_sociallinks)[!is.na(womenboards_sociallinks)],0.5) & !is.na(womenboards_sociallinks))
womenboards_socialinks_BB0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenboards_socialinks_BB0_df = womenboards_socialinks_BB0$dfs
womenboards_socialinks_BB0 = womenboards_socialinks_BB0$results
womenboards_socialinks_BB0_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenboards_socialinks_BB0_cal_df = womenboards_socialinks_BB0_cal$dfs
womenboards_socialinks_BB0_cal = womenboards_socialinks_BB0_cal$results

useonly  = which(scrub(womenboards_sociallinks) >= quantile(scrub(womenboards_sociallinks)[!is.na(womenboards_sociallinks)],1-0.5) & !is.na(womenboards_sociallinks))
womenboards_socialinks_BB1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenboards_socialinks_BB1_df = womenboards_socialinks_BB1$dfs
womenboards_socialinks_BB1 = womenboards_socialinks_BB1$results
womenboards_socialinks_BB1_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenboards_socialinks_BB1_cal_df = womenboards_socialinks_BB1_cal$dfs
womenboards_socialinks_BB1_cal = womenboards_socialinks_BB1_cal$results

tmp1=womenboards_socialinks_BB0; tmp2=womenboards_socialinks_BB1
df1=womenboards_socialinks_BB0_df; df2=womenboards_socialinks_BB1_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenboards_socialinks_BB0_minus_womenboards_socialinks_BB1 = tmp

tmp1=womenboards_socialinks_BB0_cal; tmp2=womenboards_socialinks_BB1_cal
df1=womenboards_socialinks_BB0_cal_df; df2=womenboards_socialinks_BB1_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenboards_socialinks_BB0_minus_womenboards_socialinks_BB1_cal = tmp

# Jobs connections
useonly  = which((scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$women) != 0) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$women) &
                   scrub(womenboards_jobslinks) <= quantile(scrub(womenboards_jobslinks)[!is.na(womenboards_jobslinks)],0.5) & !is.na(womenboards_jobslinks))
womenboards_jobslinks_BB0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenboards_jobslinks_BB0_df = womenboards_jobslinks_BB0$dfs
womenboards_jobslinks_BB0 = womenboards_jobslinks_BB0$results
womenboards_jobslinks_BB0_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenboards_jobslinks_BB0_cal_df = womenboards_jobslinks_BB0_cal$dfs
womenboards_jobslinks_BB0_cal = womenboards_jobslinks_BB0_cal$results

useonly  = which((scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$women) != 0) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$women) &
                   scrub(womenboards_jobslinks) > quantile(scrub(womenboards_jobslinks)[!is.na(womenboards_jobslinks)],0.5) & !is.na(womenboards_jobslinks))
womenboards_jobslinks_BB1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenboards_jobslinks_BB1_df = womenboards_jobslinks_BB1$dfs
womenboards_jobslinks_BB1 = womenboards_jobslinks_BB1$results
womenboards_jobslinks_BB1_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenboards_jobslinks_BB1_cal_df = womenboards_jobslinks_BB1_cal$dfs
womenboards_jobslinks_BB1_cal = womenboards_jobslinks_BB1_cal$results

tmp1=womenboards_jobslinks_BB0; tmp2=womenboards_jobslinks_BB1
df1=womenboards_jobslinks_BB0_df; df2=womenboards_jobslinks_BB1_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenboards_jobslinks_BB0_minus_womenboards_jobslinks_BB1 = tmp

tmp1=womenboards_jobslinks_BB0_cal; tmp2=womenboards_jobslinks_BB1_cal
df1=womenboards_jobslinks_BB0_cal_df; df2=womenboards_jobslinks_BB1_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenboards_jobslinks_BB0_minus_womenboards_jobslinks_BB1_cal = tmp

####################################
## Number of women
useonly = which((scrub(project_data_subset$women) == 0) & !is.na(project_data_subset$women))
women_sum0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
women_sum0_df = women_sum0$dfs
women_sum0 = women_sum0$results
women_sum0_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
women_sum0_cal_df = women_sum0_cal$dfs
women_sum0_cal = women_sum0_cal$results
###
useonly = which((scrub(project_data_subset$women) == 1) & !is.na(project_data_subset$women))
women_sum1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
women_sum1_df = women_sum1$dfs
women_sum1 = women_sum1$results
women_sum1_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
women_sum1_cal_df = women_sum1_cal$dfs
women_sum1_cal = women_sum1_cal$results
###
useonly = which((scrub(project_data_subset$women) == 2) & !is.na(project_data_subset$women))
women_sum2 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
women_sum2_df = women_sum2$dfs
women_sum2 = women_sum2$results
women_sum2_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
women_sum2_cal_df = women_sum2_cal$dfs
women_sum2_cal = women_sum2_cal$results
###
useonly = which((scrub(project_data_subset$women) == 3) & !is.na(project_data_subset$women))
women_sum3 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
women_sum3_df = women_sum3$dfs
women_sum3 = women_sum3$results
women_sum3_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
women_sum3_cal_df = women_sum3_cal$dfs
women_sum3_cal = women_sum3_cal$results
###
useonly = which((scrub(project_data_subset$women) > 3) & !is.na(project_data_subset$women))
women_sum4 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
women_sum4_df = women_sum4$dfs
women_sum4 = women_sum4$results
women_sum4_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
women_sum4_cal_df = women_sum4_cal$dfs
women_sum4_cal = women_sum4_cal$results


###
tmp1=women_sum0; tmp2=women_sum1
df1=women_sum0_df; df2=women_sum1_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum1 = tmp

tmp1=women_sum0_cal; tmp2=women_sum1_cal
df1=women_sum0_cal_df; df2=women_sum1_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum1_cal = tmp

tmp1=women_sum0; tmp2=women_sum2
df1=women_sum0_df; df2=women_sum2_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum2 = tmp

tmp1=women_sum0_cal; tmp2=women_sum2_cal
df1=women_sum0_cal_df; df2=women_sum2_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum2_cal = tmp

tmp1=women_sum0; tmp2=women_sum3
df1=women_sum0_df; df2=women_sum3_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum3 = tmp

tmp1=women_sum0_cal; tmp2=women_sum3_cal
df1=women_sum0_cal_df; df2=women_sum3_cal_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum3_cal = tmp

tmp1=women_sum0; tmp2=women_sum4
df1=women_sum0_df; df2=women_sum3_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum0_minus_women_sum4 = tmp

tmp1=women_sum1; tmp2=women_sum2
df1=women_sum1_df; df2=women_sum2_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum1_minus_women_sum2 = tmp

tmp1=women_sum1; tmp2=women_sum3
df1=women_sum1_df; df2=women_sum3_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum1_minus_women_sum3 = tmp

tmp1=women_sum1; tmp2=women_sum4
df1=women_sum1_df; df2=women_sum4_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum1_minus_women_sum4 = tmp

tmp1=women_sum2; tmp2=women_sum3
df1=women_sum2_df; df2=women_sum3_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
women_sum2_minus_women_sum3 = tmp

######## CEO/CFO
useonly_CEO_women = which((scrub(project_data_subset$CEO_female) != 0) & !is.na(project_data_subset$CEO_female))
CEO_women = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women], Risk_Factors_Monthly)$results
CEO_women_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_women])$results
###
useonly_CFO_women = which((scrub(project_data_subset$CFO_female) != 0) & !is.na(project_data_subset$CFO_female))
CFO_women = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women], Risk_Factors_Monthly)$results
CFO_women_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CFO_women])$results
###
useonly_senior_women = which(scrub(project_data_subset$other_senior_female) != 0 & !is.na(project_data_subset$other_senior_female))
senior_women  = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women], Risk_Factors_Monthly)$results
senior_women_cal  = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_senior_women])$results
###
useonly_CEO_men = which(scrub(project_data_subset$CEO_female) == 0 & !is.na(project_data_subset$CEO_female))
CEO_men = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men], Risk_Factors_Monthly)$results
CEO_men_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_men])$results

########################################################################################################
# DOUBLE SORT IRATS/CALENDAR
########################################################################################################
# WE USE THIS IN THE DOUBLE SORTS BY DEFAULT 
thefeature_double_sort = 1*(project_data_subset$women  > 0)

# HERE WE USE THE quantile_doublesort PERCENTILE SINCE THESE ARE ONLY USED FOR THE DOUBLESORT
High_eindex_eventsBB = scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex) > quantile(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex[!is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)], 1-quantile_doublesort) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)
Low_eindex_eventsBB = scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex) <= quantile(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex[!is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)], quantile_doublesort) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)
High_eindex_eventsBB = scrub(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex) > quantile(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex[!is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)], 1-quantile_doublesort) & !is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$eindex)
company_subset_undervalued_bb = BUYBACK_DATA_PROJECT$Valuation_Index > quantile(BUYBACK_DATA_PROJECT$Valuation_Index, 1-quantile_doublesort)
company_subset_overvalued_bb = BUYBACK_DATA_PROJECT$Valuation_Index < quantile(BUYBACK_DATA_PROJECT$Valuation_Index,quantile_doublesort)
High_perf_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score  > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score,1-quantile_doublesort)
Low_perf_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score  < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score,quantile_doublesort)
High_Size_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score  > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score,1-quantile_doublesort)
Low_Size_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score  < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score,quantile_doublesort)
High_BEME_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score  > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score,1-quantile_doublesort)
Low_BEME_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score  < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score,quantile_doublesort)
High_Idiosyncr_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score, quantile_doublesort)
Low_Idiosyncr_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score, 1-quantile_doublesort)
High_IVOL_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score, 1-quantile_doublesort)
Low_IVOL_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score, quantile_doublesort)
High_VOL_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score, 1-quantile_doublesort)
Low_VOL_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score, quantile_doublesort)
High_marketbeta_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, 1-quantile_doublesort)
Medium_marketbeta_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score >= quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, quantile_doublesort) & BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score <= quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, 1-quantile_doublesort)
Low_marketbeta_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, quantile_doublesort)
tmp = BUYBACK_DATA_PROJECT$DATASET$CRSP$leverage_lt_over_lt_plus_e
High_LEV_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_doublesort)  & !is.na(tmp)
Low_LEV_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_doublesort)  & !is.na(tmp)
tmp = BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec_score
High_EPS_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_doublesort)  & !is.na(tmp)
Low_EPS_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_doublesort)  & !is.na(tmp)
company_subset_undervalued_iss = ISSUERS_DATA_PROJECT$Valuation_Index > quantile(ISSUERS_DATA_PROJECT$Valuation_Index, 1-quantile_doublesort)
company_subset_overvalued_iss = ISSUERS_DATA_PROJECT$Valuation_Index < quantile(ISSUERS_DATA_PROJECT$Valuation_Index,quantile_doublesort)
high_EU_bb = BUYBACK_DATA_PROJECT$EU_index > quantile(BUYBACK_DATA_PROJECT$EU_index,1-quantile_doublesort)
low_EU_bb = BUYBACK_DATA_PROJECT$EU_index < quantile(BUYBACK_DATA_PROJECT$EU_index,quantile_doublesort)
High_inst_eventsBB = scrub(Institutional_score) > quantile(Institutional_score[!is.na(Institutional_score)], 1-quantile_doublesort)  & !is.na(Institutional_score)
Low_inst_eventsBB  = scrub(Institutional_score) < quantile(Institutional_score[!is.na(Institutional_score)], quantile_doublesort)  & !is.na(Institutional_score)


binary_doublesort <- function(second_feature){
  useonly = which(thefeature_double_sort == 0 & second_feature)
  women_sum0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
  women_sum0_df = women_sum0$dfs
  women_sum0 = women_sum0$results
  ###
  useonly = which(thefeature_double_sort == 1 & second_feature)
  women_sum1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
  women_sum1_df = women_sum1$dfs
  women_sum1 = women_sum1$results
  ###
  tmp1=women_sum0; tmp2=women_sum1
  df1=women_sum0_df; df2=women_sum1_df
  lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
  pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
  under_high_low = tmp
  under_high_low[nrow(under_high_low),] <- 0
  cbind(women_sum0,women_sum1,under_high_low)
}


project_Eindex_IRATStable_underBB =  cbind(binary_doublesort(Low_eindex_eventsBB),binary_doublesort(High_eindex_eventsBB))
project_Uindex_IRATStable_underBB =  cbind(binary_doublesort(company_subset_overvalued_bb),binary_doublesort(company_subset_undervalued_bb))
project_EUindex_IRATStable_underBB =  cbind(binary_doublesort(low_EU_bb),binary_doublesort(high_EU_bb))
project_Vol_IRATStable_underBB =  cbind(binary_doublesort(Low_VOL_eventsBB),binary_doublesort(High_VOL_eventsBB))
project_Idio_IRATStable_underBB =  cbind(binary_doublesort(Low_Idiosyncr_eventsBB),binary_doublesort(High_Idiosyncr_eventsBB))
project_Size_IRATStable_underBB =  cbind(binary_doublesort(Low_Size_eventsBB),binary_doublesort(High_Size_eventsBB))
project_perf_IRATStable_underBB =  cbind(binary_doublesort(Low_perf_eventsBB),binary_doublesort(High_perf_eventsBB))
project_Instit_IRATStable_underBB =  cbind(binary_doublesort(Low_inst_eventsBB),binary_doublesort(High_inst_eventsBB))
project_eindex_IRATStable_underBB =  cbind(binary_doublesort(Low_eindex_eventsBB),binary_doublesort(High_eindex_eventsBB))

# Second double sort
thefeature_double_sort = High_project_eventsBB - Low_project_eventsBB

binary_doublesort <- function(second_feature){
  useonly = which(thefeature_double_sort == -1 & second_feature)
  women_sum0 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
  women_sum0_df = women_sum0$dfs
  women_sum0 = women_sum0$results
  ###
  useonly = which(thefeature_double_sort == 1 & second_feature)
  women_sum1 = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
  women_sum1_df = women_sum1$dfs
  women_sum1 = women_sum1$results
  ###
  tmp1=women_sum0; tmp2=women_sum1
  df1=women_sum0_df; df2=women_sum1_df
  lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
  pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
  under_high_low = tmp
  under_high_low[nrow(under_high_low),] <- 0
  cbind(women_sum0,women_sum1,under_high_low)
}

project_Eindex_IRATStable_underBB2 =  cbind(binary_doublesort(Low_eindex_eventsBB),binary_doublesort(High_eindex_eventsBB))
project_Uindex_IRATStable_underBB2 =  cbind(binary_doublesort(company_subset_overvalued_bb),binary_doublesort(company_subset_undervalued_bb))
project_EUindex_IRATStable_underBB2 =  cbind(binary_doublesort(low_EU_bb),binary_doublesort(high_EU_bb))
project_Vol_IRATStable_underBB2 =  cbind(binary_doublesort(Low_VOL_eventsBB),binary_doublesort(High_VOL_eventsBB))
project_Idio_IRATStable_underBB2 =  cbind(binary_doublesort(Low_Idiosyncr_eventsBB),binary_doublesort(High_Idiosyncr_eventsBB))
project_Size_IRATStable_underBB2 =  cbind(binary_doublesort(Low_Size_eventsBB),binary_doublesort(High_Size_eventsBB))
project_perf_IRATStable_underBB2 =  cbind(binary_doublesort(Low_perf_eventsBB),binary_doublesort(High_perf_eventsBB))
project_Instit_IRATStable_underBB2 =  cbind(binary_doublesort(Low_inst_eventsBB),binary_doublesort(High_inst_eventsBB))
project_eindex_IRATStable_underBB2 =  cbind(binary_doublesort(Low_eindex_eventsBB),binary_doublesort(High_eindex_eventsBB))


##################################
### Some project specific analysis 

####### ISSUERS
#############################################
# Single sorts
thefeature_single_sort = ISSUERS_DATA_PROJECT$DATASET$boardex$women # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 
###
useonly_no_project_var_iss = which(scrub(thefeature_single_sort) == 0 & !is.na(thefeature_single_sort))
no_project_var_all = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var_iss], Risk_Factors_Monthly)
no_project_var_iss = no_project_var_all$results
no_project_var_iss_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var_iss], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly_no_project_var_iss])$results
###
useonly_some_project_var_iss = which(scrub(thefeature_single_sort) !=0 & !is.na(thefeature_single_sort))
some_project_var_all = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var_iss], Risk_Factors_Monthly)
some_project_var_iss = some_project_var_all$results
some_project_var_iss_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var_iss], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly_some_project_var_iss])$results
###
lowstd = no_project_var_all$results[,1]/no_project_var_all$results[,2]
highstd = some_project_var_all$results[,1]/some_project_var_all$results[,2]
no_project_var_minus_some_project_var_iss = (no_project_var_all$results[,1] - some_project_var_all$results[,1])
tvalHL = no_project_var_minus_some_project_var_iss/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(no_project_var_all$dfs-1, some_project_var_all$dfs -1))
no_project_var_minus_some_project_var_iss = cbind(no_project_var_minus_some_project_var_iss, tvalHL,pvalHL)
no_project_var_minus_some_project_var_iss[nrow(no_project_var_minus_some_project_var_iss),] <- 0


# WE USE THIS NOW IN THE SINGLE SORTS 
thefeature_single_sort = ISSUERS_DATA_PROJECT$DATASET$boardex$gender # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 
# Also make this into a T/F vector
High_project_eventsISS = thefeature_single_sort >= quantile(thefeature_single_sort, 1-quantile_simple)
Low_project_eventsISS = thefeature_single_sort <= quantile(thefeature_single_sort, quantile_simple)

useonly_high_project_var = which(thefeature_single_sort >= quantile(thefeature_single_sort, 1-quantile_simple))
high_project_var_all = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_high_project_var], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_high_project_var], Risk_Factors_Monthly)
high_project_var_iss = high_project_var_all$results
high_project_var_cal_all = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_high_project_var], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_high_project_var], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_high_project_var])
high_project_var_cal_iss = high_project_var_cal_all$results

lowstd = no_project_var_all$results[,1]/no_project_var_all$results[,2]
highstd = high_project_var_all$results[,1]/high_project_var_all$results[,2]
no_project_var_minus_high_project_var_iss = (no_project_var_all$results[,1] - high_project_var_all$results[,1])
tvalHL = no_project_var_minus_high_project_var_iss/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(no_project_var_all$dfs-1, high_project_var_all$dfs -1))
no_project_var_minus_high_project_var_iss = cbind(no_project_var_minus_high_project_var_iss, tvalHL,pvalHL)
no_project_var_minus_high_project_var_iss[nrow(no_project_var_minus_high_project_var_iss),] <- 0

#############################################

## Number of women
thefeature_single_sort = ISSUERS_DATA_PROJECT$DATASET$boardex$women # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 

useonly = which((scrub(thefeature_single_sort) == 0) & !is.na(thefeature_single_sort))
womenISS_sum0 = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenISS_sum0_df = womenISS_sum0$dfs
womenISS_sum0 = womenISS_sum0$results
womenISS_sum0_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenISS_sum0_cal_df = womenISS_sum0_cal$dfs
womenISS_sum0_cal = womenISS_sum0_cal$results
###
useonly = which((scrub(thefeature_single_sort) == 1) & !is.na(thefeature_single_sort))
womenISS_sum1 = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenISS_sum1_df = womenISS_sum1$dfs
womenISS_sum1 = womenISS_sum1$results
womenISS_sum1_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenISS_sum1_cal_df = womenISS_sum1_cal$dfs
womenISS_sum1_cal = womenISS_sum1_cal$results
###
useonly = which((scrub(thefeature_single_sort) == 2) & !is.na(thefeature_single_sort))
womenISS_sum2 = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenISS_sum2_df = womenISS_sum2$dfs
womenISS_sum2 = womenISS_sum2$results
womenISS_sum2_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenISS_sum2_cal_df = womenISS_sum2_cal$dfs
womenISS_sum2_cal = womenISS_sum2_cal$results
###
useonly = which((scrub(thefeature_single_sort) == 3) & !is.na(thefeature_single_sort))
womenISS_sum3 = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenISS_sum3_df = womenISS_sum3$dfs
womenISS_sum3 = womenISS_sum3$results
womenISS_sum3_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenISS_sum3_cal_df = womenISS_sum3_cal$dfs
womenISS_sum3_cal = womenISS_sum3_cal$results
###
useonly = which((scrub(thefeature_single_sort) >3) & !is.na(thefeature_single_sort))
womenISS_sum4 = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)
womenISS_sum3_df = womenISS_sum4$dfs
womenISS_sum4 = womenISS_sum4$results
womenISS_sum3_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly])
womenISS_sum3_cal_df = womenISS_sum3_cal$dfs
womenISS_sum3_cal = womenISS_sum3_cal$results

###
tmp1=womenISS_sum0; tmp2=womenISS_sum1
df1=womenISS_sum0_df; df2=womenISS_sum1_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenISS_sum0_minus_women_sum1 = tmp

tmp1=womenISS_sum0; tmp2=womenISS_sum2
df1=womenISS_sum0_df; df2=womenISS_sum2_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenISS_sum0_minus_women_sum2 = tmp


tmp1=womenISS_sum1; tmp2=womenISS_sum2
df1=womenISS_sum1_df; df2=womenISS_sum2_df
lowstd = tmp1[,1]/tmp1[,2]; highstd = tmp2[,1]/tmp2[,2]; tmp = (tmp1[,1] - tmp2[,1]); tvalHL = tmp/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1*(tvalHL > 0)-pt(tvalHL, df = pmin(df1-1, df2 -1)); tmp = cbind(tmp, tvalHL,pvalHL); tmp[nrow(tmp),] <- 0
womenISS_sum1_minus_women_sum2 = tmp

#all_womenISS_sum = cbind(womenISS_sum0,womenISS_sum1,womenISS_sum2,womenISS_sum3,womenISS_sum4)
#all_womenISS_sum_diff0 = cbind(womenISS_sum0_minus_women_sum1,womenISS_sum0_minus_women_sum2,womenISS_sum1_minus_women_sum2)
#all_women_sum_cal = cbind(women_sum0_cal,women_sum1_cal,women_sum2_cal,women_sum3_cal)

#####################

useonly_CEO_women_iss = which((scrub(project_data_subset_iss$CEO_female) != 0) & !is.na(project_data_subset_iss$CEO_female))
CEO_women_iss = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women_iss], Risk_Factors_Monthly)$results
CEO_women_cal_iss = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_women])$results
###
useonly_CFO_women_iss = which((scrub(project_data_subset_iss$CFO_female) != 0) & !is.na(project_data_subset_iss$CFO_female))
CFO_women_iss = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women_iss], Risk_Factors_Monthly)$results
CFO_women_cal_iss = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CFO_women])$results
###
useonly_senior_women_iss = which(scrub(project_data_subset_iss$other_senior_female) != 0 & !is.na(project_data_subset_iss$other_senior_female))
senior_women_iss  = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women_iss], Risk_Factors_Monthly)$results
senior_women_cal_iss  = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_senior_women])$results
###
useonly_CEO_men_iss = which(scrub(project_data_subset_iss$CEO_female) == 0 & !is.na(project_data_subset_iss$CEO_female))
CEO_men_iss = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men_iss], Risk_Factors_Monthly)$results
CEO_men_cal_iss = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_men])$results

################################################################################################################################
# Some descriptive statistics and relations with other firm characteristics
################################################################################################################################

all_fund_sources = unique(unlist(sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Source...of..Funds..Code, function(i) unlist(str_split(i,"\\+")))))
cash_funds = c("CR")
credit_funds = c("BL","BOR","CF","DS")
other_funds = setdiff(all_fund_sources,c(cash_funds,credit_funds))
all_purposes = unique(unlist(sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) unlist(str_split(i,"\\+")))))
good_purpose = c("ESV","UVL","ISV")
other_purpose = setdiff(all_purposes,c(good_purpose))

EUindex_bb = BUYBACK_DATA_PROJECT$EU_index
high_leverage = 0*EUindex_bb
high_leverage[High_LEV_eventsBB] <-1
low_leverage = 0*EUindex_bb
low_leverage[Low_LEV_eventsBB] <-1
Downgraded = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec < BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
Upgraded = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec > BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
low_epsunc = 0*EUindex_bb
low_epsunc[Low_EPS_eventsBB] <-1
ISS_Later = ifelse((BUYBACK_DATA_PROJECT$DATASET$SDC$OtherlaterEvent != 0), "Yes", "No")
Credit = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), credit_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(cash_funds,other_funds)))==0)
Cash = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), cash_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(credit_funds,other_funds)))==0)
Good_purpose = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), good_purpose))!=0 & length(intersect(unlist(str_split(i,"\\+")), other_purpose))==0)
Stock_Option_Plan = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "STP"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV","UVL")))==0)
Undervalued = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "UVL"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("STP")))==0 )

project_relations = cbind(Downgraded,Upgraded,Cash,Undervalued,Stock_Option_Plan)

## PROJECT SPECIFIC ADDITIONS HERE:
CEO.female = BUYBACK_DATA_PROJECT$DATASET$boardex$CEO_female !=0
CFO.female = BUYBACK_DATA_PROJECT$DATASET$boardex$CFO_female !=0
tmp = cbind(CEO.female,CFO.female)
colnames(tmp) <- c("CEO is Female","CFO is Female")
project_relations = cbind(project_relations,tmp)

project_relations_summary = project_relations

project_relationsHL= t(apply(project_relations,2,function(r){
  r = r[(High_project_eventsBB+Low_project_eventsBB)==1]
  High_project_eventsBB1 = High_project_eventsBB[(High_project_eventsBB+Low_project_eventsBB)==1]
  x = table(High_project_eventsBB1[!is.na(r)], r[!is.na(r)])
  x = matrix(c(100*x[,2]/(x[,1]+x[,2]),prop.test(x)$p.value,prop.test(x)$statistic),ncol=1)
  x
}))
rownames(project_relationsHL) <- gsub("_"," ", rownames(project_relationsHL))

project_relations_continuousHL = t(apply(all_characteristics_continuous_summary,2,function(r){
  useonlyL = which(Low_project_eventsBB == TRUE)
  useonlyH = which(High_project_eventsBB == TRUE)
  #cat(length(useonlyL), length(useonlyH),", ")
  c(ifelse(length(r[useonlyL][!is.na(r[useonlyL])]), mean(r[useonlyL][!is.na(r[useonlyL])]),NA), 
    ifelse(length(r[useonlyH][!is.na(r[useonlyH])]),mean(r[useonlyH][!is.na(r[useonlyH])]), NA),
    ifelse(length(r[useonlyL][!is.na(r[useonlyL])]) & length(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$p.value,NA),
    ifelse(length(r[useonlyL][!is.na(r[useonlyL])]) & length(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$statistic, NA))
}))
rownames(project_relations_continuousHL) <-  colnames(all_characteristics_continuous_summary)
project_relationsHL = rbind(project_relationsHL, project_relations_continuousHL)

#Use median to be sure outliers are not a problem
project_relations_continuousHL_median = t(apply(all_characteristics_continuous_summary,2,function(r){
  useonlyL = which(Low_project_eventsBB == TRUE)
  useonlyH = which(High_project_eventsBB == TRUE)
  #cat(length(useonlyL), length(useonlyH),", ")
  c(ifelse(length(r[useonlyL][!is.na(r[useonlyL])]), median(r[useonlyL][!is.na(r[useonlyL])]),NA), 
    ifelse(length(r[useonlyH][!is.na(r[useonlyH])]),median(r[useonlyH][!is.na(r[useonlyH])]), NA),
    ifelse(length(r[useonlyL][!is.na(r[useonlyL])]) & length(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$p.value,NA),
    ifelse(length(r[useonlyL][!is.na(r[useonlyL])]) & length(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$statistic, NA))
}))
rownames(project_relations_continuousHL_median) <-  colnames(all_characteristics_continuous_summary)
project_relationsHL_median = rbind(project_relationsHL, project_relations_continuousHL_median)

# Now using the split of not/some women
High_project_eventsBB01 = women!=0
Low_project_eventsBB01 = women == 0
project_relations01= t(apply(project_relations,2,function(r){
  r = r[(High_project_eventsBB01+Low_project_eventsBB01)==1]
  High_project_eventsBB1 = High_project_eventsBB01[(High_project_eventsBB01+Low_project_eventsBB01)==1]
  x = table(High_project_eventsBB1[!is.na(r)], r[!is.na(r)])
  x = matrix(c(100*x[,2]/(x[,1]+x[,2]),prop.test(x)$p.value,prop.test(x)$statistic),ncol=1)
  x
}))
rownames(project_relations01) <- gsub("_"," ", rownames(project_relations01))

project_relations_continuous01 = t(apply(all_characteristics_continuous_summary,2,function(r){
  useonlyL = which(Low_project_eventsBB01 == TRUE)
  useonlyH = which(High_project_eventsBB01 == TRUE)
  #cat(length(useonlyL), length(useonlyH),", ")
  c(ifelse(length(r[useonlyL][!is.na(r[useonlyL])]), mean(r[useonlyL][!is.na(r[useonlyL])]),NA), 
    ifelse(length(r[useonlyH][!is.na(r[useonlyH])]),mean(r[useonlyH][!is.na(r[useonlyH])]), NA),
    ifelse(length(r[useonlyL][!is.na(r[useonlyL])]) & length(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$p.value,NA),
    ifelse(length(r[useonlyL][!is.na(r[useonlyL])]) & length(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$statistic, NA))
}))
rownames(project_relations_continuous01) <-  colnames(all_characteristics_continuous_summary)
project_relations01 = rbind(project_relations01, project_relations_continuous01)

buybacks_summary = cbind(all_characteristics_continuous_summary,project_relations_summary)

########################################################################################################
# Cross sectional analyses
########################################################################################################
# Get the predicted post-announce abnormal returns (as developed by "../BuybacksApril2016/bb_issuers_new.R")
load("~/OneDrive - INSEAD/FinanceData/created_projects_datasets/BUYBACKSnew_BSC1998_event_study_factor_coeffs.Rdata")
events_used = paste(BUYBACK_DATA_PROJECT$DATASET$SDC$permno, BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, sep=" ")
Estimated_returns = 100*Estimated_returns[events_used,]
BUYBACK_PreEvent_Factor_coeffs = BUYBACK_PreEvent_Factor_coeffs[events_used]
rm("events_used","BUYBACK_PreEvent_Factor_coeffs") # we don't need for now BUYBACK_PreEvent_Factor_coeffs

company_features_all = as.data.frame(all_characteristics_continuous)

# Add dummies
year_dummies_cross = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, function(i) str_sub(i, start=1,end=4))
industry_dummies = ifelse(!is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$SICH), str_sub(as.character(BUYBACK_DATA_PROJECT$DATASET$CRSP$SICH), start=1,end=2), NA)
company_features_all$year_dummies_cross = year_dummies_cross
company_features_all$industry_dummies = industry_dummies

cross_regressions_variables_complete = c(
  "Market Cap.",
  "BE/ME",
  "Prior Returns",
  "Volatility",
  "One minus Rsq",
  "Analyst Coverage",
  "Board Size",
  "Percent Independent Directors",
  "Percent Shares", 
  "Profitability (ROA)",
  "Net Debt",
  "Tax Rate",
  "Lag Dividend Payout Ratio",
  "Leverage",
  "Institutional Holdings",
  "Percent of Women School Links",
  "Numb. of Other Boards for Women",
  "Numb. of Other Boards for Men",
  "industry_dummies",
  "year_dummies_cross"
)
nomissing_allowed = c("alphaT", "project.main.IV.variable","CEO.GenderInteraction")
useonly = 1:length(BUYBACK_DATA_PROJECT$DATASET$SDC$CUSIP)

## Default analysis
tmp = gender
names(tmp) <- paste(str_sub(AddMonths(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,-1),start = 1, end=7), BUYBACK_DATA_PROJECT$DATASET$SDC$permno, sep=" ")
tmp = tmp[rownames(company_features_all)]
##
company_features = company_features_all[,cross_regressions_variables_complete]
company_features$`E-index` = BUYBACK_DATA_PROJECT$DATASET$boardex$eindex
company_features$dummyNoEindex = ifelse(is.na(company_features_all$`E-index`),1,0)
company_features$`E-index` = scrub(company_features_all$`E-index`)
company_features$`Numb. of Other Boards for Women` <- scrub(womenboards) 
company_features$`Numb. of Other Boards for Men` <- scrub(menboards)
company_features$`Percent of Women School Links` <- scrub(womenboards_sociallinks)

company_features$CEO.GenderInteraction = (BUYBACK_DATA_PROJECT$DATASET$boardex$CEO_female !=0)#*tmp
company_features$project.main.IV.variable = tmp
project.main.IV.variable.name = "Women Percent"
project.main.IV.variable.name_interact = "CEO Female" #"CEO Female x Women on Board (Dummy)"

#####
useonly = 1:nrow(company_features)
observations_BSC1998_completemodel = sum(apply(company_features[,setdiff(colnames(company_features),c("year_dummies_cross","industry_dummies"))],1,function(r) sum(is.na(as.numeric(r)))==0))
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel <- BSC1998_completemodel[setdiff(1:nrow(BSC1998_completemodel),which(rownames(BSC1998_completemodel) == "dummyNoEindex")),]
# Dummies' coefficients are removed at the end inside BSC1998_event_study_coeffs
rownames(BSC1998_completemodel) <- c("Intercept",setdiff(cross_regressions_variables_complete,c("industry_dummies","year_dummies_cross")),
                                     "E-index", project.main.IV.variable.name_interact,project.main.IV.variable.name) 
round(BSC1998_completemodel,2)
##

# Version 2: # of women with dummies, set women=1 as the dummy 0 so we see the effect of adding more women
tmp = BUYBACK_DATA_PROJECT$DATASET$boardex$women == 1
names(tmp) <- paste(str_sub(AddMonths(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,-1),start = 1, end=7), BUYBACK_DATA_PROJECT$DATASET$SDC$permno, sep=" ")
tmp = tmp[rownames(company_features_all)]
tmp2 = BUYBACK_DATA_PROJECT$DATASET$boardex$women == 2
names(tmp2) <- names(tmp); tmp2 = tmp2[rownames(company_features_all)]
tmp3 = BUYBACK_DATA_PROJECT$DATASET$boardex$women > 2
names(tmp3) <- names(tmp); tmp3 = tmp3[rownames(company_features_all)]
##
company_features = company_features_all[,cross_regressions_variables_complete]
company_features$dummyNoEindex = ifelse(is.na(company_features_all$`E-index`),1,0)
company_features$`E-index` = scrub(company_features_all$`E-index`)
company_features$`Numb. of Other Boards for Women` <- scrub(womenboards) #womenboards_sociallinks
company_features$`Numb. of Other Boards for Men` <- scrub(menboards)
company_features$`Percent of Women School Links` <- scrub(womenboards_sociallinks)

company_features$CEO.GenderInteraction = (BUYBACK_DATA_PROJECT$DATASET$boardex$CEO_female !=0)#*(tmp==0)
company_features$project.main.IV.variable = tmp
company_features$project.main.IV.variable2 = tmp2
company_features$project.main.IV.variable3 = tmp3
project.main.IV.variable.name = "One Woman"
project.main.IV.variable.name2 = "Two Women"
project.main.IV.variable.name3 = "Three than two Women"
project.main.IV.variable.name_interact = "CEO Female" #"CEO Female x Women on Board"
####
useonly = 1:nrow(company_features)
observations_BSC1998_completemodel_robust_women = sum(apply(company_features[,setdiff(colnames(company_features),c("year_dummies_cross","industry_dummies"))],1,function(r) sum(is.na(as.numeric(r)))==0))
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
BSC1998_completemodel_robust_women =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
BSC1998_completemodel_robust_women <- BSC1998_completemodel_robust_women[setdiff(1:nrow(BSC1998_completemodel_robust_women),which(rownames(BSC1998_completemodel_robust_women) == "dummyNoEindex")),]
# Dummies' coefficients are removed at the end inside BSC1998_event_study_coeffs
rownames(BSC1998_completemodel_robust_women) <- c("Intercept",setdiff(cross_regressions_variables_complete,c("industry_dummies","year_dummies_cross")),
                                                  "E-index",project.main.IV.variable.name_interact,project.main.IV.variable.name,project.main.IV.variable.name2,project.main.IV.variable.name3) 
round(BSC1998_completemodel_robust_women,2)
##

########################################################################################################
# Cross sectional analyses WITH INSTRUMENTAL VARIABLE
########################################################################################################

source("test_instrument.R")

################################################################################################################################
# Logistic regression for buyback event prediction
################################################################################################################################
if (0){ # takes time so we run once and save
  
  load("~/OneDrive - INSEAD/FinanceData/created_projects_datasets/BUYBACK_otherdata.Rdata")
  #logistic_data_all = logistic.data.all.yearly 
  #yearly.regression.variable = yearly.regression.variable.yearly
  logistic_data_all = logistic.data.all.monthly 
  yearly.regression.variable = yearly.regression.variable.monthly
  Daily_Returns = Buyback_Daily_Returns[,paste(BUYBACK_DATA_PROJECT$DATASET$SDC$permno, BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, sep="_")]
  rm("logistic.data.all.yearly","logistic.data.all.monthly","buybacks.events","Buyback_Daily_Returns")
  
  ##########################################
  # ADD SOME PROJECT SPECIFIC DATA
  
  load("~/OneDrive - INSEAD/FinanceData/created_boardex_data/GLOBAL_BOARDEX_DATABASE.Rdata")
  # A helper function to take a "template" matrix and create a YEARLY vector
  tmp_gender = apply(GLOBAL_BOARDEX_DATABASE$gender,2,function(r) fill_NA_previous(r,lastfill=T))
  tmp_women = apply(GLOBAL_BOARDEX_DATABASE$women,2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
  tmp_ind_dir = apply(GLOBAL_BOARDEX_DATABASE$number_of_independent_NED,2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
  
  tmp = ifelse(scrub(GLOBAL_BOARDEX_DATABASE$women), GLOBAL_BOARDEX_DATABASE$number_women_menboard_social_ties/GLOBAL_BOARDEX_DATABASE$women, NA)
  tmp = ifelse(tmp > 1, NA, tmp)
  tmp_schoolconn_dir = apply(tmp,2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
  tmp = ifelse(scrub(GLOBAL_BOARDEX_DATABASE$women), GLOBAL_BOARDEX_DATABASE$number_women_board_job_ties/GLOBAL_BOARDEX_DATABASE$women, NA)
  tmp = ifelse(tmp > 1, NA, tmp)
  tmp_boardconn_dir = apply(tmp,2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
  
  # Data cleanup
  tmp1 = apply(GLOBAL_BOARDEX_DATABASE$women + GLOBAL_BOARDEX_DATABASE$men,2,function(r) fill_NA_previous(r,lastfill=T))
  tmp2 = apply(GLOBAL_BOARDEX_DATABASE$total_directors,2,function(r) fill_NA_previous(r,lastfill=T))
  tmp_ind_dir = apply(GLOBAL_BOARDEX_DATABASE$number_of_independent_NED,2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
  tmp_ind_dir_year = yearly.regression.variable(tmp_ind_dir)
  tmp_total_dir_year = yearly.regression.variable(tmp2)
  logistic_data_all$total.inddir.percent = ifelse(scrub(tmp_total_dir_year)!=0,tmp_ind_dir_year/tmp_total_dir_year,NA)
  rm("tmp_total_dir_year","tmp_ind_dir_year","tmp_ind_dir")
  
  women.dummy = ifelse(is.na(tmp_women), NA, 1*(tmp_women!=0))
  
  rm("tmp1","tmp2")
  
  logistic_data_all$women.dummy <- yearly.regression.variable(women.dummy)
  logistic_data_all$gender <- yearly.regression.variable(tmp_gender) 
  logistic_data_all$women <- yearly.regression.variable(tmp_women)
  logistic_data_all$CEO_female_all <- yearly.regression.variable(apply(GLOBAL_BOARDEX_DATABASE$CEO_female,2,function(r) fill_NA_previous(r,lastfill=T))) # Fill all the way to the last month
  logistic_data_all$boardsize <- yearly.regression.variable(apply(GLOBAL_BOARDEX_DATABASE$total_directors,2,function(r) fill_NA_previous(r,lastfill=T))) # Fill all the way to the last month
  
  tmp = yearly.regression.variable(apply(GLOBAL_BOARDEX_DATABASE$womenboards_sum,2,function(r) fill_NA_previous(r,lastfill=T)))
  logistic_data_all$womenboards <- scrub(tmp/logistic_data_all$women)
  #
  tmp = yearly.regression.variable(apply(GLOBAL_BOARDEX_DATABASE$menboards_sum,2,function(r) fill_NA_previous(r,lastfill=T)))
  tmp_men = yearly.regression.variable(apply(GLOBAL_BOARDEX_DATABASE$men,2,function(r) fill_NA_previous(r,lastfill=T))) # Fill all the way to the last month
  logistic_data_all$menboards <-scrub(tmp/tmp_men)
  #
  tmp = yearly.regression.variable(tmp_schoolconn_dir)
  logistic_data_all$women_school_ties <- scrub(tmp)*(scrub(tmp)<1) # must be less than 100% 
  tmp = yearly.regression.variable(tmp_boardconn_dir)
  logistic_data_all$women_board_ties <- scrub(tmp)*(scrub(tmp)<1) # must be less than 100%
  rm("tmp","tmp_men")
  
  
  ## Eindex
  load("~/OneDrive - INSEAD/FinanceData/created_projects_datasets/eindex_all_matrix.Rdata")
  # ADDED THIS LINE RELATIVE TO THE OLD WAY
  logistic_data_all$eindex = yearly.regression.variable(eindex_all_matrix)
  logistic_data_all$eindex = (logistic_data_all$eindex)
  logistic_data_all$eindex_dummy = ifelse(is.na(logistic_data_all$eindex), 1, 0)
  logistic_data_all$eindex = scrub(logistic_data_all$eindex)
  rm("GLOBAL_BOARDEX_DATABASE","tmp_women","tmp_gender","women.dummy","eindex_all_matrix")
  
  # START FROM WHERE WE START HAVING BUYBACK DATA + 2 years (to get the previous buybacks)
  tmp = as.Date(paste(str_sub(rownames(logistic_data_all), start=7, end=13), "01",sep="-"))
  useonly = tmp >= (min(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date) + 2*370)
  logistic_data_all = logistic_data_all[useonly,]
  
  # As we use the full sample of companies, it is important to winsorize here
  winsorise_vars <- c(
    "size.all", 
    "MEBE", 
    "prior.returns.all",
    "Total.Payout", 
    "Total.Payout.last.year",
    "Leverage", 
    "profitability", 
    "operating.income", 
    "non.operating.income", 
    "std.operating.income", 
    "dividend.payout.ratio", 
    "liquid.assets", 
    "price.earnings.ratio", 
    "capital.expenditures", 
    "institutional.ratio.all", 
    "institutional.number.all"
  )
  logistic_data_all_winsorized = logistic_data_all
  logistic_data_all_winsorized[,winsorise_vars] <- apply(logistic_data_all_winsorized[,winsorise_vars], 2, winsorize)
  
  save(logistic_data_all, logistic_data_all_winsorized,Daily_Returns,file = "logisticdata.Rdata")
} else {
  load("logisticdata.Rdata")
}

logistic_IV_used_ini = c(
  "buybacks.events.past2years", 
  "size.all", 
  "MEBE", 
  "prior.returns.all",
  "total.inddir.percent",
  "Total.Payout", 
  "Total.Payout.last.year",
  "Leverage", 
  "profitability", 
  "operating.income", 
  "non.operating.income", 
  "std.operating.income", 
  "dividend.payout.ratio", 
  "liquid.assets", 
  "price.earnings.ratio", 
  "capital.expenditures", 
  "institutional.ratio.all", 
  "institutional.number.all",
  "boardsize",
  "eindex",
  "eindex_dummy"
)

####################################
## Using the women dummy first
logistic_data_all = logistic_data_all_winsorized # Should not make a difference

####################################################################################
# REMOVE THE CEO FROM THE WOMEN
logistic_data_all$women <- logistic_data_all$women - (logistic_data_all$CEO_female_all !=0)*1
logistic_data_all$women.dummy <- logistic_data_all$women !=0
####################################################################################

#### First without the board connections
logistic_IV_used =  c(logistic_IV_used_ini, "year.dummies","industry.dummies.2digit"
)
logistic_data = logistic_data_all[,c(logistic_IV_used,"buybacks.events.this.period")]
logistic_data$CEO.GenderInteraction <- (logistic_data_all$CEO_female_all)#*logistic_data_all$women.dummy
logistic_data$projectmainvar <- logistic_data_all$gender # WE USE PERCENT IN THE PAPER

logistic_data = logistic_data[apply(logistic_data,1,function(r) sum(is.na(r))) == 0,]
names(logistic_data) <- str_replace_all(names(logistic_data), "\\.","")
###
logistic_data_used = logistic_data

the_logistic_formula = paste("buybackseventsthisperiod ~ ", paste(setdiff(names(logistic_data_used), "buybackseventsthisperiod"), collapse = " + " ), sep=" ")
the_logistic_formula = as.formula(the_logistic_formula)
observations_logistic_data_noconn <- nrow(logistic_data_used)

logistic_regression_buyback = glm(formula = the_logistic_formula, family = "binomial", data = logistic_data_used)
logistic_regression_buyback = summary(logistic_regression_buyback)$coefficients
logistic_regression_buyback = logistic_regression_buyback[,c(1,3,4)]
colnames(logistic_regression_buyback) <- c("Coeff.","t-stat","p-value")
round(logistic_regression_buyback[!str_detect(rownames(logistic_regression_buyback),"dumm"),],3)

probit_regression_buyback_glm = glm(formula = the_logistic_formula, family = binomial(link = "probit"), data = logistic_data_used)
probit_regression_buyback = summary(probit_regression_buyback_glm)$coefficients
probit_regression_buyback = probit_regression_buyback[,c(1,3,4)]
colnames(probit_regression_buyback) <- c("Coeff.","t-stat","p-value")
round(probit_regression_buyback[!str_detect(rownames(probit_regression_buyback),"dumm"),],2)

logistic_regression_buyback_project = logistic_regression_buyback[!str_detect(rownames(logistic_regression_buyback),"dumm"),]
probit_regression_buyback_project = probit_regression_buyback[!str_detect(rownames(probit_regression_buyback),"dumm"),]
HeckmanIMR_noconn = invMillsRatio(probit_regression_buyback_glm)$IMR1
names(HeckmanIMR_noconn) <- rownames(logistic_data)

rm("logistic_regression_buyback","probit_regression_buyback","the_logistic_formula","logistic_IV_used","probit_regression_buyback_glm")

rm("tmp", "Estimated_returns", "useonly")

rm("logistic_data_all", "logistic_data_all_winsorized","logistic_data","logistic_data_used","counties","logistic_data_ini",
   "zip_matches","counties_gvkey_yearmonth","HeckmanIMR_conn","HeckmanIMR_gender_conn","HeckmanIMR1_matched_instrument1",
   "HeckmanIMR_gender_noconn","HeckmanIMR_noconn","project_data_subset","project_data_subset_iss")


#######################################################################################################################
# Save the data now
save(list = setdiff(ls(all = TRUE),initial_vars), file = paste("board_diversity.",project_var_name ,".Rdata", sep=""))

