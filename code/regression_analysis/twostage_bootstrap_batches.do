////// Two-stage procedure, bootstrapping clustered standard errors
* This script bootstraps SE for cases and deaths two-stage regression procedures
* We run bootstraps in batches to simplify the handling of Stata errors
* Dependencies: reghdfe, ppmlhdfe

// Set working directory here
* cd "" 

// Generate empty table file, if it does not exist yet
clear 
g date = .
cap save "outputs/tables/twostagePoissonBootstrap.dta"

//// Outcome: cases
// Define control variable sets
global weather precipitation  humidity temperature  ///
             still_air
global controls_c C1_School_closing_2  C1_School_closing_3  ///
               C2_Workplace_closing_2  C2_Workplace_closing_3  C2_Workplace_closing_1 ///
               C3_Cancel_public_events_2  C3_Cancel_public_events_1 ///
               C5_Close_public_transport_2  C5_Close_public_transport_1 ///
               C6_Stay_at_home_requirements_1  C6_Stay_at_home_requirements_2  C6_Stay_at_home_requirements_3 ///
               C7_Rest_on_Int_Mov_2  C7_Rest_on_Int_Mov_1
global controls2 H2_Testing_policy_1  H2_Testing_policy_2 H2_Testing_policy_3 ///
               H3_Contact_tracing_2 H3_Contact_tracing_1

// Bootstrapping standard errors for weather, controls_c, and controls2
use "data/2sls_bootstrap_cases.dta", clear
capture program drop aodBoot
program aodBoot, rclass
reghdfe AOD_cc inv_contin lag_cases $weather $controls_c $controls2, absorb(GID_2 country_month week) residuals(fsResid)
ereturn clear
ppmlhdfe cases AOD_cc fsResid lag_cases $weather $controls_c $controls2, absorb(GID_2 country_month week)
return scalar bAOD_cc = _b[AOD_cc]
drop fsResid
end

bootstrap r(bAOD_cc), nodrop reps(500) seed(987) cluster(GID_2) idcluster(newid): aodBoot
regsave,cmdline
g date = "$S_DATE"
g time = "$S_TIME"

g inversions = "inv_contin"
g weather_controls = 1
g controls_c = 1
g controls_d = 0
g controls2 = 0
g GID_2_fe = 1
g country_month_fe = 1
g week_fe = 1
g outcome = "cases"
g pollution = "AOD_cc"

append using "outputs/tables/twostagePoissonBootstrap.dta"
save "outputs/tables/twostagePoissonBootstrap.dta",replace

// Bootstrapping standard errors for weather, controls_c, and controls2
use "data/2sls_bootstrap_cases.dta", clear
capture program drop aodBoot
program aodBoot, rclass
reghdfe AOD_cc inv_contin lag_cases $weather $controls_c, absorb(GID_2 country_month week) residuals(fsResid)
ereturn clear
ppmlhdfe cases AOD_cc fsResid lag_cases $weather $controls_c, absorb(GID_2 country_month week)
return scalar bAOD_cc = _b[AOD_cc]
drop fsResid
end


bootstrap r(bAOD_cc), nodrop reps(500) seed(987) cluster(GID_2) idcluster(newid): aodBoot
regsave,cmdline
g date = "$S_DATE"
g time = "$S_TIME"

g inversions = "inv_contin"
g weather_controls = 1
g controls_c = 1
g controls_d = 0
g controls2 = 0
g GID_2_fe = 1
g country_month_fe = 1
g week_fe = 1
g outcome = "cases"
g pollution = "AOD_cc"

append using "outputs/tables/twostagePoissonBootstrap.dta"
save "outputs/tables/twostagePoissonBootstrap.dta",replace


// Bootstrapping standard errors for weather
use "data/2sls_bootstrap_cases.dta", clear
capture program drop aodBoot
program aodBoot, rclass
reghdfe AOD_cc inv_contin lag_cases $weather, absorb(GID_2 country_month week) residuals(fsResid)
ereturn clear
ppmlhdfe cases AOD_cc fsResid lag_cases $weather, absorb(GID_2 country_month week)
return scalar bAOD_cc = _b[AOD_cc]
drop fsResid
end


bootstrap r(bAOD_cc), nodrop reps(500) seed(987) cluster(GID_2) idcluster(newid): aodBoot
regsave,cmdline
g date = "$S_DATE"
g time = "$S_TIME"

g inversions = "inv_contin"
g weather_controls = 1
g controls_c = 0
g controls_d = 0
g controls2 = 0
g GID_2_fe = 1
g country_month_fe = 1
g week_fe = 1
g outcome = "cases"
g pollution = "AOD_cc"

append using "outputs/tables/twostagePoissonBootstrap.dta"
save "outputs/tables/twostagePoissonBootstrap.dta",replace


//// Outcome: deaths
// Define control variable sets
global weather precipitation  humidity temperature  ///
             still_air
global controls_d C1_School_closing C2_Workplace_closing_3
global controls2 H2_Testing_policy_1 H2_Testing_policy_2 H2_Testing_policy_3


// Bootstrapping standard errors for weather, controls_d, and controls2
use "data/2sls_bootstrap_deaths.dta", clear
capture program drop aodBoot
program aodBoot, rclass
reghdfe AOD_cc inv_contin lag_deaths $weather $controls_d $controls2, absorb(GID_2 country_month week) residuals(fsResid)
return clear
ppmlhdfe deaths AOD_cc fsResid lag_deaths $weather $controls_d $controls2, absorb(GID_2 country_month week)
return scalar bAOD_cc = _b[AOD_cc]
drop fsResid
end

bootstrap r(bAOD_cc), nodrop reps(500) seed(987) cluster(GID_2) idcluster(newid): aodBoot
regsave,cmdline
g date = "$S_DATE"
g time = "$S_TIME"

g inversions = "inv_contin"
g weather_controls_new = 1
g controls_c = 0
g controls_d_new = 1
g controls2_new = 1
g GID_2_fe = 1
g country_month_fe = 1
g week_fe = 1
g outcome = "deaths"
g pollution = "AOD_cc"

append using "outputs/tables/twostagePoissonBootstrap.dta"
save "outputs/tables/twostagePoissonBootstrap.dta",replace


// Bootstrapping standard errors for weather, controls_d
use "data/2sls_bootstrap_deaths.dta", clear
capture program drop aodBoot
program aodBoot, rclass
reghdfe AOD_cc inv_contin lag_deaths $weather $controls_d, absorb(GID_2 country_month week) residuals(fsResid)
ereturn clear
ppmlhdfe deaths AOD_cc fsResid lag_deaths $weather $controls_d, absorb(GID_2 country_month week)
return scalar bAOD_cc = _b[AOD_cc]
drop fsResid
end


bootstrap r(bAOD_cc), nodrop reps(500) seed(987) cluster(GID_2) idcluster(newid): aodBoot
regsave,cmdline
g date = "$S_DATE"
g time = "$S_TIME"

g inversions = "inv_contin"
g weather_controls_new = 1
g controls_c = 0
g controls_d_new = 1
g controls2_new = 0
g GID_2_fe = 1
g country_month_fe = 1
g week_fe = 1
g outcome = "deaths"
g pollution = "AOD_cc"

append using "outputs/tables/twostagePoissonBootstrap.dta"
save "outputs/tables/twostagePoissonBootstrap.dta",replace


// Bootstrapping standard errors for weathe
use "data/2sls_bootstrap_deaths.dta", clear
capture program drop aodBoot
program aodBoot, rclass
reghdfe AOD_cc inv_contin lag_deaths $weather, absorb(GID_2 country_month week) residuals(fsResid)
ereturn clear
ppmlhdfe deaths AOD_cc fsResid lag_deaths $weather, absorb(GID_2 country_month week)
return scalar bAOD_cc = _b[AOD_cc]
drop fsResid
end


bootstrap r(bAOD_cc), nodrop reps(500) seed(987) cluster(GID_2) idcluster(newid): aodBoot
regsave,cmdline
g date = "$S_DATE"
g time = "$S_TIME"

g inversions = "inv_contin"
g weather_controls_new = 1
g controls_c = 0
g controls_d_new = 0
g controls2_new = 0
g GID_2_fe = 1
g country_month_fe = 1
g week_fe = 1
g outcome = "deaths"
g pollution = "AOD_cc"

append using "outputs/tables/twostagePoissonBootstrap.dta"
save "outputs/tables/twostagePoissonBootstrap.dta",replace
