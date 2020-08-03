## Regression Analysis at the weekly level
rm(list = ls())

# Preparation----
# Defining the control variable sets

# i) weather controls
weather    <- c("precipitation + humidity    + temperature + still_air")

# ii) containment and closure policies
controls_c <- c("C1_School_closing_2 + C1_School_closing_3 +
                 C2_Workplace_closing_2 + C2_Workplace_closing_3 + 
                 C2_Workplace_closing_1+ C3_Cancel_public_events_2 +
                 C3_Cancel_public_events_1+ C5_Close_public_transport_2 + 
                 C5_Close_public_transport_1+ C6_Stay_at_home_requirements_1 +
                 C6_Stay_at_home_requirements_2 + C6_Stay_at_home_requirements_3+
                 C7_Rest_on_Int_Mov_2 + C7_Rest_on_Int_Mov_1")

controls_d <- c("C1_School_closing+ C2_Workplace_closing_3")

# iii) health system policies
controls_d2 <- c("H2_Testing_policy_1 + H2_Testing_policy_2+ H2_Testing_policy_3")

controls_c2 <- c("H2_Testing_policy_1 + H2_Testing_policy_2+ H2_Testing_policy_3+
               H3_Contact_tracing_2 +H3_Contact_tracing_1")

# I. First Stage regressions ----

# i) three-week time window
load("data/cases_weekly.RData")

# weather controls
reg1 <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_cases ", 
                                     weather,sep = "+"),
                               "|GID_2 + country_month + week ", sep = "")), data = df)

# weather + containment controls
reg2 <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_cases ", 
                                     weather,controls_c,sep = "+"), 
                               "|GID_2 + country_month + week ", sep = "")), data = df)

# weather + containment + health system controls
reg3 <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_cases ", 
                                     weather,controls_c,controls_c2,sep = "+"), 
                               "|GID_2 + country_month + week ", sep = "")), data = df)

# ii) four-week time window
load("data/deaths_weekly.RData")

# weather controls
reg4 <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_deaths ", 
                                     weather,sep = "+"), 
                               "|GID_2 + country_month + week ", sep = "")),  data = df)

# weather + containment controls
reg5 <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_deaths ", 
                                     weather,controls_d,sep = "+"), 
                               "|GID_2 + country_month + week ", sep = "")),  data = df)

# weather + containment + health system controls
reg6 <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_deaths ", 
                                     weather,controls_d,controls_d2,sep = "+"), 
                               "|GID_2 + country_month + week ", sep = "")), data = df)


# Latex Output 
esttex(reg1, reg2, reg3,reg4, reg5, reg6, se = "cluster", cluster = "GID_2", 
       drop =  c(names(reg3$coefficients)[2:length(names(reg3$coefficients))],
                 names(reg6$coefficients)[2:length(names(reg6$coefficients))]))



# II. Reduced Form Regressions----

# i) COVID-19 cases
load("data/cases_weekly.RData")
df <- df %>% filter(is.na(AOD_cc) == FALSE)

# weather controls
reg1 <- femlm(as.formula(paste("cases ~ inv_contin + lag_cases +",
                               weather,
                               " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# weather + containment controls
reg2 <- femlm(as.formula(paste("cases ~ inv_contin + lag_cases +",
                               weather,"+",controls_c,
                               " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# weather + containment + health system controls
reg3 <- femlm(as.formula(paste("cases ~ inv_contin + lag_cases +",
                               weather,"+",controls_c,"+",controls_c2,
                               " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# ii) COVID-19 deaths
load("data/deaths_weekly.RData")
df <- df %>% filter(is.na(AOD_cc) == FALSE)

# weather controls
reg4 <- femlm(as.formula(paste("deaths ~ inv_contin + lag_deaths +",
                               weather,
                               " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# weather + containment controls
reg5 <- femlm(as.formula(paste("deaths ~ inv_contin + lag_deaths +",
                               weather,"+", controls_d,
                               " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# weather + containment + health system controls
reg6 <- femlm(as.formula(paste("deaths ~ inv_contin + lag_deaths +",
                               weather,"+", controls_d,"+", controls_d2,
                               " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# Latex Output 
esttex(reg1, reg2, reg3,reg4, reg5, reg6, se = "cluster", cluster = "GID_2", 
       drop =  c(names(reg3$coefficients)[2:length(names(reg3$coefficients))],
                 names(reg6$coefficients)[2:length(names(reg6$coefficients))]))
