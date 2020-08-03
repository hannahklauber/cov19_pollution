## Regression Analysis - Robustness Checks
rm(list = ls())

# Defining the control variable sets----

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

# I. Movement Patterns----
load("data/data_daily.RData")

# Facebook data are available at the district level for Brazil and the US and at the 
# state level for Belgium, Germany, Italy and the UK

# i) Aggregate the daily inversion data to the respective spatial levels
mobility <- data_daily %>%  select(inv_contin_pixel, week, date, month, 
                                   humidity, precipitation, still_air, temperature)

mobility_gid2 <- mobility 
mobility_gid1 <- mobility %>% ungroup() %>% group_by(GID_0,  GID_1,
                                                     NAME_0, NAME_1,
                                                     week, date, month) %>% 
  summarise(inv_contin_pixel = mean(inv_contin_pixel, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            precipitation = mean(precipitation, na.rm = T),
            temperature = mean(temperature, na.rm = T),
            still_air =mean(still_air, na.rm = T),)

load("data/mobility.RData")
fb <- mobility %>% filter(country %in% c("BEL", "BRA", "DEU", "GBR", "ITA", "USA"))

fb <- fb %>% select("polygon_id", "ds" ,                                            
                    "all_day_bing_tiles_visited_relative_change",
                    "all_day_ratio_single_tile_users") %>% filter(is.na(polygon_id) == FALSE)

fb <- fb %>% ungroup() %>% group_by(polygon_id, ds) %>% summarise_all(.funs = ~mean(., na.rm = T)) 

fb1 <- left_join(fb, mobility_gid1, by = c("polygon_id" = "GID_1", "ds" = "date")) %>% 
  filter(GID_0 %in% c("BEL", "DEU", "GBR", "ITA"))
fb1$GID_2  <- NA; fb1$NAME_2 <- NA

fb2 <- left_join(fb, mobility_gid2, by = c( "polygon_id" = "GID_2","ds" = "date")) %>% 
  filter(GID_0 %in% c("USA", "BRA"))

df <- bind_rows(fb1, fb2)

df_fb <- df %>% mutate(fb_id = polygon_id, date = ds,
                       fb_id_month = as.factor(paste(month, fb_id, sep = "_")),
                       state_week = as.factor(paste(GID_1, week, sep = "_")),
                       country_day = as.factor(paste(GID_0, date, sep = "_")))

# ii) Regressions
reg1 <- feols(all_day_bing_tiles_visited_relative_change ~ inv_contin_pixel  +
                humidity + still_air + precipitation + temperature| 
                fb_id_month + state_week + country_day,
              data = df_fb )

reg2 <- feols(all_day_ratio_single_tile_users ~ inv_contin_pixel  +
                humidity + still_air + precipitation + temperature| 
                fb_id_month + state_week + country_day,
              data = df_fb )

esttex(reg1, reg2, se = "cluster", cluster = "fb_id", 
       drop =  names(reg1$coefficients)[2:length(names(reg1$coefficients))])


# II. Reduced Form Estimations ----
# included in Script "regression_analysis_weekly.R"
# III. Different Weather controls----
# i ) Defining the different covariate sets----
weather1 <- c("precipitation")

weather2 <- c("temperature")

weather3 <- c("humidity")

weather4 <- c("still_air")

weather5 <- c("u_component + v_component")

weather6 <- c("radiation")

weather7 <- c("precipitation + humidity   + temperature + still_air ")

weather8 <- c("precipitation + temperature + humidity + u_component + v_component")

weather9 <- c("radiation + humidity + u_component + v_component + still_air")

weather10 <- c("precipitation + precipitation:precipitation + precipitation:precipitation:precipitation + 
                temperature+ temperature:temperature + temperature:temperature:temperature+
                humidity + humidity:humidity + humidity:humidity:humidity + still_air ")

weather11 <- c("precipitation + precipitation:precipitation + precipitation:precipitation:precipitation + 
                temperature+ temperature:temperature + temperature:temperature:temperature+
                humidity + humidity:humidity + humidity:humidity:humidity + still_air+
               still_air:(temperature + humidity + precipitation)")

weather12 <- c("precipitation + precipitation:precipitation + precipitation:precipitation:precipitation + 
                temperature+ temperature:temperature + temperature:temperature:temperature+
                humidity + humidity:humidity + humidity:humidity:humidity +
               u_component + u_component:u_component + u_component:u_component:u_component +
               v_component + v_component:v_component + v_component:v_component:v_component")

weather13 <- c("precipitation + precipitation:precipitation + precipitation:precipitation:precipitation + 
                temperature+ temperature:temperature + temperature:temperature:temperature+
                humidity + humidity:humidity + humidity:humidity:humidity +
               u_component + u_component:u_component + u_component:u_component:u_component +
               v_component + v_component:v_component + v_component:v_component:v_component+ 
               u_component:(precipitation+temperature + humidity ) +
               v_component:(precipitation+temperature + humidity)")

weather14 <- c("humidity + humidity:humidity + humidity:humidity:humidity + 
               radiation + radiation:radiation + radiation:radiation:radiation +
               u_component + u_component:u_component + u_component:u_component:u_component +
               v_component + v_component:v_component + v_component:v_component:v_component+ still_air")

weather15 <- c("humidity + humidity:humidity + humidity:humidity:humidity + 
               radiation + radiation:radiation + radiation:radiation:radiation +
               u_component:humidity + v_component:humidity+
               u_component + u_component:u_component + u_component:u_component:u_component +
               v_component + v_component:v_component + v_component:v_component:v_component+ still_air ")

# ii) Regressions----

fy1 <- function(y){
  # weather + containment + health system controls
  reg <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_cases ", 
                                      get(paste("weather", y, sep = "")),controls_c,controls_c2,sep = "+"), 
                                "|GID_2 + country_month + week ", sep = "")), data = df)
  
  res <- paste(esttable(reg,  se = "cluster", cluster = "GID_2")[1,])
   as.data.frame(matrix(c(reg$coefficients[["inv_contin"]],
                 sub(".*\\((.*)\\).*", "\\1", res, perl=TRUE) ), ncol = 2, nrow= 1))
  } # First Stage three-week window
fy2 <- function(y){
  # weather + containment + health system controls
  reg <- feols(as.formula(paste(paste("AOD_cc ~ inv_contin + lag_deaths ", 
                                      get(paste("weather", y, sep = "")),controls_d,controls_d2, sep = "+"), 
                                "|GID_2 + country_month + week ", sep = "")),data = df)
  res <- paste(esttable(reg,  se = "cluster", cluster = "GID_2")[1,])
  as.data.frame(matrix(c(reg$coefficients[["inv_contin"]],
                         sub(".*\\((.*)\\).*", "\\1", res, perl=TRUE) ), ncol = 2, nrow= 1))
  } # First Stage four-week window
fy3 <- function(y){
  gc()
  reg <- femlm(as.formula(paste("cases ~ inv_contin + lag_cases +",
                                get(paste("weather", y, sep = "")),"+",controls_c,"+",controls_c2,
                                " |GID_2 + week + country_month ", sep = "")),
               family = "poisson", data = df, verbose=1)
  res <- paste(esttable(reg,  se = "cluster", cluster = "GID_2")[1,])
  as.data.frame(matrix(c(reg$coefficients[["inv_contin"]],
                         sub(".*\\((.*)\\).*", "\\1", res, perl=TRUE) ), ncol = 2, nrow= 1))
} # Reduced Form COVID-19 cases
fy4 <- function(y){
  gc()
  reg <- femlm(as.formula(paste("deaths ~ inv_contin + lag_deaths +",
                                get(paste("weather", y, sep = "")),"+",controls_d,"+",controls_d2,
                                " |GID_2 + week + country_month ", sep = "")),
               family = "poisson", data = df, verbose=1)
  res <- paste(esttable(reg,  se = "cluster", cluster = "GID_2")[1,])
  as.data.frame(matrix(c(reg$coefficients[["inv_contin"]],
                         sub(".*\\((.*)\\).*", "\\1", res, perl=TRUE) ), ncol = 2, nrow= 1))
} # Reduced Form COVID-19 death

load("data/cases_weekly.RData")
coefs   <- bind_rows(map(c(1:15), fy1))
save(coefs,   file = "data/coefficients_specification_chart/first_stage_case_weather.RData")

# Note: Running all 15 reduced form regressions at once may lead to crashes in R-Studio!
coefs  <- bind_rows(map(c(1:15), fy3))
save(coefs, file = "data/coefficients_specification_chart/reduced_form_cases_weather.RData")

load("data/deaths_weekly.RData")
coefs   <- bind_rows(map(c(1:15), fy2))
save(coefs,  file = "data/coefficients_specification_chart/first_stage_deaths_weather.RData")

# Note: Running all 15 reduced form regressions at once may lead to crashes in R-Studio!
coefs  <- bind_rows(map(c(1:15), fy4))
save(coefs, file = "data/coefficients_specification_chart/reduced_form_deaths_weather.RData")

# IV. Lock-down Sample----
load("data/cases_weekly.RData")

#filter observations where stay at home requirements are continuously in place
df <- df %>%  mutate(stay = C6_Stay_at_home_requirements_2 + C6_Stay_at_home_requirements_3) %>% 
  filter(stay == 21)

reg1 <- femlm(as.formula(paste("cases ~ inv_contin + lag_cases+",
                               weather, " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

load("data/deaths_weekly.RData")

#filter observations where stay at home requirements are continuously in place
df <- df %>%  mutate(stay = C6_Stay_at_home_requirements_2 + C6_Stay_at_home_requirements_3) %>% 
  filter(stay == 28)

reg2 <- femlm(as.formula(paste("deaths ~ inv_contin + lag_deaths+",
                               weather, " |GID_2 + week + country_month ", sep = "")),
              family = "poisson", data = df, verbose=1)

# Latex Output 
esttex(reg1, reg2, se = "cluster", cluster = "GID_2", 
       drop =  names(reg2$coefficients)[2:length(names(reg2$coefficients))])
       
