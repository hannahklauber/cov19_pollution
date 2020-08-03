## Regression Analysis at the daily level
rm(list = ls())

load("data/data_daily.RData")

# i) COVID-19 cases
reg1 = femlm(cases ~ l(inv_contin_pixel,0:20) + lag_cases +
               l(precipitation,0:20) + l(humidity,0:20) + 
               l(still_air,0:20) + l(temperature,0:20) | 
               district_month + state_week + country_time,
             data = data_daily, 
             family = "poisson",verbose=1,
             panel.id = ~GID_2+date)

esttex(reg1, se = "cluster", cluster = "GID_2", 
       drop =  names(reg1$coefficients)[2:length(names(reg1$coefficients))])

reg2 = femlm(deaths ~ l(inv_contin_pixel,0:27) + lag_deaths +
               l(precipitation,0:27) + l(humidity,0:27) + 
               l(still_air,0:27) + l(temperature,0:27) | 
               district_month + state_week + country_time,
             data = data_daily, 
             family = "poisson",verbose=1,
             panel.id = ~GID_2+date)

esttex(reg2, se = "cluster", cluster = "GID_2", 
       drop =  names(reg2$coefficients)[2:length(names(reg2$coefficients))])
