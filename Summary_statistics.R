library(lfe)
library(stargazer)
library(tidyverse)
library(lubridate)
library(ivreg)
library(sandwich)
library(lmtest)
library(car)

# read data
data <- readRDS("output/06_ready_for_estimation.rds")

# exclude pre 2016 data and 2020
data <- subset(data, Year %in% c(as.character(2016:2019), as.character(2021:2023)))

# taking the log requires:
data <- subset(data, data$price > 0) # no negative prices
data$wm_t2m <- data$wm_t2m + 273.15 # kelvin degrees

# power plant demand
data$storage_quantity_swissgrid <- data$quantity_swissgrid - data$end_quantity_swissgrid

# summary statistics -------------------------------------------------------------
sdata <- data %>% dplyr::select(quantity_swissgrid,  
                                end_quantity_swissgrid,
                                storage_quantity_swissgrid,
                                price, 
                                wm_t2m, 
                                ETS_price, 
                                Gas_price,
                                level_reservoirs,
                                solar_gen,
                                wind_gen_DE,
                                rolling_tp_week_capacity) %>%
  # demand in GW
  as.data.frame()


stargazer(sdata,  header = F, digits = 1, 
          summary.stat = c("mean", "sd", "min", "max"),
          column.labels = c("Mean", "Std. Dev.", "Min", "Max"),
          covariate.labels = c("Load (MW)",
                               "End-User Demand (MW)",
                               "Power Plants' Demand (MW)",
                               "Price (\\EUR/MWh)", 
                               "Temperature (K)",
                               "Carbon Price (\\EUR/t)", 
                               "Gas Price (\\EUR/MWh)", 
                               "Reservoirs (GWh)",
                               "Solar Energy (MW)",
                               "German Wind Energy (GW)",
                               "Weekly Precipitation (mm)"),
          style = "qje")



