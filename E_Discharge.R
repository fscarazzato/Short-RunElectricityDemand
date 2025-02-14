library(stargazer)
library(tidyverse)
library(lubridate)
library(ivreg)
library(sandwich)
library(lmtest)



################################################################################

# Discharge data for the 4 stations
# skip the first 3 rows
read.csv("data/discharge/2174_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2174

s2174$dt <- ymd_hms(s2174$Zeitstempel)

read.csv("data/discharge/2289_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2289

s2289$dt <- ymd_hms(s2289$Zeitstempel)

read.csv("data/discharge/2020_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2020

s2020$dt <- ymd_hms(s2020$Zeitstempel)


read.csv("data/discharge/2265_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2265

s2265$dt <- ymd_hms(s2265$Zeitstempel)




################################################################################

# Precipitation data
TP <- readRDS("output/05_weekly_sum_2016_2023.rds")


# merge data
s2174 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2174" = Wert) -> s2174
s2289 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2289" = Wert) -> s2289
s2020 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2020" = Wert) -> s2020
s2265 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2265" = Wert) -> s2265

TP <- TP %>% left_join(s2174, by = "dt") %>%
  left_join(s2289, by = "dt") %>%
  left_join(s2020, by = "dt") %>%
  left_join(s2265, by = "dt")

#############################################################################
# correlation between discharge and rolling_precip



# rhine
lm(Discharge_2289 ~ rolling_tp_2289_week +
     rolling_tp_2289_week_lag1 +
     rolling_tp_2289_week_lag2 +
     rolling_tp_2289_week_lag3 +
     rolling_tp_2289_week_lag4 +
     rolling_tp_2289_week_lag5 +
     rolling_tp_2289_week_lag6,
   data = TP) -> m2289

summary(m2289)

# Durbin-Watson test for autocorrelation
dwtest(m2289)



# HAC robust standard errors
NeweyWest(m2289, 
          prewhite = F, 
          adjust = T) -> nw2289

coeftest(m2289, vcov=nw2289) -> c2289


# rhone
lm(Discharge_2174 ~ rolling_tp_2174_week +
     rolling_tp_2174_week_lag1 +
     rolling_tp_2174_week_lag2 +
     rolling_tp_2174_week_lag3 +
     rolling_tp_2174_week_lag4 +
     rolling_tp_2174_week_lag5 +
     rolling_tp_2174_week_lag6,
   data = TP) -> m2174

summary(m2174)

# Durbin-Watson test for autocorrelation
dwtest(m2174)

# HAC robust standard errors
NeweyWest(m2174, 
          prewhite = F, 
          adjust = T) -> nw2174

coeftest(m2174, vcov=nw2174) -> c2174



# inn
lm(Discharge_2265 ~ rolling_tp_2265_week +
     rolling_tp_2265_week_lag1 +
     rolling_tp_2265_week_lag2 +
     rolling_tp_2265_week_lag3 +
     rolling_tp_2265_week_lag4 +
     rolling_tp_2265_week_lag5 +
     rolling_tp_2265_week_lag6,
   data = TP) -> m2265

summary(m2265)

# Durbin-Watson test for autocorrelation
dwtest(m2265)

# HAC robust standard errors
NeweyWest(m2265, 
          prewhite = F, 
          adjust = T) -> nw2265

coeftest(m2265, vcov=nw2265) -> c2265


# ticino
lm(Discharge_2020 ~ rolling_tp_2020_week +
     rolling_tp_2020_week_lag1 +
     rolling_tp_2020_week_lag2 +
     rolling_tp_2020_week_lag3 +
     rolling_tp_2020_week_lag4 +
     rolling_tp_2020_week_lag5 +
     rolling_tp_2020_week_lag6,
   data = TP) -> m2020

summary(m2020)

# Durbin-Watson test for autocorrelation
dwtest(m2020)

# HAC robust standard errors
NeweyWest(m2020, 
          prewhite = F, 
          adjust = T) -> nw2020

coeftest(m2020, vcov=nw2020) -> c2020


Rhine <- c2289
Rhone <- c2174
Inn <- c2265
Ticino <- c2020

rownames(Rhine) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")
rownames(Rhone) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")
rownames(Inn) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")
rownames(Ticino) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")

# stargazer table# as.character()stargazer table
stargazer(Rhine, Rhone, Inn, Ticino, header = F, digits = 3,
          
          dep.var.labels = "Discharge",
          
          column.labels = c("Rhine", "Rhône", "Inn", "Ticino"),
          
          covariate.labels = c("Weekly Precipitation$_{t}$",
                               "Weekly Precipitation$_{t-168}$",
                               "Weekly Precipitation$_{t-336}$",
                               "Weekly Precipitation$_{t-504}$",
                               "Weekly Precipitation$_{t-672}$",
                               "Weekly Precipitation$_{t-840}$",
                               "Weekly Precipitation$_{t-1008}$"),
          add.lines = list(c("Adjusted R$^{2}$",
                             round(summary(m2289)$adj.r.squared,2),
                             round(summary(m2174)$adj.r.squared,2),
                             round(summary(m2265)$adj.r.squared,2),
                             round(summary(m2020)$adj.r.squared,2)),          
                           c("Observations",
                             prettyNum(nobs(m2289), big.mark=","),
                             prettyNum(nobs(m2174), big.mark=","),
                             prettyNum(nobs(m2265), big.mark=","),
                             prettyNum(nobs(m2020), big.mark=","))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("HAC standard errors.",
                    "$^{***}$Significant at the 0.1 percent level.",
                    "$^{**}$Significant at the 1 percent level.",
                    "$^{*}$Significant at the 5 percent level."),
          notes.append = F,
          
          
          table.layout = "-d#c-!t-as-n")
