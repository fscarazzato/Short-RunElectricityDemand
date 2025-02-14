library(lfe)
library(stargazer)
library(tidyverse)
library(lubridate)
library(ivreg)
library(sandwich)
library(lmtest)
library(car)

# IHS() transformation
ihs <- function(x) log(x + sqrt((x^2)+1))

# read data
data <- readRDS("output/06_ready_for_estimation.rds")

# exclude pre 2016 data and 2020
data <- subset(data, Year %in% c(as.character(2016:2019), as.character(2021:2023)))


data$Hour <- as.character(data$Hour)
data$Wday <- as.character(data$Wday)
data$Week <- as.character(data$Week)
data$Month <- as.character(data$Month)
data$Year <- as.character(data$Year)


# first stage -------------------------------------------------------------

data <- subset(data, data$price > 0)
data$price <- log(data$price)
data$rolling_tp_week <- log(data$rolling_tp_week_capacity) # use the precipitation weighted by ROR capacity
data$wm_t2m <- log(data$wm_t2m + 273.15) # kelvin degrees
data$wind_gen_DE <- log(data$wind_gen_DE)
data$ETS_price <- log(data$ETS_price)
data$Gas_price <- log(data$Gas_price)
data$level_reservoirs <- log(data$level_reservoirs)
data$solar_gen <- ihs(data$solar_gen)


# First stage model 1
first1 <- lm(price ~  rolling_tp_week +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(first1) -> s_f1
s_f1


s_f1$adj.r.squared -> frsq1
nobs(first1) -> fn1


# HC robust standard errors
coeftest(first1, vcov=vcovHC(first1, "HC1")) -> t_first1

# Durbin-Watson test for autocorrelation
dwtest(first1)

# HAC robust standard errors
NeweyWest(first1, 
          prewhite = F, 
          adjust = T) -> fnw1

coeftest(first1, vcov=fnw1) -> ta_first1


linearHypothesis(first1, c("rolling_tp_week = 0"), vcov. = fnw1) -> fhac1






# first stage model 2
first2 <- lm(price ~ wind_gen_DE +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(first2) -> s_f2
s_f2


s_f2$adj.r.squared -> frsq2
nobs(first2) -> fn2


# HC robust standard errors
coeftest(first2, vcov=vcovHC(first2, "HC1")) -> t_first2


# Durbin-Watson test for autocorrelation
dwtest(first2)

# HAC robust standard errors
NeweyWest(first2, 
          prewhite = F, 
          adjust = T) -> fnw2

coeftest(first2, vcov=fnw2) -> ta_first2


linearHypothesis(first2, c("wind_gen_DE = 0"),  vcov. = fnw2) -> fhac2





# first stage model 3
first3 <- lm(price ~ rolling_tp_week +
               wind_gen_DE +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(first3) -> s_f3
s_f3


s_f3$adj.r.squared -> frsq3
nobs(first3) -> fn3


# HC robust standard errors
coeftest(first3, vcov=vcovHC(first3, "HC1")) -> t_first3

# Durbin-Watson test for autocorrelation
dwtest(first3)


# HAC robust standard errors
NeweyWest(first3, 
          prewhite = F, 
          adjust = T) -> fnw3


coeftest(first3, vcov=fnw3) -> ta_first3

linearHypothesis(first3, c("rolling_tp_week = 0", "wind_gen_DE = 0"), vcov. = fnw3) -> fhac3







# stargazer table  -----------------------------
stargazer(ta_first1, ta_first2, ta_first3, header = F, digits = 3,
          
          dep.var.labels = "ln(Price)",
          
          keep = c("rolling_tp_week", "wind_gen"),
          
          covariate.labels = c("ln(Weekly Precipitation)",
                               "ln(German Wind Energy)"),
          add.lines = list(c("Controls",
                             "\\checkmark", 
                             "\\checkmark",
                             "\\checkmark"),
                           c("Time Dummies",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("","","",""),
                           c("Adjusted R$^{2}$",
                             round(frsq1,2),
                             round(frsq2,2),
                             round(frsq3,2)),
                           c("Partial F-statistic",
                             round(fhac1[2,3],1),
                             round(fhac2[2,3],1),
                             round(fhac3[2,3],1)),
                           c("Observations",
                             prettyNum(fn1, big.mark=","),
                             prettyNum(fn2, big.mark=","),
                             prettyNum(fn3, big.mark=","))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("HAC standard errors.",
                    "$^{***}$Significant at the 0.1 percent level.",
                    "$^{**}$Significant at the 1 percent level.",
                    "$^{*}$Significant at the 5 percent level."),
          notes.append = F,
          
          
          
          table.layout = "=d#-t-as=n",
          
          style = "qje")




# stargazer table appendix -----------------------------
stargazer(ta_first1, ta_first2, ta_first3, header = F, digits = 3,
          
          dep.var.labels = "ln(Price)",
          
          
          keep = c("rolling_tp_week", "wind_gen", "wm_t2m", "I(wm_t2m^2)", "solar_gen", "ETS_price", 
                   "Gas_price", "level_reservoirs", "Constant"),
          
          covariate.labels = c("ln(Weekly Precipitation)",
                               "ln(German Wind Energy)", "ln(Temperature)",
                               "ln(Temperature)$^2$", "ihs(Solar Energy)",
                               "ln(Carbon Price)", "ln(Gas Price)", "ln(Reservoirs)", "Constant"),
          
          

          add.lines = list(c("Time Dummies",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("","","",""),
                           c("Adjusted R$^{2}$",
                             round(frsq1,2),
                             round(frsq2,2),
                             round(frsq3,2)),
                           c("Partial F-statistic",
                             round(fhac1[2,3],1),
                             round(fhac2[2,3],1),
                             round(fhac3[2,3],1)),
                           c("Observations",
                             prettyNum(fn1, big.mark=","),
                             prettyNum(fn2, big.mark=","),
                             prettyNum(fn3, big.mark=","))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("HAC standard errors.",
                    "$^{***}$Significant at the 0.1 percent level.",
                    "$^{**}$Significant at the 1 percent level.",
                    "$^{*}$Significant at the 5 percent level."),
          notes.append = F,
          
          
          
          table.layout = "=d#-t-as=n",
          
          style = "qje")


