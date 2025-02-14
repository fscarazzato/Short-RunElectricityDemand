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

data$storage_quantity_swissgrid <- data$quantity_swissgrid - data$end_quantity_swissgrid


data <- subset(data, data$price > 0)
data$quantity_swissgrid <- log(data$quantity_swissgrid)
data$end_quantity_swissgrid <- log(data$end_quantity_swissgrid)
data$storage_quantity_swissgrid <- log(data$storage_quantity_swissgrid)
data$price <- log(data$price)
data$rolling_tp_week <- log(data$rolling_tp_week_capacity) # use the precipitation weighted by ROR capacity
data$wm_t2m <- log(data$wm_t2m + 273.15) # kelvin degrees
data$wind_gen_DE <- log(data$wind_gen_DE)
data$ETS_price <- log(data$ETS_price)
data$Gas_price <- log(data$Gas_price)
data$level_reservoirs <- log(data$level_reservoirs)
data$solar_gen <- ihs(data$solar_gen)


# whole load ------------------------------------------------------------------


# tp as instrument
iv1 <- ivreg(quantity_swissgrid ~ price + 
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               
               rolling_tp_week +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv1) -> s_iv1



# adj r2
s_iv1$adj.r.squared -> rsq1

# N
iv1$nobs -> n1

beta_iv1 <- iv1$coefficients["price"]

# HC robust standard errors
coeftest(iv1, vcov=vcovHC(iv1, "HC1")) -> t_iv1


# Durbin-Watson test for autocorrelation
dwtest(iv1)


# HAC robust standard errors
NeweyWest(iv1, 
          prewhite = F, 
          adjust = T) -> nw1

coeftest(iv1, vcov=nw1) -> ta_iv1










# only german wind
iv2 <- ivreg(quantity_swissgrid ~ price + 
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               
               wind_gen_DE +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv2) -> s_iv2
s_iv2



# adj r2
s_iv2$adj.r.squared -> rsq2

# N
iv2$nobs -> n2

beta_iv2 <- iv2$coefficients["price"]


# HC robust standard errors
coeftest(iv2, vcov=vcovHC(iv2, "HC1")) -> t_iv2


# Durbin-Watson test for autocorrelation
dwtest(iv2)

# HAC robust standard errors
NeweyWest(iv2, 
          prewhite = F, 
          adjust = T) -> nw2

coeftest(iv2, vcov=nw2) -> ta_iv2








# tp and german wind
iv3 <- ivreg(quantity_swissgrid ~ price + 
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               
               rolling_tp_week +
               wind_gen_DE +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv3) -> s_iv3
s_iv3



# adj r2
s_iv3$adj.r.squared -> rsq3

# N
iv3$nobs -> n3

# elasticity
beta_iv3 <- iv3$coefficients["price"]


# HC robust standard errors
coeftest(iv3, vcov=vcovHC(iv3, "HC1")) -> t_iv3

# Durbin-Watson test for autocorrelation
dwtest(iv3)

# HAC robust standard errors
NeweyWest(iv3, 
          prewhite = F, 
          adjust = T) -> nw3

coeftest(iv3, vcov=nw3) -> ta_iv3



# end users only ----------------------------------------------------------


iv4 <- ivreg(end_quantity_swissgrid ~ price + 
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               
               rolling_tp_week +
               wind_gen_DE +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv4) -> s_iv4

# first stage partial F-test
s_iv4$diagnostics[1,3] -> f4

# adj r2
s_iv4$adj.r.squared -> rsq4

# N
iv4$nobs -> n4

beta_iv4 <- iv4$coefficients["price"]


# HC robust standard errors
coeftest(iv4, vcov=vcovHC(iv4, "HC1")) -> t_iv4


# Durbin-Watson test for autocorrelation
dwtest(iv4)


# HAC robust standard errors
NeweyWest(iv4, 
          prewhite = F, 
          adjust = T) -> nw4

coeftest(iv4, vcov=nw4) -> ta_iv4
ta_iv4


# storage & power plans ---------------------------------------------------



iv5 <- ivreg(storage_quantity_swissgrid ~ price + 
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               
               rolling_tp_week +
               wind_gen_DE +
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv5) -> s_iv5



# adj r2
s_iv5$adj.r.squared -> rsq5

# N
iv5$nobs -> n5

beta_iv5 <- iv5$coefficients["price"]


# HC robust standard errors
coeftest(iv5, vcov=vcovHC(iv5, "HC1")) -> t_iv5

# Durbin-Watson test for autocorrelation
dwtest(iv5)

# HAC robust standard errors
NeweyWest(iv5, 
          prewhite = F, 
          adjust = T) -> nw5

coeftest(iv5, vcov=nw5) -> ta_iv5
ta_iv5








# table -------------------------------------------------------------------



stargazer(ta_iv1, ta_iv2, ta_iv3, ta_iv4, ta_iv5, header = F, digits = 2,
          
          dep.var.labels = c("ln(Quantity)"),
          
          dep.var.caption = c("Dependent variable"),
          
          dep.var.labels.include = T,
          
          column.labels = c("Load","Load","Load","End Users","Power Plants"),
          
          keep = "price",
          
          covariate.labels = c("ln(Price)"),
          
          add.lines = list(c("Controls",
                              "\\checkmark",
                              "\\checkmark",
                              "\\checkmark",
                              "\\checkmark",
                              "\\checkmark"),
                            c("Time Dummies",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("IV: Weekly Precipitation", 
                             "\\checkmark",
                             "-",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("IV: German Wind Energy",
                             "-",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("","","","","",""),
                           c("Adjusted R$^{2}$",
                             round(rsq1,2),
                             round(rsq2,2),
                             round(rsq3,2),
                             round(rsq4,2),
                             round(rsq5,2)),
                           c("Observations", 
                             prettyNum(n1, big.mark=","),
                             prettyNum(n2, big.mark=","),
                             prettyNum(n3, big.mark=","),
                             prettyNum(n4, big.mark=","),
                             prettyNum(n5, big.mark=","))),
          
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("HAC standard errors.",
                    "$^{***}$Significant at the 0.1 percent level.",
                    "$^{**}$Significant at the 1 percent level.",
                    "$^{*}$Significant at the 5 percent level."),
          notes.append = F,
          
          
          table.layout = "-d#c-t-as-n")


# table appendix ------------------------------------------------------

stargazer(ta_iv1, ta_iv2, ta_iv3, ta_iv4, ta_iv5, header = F, digits = 2,
          
          dep.var.labels = "ln(Quantity)",
          
          column.labels = c("Load","Load","Load","End Users","Power Plants"),
          
          keep = c("price", "wm_t2m", "I(wm_t2m^2)", "solar_gen", "ETS_price", 
                   "Gas_price", "level_reservoirs", "Constant"),
          
          covariate.labels = c("ln(Price)", "ln(Temperature)", "ln(Temperature)$^2$", "ihs(Solar Energy)",
                               "ln(Carbon Price)", "ln(Gas Price)", "ln(Reservoirs)", "Constant"),
          
          add.lines = list(c("Time Dummies",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("IV: Weekly Precipitation", 
                             "\\checkmark",
                             "-",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("IV: German Wind Energy",
                             "-",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark",
                             "\\checkmark"),
                           c("","","","","",""),
                           c("Adjusted R$^{2}$",
                             round(rsq1,2),
                             round(rsq2,2),
                             round(rsq3,2),
                             round(rsq4,2),
                             round(rsq5,2)),
                           c("Observations", 
                             prettyNum(n1, big.mark=","),
                             prettyNum(n2, big.mark=","),
                             prettyNum(n3, big.mark=","),
                             prettyNum(n4, big.mark=","),
                             prettyNum(n5, big.mark=","))),
          
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("HAC standard errors.",
                    "$^{***}$Significant at the 0.1 percent level.",
                    "$^{**}$Significant at the 1 percent level.",
                    "$^{*}$Significant at the 5 percent level."),
          notes.append = F,
          
          
          table.layout = "-d#c-t-as-n")



