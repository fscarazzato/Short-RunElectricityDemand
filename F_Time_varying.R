library(lfe)
library(stargazer)
library(tidyverse)
library(lubridate)
library(ivreg)
library(sandwich)
library(lmtest)


# IHS() transformation
ihs <- function(x) log(x + sqrt((x^2)+1))


# read data
data <- readRDS("output/06_ready_for_estimation.rds")



# exclude pre 2016 data
data <- subset(data, Year %in% c(as.character(2016:2023)))



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
data$rolling_tp_week <- log(data$rolling_tp_week_capacity)
data$wm_t2m <- log(data$wm_t2m + 273.15) # kelvin degrees
data$wind_gen_DE <- log(data$wind_gen_DE)
data$ETS_price <- log(data$ETS_price)
data$Gas_price <- log(data$Gas_price)
data$level_reservoirs <- log(data$level_reservoirs)
data$solar_gen <- ihs(data$solar_gen)


#  demand ------------------------------------------------------------


# second stage all
iv1 <- ivreg(quantity_swissgrid ~ price:Year +
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               
               
               
               rolling_tp_week:Year +
               wind_gen_DE:Year +
               
               
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price +
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)



# HAC robust standard errors
NeweyWest(iv1, 
          prewhite = F, 
          adjust = T) -> nw1

coeftest(iv1, vcov=nw1) -> ta_iv1
ta_iv1




beta_16 <- iv1$coefficients["price:Year2016"] 
beta_17 <- iv1$coefficients["price:Year2017"] 
beta_18 <- iv1$coefficients["price:Year2018"]
beta_19 <- iv1$coefficients["price:Year2019"]
beta_20 <- iv1$coefficients["price:Year2020"]
beta_21 <- iv1$coefficients["price:Year2021"]
beta_22 <- iv1$coefficients["price:Year2022"] 
beta_23 <- iv1$coefficients["price:Year2023"] 

beta1 <- c(beta_16, beta_17, beta_18, beta_19, beta_20, beta_21, beta_22, beta_23)

ta_iv1['price:Year2016',2] -> se_16
ta_iv1['price:Year2017',2] -> se_17
ta_iv1['price:Year2018',2] -> se_18
ta_iv1['price:Year2019',2] -> se_19
ta_iv1['price:Year2020',2] -> se_20
ta_iv1['price:Year2021',2] -> se_21
ta_iv1['price:Year2022',2] -> se_22
ta_iv1['price:Year2023',2] -> se_23

se1 <- c(se_16, se_17, se_18, se_19, se_20, se_21, se_22, se_23)

year <- c(2016:2023)


# datafrme
df_Se1 <- data.frame(cbind(beta1, se1, year))


df_Se1$beta <- as.numeric(df_Se1$beta1)
df_Se1$se <- as.numeric(df_Se1$se1)



df_Se1$b_lb <- df_Se1$beta  + qnorm((1-0.95)/2)*df_Se1$se
df_Se1$b_ub <- df_Se1$beta  - qnorm((1-0.95)/2)*df_Se1$se





# plot
ggplot(df_Se1, aes(x=as.character(year), y=as.numeric(beta), ymin = b_lb, ymax = b_ub)) + 
  geom_pointrange(size=1, lwd = 1, color = "black", shape = 23, stroke = 2, fill = "white") + 
  theme_bw() +   labs(x = "", y = "", fill="", shape = "") +
  theme(axis.text=element_text(size=rel(1)),
        axis.title=element_text(size=rel(1)))



# end demand ------------------------------------------------------------


# second stage all
iv2 <- ivreg(end_quantity_swissgrid ~  price:Year +
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               

               

               rolling_tp_week:Year +
               wind_gen_DE:Year +

  
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price +
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv2) -> s_iv2


# HAC robust standard errors
NeweyWest(iv2, 
          prewhite = F, 
          adjust = T) -> nw2

coeftest(iv2, vcov=nw2) -> ta_iv2
ta_iv2




beta_16 <- iv2$coefficients["price:Year2016"]
beta_17 <- iv2$coefficients["price:Year2017"] 
beta_18 <- iv2$coefficients["price:Year2018"]
beta_19 <- iv2$coefficients["price:Year2019"]
beta_20 <- iv2$coefficients["price:Year2020"]
beta_21 <- iv2$coefficients["price:Year2021"]
beta_22 <- iv2$coefficients["price:Year2022"] 
beta_23 <- iv2$coefficients["price:Year2023"] 

beta2 <- c(beta_16, beta_17, beta_18, beta_19, beta_20, beta_21, beta_22, beta_23)


ta_iv2['price:Year2016',2] -> se_16
ta_iv2['price:Year2017',2] -> se_17
ta_iv2['price:Year2018',2] -> se_18
ta_iv2['price:Year2019',2] -> se_19
ta_iv2['price:Year2020',2] -> se_20
ta_iv2['price:Year2021',2] -> se_21
ta_iv2['price:Year2022',2] -> se_22
ta_iv2['price:Year2023',2] -> se_23

se2 <- c(se_16, se_17, se_18, se_19, se_20, se_21, se_22, se_23)

year <- c(2016:2023)


# datafrme
df_Se2 <- data.frame(cbind(beta2, se2, year))


df_Se2$beta <- as.numeric(df_Se2$beta2)
df_Se2$se <- as.numeric(df_Se2$se2)



df_Se2$b_lb <- df_Se2$beta  + qnorm((1-0.95)/2)*df_Se2$se
df_Se2$b_ub <- df_Se2$beta  - qnorm((1-0.95)/2)*df_Se2$se





# plot
ggplot(df_Se2, aes(x=as.character(year), y=as.numeric(beta), ymin = b_lb, ymax = b_ub)) + 
  geom_pointrange(size=1, lwd = 1, color = "black", shape = 21, stroke = 2, fill = "white") + 
  theme_bw() +   labs(x = "", y = "", fill="", shape = "") +
  theme(axis.text=element_text(size=rel(1)),
        axis.title=element_text(size=rel(1)))




# storage demand ------------------------------------------------------------

# second stage
iv3 <- ivreg(storage_quantity_swissgrid ~ price:Year +
               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year | 
               

               rolling_tp_week:Year +
               wind_gen_DE:Year +

               wm_t2m + I(wm_t2m^2) +
               ETS_price +  Gas_price + 
               level_reservoirs + solar_gen +
               Holiday + Hour + Wday + Month + Year, data = data)

summary(iv3) -> s_iv3

# HAC robust standard errors
NeweyWest(iv3, 
          prewhite = F, 
          adjust = T) -> nw3

coeftest(iv3, vcov=nw3) -> ta_iv3
ta_iv3


beta_16 <- iv3$coefficients["price:Year2016"] 
beta_17 <- iv3$coefficients["price:Year2017"] 
beta_18 <- iv3$coefficients["price:Year2018"]
beta_19 <- iv3$coefficients["price:Year2019"] 
beta_20 <- iv3$coefficients["price:Year2020"]
beta_21 <- iv3$coefficients["price:Year2021"]
beta_22 <- iv3$coefficients["price:Year2022"] 
beta_23 <- iv3$coefficients["price:Year2023"] 

beta3 <- c(beta_16, beta_17, beta_18, beta_19, beta_20, beta_21, beta_22, beta_23)

ta_iv3['price:Year2016',2] -> se_16
ta_iv3['price:Year2017',2] -> se_17
ta_iv3['price:Year2018',2] -> se_18
ta_iv3['price:Year2019',2] -> se_19
ta_iv3['price:Year2020',2] -> se_20
ta_iv3['price:Year2021',2] -> se_21
ta_iv3['price:Year2022',2] -> se_22
ta_iv3['price:Year2023',2] -> se_23

se3 <- c(se_16, se_17, se_18, se_19, se_20, se_21, se_22, se_23)




# datafrme
df_Se3 <- data.frame(cbind(beta3, se3, year))


df_Se3$beta <- as.numeric(df_Se3$beta3)
df_Se3$se <- as.numeric(df_Se3$se3)



df_Se3$b_lb <- df_Se3$beta  + qnorm((1-0.95)/2)*df_Se3$se
df_Se3$b_ub <- df_Se3$beta  - qnorm((1-0.95)/2)*df_Se3$se





# plot
ggplot(df_Se3, aes(x=as.character(year), y=as.numeric(beta), ymin = b_lb, ymax = b_ub)) + 
  geom_pointrange(size=1, lwd = 1, color = "black", shape = 22, stroke = 2, fill = "white") + 
  theme_bw() +   labs(x = "", y = "", fill="", shape = "") +
  theme(axis.text=element_text(size=rel(1)),
        axis.title=element_text(size=rel(1)))

