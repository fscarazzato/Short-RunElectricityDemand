# PUT EVERYTHING TOGETHER AND ADD TIME DUMMIES


library(tidyverse)
library(lubridate)
library(readxl)


# READ DATA ---------------------------------------------------------------

# data present in the repository

load_swissgrid <- readRDS("output/swissgrid_load_2016_2023.rds")
load_end <- readRDS("output/swissgrid_end_load_2016_2023.rds")
t2m <- readRDS("output/02_t2m_ERA5_2016_2023_LONG.rds")
tp <- readRDS("output/05_weekly_sum_2016_2023.rds") %>%
  dplyr::select(dt, rolling_tp_week_capacity)


# weekly filling rate of reservoirs
# from https://www.bfe.admin.ch/bfe/en/home/supply/statistics-and-geodata/energy-statistics/electricity-statistics.html/
reservoirs <- read.csv("data/ogd17_fuellungsgrad_speicherseen.csv") %>% 
  mutate(dt = ymd(Datum),
         level_reservoirs = TotalCH_speicherinhalt_gwh) %>%
  select(dt, level_reservoirs)


# data I don't have permission to share ------------------------------------

price <- readRDS("data/day_ahead_price_2016_2023.rds") %>%
  dplyr::select(dt, price) # 00_download_ENTSOE.R
solar <- readRDS("data/Solar_gen_2016_2023.rds") %>%
  dplyr::select(dt, quantity )%>% rename(solar_gen = quantity) # refer to script 00_download_ENTSOE.R
wind_DE <- readRDS("data/Wind_gen_DE_2016_2023.rds") # 00_download_ENTSOE.R


ets_price <- readRDS("output/ETS_price_2016_2023.rds") # 03_ETS_gas.R
gas_price <- readRDS("output/Gas_price_2016_2023.rds") # 03_ETS_gas.R

# MERGE -------------------------------------------------------------------



all <- inner_join(load_swissgrid, price, by = "dt") %>%
  
  left_join(load_end, by = "dt") %>%

  left_join(solar, by = "dt") %>%
  
  left_join(wind_DE, by = "dt") %>%
  
  left_join(t2m, by = "dt") %>% 
  
  full_join(tp, by = "dt") %>% distinct() %>%
  
  left_join(all, ets_price, by = "dt") %>% 
  
  left_join(gas_price, by = "dt") %>%
  
  left_join(reservoirs, by = "dt") %>%
  
  # fill missing values with last available value
  fill(ETS.Price, TTF_NatGas_Spot_price, level_reservoirs)


# arrange all by dt
all <- all %>% arrange(dt)

# Year
all$Year <- year(all$dt)

# rename
all %>% rename(ETS_price = ETS.Price, Gas_price = TTF_NatGas_Spot_price) -> all



# CREATE DUMMIES ----------------------------------------------------------


all$Hour <- as.factor(hour(all$dt))
all$Wday <- as.factor(wday(all$dt, label = T))
all$Month <- as.factor(month(all$dt, label = T))
all$Year <- as.factor(year(all$dt))
all$Week <- as.factor(week(all$dt))


# public holidays in at least half (13/26) the Cantons:
# https://en.wikipedia.org/wiki/Public_holidays_in_Switzerland
#

all$Holiday <- NA

# Ascension day (variable)
all$Holiday[all$dt %within%  interval(ymd("2016-05-05", tz = "Europe/Zurich"), ymd_h("2016-05-05-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-05-25", tz = "Europe/Zurich"), ymd_h("2017-05-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-05-10", tz = "Europe/Zurich"), ymd_h("2018-05-10-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-05-30", tz = "Europe/Zurich"), ymd_h("2019-05-30-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-05-21", tz = "Europe/Zurich"), ymd_h("2020-05-21-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-05-13", tz = "Europe/Zurich"), ymd_h("2021-05-13-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-05-26", tz = "Europe/Zurich"), ymd_h("2022-05-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-05-18", tz = "Europe/Zurich"), ymd_h("2023-05-18-23", tz = "Europe/Zurich"))] <- 1

# Whit Monday (variable)
all$Holiday[all$dt %within%  interval(ymd("2016-05-16", tz = "Europe/Zurich"), ymd_h("2016-05-16-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-06-05", tz = "Europe/Zurich"), ymd_h("2017-06-05-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-05-21", tz = "Europe/Zurich"), ymd_h("2018-05-21-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-06-10", tz = "Europe/Zurich"), ymd_h("2019-06-10-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-06-01", tz = "Europe/Zurich"), ymd_h("2020-06-01-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-05-24", tz = "Europe/Zurich"), ymd_h("2021-05-24-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-06-06", tz = "Europe/Zurich"), ymd_h("2022-06-06-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-05-29", tz = "Europe/Zurich"), ymd_h("2023-05-29-23", tz = "Europe/Zurich"))] <- 1

# Corpus Christi (variable)
all$Holiday[all$dt %within%  interval(ymd("2016-05-26", tz = "Europe/Zurich"), ymd_h("2016-05-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-06-15", tz = "Europe/Zurich"), ymd_h("2017-06-15-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-05-31", tz = "Europe/Zurich"), ymd_h("2018-05-31-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-06-20", tz = "Europe/Zurich"), ymd_h("2019-06-20-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-06-11", tz = "Europe/Zurich"), ymd_h("2020-06-11-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-06-03", tz = "Europe/Zurich"), ymd_h("2021-06-03-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-06-16", tz = "Europe/Zurich"), ymd_h("2022-06-16-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-06-08", tz = "Europe/Zurich"), ymd_h("2023-06-08-23", tz = "Europe/Zurich"))] <- 1

# 1 Aug  (national day)
all$Holiday[all$dt %within%  interval(ymd("2016-08-1", tz = "Europe/Zurich"), ymd_h("2016-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-08-1", tz = "Europe/Zurich"), ymd_h("2017-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-08-1", tz = "Europe/Zurich"), ymd_h("2018-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-08-1", tz = "Europe/Zurich"), ymd_h("2019-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-08-1", tz = "Europe/Zurich"), ymd_h("2020-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-08-1", tz = "Europe/Zurich"), ymd_h("2021-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-08-1", tz = "Europe/Zurich"), ymd_h("2022-08-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-08-1", tz = "Europe/Zurich"), ymd_h("2023-08-1-23", tz = "Europe/Zurich"))] <- 1

# 1 Nov 
all$Holiday[all$dt %within%  interval(ymd("2016-11-1", tz = "Europe/Zurich"), ymd_h("2016-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-11-1", tz = "Europe/Zurich"), ymd_h("2017-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-11-1", tz = "Europe/Zurich"), ymd_h("2018-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-11-1", tz = "Europe/Zurich"), ymd_h("2019-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-11-1", tz = "Europe/Zurich"), ymd_h("2020-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-11-1", tz = "Europe/Zurich"), ymd_h("2021-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-11-1", tz = "Europe/Zurich"), ymd_h("2022-11-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-11-1", tz = "Europe/Zurich"), ymd_h("2023-11-1-23", tz = "Europe/Zurich"))] <- 1

# 8 Dec 
all$Holiday[all$dt %within%  interval(ymd("2016-12-8", tz = "Europe/Zurich"), ymd_h("2016-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-12-8", tz = "Europe/Zurich"), ymd_h("2017-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-12-8", tz = "Europe/Zurich"), ymd_h("2018-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-12-8", tz = "Europe/Zurich"), ymd_h("2019-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-12-8", tz = "Europe/Zurich"), ymd_h("2020-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-12-8", tz = "Europe/Zurich"), ymd_h("2021-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-12-8", tz = "Europe/Zurich"), ymd_h("2022-12-8-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-12-8", tz = "Europe/Zurich"), ymd_h("2023-12-8-23", tz = "Europe/Zurich"))] <- 1

# 25 Dec 
all$Holiday[all$dt %within%  interval(ymd("2016-12-25", tz = "Europe/Zurich"), ymd_h("2016-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-12-25", tz = "Europe/Zurich"), ymd_h("2017-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-12-25", tz = "Europe/Zurich"), ymd_h("2018-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-12-25", tz = "Europe/Zurich"), ymd_h("2019-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-12-25", tz = "Europe/Zurich"), ymd_h("2020-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-12-25", tz = "Europe/Zurich"), ymd_h("2021-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-12-25", tz = "Europe/Zurich"), ymd_h("2022-12-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-12-25", tz = "Europe/Zurich"), ymd_h("2023-12-25-23", tz = "Europe/Zurich"))] <- 1

# 26 Dec 
all$Holiday[all$dt %within%  interval(ymd("2016-12-26", tz = "Europe/Zurich"), ymd_h("2016-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-12-26", tz = "Europe/Zurich"), ymd_h("2017-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-12-26", tz = "Europe/Zurich"), ymd_h("2018-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-12-26", tz = "Europe/Zurich"), ymd_h("2019-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-12-26", tz = "Europe/Zurich"), ymd_h("2020-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-12-26", tz = "Europe/Zurich"), ymd_h("2021-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-12-26", tz = "Europe/Zurich"), ymd_h("2022-12-26-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-12-26", tz = "Europe/Zurich"), ymd_h("2023-12-26-23", tz = "Europe/Zurich"))] <- 1

# 1 Jan 
all$Holiday[all$dt %within%  interval(ymd("2016-01-1", tz = "Europe/Zurich"), ymd_h("2016-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-01-1", tz = "Europe/Zurich"), ymd_h("2017-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-01-1", tz = "Europe/Zurich"), ymd_h("2018-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-01-1", tz = "Europe/Zurich"), ymd_h("2019-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-01-1", tz = "Europe/Zurich"), ymd_h("2020-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-01-1", tz = "Europe/Zurich"), ymd_h("2021-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-01-1", tz = "Europe/Zurich"), ymd_h("2022-01-1-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-01-1", tz = "Europe/Zurich"), ymd_h("2023-01-1-23", tz = "Europe/Zurich"))] <- 1

# 2 Jan 
all$Holiday[all$dt %within%  interval(ymd("2016-01-2", tz = "Europe/Zurich"), ymd_h("2016-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-01-2", tz = "Europe/Zurich"), ymd_h("2017-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-01-2", tz = "Europe/Zurich"), ymd_h("2018-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-01-2", tz = "Europe/Zurich"), ymd_h("2019-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-01-2", tz = "Europe/Zurich"), ymd_h("2020-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-01-2", tz = "Europe/Zurich"), ymd_h("2021-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-01-2", tz = "Europe/Zurich"), ymd_h("2022-01-2-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-01-2", tz = "Europe/Zurich"), ymd_h("2023-01-2-23", tz = "Europe/Zurich"))] <- 1

# Good Friday (variable)
all$Holiday[all$dt %within%  interval(ymd("2016-03-25", tz = "Europe/Zurich"), ymd_h("2016-03-25-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-04-14", tz = "Europe/Zurich"), ymd_h("2017-04-14-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-03-30", tz = "Europe/Zurich"), ymd_h("2018-03-30-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-04-19", tz = "Europe/Zurich"), ymd_h("2019-04-19-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-04-10", tz = "Europe/Zurich"), ymd_h("2020-04-10-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-04-02", tz = "Europe/Zurich"), ymd_h("2021-04-02-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-04-15", tz = "Europe/Zurich"), ymd_h("2022-04-15-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-04-07", tz = "Europe/Zurich"), ymd_h("2023-04-07-23", tz = "Europe/Zurich"))] <- 1

# Easter (variable)
all$Holiday[all$dt %within%  interval(ymd("2016-03-27", tz = "Europe/Zurich"), ymd_h("2016-03-27-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-04-16", tz = "Europe/Zurich"), ymd_h("2017-04-16-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-04-01", tz = "Europe/Zurich"), ymd_h("2018-04-01-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-04-21", tz = "Europe/Zurich"), ymd_h("2019-04-21-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-04-12", tz = "Europe/Zurich"), ymd_h("2020-04-12-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-04-04", tz = "Europe/Zurich"), ymd_h("2021-04-04-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-04-17", tz = "Europe/Zurich"), ymd_h("2022-04-17-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-04-09", tz = "Europe/Zurich"), ymd_h("2023-04-09-23", tz = "Europe/Zurich"))] <- 1

# Easter Monday (variable)
all$Holiday[all$dt %within%  interval(ymd("2016-03-28", tz = "Europe/Zurich"), ymd_h("2016-03-28-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2017-04-17", tz = "Europe/Zurich"), ymd_h("2017-04-17-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2018-04-02", tz = "Europe/Zurich"), ymd_h("2018-04-02-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2019-04-22", tz = "Europe/Zurich"), ymd_h("2019-04-22-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2020-04-13", tz = "Europe/Zurich"), ymd_h("2020-04-13-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2021-04-05", tz = "Europe/Zurich"), ymd_h("2021-04-05-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2022-04-18", tz = "Europe/Zurich"), ymd_h("2022-04-18-23", tz = "Europe/Zurich"))] <- 1
all$Holiday[all$dt %within%  interval(ymd("2023-04-10", tz = "Europe/Zurich"), ymd_h("2023-04-10-23", tz = "Europe/Zurich"))] <- 1

# zero to the hours that are not public holidays
all$Holiday[is.na(all$Holiday)] <- 0


# MORE TRASFORMATIONS -----------------------------------------------------

# arrange all by dt
all <- all %>% arrange(dt)


# german wind energy in GW
all$wind_gen_DE <- all$wind_gen_DE / 1000


# SAVE --------------------------------------------------------------------
saveRDS(all, "output/06_ready_for_estimation.rds")

