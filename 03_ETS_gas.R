library(tidyverse)
library(lubridate)

# EU Carbon prices and Gas prices

# My data source is the Bloomberg terminal, therefore I do not have permission to share the data
# The same data can be easily downloaded from the Bloomberg terminal

# Carbon price: search for EUETSSY1, download PX_BID, PX_ASK for all available dates between 1.1.2026 and 31.12.2023
# Gas price: search for TTF0NXHR, download PX_LAST for all available dates between 1.1.2026 and 31.12.2023


# ETS ---------------------------------------------------------------------


ets <- read.csv("Short-RunElectricityDemand/data/ETS price 2016-2023.csv") %>% na.omit()

# check difference between bid and ask price 
ets$diff <- ets$PX_ASK - ets$PX_BID
summary(ets$diff)

# take row-wise max between bid and ask price  
ets$ETS.Price <- pmax(ets$PX_ASK, ets$PX_BID)


# trasform into lubridate date
ets$dt <- dmy(ets$Date)

# arrange by date
ets %>% arrange(ets$dt) %>% select(dt, ETS.Price)  -> ets


# all dates in the series
ts <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"), as.POSIXct("2024-06-26", tz = "UTC"), by = "day")

df <- data.frame(dt=ts)

# join all dates with data
ets <- full_join(df, ets)

# fill missing prices with last available price
ets %>% 
  fill(ETS.Price) -> ets



# save
saveRDS(ets, "Short-RunElectricityDemand/output/ETS_price_2016_2023.rds")



# Gas ---------------------------------------------------------------------

gas <- read.csv("Short-RunElectricityDemand/data/Gas 2016-2023.csv") %>% na.omit()

# trasform into lubridate date
gas$dt <- dmy(gas$Date)

# arrange by date
gas %>% arrange(gas$dt) %>% rename(TTF_NatGas_Spot_price = PX_LAST) %>% select(dt, TTF_NatGas_Spot_price) -> gas


# all dates in the series
ts <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"), as.POSIXct("2024-06-26", tz = "UTC"), by = "day")

df <- data.frame(dt=ts)

# join all dates with data
gas <- full_join(df, gas)

# fill missing prices with last available price
gas %>% 
  fill(TTF_NatGas_Spot_price) -> gas


# save
saveRDS(gas, "Short-RunElectricityDemand/output/Gas_price_2016_2023.rds")

