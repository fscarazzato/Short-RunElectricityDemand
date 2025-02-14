# Quantity data, from Swissgrid

library(tidyverse)
library(lubridate)
library(stringr)

# from https://www.swissgrid.ch/en/home/operation/grid-data/load.html#links-downloads
load_grid <- read.csv("data/Load_swissgrid_2016_2023.csv")
load_end <- read.csv("data/Load_swissgrid_end_users_2016_2023.csv")


# need to aggregate into 1h intervals
load_grid$H <- 0:(nrow(load_grid)-1) %/% 4
load_end$H <- 0:(nrow(load_end)-1) %/% 4


load_grid %>%
  
  # goup by hour and sum load
  group_by(H) %>%
  
  summarise(Total.energy.consumption.kWh = sum(Total.energy.consumption.kWh)) -> load_grid_2

# give an exact date to each hour
load_grid_2$dt <- as_datetime(c(load_grid_2$H*60*60), origin="2016-01-01")


# change unit to MWh
load_grid_2$quantity_swissgrid <- load_grid_2$Total.energy.consumption.kWh / 1000


load_end %>%
  
  # goup by hour and sum load
  group_by(H) %>%
  
  summarise(Total.end.use.kWh = sum(Total.energy.consumed.by.end.users.in.the.Swiss.controlblock)) -> load_end_2

# give an exact date to each hour
load_end_2$dt <- as_datetime(c(load_end_2$H*60*60), origin="2016-01-01")

# change unit to MWh
load_end_2$end_quantity_swissgrid <- load_end_2$Total.end.use.kWh / 1000


###################
# save
load_grid_2 %>% select(dt, quantity_swissgrid) -> load_swissgrid
load_end_2 %>% select(dt, end_quantity_swissgrid) -> load_swissgrid_end

saveRDS(load_swissgrid, "output/swissgrid_load_2016_2023.rds")
saveRDS(load_swissgrid_end, "output/swissgrid_end_load_2016_2023.rds")
