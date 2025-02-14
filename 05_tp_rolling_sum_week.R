# Weekly rolling sum of precipitation data

library(tidyverse)
library(lubridate)
library(RcppRoll)
library(sf)


# Precipitation data
readRDS("output/tp_river_stations_2016_2023.rds") -> TP


# ROR capacity per catchemnt area ----------------------------------------------------

wghts <- readRDS("output/ROR_capacity_2020_by_catchemnt_area.rds")

# precipitation weighted by basing ROR capacity
TP$tp_diff_ror_wght  <- (TP$tp_diff_2289 * wghts$ROR_capacity_2020[wghts$ID == 2289] +
  TP$tp_diff_2265 * wghts$ROR_capacity_2020[wghts$ID == 2265] +
  TP$tp_diff_2174 * wghts$ROR_capacity_2020[wghts$ID == 2174] + 
  TP$tp_diff_2020 * wghts$ROR_capacity_2020[wghts$ID == 2020]) / sum(wghts$ROR_capacity_2020)


################################################################################
# rolling weekly sum

# weights:
# exclude the first 6 hours, then go back one week
c(rep(0,12), rep(1, 168)) -> w

# one week before
roll_sum(TP$tp_diff_2174, weights = w, fill = NA, align = "right") -> TP$rolling_tp_2174_week
roll_sum(TP$tp_diff_2289, weights = w, fill = NA, align = "right") -> TP$rolling_tp_2289_week
roll_sum(TP$tp_diff_2020, weights = w, fill = NA, align = "right") -> TP$rolling_tp_2020_week
roll_sum(TP$tp_diff_2265, weights = w, fill = NA, align = "right") -> TP$rolling_tp_2265_week
roll_sum(TP$tp_diff_all, weights = w, fill = NA, align = "right") -> TP$rolling_tp_week
# weighted by capacity
roll_sum(TP$tp_diff_ror_wght, weights = w, fill = NA, align = "right") -> TP$rolling_tp_week_capacity

# one week lag
TP$rolling_tp_2174_week_lag1 <- lag(TP$rolling_tp_2174_week, 181  )
TP$rolling_tp_2289_week_lag1 <- lag(TP$rolling_tp_2289_week, 181  )
TP$rolling_tp_2020_week_lag1 <- lag(TP$rolling_tp_2020_week, 181  )
TP$rolling_tp_2265_week_lag1 <- lag(TP$rolling_tp_2265_week, 181  )
TP$rolling_tp_week_lag1 <- lag(TP$rolling_tp_week, 181  )
TP$rolling_tp_week_capacity_lag1 <- lag(TP$rolling_tp_week_capacity, 181)

# two week lag
TP$rolling_tp_2174_week_lag2 <- lag(TP$rolling_tp_2174_week, 349  )
TP$rolling_tp_2289_week_lag2 <- lag(TP$rolling_tp_2289_week, 349  )
TP$rolling_tp_2020_week_lag2 <- lag(TP$rolling_tp_2020_week, 349  )
TP$rolling_tp_2265_week_lag2 <- lag(TP$rolling_tp_2265_week, 349  )
TP$rolling_tp_week_lag2 <- lag(TP$rolling_tp_week, 349  )
TP$rolling_tp_week_capacity_lag2 <- lag(TP$rolling_tp_week_capacity, 349)


# three week lag
TP$rolling_tp_2174_week_lag3 <- lag(TP$rolling_tp_2174_week, 517  )
TP$rolling_tp_2289_week_lag3 <- lag(TP$rolling_tp_2289_week, 517  )
TP$rolling_tp_2020_week_lag3 <- lag(TP$rolling_tp_2020_week, 517  )
TP$rolling_tp_2265_week_lag3 <- lag(TP$rolling_tp_2265_week, 517  )
TP$rolling_tp_week_lag3 <- lag(TP$rolling_tp_week, 517  )
TP$rolling_tp_week_capacity_lag3 <- lag(TP$rolling_tp_week_capacity, 517)

# four week lag
TP$rolling_tp_2174_week_lag4 <- lag(TP$rolling_tp_2174_week, 685  )
TP$rolling_tp_2289_week_lag4 <- lag(TP$rolling_tp_2289_week, 685  )
TP$rolling_tp_2020_week_lag4 <- lag(TP$rolling_tp_2020_week, 685  )
TP$rolling_tp_2265_week_lag4 <- lag(TP$rolling_tp_2265_week, 685  )
TP$rolling_tp_week_lag4 <- lag(TP$rolling_tp_week, 685  )
TP$rolling_tp_week_capacity_lag4 <- lag(TP$rolling_tp_week_capacity, 685)

# five week lag
TP$rolling_tp_2174_week_lag5 <- lag(TP$rolling_tp_2174_week, 853 )
TP$rolling_tp_2289_week_lag5 <- lag(TP$rolling_tp_2289_week, 853 )
TP$rolling_tp_2020_week_lag5 <- lag(TP$rolling_tp_2020_week, 853 )
TP$rolling_tp_2265_week_lag5 <- lag(TP$rolling_tp_2265_week, 853 )
TP$rolling_tp_week_lag5 <- lag(TP$rolling_tp_week, 853 )
TP$rolling_tp_week_capacity_lag5 <- lag(TP$rolling_tp_week_capacity, 853)


# six week lag
TP$rolling_tp_2174_week_lag6 <- lag(TP$rolling_tp_2174_week, 1021)
TP$rolling_tp_2289_week_lag6 <- lag(TP$rolling_tp_2289_week, 1021)
TP$rolling_tp_2020_week_lag6 <- lag(TP$rolling_tp_2020_week, 1021)
TP$rolling_tp_2265_week_lag6 <- lag(TP$rolling_tp_2265_week, 1021)
TP$rolling_tp_week_lag6 <- lag(TP$rolling_tp_week, 1021)
TP$rolling_tp_week_capacity_lag6 <- lag(TP$rolling_tp_week_capacity, 1021)


# SAVE
saveRDS(TP, "output/05_weekly_sum_2016_2023.rds")

