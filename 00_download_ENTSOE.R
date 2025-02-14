library(dplyr)
library(entsoeapi)
library(lubridate)

# https://github.com/krose/entsoeapi

# In order to download data from the ENTSO-E API, you need to register at the ENTSO-E Transparency Platform and get an API key.
# Create a file called .Renviron in your project directory and add the following line to it:
# ENTSOE_PAT=your_api_key

en_eic() %>% 
  filter(AreaName == "Switzerland") %>% 
  glimpse()



# Price, day ahead -------------------------------------------------------------

yr_l <- as.character(2015:2022)
yr <- as.character(2016:2023)

for (yy in 1:length(yr)) {
  
  
  assign(paste0("price", yr[yy]),  en_transmission_day_ahead_prices(eic = "10YCH-SWISSGRIDZ",
                                                                    period_start = lubridate::ymd_hm(paste0(yr_l[yy], "-12-31 23:00"), tz = "CET"),
                                                                    period_end = lubridate::ymd_hm(paste0(yr[yy], "-12-31 23:00"), tz = "CET"))
  )
  
}



Price <- as.data.frame(rbind(price2016,
                             price2017,
                             price2018,
                             price2019,
                             price2020,
                             price2021,
                             price2022,
                             price2023))



saveRDS(Price, "data/day_ahead_price_2016_2023.rds")


# generation by type --------------------------------------------------------
yr_l <- as.character(2015:2022)
yr <- as.character(2016:2023)

for (yy in 1:length(yr)) {
  

assign(paste0("gen", yr[yy]),  en_generation_agg_gen_per_type(eic = "10YCH-SWISSGRIDZ", 
                                      period_start = lubridate::ymd_hm(paste0(yr_l[yy], "-12-31 23:00"), tz = "CET"),
                                      period_end = lubridate::ymd_hm(paste0(yr[yy], "-12-31 23:00"), tz = "CET"))
)

}


# bind
GEN <- as.data.frame(rbind(gen2016,
                           gen2017,
                           gen2018,
                           gen2019,
                           gen2020,
                           gen2021,
                           gen2022,
                           gen2023))

# subset for type
GEN %>% subset(resource_psr_type %in% c("B16")) -> GEN_Solar


GEN_Solar %>% select(inbiddingzone_domain_mrid, resource_psr_type,
                     resource_psr_type_def, resolution, start, quantity, 
                     quantity_measure_unit) %>% 
  rename(dt = start) -> GEN_Solar





# generation by type GERMANY ------------------------------------------------
yr_l <- as.character(2015:2022)
yr <- as.character(2016:2023)

for (yy in 1:length(yr)) {
  
  
  assign(paste0("gen", yr[yy]),  en_generation_agg_gen_per_type(eic = "10Y1001A1001A83F", 
                                                                period_start = lubridate::ymd_hm(paste0(yr_l[yy], "-12-31 23:00"), tz = "CET"),
                                                                period_end = lubridate::ymd_hm(paste0(yr[yy], "-12-31 23:00"), tz = "CET"))
  )
  
}

yr_l <- as.character(2020:2022)
yr <- as.character(2021:2023)

for (yy in 1:length(yr)) {
  
  
  assign(paste0("gen", yr[yy]),  en_generation_agg_gen_per_type(eic = "10Y1001A1001A82H", 
                                                                period_start = lubridate::ymd_hm(paste0(yr_l[yy], "-12-31 23:00"), tz = "CET"),
                                                                period_end = lubridate::ymd_hm(paste0(yr[yy], "-12-31 23:00"), tz = "CET"))
  )
  
}


# bind
GEN_DE <- as.data.frame(rbind(gen2016,
                           gen2017,
                           gen2018,
                           gen2019,
                           gen2020,
                           gen2021,
                           gen2022,
                           gen2023))

# subset for type

GEN_DE %>% subset(resource_psr_type %in% c("B19", "B18")) -> GEN_Wind_DE



GEN_Wind_DE %>% select(start, quantity) %>% 
  rename(dt = start) %>%
  
  mutate(dt = ymd_h(paste0(date(dt), " ", hour(dt)))) %>%
  group_by(dt) %>%
  
  summarise(wind_gen_DE = sum(quantity, na.rm = T)) %>% 
  
  ungroup() -> GEN_Wind_DE




# save
saveRDS(GEN_Solar, "data/Solar_gen_2016_2023.rds")
saveRDS(GEN_Wind_DE, "data/Wind_gen_DE_2016_2023.rds")

