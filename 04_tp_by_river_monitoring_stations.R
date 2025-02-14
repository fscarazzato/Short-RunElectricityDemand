# Precipitation by river basins in switzerland

library(sf)
library(tidyverse)
library(mapview)
library(tmap)
library(readxl)
library(rnaturalearth)
library(rnaturalearthhires)
library(ncdf4)
library(chron)
library(lattice)
library(rasterVis)
library(raster)
library(RColorBrewer)
library(sp)
library(lubridate)
library(exactextractr)

# MAP OF RIVER BASINS
# https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.wasser-einzugsgebietsgliederung/items/wasser-einzugsgebietsgliederung
fc <- st_read("data/wasser-einzugsgebietsgliederung_2056.gdb/EZG_Flussgebiete.gdb")

# MAP OF river monitoring stations 
# https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.hydrologie-hydromessstationen/items/hydrologie-hydromessstationen
stations <- st_read("data/hydrologie-hydromessstationen_2056.shp")


# hydropower stations data as of 1.1.2020
# https://www.bfe.admin.ch/bfe/en/home/supply/digitalization-and-geoinformation/geoinformation/geodata/water/hydropower-plants-statistics.html
hys <- read_excel("data/10085-Statistik der Wasserkraftanlagen der Schweiz. Stand 1.1.2020/2019  Statistik der Wasserkraftanlagen der Schweiz 1.1.2020.xlsx")


# map of run of river plants
hys %>% subset(`WKA-Typ` == "L") %>%
  
  dplyr::select(`ZE-Nr`, `Inst. Turbinenleistung`, `WKA-Typ`,
                `ZE-Koordinaten unscharf (Ost)`, `ZE-Koordinaten unscharf (Nord)`) %>%
  
  rename("Code" = `ZE-Nr`, 
         "Type" = `WKA-Typ`,
         "Power_L" = `Inst. Turbinenleistung`, 
         "East" = `ZE-Koordinaten unscharf (Ost)`,
         "North" = `ZE-Koordinaten unscharf (Nord)`) -> hys_l


hys_l <- st_as_sf(hys_l, coords = c("East", "North"), crs = 2056)



# catchment area of river station -----------------------------------------

# a map of swiss borders
map_ch <- ne_countries(country = "switzerland", returnclass = "sf", scale = "large")


# stations, point
DT = data.frame(
  a = c(2613389, 2486603, 2721260, 2816820),
  b = c(1267678, 1112335, 1117079, 1185921),
  Name = c("Basel, Rheinhalle", "Chancy, Aux Ripes", "Bellinzona", "Tarasp"))

DT = st_as_sf(DT, coords = c("a","b"),  crs = 2056,  remove = FALSE)

# stations, area
stations[stations$ID %in% c("2289", "2174", "2020", "2265"),] -> station_to_extract

st_transform(station_to_extract, crs = 4326) -> station_to_extract


# union of all basins
union_basins <- st_union(station_to_extract)





# ROR capacity per catchemnt area ----------------------------------------------------


# stations, area
stations[stations$ID %in% c("2289", "2174", "2020", "2265"),] -> catch

st_transform(catch, crs = 2056) -> catch


# join stations to catchment areas
hys_l %>% 
  st_intersection(catch) -> catch
 
# power by catchment area
catch %>% st_drop_geometry() %>% 
  group_by(ID, Ortsbezeic) %>% 
  summarise(ROR_capacity_2020 = sum(Power_L)) -> catch

# save
saveRDS(catch, "output/ROR_capacity_2020_by_catchemnt_area.rds")


# total precipitation data by catchment area of each station ------------------

# set path and filename
ncpath <- "data/ERA5_Land/"

# names of all nc files
ncfiles <- dir(ncpath) %>% str_subset("total_precipitation.nc")


# prepare empty for storing the time series
TP <- list()


# loop over all files
for (ff in ncfiles) {
  
  
  ncname <- ff
  ncfname <- paste0(ncpath, ncname)
  tp <- "tp" 
  
  
  # open a netCDF file
  ncin <- nc_open(ncfname)
  
  
  
  #extract the time
  t <- ncvar_get(ncin, "time")
  
  #time unit: hours since 1900-01-01
  ncatt_get(ncin,'time')
  
  #convert the hours into date + hour
  #as_datetime() function of the lubridate package needs seconds
  timestamp <- as_datetime(c(t*60*60), origin="1900-01-01")
  
  
  
  
  
  # get longitude and latitude
  lon <- ncvar_get(ncin,"longitude")
  nlon <- dim(lon)
  head(lon)
  lat <- ncvar_get(ncin,"latitude")
  nlat <- dim(lat)
  head(lat)
  
  
  # get temperature
  tp_array <- ncvar_get(ncin,tp)
  
  fillvalue <- ncatt_get(ncin,tp,"_FillValue")
  
  # replace netCDF fill values with NA's
  tp_array[tp_array==fillvalue$value] <- NA
  
  
  #close the conection with the ncdf file
  nc_close(ncin)
  
  
  
  # assign precipitation to each river basin --------------------------------
  
  
  # empty df ready for dates and tp
  tp_series <- data.frame(dt = rep(NA, length(timestamp)), 
                          tp_2289 = rep(NA, length(timestamp)), 
                          tp_2174 = rep(NA, length(timestamp)),
                          tp_2020 = rep(NA, length(timestamp)),
                          tp_2265 = rep(NA, length(timestamp)),
                          tp_all = rep(NA, length(timestamp)))
  
  
  # loop over all hors in the month
  for (tt in 1:length(timestamp)) {
    
    # transform the matrix into a raster
    r_tp <- raster(t(tp_array[,,tt]),
                   xmn=range(lon)[1], xmx=range(lon)[2],
                   ymn=range(lat)[1], ymx=range(lat)[2], 
                   crs=CRS("+init=EPSG:4326"))
    
    # extract mean tp by basin
    kelv <- exact_extract(r_tp,
                          station_to_extract, 
                          'mean',
                          append_cols = "ID",
                          progress = F) %>% na.omit()
    
    # extract mean tp for all
    all <- exact_extract(r_tp,
                          union_basins, 
                          'mean',
                          progress = F) %>% na.omit()
    
    
    tp_series$tp_2289[tt] <- kelv$mean[kelv$ID == "2289"] * 1000 # transform into mm
    tp_series$tp_2174[tt] <- kelv$mean[kelv$ID == "2174"] * 1000
    tp_series$tp_2020[tt] <- kelv$mean[kelv$ID == "2020"] * 1000
    tp_series$tp_2265[tt] <- kelv$mean[kelv$ID == "2265"] * 1000
    tp_series$tp_all[tt] <- all * 1000
    tp_series$dt[tt] <- timestamp[tt]
    
  }
  
  tp_series$dt <- as_datetime(c(t*60*60), origin="1900-01-01")
  
  
  TP <- rbind(TP, tp_series)
  
  print(ff)
  timestamp()
  
}



TP %>% arrange(dt) -> TP




############################################################
# as precipitation is measured as cumulative over each UTC day,
#I need to de-cumulate it


# 
TP %>% 
  
  # diff between cumulative precip and previous hour cumulative precip
  mutate(tp_diff_2289 = tp_2289 - lag(tp_2289),
         tp_diff_2174 = tp_2174 - lag(tp_2174),
         tp_diff_2020 = tp_2020 - lag(tp_2020),
         tp_diff_2265 = tp_2265 - lag(tp_2265),
         tp_diff_all = tp_all - lag(tp_all)) -> TP

# make 1 am value equal to itself
TP$tp_diff_2289[hour(TP$dt) == 1] <- TP$tp_2289[hour(TP$dt) == 1]
TP$tp_diff_2174[hour(TP$dt) == 1] <- TP$tp_2174[hour(TP$dt) == 1]
TP$tp_diff_2020[hour(TP$dt) == 1] <- TP$tp_2020[hour(TP$dt) == 1]
TP$tp_diff_2265[hour(TP$dt) == 1] <- TP$tp_2265[hour(TP$dt) == 1]
TP$tp_diff_all[hour(TP$dt) == 1] <- TP$tp_all[hour(TP$dt) == 1]




############################################################

# SAVE
saveRDS(TP, "output/tp_river_stations_2016_2023.rds")
