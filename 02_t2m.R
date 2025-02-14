library(ncdf4)
library(chron)
library(lattice)
library(rasterVis)
library(raster)
library(RColorBrewer)
library(sf)
library(sp)
library(mapview)
library(lubridate)
library(tidyverse)
library(tmap)
library(exactextractr)

# swiss municipalities and their population in 2021 ---------------------------

# Geodata from the fedeal office of topography swisstopo
# https://www.swisstopo.admin.ch/en/landscape-model-swissboundaries3d

# load map and give GPS crs
map <- st_read("data/map switzerland shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
map <- st_transform(map, 4326)
map <- st_cast(map, "MULTIPOLYGON")




# ERA5-Land data ----------------------------------------------------------



# set path and filename
ncpath <- "data/ERA5_Land/"

# names of all nc files
ncfiles <- dir(ncpath) %>% str_subset("2m_temperature")


# prepare empty for storing the time series
T2M <- list()


# loop over all files
for (ff in ncfiles) {
  
  
  ncname <- ff
  ncfname <- paste0(ncpath, ncname)
  t2m <- "t2m"  # note: avg temperature 2 meters from the ground
  
  
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
  t2m_array <- ncvar_get(ncin,t2m)
  
  fillvalue <- ncatt_get(ncin,t2m,"_FillValue")
  
  # replace netCDF fill values with NA's
  t2m_array[t2m_array==fillvalue$value] <- NA
  
  
  #close the conection with the ncdf file
  nc_close(ncin)
  
  
  
  # assign temperature to each municipality ---------------------------------
  
  
  # empty df ready for dates and temperatures
  t2m_series <- data.frame(dt = rep(NA, length(timestamp)), wm_t2m = rep(NA, length(timestamp)))
  
  
  # loop over all hours in the month
  for (tt in 1:length(timestamp)) {
    
    # transform the matrix into a raster
    r_t2m <- raster(t(t2m_array[,,tt]),
                    xmn=range(lon)[1], xmx=range(lon)[2],
                    ymn=range(lat)[1], ymx=range(lat)[2], 
                    crs=CRS("+init=EPSG:4326"))
    
    # extract mean temperature by municipality
    kelv <- exact_extract(r_t2m,
                          map, 
                          'mean',
                          
                          # drop lines with no temperature or not population
                          append_cols = "EINWOHNERZ",
                          progress = F) %>% na.omit()
    
    # national temperature as a weighted mean by population
    # subtract 273.15 to get celsius degrees
    w_swiss_t2m <- weighted.mean(kelv$mean, w = kelv$EINWOHNERZ, na.rm = T) - 273.15 
    
    t2m_series$wm_t2m[tt] <- w_swiss_t2m
    t2m_series$dt[tt] <- timestamp[tt]
    

  }
  
  t2m_series$dt <- as_datetime(c(t*60*60), origin="1900-01-01")
  plot(t2m_series$wm_t2m ~ t2m_series$dt, type = "l", lwd = 2, col = "red")
  
  T2M <- rbind(T2M, t2m_series)
  
  print(ff)
  timestamp()
  
}

T2M %>% arrange(dt) -> T2M


plot(T2M$wm_t2m ~ T2M$dt, type = "l", lwd = 2, col = "red")



# SAVE
saveRDS(T2M, "output/02_t2m_ERA5_2016_2023_LONG.rds")


