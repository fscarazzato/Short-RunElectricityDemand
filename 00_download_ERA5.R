library(foreach)
library(ecmwfr)


# API key for the Copernicus Climate Data Store (CDS)
wf_set_key(user ="user_id_here", key = "API_key_here", service = "cds")



# destination folder
ph <- "data/ERA5_Land" 
# variable (one at a time)
#vr <- c("2m_temperature")
#vr <- c("total_precipitation")
# years
yr <- c(2016:2023)
# months
mn <- c(12)
# days
dy <- c(sprintf("0%d",1:9), 10:31)
# time
tm <- c(paste0("0",0:9,":00"),paste0(10:23,":00"))


# request looping over years and months
for (i in 1:length(yr)) {
  for (j in 1:length(mn)) {
    request <- list(
      "dataset_short_name" = "reanalysis-era5-land",
      "product_type" = "reanalysis",
      "variable" = vr[1],
      "year" = yr[i],
      "month" = mn[j],
      "day" = dy,
      "time" = tm,
      "area" = "48/5.5/45.5/11", # Swiss coordinates N, W, S, E
      "format" = "netcdf",
      "target" = paste0("era5_",yr[i],"_",mn[j],"_", vr, ".nc")
    )
    
    file <- wf_request(
      user     = "user_id_here",   # user ID (for authentication)
      request  = request,  # the request
      transfer = TRUE,     # download the file
      path     = ph       # store data in current working directory
    )
    
  }
}
