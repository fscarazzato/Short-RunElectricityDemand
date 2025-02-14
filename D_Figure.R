
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


# MAP OF river stations
# data from https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.hydrologie-hydromessstationen/items/hydrologie-hydromessstationen?.asset=asset-hydrologie-hydromessstationen_2056.shp.zip
stations <- st_read("data/hydrologie-hydromessstationen_2056.shp")


# MAP OF RIVER BASINS
# https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.wasser-einzugsgebietsgliederung/items/wasser-einzugsgebietsgliederung
fc <- st_read("data/wasser-einzugsgebietsgliederung_2056.gdb/EZG_Flussgebiete.gdb")

# RIVERS AND LAKES
# http://data.geo.admin.ch/ch.bafu.hydrologie-hintergrundkarte/data.zip
lakes <- st_read("data/rivers and lakes/See_500.shp")


# rivers can be added
# data needs to be downloaded from:
# https://doi.org/10.2909/393359a7-7ebd-4a52-80ac-1a18d5f3db9c

rhine <- st_read("data/rivers and lakes/euhydro_rhine_v013_FGDB/euhydro_rhine_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

rhone <- st_read("data/rivers and lakes/euhydro_rhone_v013_FGDB/euhydro_rhone_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

danube <- st_read("data/rivers and lakes/euhydro_danube_v013_FGDB/euhydro_danube_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

po <- st_read("data/rivers and lakes/euhydro_po_v013_FGDB/euhydro_po_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

# set crs to CH1903 / LV03 
st_crs(lakes) <- 21781

# hydropower stations data as of 1.1.2020
hys <- read_excel("data/10085-Statistik der Wasserkraftanlagen der Schweiz. Stand 1.1.2020/2019  Statistik der Wasserkraftanlagen der Schweiz 1.1.2020.xlsx")


# map of run of river stations
hys %>% subset(`WKA-Typ` == "L") %>%
  
  dplyr::select(`ZE-Nr`, `Inst. Turbinenleistung`, `WKA-Typ`,
                `ZE-Koordinaten unscharf (Ost)`, `ZE-Koordinaten unscharf (Nord)`) %>%
  
  rename("Code" = `ZE-Nr`, 
         "Type" = `WKA-Typ`,
         "Power_L" = `Inst. Turbinenleistung`, 
         "East" = `ZE-Koordinaten unscharf (Ost)`,
         "North" = `ZE-Koordinaten unscharf (Nord)`) -> hys_l

# NOT run of river stations
hys %>% subset(`WKA-Typ` != "L") %>%
  
  dplyr::select(`ZE-Nr`, `Inst. Turbinenleistung`,`WKA-Typ`,
                `ZE-Koordinaten unscharf (Ost)`, `ZE-Koordinaten unscharf (Nord)`) %>%
  
  rename("Code" = `ZE-Nr`, 
         "Type" = `WKA-Typ`,
         "Power_N" = `Inst. Turbinenleistung`,
         "East" = `ZE-Koordinaten unscharf (Ost)`,
         "North" = `ZE-Koordinaten unscharf (Nord)`) -> hys_n



hys_l <- st_as_sf(hys_l, coords = c("East", "North"), crs = 2056)
hys_n <- st_as_sf(hys_n, coords = c("East", "North"), crs = 2056)

# download a map of swiss borders
map_ch <- ne_countries(country = "switzerland", returnclass = "sf", scale = "large")


# stations, area
stations[stations$ID %in% c("2289"),] -> basel
stations[stations$ID %in% c("2289", "2174", "2020", "2265"),] -> station_to_extract

st_transform(basel, crs = 2056) -> basel
st_transform(station_to_extract, crs = 2056) -> station_to_extract


# union of all basins
union_basins <- st_union(station_to_extract)


# % of the area of the rhine catchment area
st_area(basel) / st_area(union_basins) 

# % of ror plants in the  cathment area
st_contains( union_basins, hys_l) -> included



hys_l$included <- 0
hys_l$included[included[[1]]] <- 1

sum(hys_l$Power_L[hys_l$included == 1]) / sum(hys_l$Power_L) 



# stations, point
DT = data.frame(
  a = c(2613389, 2486603, 2721260, 2816820),
  b = c(1267678, 1112335, 1117079, 1185921),
  Name = c("Basel, Rheinhalle", "Chancy, Aux Ripes", "Bellinzona", "Tarasp"))

DT = st_as_sf(DT, coords = c("a","b"),  crs = 2056,  remove = FALSE)




# make a map with the river basins and the run of river stations
extent(stations) -> ext
ext[2] <- 829839.7


col1 <- "#FDE725FF"
col2 <- "#7AD151FF"
col3 <- "#22A884FF"
col4 <- "#2A788EFF"
col5 <- "#440154FF"




tm_shape(stations[stations$ID %in% c("2289", "2174"),], bbox = ext) +
  tm_fill("Gewässern", palette = c(col2, col1), title = "",
          labels = c("Rhine", "Rhône"), alpha = 0.7) + 

  
  tm_shape(stations[stations$ID %in% c("2265", "2020"),], bbox = ext) +
  tm_fill("Gewässern", palette = c(col3, col4), title = "",
          labels = c("Inn", "Ticino"), alpha = 0.7) + 
  
  #tm_shape(rhine) + tm_lines(col = "white", lwd = 0.5) +
  #tm_shape(rhone) + tm_lines(col = "white", lwd = 0.5) +
  #tm_shape(danube) + tm_lines(col = "white", lwd = 0.5) +
  #tm_shape(po) + tm_lines(col = "white", lwd = 0.5) +
  tm_shape(lakes) + tm_fill(col = "white") +
  
  tm_shape(map_ch) + tm_borders(lwd = 1.3, lty = 1, col = "black") +
  tm_shape(hys_l) +
  tm_bubbles(size = "Power_L", scale = 1, title.size = "",
             col = col5, border.col = col5, 
             border.lwd = 0.5, alpha = 0.25, border.alpha = 1,
             sizes.legend=c(1.5,15,150), 
             sizes.legend.labels=c("1.5 MW","15 MW","150 MW")) +
  
  tm_layout(legend.outside = T,
            legend.outside.position = c("top"),
            legend.stack = "horizontal",
            legend.outside.size = 0.2,
            legend.text.size = 0.4,
            frame = F) +
  
  # add s1 as point on the map
  tm_shape(DT) + tm_symbols(size = 0.25, col = "red", shape = 4, border.lwd = 2) 

