# Water stations in Switzerland
# data from https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.hydrologie-hydromessstationen/items/hydrologie-hydromessstationen?.asset=asset-hydrologie-hydromessstationen_2056.shp.zip




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
stations <- st_read("Demand estimation Switzerland - LONG/data/hydrologie-hydromessstationen_2056.shp")


# MAP OF RIVER BASINS
fc <- st_read("Demand estimation Switzerland - LONG/data/wasser-einzugsgebietsgliederung_2056.gdb/EZG_Flussgebiete.gdb")

# RIVERS AND LAKES

lakes <- st_read("Demand estimation Switzerland - LONG/data/rivers and lakes/See_500.shp")

rhine <- st_read("Demand estimation Switzerland - LONG/data/rivers and lakes/euhydro_rhine_v013_FGDB/euhydro_rhine_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

rhone <- st_read("Demand estimation Switzerland - LONG/data/rivers and lakes/euhydro_rhone_v013_FGDB/euhydro_rhone_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

danube <- st_read("Demand estimation Switzerland - LONG/data/rivers and lakes/euhydro_danube_v013_FGDB/euhydro_danube_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

po <- st_read("Demand estimation Switzerland - LONG/data/rivers and lakes/euhydro_po_v013_FGDB/euhydro_po_v013.gdb", layer = "River_Net_l") %>%
  # only rivers whose cumulative length is greater than 20 km
  subset(CUM_LEN > 20000)

# set crs to CH1903 / LV03 
st_crs(lakes) <- 21781

# hydropower stations data as of 1.1.2020
hys <- read_excel("Demand estimation Switzerland - LONG/data/10085-Statistik der Wasserkraftanlagen der Schweiz. Stand 1.1.2020/2019  Statistik der Wasserkraftanlagen der Schweiz 1.1.2020.xlsx")


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

# make a map with the river basins and the run of river stations
tm_shape(fc) + tm_fill("FLUSSGB", alpha = 0.5, title = "River Basin") +
  tm_shape(hys_l) +
  tm_bubbles(size = "Power_L", scale = 1, shape = 16, title.size = "Megawatt", col = "blue") +
  tm_layout(legend.outside = T, frame = F) +
  tm_shape(map_ch) + tm_borders(lwd = 2, lty = 2, col = "red") +
  tm_shape(stations[stations$ID == "2289",]) + tm_borders(lwd = 2, lty = 2, col = "green") + 
  tm_shape(stations[stations$ID == "2174",]) + tm_borders(lwd = 2, lty = 2, col = "gold")

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
  
  tm_shape(rhine) + tm_lines(col = "white", lwd = 0.5) +
  tm_shape(rhone) + tm_lines(col = "white", lwd = 0.5) +
  tm_shape(danube) + tm_lines(col = "white", lwd = 0.5) +
  tm_shape(po) + tm_lines(col = "white", lwd = 0.5) +
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


################################################################################

# Discharge data for the 4 stations
# skip the first 3 rows
read.csv("Demand estimation Switzerland - LONG/data/raw data/2174_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2174

s2174$dt <- ymd_hms(s2174$Zeitstempel)

read.csv("Demand estimation Switzerland - LONG/data/raw data/2289_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2289

s2289$dt <- ymd_hms(s2289$Zeitstempel)

read.csv("Demand estimation Switzerland - LONG/data/raw data/2020_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2020

s2020$dt <- ymd_hms(s2020$Zeitstempel)


read.csv("Demand estimation Switzerland - LONG/data/raw data/2265_Abfluss_Stundenmittel_2016-01-01_2023-12-31.csv",
         skip = 8, header = T, sep = ";", encoding = "latin1") -> s2265

s2265$dt <- ymd_hms(s2265$Zeitstempel)


plot(s2289$dt, s2289$Wert, 
     type = "l",
     col = "steelblue",
     xlab = "Date", ylab = "Discharge (m3/s)", ylim = c(0, 3500))
lines(s2174$dt, s2174$Wert, col = "red4")
lines(s2020$dt, s2020$Wert, col = "green4")
lines(s2265$dt, s2265$Wert, col = "orange")

################################################################################

# Precipitation data
TP <- readRDS("Demand estimation Switzerland - LONG/output/06c_weekly_sum_2016_2023.rds")


# merge data
s2174 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2174" = Wert) -> s2174
s2289 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2289" = Wert) -> s2289
s2020 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2020" = Wert) -> s2020
s2265 %>% dplyr::select(dt, Wert) %>% rename("Discharge_2265" = Wert) -> s2265

TP <- TP %>% left_join(s2174, by = "dt") %>%
  left_join(s2289, by = "dt") %>%
  left_join(s2020, by = "dt") %>%
  left_join(s2265, by = "dt")

#############################################################################
# correlation between discharge and rolling_precip

# rhine
lm(Discharge_2289 ~ rolling_tp_2289_week +
     rolling_tp_2289_week_lag1 +
     rolling_tp_2289_week_lag2 +
     rolling_tp_2289_week_lag3 +
     rolling_tp_2289_week_lag4 +
     rolling_tp_2289_week_lag5 +
     rolling_tp_2289_week_lag6,
   data = TP) -> m2289

summary(m2289)

# Durbin-Watson test for autocorrelation
dwtest(m2289)



# HAC robust standard errors
NeweyWest(m2289, 
          prewhite = F, 
          adjust = T) -> nw2289

coeftest(m2289, vcov=nw2289) -> c2289


# rhone
lm(Discharge_2174 ~ rolling_tp_2174_week +
     rolling_tp_2174_week_lag1 +
     rolling_tp_2174_week_lag2 +
     rolling_tp_2174_week_lag3 +
     rolling_tp_2174_week_lag4 +
     rolling_tp_2174_week_lag5 +
     rolling_tp_2174_week_lag6,
   data = TP) -> m2174

summary(m2174)

# Durbin-Watson test for autocorrelation
dwtest(m2174)

# HAC robust standard errors
NeweyWest(m2174, 
          prewhite = F, 
          adjust = T) -> nw2174

coeftest(m2174, vcov=nw2174) -> c2174



# inn
lm(Discharge_2265 ~ rolling_tp_2265_week +
     rolling_tp_2265_week_lag1 +
     rolling_tp_2265_week_lag2 +
     rolling_tp_2265_week_lag3 +
     rolling_tp_2265_week_lag4 +
     rolling_tp_2265_week_lag5 +
     rolling_tp_2265_week_lag6,
   data = TP) -> m2265

summary(m2265)

# Durbin-Watson test for autocorrelation
dwtest(m2265)

# HAC robust standard errors
NeweyWest(m2265, 
          prewhite = F, 
          adjust = T) -> nw2265

coeftest(m2265, vcov=nw2265) -> c2265


# ticino
lm(Discharge_2020 ~ rolling_tp_2020_week +
     rolling_tp_2020_week_lag1 +
     rolling_tp_2020_week_lag2 +
     rolling_tp_2020_week_lag3 +
     rolling_tp_2020_week_lag4 +
     rolling_tp_2020_week_lag5 +
     rolling_tp_2020_week_lag6,
   data = TP) -> m2020

summary(m2020)

# Durbin-Watson test for autocorrelation
dwtest(m2020)

# HAC robust standard errors
NeweyWest(m2020, 
          prewhite = F, 
          adjust = T) -> nw2020

coeftest(m2020, vcov=nw2020) -> c2020

rownames(c2289) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")
rownames(c2174) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")
rownames(c2265) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")
rownames(c2020) <- c("(Intercept)", "a", "b", "c", "d", "e", "f", "g")

# stargazer table# as.character()stargazer table
stargazer(c2289, c2174, c2265, c2020, header = F, digits = 3,
          
          dep.var.labels = "Discharge",

          covariate.labels = c("Weekly Precipitations$_{w}$",
                               "Weekly Precipitations$_{w-1}$",
                               "Weekly Precipitations$_{w-2}$",
                               "Weekly Precipitations$_{w-3}$",
                               "Weekly Precipitations$_{w-4}$",
                               "Weekly Precipitations$_{w-5}$",
                               "Weekly Precipitations$_{w-6}$"),
          add.lines = list(c("Adjusted R$^{2}$",
                             round(summary(m2289)$adj.r.squared,2),
                             round(summary(m2174)$adj.r.squared,2),
                             round(summary(m2265)$adj.r.squared,2),
                             round(summary(m2020)$adj.r.squared,2)),          
                           c("Observations",
                             prettyNum(nobs(m2289), big.mark=","),
                             prettyNum(nobs(m2174), big.mark=","),
                             prettyNum(nobs(m2265), big.mark=","),
                             prettyNum(nobs(m2020), big.mark=","))),
          notes = c("HAC standard errors.",
                    "$^{***}$Significant at the 1 percent level.",
                    "$^{**}$Significant at the 5 percent level.",
                    "$^{*}$Significant at the 10 percent level."),
          notes.append = F,
          
          
          table.layout = "=d#-t-as=n",
          
          style = "qje")
