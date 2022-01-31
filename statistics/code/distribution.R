# Loading packages (install if not yet installed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, informR, hrbrthemes, openxlsx, lubridate, RColorBrewer, knitr, rmarkdown, tidygeocoder, rgdal, grid, gridExtra)

# Set WD
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Projects/foodcoop_zurgertrud/")

# Reading files
members <- read_delim("members/members_export_b7206ee42e/subscribed_members_export_b7206ee42e.csv", delim = ",", locale = locale(encoding = "UTF-8"))

# Change Adress
members$id <- as.numeric(row.names(members))
members$Adresse[members$id == 96] <- "Carmenstrasse 10, 8032 Zürich"
members$Adresse[members$id == 80] <- "Manessestrasse 132, 8045 Zürich"

# Mitgliederverteilung
members$lon <- 0
members$lat <- 0

for(i in 1:nrow(members)) {
  result <- tryCatch(geo_cascade(members$Adresse[i]), error=function(e) e, warning=function(w) w)
  ifelse(nrow(result) != 0,
         members$lon[i] <- result$long,
         NA)
  ifelse(nrow(result) != 0,
         members$lat[i] <- result$lat,
         NA)
  print(i)
}

members$member[grepl("Mitglied", members$TAGS) == TRUE & 
                 grepl("Mitgliederbeitrag offen", members$TAGS) == FALSE] <- 1

# Changing projection
df <- subset(members, lon != "")
df <- subset(df, member == 1)
coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556
  +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000
  +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
df <- spTransform(df, CRS.new)
data <- as.data.frame(df)

shape <- readOGR(dsn = "/Users/laz/Library/Mobile Documents/com~apple~CloudDocs/Projects/foodcoop_zurgertrud/data/stzh.adm_stadtkreise_v_polygon.shp")
shapefile_df <- fortify(shape)

xmin = 678540.51
xmax = 687316.61-1000
ymin = 242423.73+2000
ymax = 251001.62-1000

mar = 1000

load("/Users/laz/Library/Mobile Documents/com~apple~CloudDocs/Projects/strava/strava-master/data/auxilliary_data/lakes_small.RData")
lakes_small_red <- subset(lakes_small, (long > xmin-mar) & (long < xmax+mar) & (lat > ymin-mar) & (lat < ymax+mar))
load("/Users/laz/Library/Mobile Documents/com~apple~CloudDocs/Projects/strava/strava-master/data/auxilliary_data/rivers.RData")
rivers_red <- subset(rivers, (long > xmin-mar) & (long < xmax+mar) & (lat > ymin-mar) & (lat < ymax+mar))
load("/Users/laz/Library/Mobile Documents/com~apple~CloudDocs/Projects/strava/strava-master/data/auxilliary_data/roads.RData")
road_red <- subset(roads, (long > xmin-mar) & (long < xmax+mar) & (lat > ymin-mar) & (lat < ymax+mar))
load("/Users/laz/Library/Mobile Documents/com~apple~CloudDocs/Projects/strava/strava-master/data/auxilliary_data/rail.RData")
rail_red <- subset(rail, (long > xmin-mar) & (long < xmax+mar) & (lat > ymin-mar) & (lat < ymax+mar))

coordinates(shapefile_df) <- c("long", "lat")
proj4string(shapefile_df) <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs") # WGS 84
shapefile_df <- spTransform(shapefile_df, CRS.new)
shapefile_df <- as.data.frame(shapefile_df)

trudi <- data.frame(x = 681304.14, y = 247529.53)

p <- ggplot() +
  geom_path(data = shapefile_df, aes(x = long, y = lat, group = group), color = 'black', size = 0.5) +
  geom_path(data = rivers_red, aes(x = long, y = lat, group = osm_id),
            alpha = 0.5, size = ifelse(rivers_red$type == "river" & rivers_red$osm_id != 27264395, 3, 1), color = ifelse(rivers_red$type == "river" & rivers_red$osm_id != 27264395, "skyblue2", "white"), lineend = "round", show.legend = FALSE) + 
  geom_polygon(data = lakes_small_red, aes(x = long, y = lat, group = osm_id),
               fill = 'skyblue2', alpha = 0.5, size = 0.025, show.legend = FALSE) + #color = 'skyblue2'
  geom_path(data = road_red, aes(x = long, y = lat, group = osm_id), color = 'black', alpha = 0.2, size = 0.3) +
  #geom_path(data = rail_red, aes(x = long, y = lat, group = osm_id), color = 'black', alpha = 0.4, size = 0.5) +
  geom_point(data = trudi, aes(x = x, y = y), size = 8, alpha = 1, shape = 8, color = "#F05F40") +
  geom_point(data = data, aes(x = lon, y = lat), size = 2, alpha = 0.6, color = "black") +
  coord_equal(xlim = c(round(xmin, 1), round(xmax, 1)), ylim = c(round(ymin, 1), round(ymax, 1))) +
  theme_void() +
  labs(x = NULL, 
       y = NULL)
p
