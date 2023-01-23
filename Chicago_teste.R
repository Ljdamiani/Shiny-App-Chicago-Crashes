
# Testando Banco de Dados
library(tidyverse)
library(ggplot2)
library(ggmap)
library(osmdata)
library(lubridate)
library(RSocrata)
library(leaflet)
library(leaflet.extras)
library(sf)

# Últimos dois anos
years_ago <- today() - years(1)

# Data
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

# Selecionando as colunas - SOMENTE LESOES
crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "injuries", "none"),
    crash_date,
    crash_day_of_week,
    crash_hour,
    report_type = if_else(report_type == "", "UNKNOWN", report_type),
    num_units,
    posted_speed_limit,
    damage,
    weather_condition,
    lighting_condition,
    roadway_surface_cond,
    first_crash_type,
    trafficway_type,
    prim_contributory_cause,
    street_name,
    latitude, longitude
  ) %>%
  na.omit() %>% # sem NAs
  filter(injuries == "injuries")

# Total de acidentes
nrow(crash)

# Imagem de chicago_neigh #c(lon = -87.66161, lat = 41.84741)
chi_basemap <- get_map(location=getbb("chicago"), source = "stamen", zoom = 11)
#ggmap(chi_basemap)

# Shape File chicago_neigh e city
chicago_neigh <- st_read(dsn = 'shapefile/chicago_neigh.shp', quiet = T)
chicago_city <- st_read(dsn = 'shapefile/chicago_city.shp', quiet = T)

# Point
crash <- crash %>%
  filter(latitude > 0) %>%
  mutate(Long = as.numeric(as.character(longitude)),
         Lat = as.numeric(as.character(latitude))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(chicago_neigh))

# Acidentes por bairro
chicago_neigh$crash = 0
for (i in 1:nrow(crash)){
  
  # Coordenadas
  long = crash$longitude[i]
  lat = crash$latitude[i]
  
  # Verificando qual bairro o ponto pertence
  for (k in 1:98){
    
    # Coordenadas do bairro
    neigh_coords = st_bbox(chicago_neigh$geometry[k])
    
    if (neigh_coords["xmin"] < long && long < neigh_coords["xmax"] &&
        neigh_coords["ymin"] < lat && lat < neigh_coords["ymax"]){
      
      chicago_neigh$crash[k] = chicago_neigh$crash[k] + 1
      break
      
    }
  }
}

###########  Month

# Month 1

crash %>%
  filter(latitude > 0 & month(crash_date) == 1 & year(crash_date) == 2022) %>%
  mutate(Long = as.numeric(as.character(longitude)),
         Lat = as.numeric(as.character(latitude))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(chicago_neigh)) %>%
  ggplot() +
  geom_sf(data = chicago_city, fill = "lightgreen") +
  geom_sf(size = 0.7, color = "deeppink4") +
  ggtitle("Month 1") + 
  theme_minimal() 

month1 <- crash %>%
  filter(latitude > 0 & month(crash_date) == 1 & year(crash_date) == 2022)

ggmap(chi_basemap) +
  geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
  geom_sf(data = month1$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326))

###########  Days of Week

# Day 1
crash %>%
  filter(latitude > 0 & crash_day_of_week == 1) %>%
  mutate(Long = as.numeric(as.character(longitude)),
         Lat = as.numeric(as.character(latitude))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(chicago_neigh)) %>%
  ggplot() +
  geom_sf(data = chicago_neigh, fill = "lightgreen") +
  geom_sf(size = 0.7, color = "deeppink4") +
  ggtitle("Day 1") + 
  theme_minimal() 


########### Hour

# Hour 22
crash %>%
  filter(latitude > 0 & crash_hour == 22) %>%
  mutate(Long = as.numeric(as.character(longitude)),
         Lat = as.numeric(as.character(latitude))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(chicago_neigh)) %>%
  ggplot() +
  geom_sf(data = chicago_neigh, fill = "lightgreen") +
  geom_sf(size = 0.7, color = "deeppink4") +
  ggtitle("Hour 22:00") + 
  theme_minimal() 

  
############ Estratificações

# Gráficos

crash_street <- crash %>%
  filter(latitude > 0, street_name == "ASHLAND AVE")

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
  addCircleMarkers(lng = crash_street$longitude, 
                   lat = crash_street$latitude,
                   popup = crash_street$injuries, 
                   weight = 2, radius = 2, color = "purple") 

########## Por Bairro

bins <- c(0, 50, 100, 200, 300, Inf)
pal <- colorBin("YlOrRd", domain = chicago_neigh$crash, bins = bins)

leaflet(chicago_neigh) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
  addPolygons(fillColor = ~pal(crash), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(weight = 5,
                                           color = "yellow",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = sprintf("%s - Crashes: %s", chicago_neigh$pri_neigh, chicago_neigh$crash),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>%
  addLegend(pal = pal, values = ~crash, opacity = 0.7, title = NULL,
            position = "bottomright")

