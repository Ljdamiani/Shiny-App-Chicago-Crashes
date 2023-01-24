
### Chicago Dados

library(tidyverse)
library(ggplot2)
library(ggmap)
library(osmdata)
library(lubridate)
library(RSocrata)

# 2022
years_22 <- date("2021-12-31")

# Data
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_22}'")
crash_raw <- as_tibble(read.socrata(crash_url))

# Selecionando as colunas - SOMENTE LESOES
crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "injuries", "none"),
    crash_date = crash_date,
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
  filter(year(crash_date) == 2022) %>%
  mutate(day_of_week = factor(crash_day_of_week,
                              labels = c("Domingo", "Segunda-feira",
                                         "Terca-feira", "Quarta-feira",
                                         "Quinta-feira", "Sexta-feira",
                                         "Sabado"))) %>%
  na.omit() # sem NAs 

# Imagem de chicago_neigh #c(lon = -87.66161, lat = 41.84741)
chi_basemap <- get_map(location=getbb("chicago"), source = "stamen", zoom = 11)

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
crash$neigh = ""; chicago_neigh$crash = 0
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
      
      crash$neigh[i] = chicago_neigh$pri_neigh[k]
      chicago_neigh$crash[k] = chicago_neigh$crash[k] + 1
      
      break
      
    }
  }
}

# Limpa todos menos os importantes
rm(list=setdiff(ls(),c('crash', 'chicago_city', 'chicago_neigh', 'chi_basemap')))

# Salva os dados
save.image("crashes.RData")



