
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
  mutate(posted_speed_limit = ifelse(posted_speed_limit <= 20, "[0, 20]",
                              ifelse(posted_speed_limit <= 40, "(20, 40]",
                              ifelse(posted_speed_limit <= 60, "(40, 60]", "(60, 80]")))) %>%
  mutate(posted_speed_limit = factor(posted_speed_limit,  
                              levels = c("[0, 20]", "(20, 40]", "(40, 60]", "(60, 80]"))) %>%
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

# Limpa todos menos os importantes
rm(list=setdiff(ls(),c('crash', 'chicago_city', 'chicago_neigh', 'chi_basemap')))

# Salva os dados
save.image("crashes.RData")



