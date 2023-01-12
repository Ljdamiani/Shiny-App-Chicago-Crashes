
# Testando Banco de Dados

library(tidyverse)
library(lubridate)
library(RSocrata)
library(leaflet)
library(leaflet.extras)
library(sf)

# Últimos dois anos
years_ago <- today() - years(4)

# Data
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

# Mudando as colunas
crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "injuries", "none"),
    crash_date,
    crash_hour,
    report_type = if_else(report_type == "", "UNKNOWN", report_type),
    num_units,
    posted_speed_limit,
    weather_condition,
    lighting_condition,
    roadway_surface_cond,
    first_crash_type,
    trafficway_type,
    prim_contributory_cause,
    latitude, longitude
  ) %>%
  na.omit() # sem NAs


# Acidentes com lesões e sem lesões -- GGplot

crash %>%
  filter(latitude > 0) %>%
  ggplot(aes(longitude, latitude, color = injuries)) +
  geom_point(size = 0.5, alpha = 0.4) +
  labs(color = NULL) +
  scale_color_manual(values = c("deeppink4", "gray80")) +
  coord_fixed()

# Marcador
marker_icon <- makeIcon(
  iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-icon.png",
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-shadow.png",
)


# Gráficos
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
  addMarkers(lng = crash$longitude, 
             lat = crash$latitude, 
             icon = marker_icon,
             popup = crash$injuries)


chicago <- st_read(dsn = 'shapefile/chicago_neigh.shp', quiet = T)

rio <- st_read(dsn = 'shapefile/rio_sf.shp', quiet = T)
bins <- c(0, 1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = rio$SMR, bins = bins)

leaflet(rio) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal(SMR), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = sprintf("%s - SMR: %s", rio$Name, round(rio$SMR, 3)),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.7, title = NULL,
            position = "bottomright")

