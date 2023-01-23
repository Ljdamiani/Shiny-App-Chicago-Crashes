
###################################################
#
##### MAT02040 - ESTATÍSTICA ESPACIAL #############
# 
##### App em Shiny para Acidentes com Lesões ######
#
# Alunos: Giordano, João, Kevin, Leonardo.
#
###################################################



###################### Packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(osmdata)
library(lubridate)
library(RSocrata)
library(leaflet)
library(leaflet.extras)
library(sf)



######################  Dados

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
  na.omit() %>% # sem NAs
  filter(injuries == "injuries")

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

####################### ui

ui <- dashboardPage(
  skin = "red",
  header <- dashboardHeader(title= "Injuries from Crashes in Chicago", # disable = T
                            titleWidth = 450),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("car")),
      menuItem("When?", tabName = "When?", icon = icon("calendar"),
               menuSubItem("Month", tabName = "Month"),
               menuSubItem("Day of Week", tabName = "Day"),
               menuSubItem("Hours", tabName = "Hour")),
      menuItem("Where?", tabName = "Where?", icon = icon("crosshairs"),
               menuSubItem("Neighbourhood", tabName = "Neighbourhood"),
               menuSubItem("Street", tabName = "Street")),
      menuItem("How?", tabName = "How?", icon = icon("exclamation"),
               menuSubItem("Speed Limit", tabName = "Speed"),
               menuSubItem("Damage", tabName = "Damage"),
               menuSubItem("Ligthing Condition", tabName = "Light"),
               menuSubItem("Crash Type", tabName = "Type"))
    )
  ),
  body <- dashboardBody(
    tabItem("Introduction",
              fluidPage(
                titlePanel("Acidentes de Trânsito em Chicago com Feridos no Último Ano"),
                fluidRow(
                  br(),
                  p("Nesse aplicativo, estamos procurando analisar os acidentes em Chicago de 
                    diversos ângulos:", style = "font-size:20px"),
                  tagList(p(strong("- Tempo"), style = "font-size:20px"),
                          p(strong("- Localização"), style = "font-size:20px"),
                          p(strong("- Forma"), style = "font-size:20px"))
                )
              )
    ),
    tabItem("When?",
              tabItems(
                tabItem("Month",
                        fluidPage(
                          titlePanel("Variação dos Acidentes Conforme os Meses"),
                          fluidRow(
                            p("Abaixo você pode selecionar o mês dentre os possíveis do último ano.", 
                              style = "font-size:20px"),
                            sidebarPanel(
                              h3("Filtro"),
                              selectInput("Month_escolhido",
                                          label = "Escolha o Mês:",
                                          choices = unique(str_sub(as.character(crash$crash_date), 1, 7)))
                            ),
                            box(plotOutput("chicago_month"), height = 10)
                          )
                        )
                ),
                tabItem("Day"),
                tabItem("Hour")
              )
    ),
    tabItem("Where?",
            tabItems(
              tabItem("Neighbourhood"),
              tabItem("Street"
            )
              )
    ),
    tabItem("How?", 
            tabItems(
              tabItem("Speed"),
              tabItem("Damage"),
              tabItem("Light"),
              tabItem("Type")
            )
    )
  )
)

# Dashboard
dashboardPage(header, sidebar, body)


####################### Server

server <- function(input, output) {
  
  ###### Month
  output$chicago_month <- renderPlot({
    
    mes <- month(as.Date(paste0(input$Month_escolhido, "-01")))
    ano <- year(as.Date(paste0(input$Month_escolhido, "-01")))
      
    # Mês Escolhido
    month <- crash %>%
      filter(latitude > 0 & 
               month(crash_date) == mes & 
               year(crash_date) == ano)
    
    # Mapa dos Acidentes com o Mês Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = month$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
}

# App
shinyApp(ui, server)
