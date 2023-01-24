
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

load(file = "crashes")

# Últimos dois anos
#years_ago <- today() - years(1)

# Data
#crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
#crash_raw <- as_tibble(read.socrata(crash_url))

# Selecionando as colunas - SOMENTE LESOES
#crash <- crash_raw %>%
#  arrange(desc(crash_date)) %>%
#  transmute(
#    injuries = if_else(injuries_total > 0, "injuries", "none"),
#    crash_date = crash_date,
#    crash_day_of_week,
#    crash_hour,
#    report_type = if_else(report_type == "", "UNKNOWN", report_type),
#    num_units,
#    posted_speed_limit,
#    damage,
#    weather_condition,
#    lighting_condition,
#    roadway_surface_cond,
#    first_crash_type,
#    trafficway_type,
#    prim_contributory_cause,
#    street_name,
#    latitude, longitude
#  ) %>%
#  na.omit() # sem NAs

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


####################### ui

ui <- dashboardPage(
  skin = "red",
  header <- dashboardHeader(title= "Crashes in Chicago", # disable = T
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
              titlePanel("Acidentes de Trânsito em Chicago no Último Ano"),
              fluidRow(
                br(),
                p("Nesse aplicativo, estamos procurando analisar os acidentes em Chicago de 
                    diversos ângulos:", style = "font-size:20px"), br(),
                tagList(tags$ul(tags$li(strong("Tempo (When?)"), style = "font-size:20px")),
                        tags$ul(tags$li(strong("Localização (Where?)"), style = "font-size:20px")),
                        tags$ul(tags$li(strong("Forma (How?)"), style = "font-size:20px")))
              )
            )
    ),
    tabItem("When?",
            tabItems(
              tabItem("Month",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme os Meses"), 
                        p("Abaixo podemos verificar os acidentes através dos meses e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosMonth", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Month_escolhido",
                                               label = "Escolha o Mês:",
                                               choices = unique(str_sub(as.character(crash$crash_date), 1, 7)))
                                 ), br(),
                                 box(title = "Acidentes Por Mês", solidHeader = T, status = "primary",
                                     plotOutput("chicago_month1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_month2", width = "100%", height = "600"), width = 12),
                          )
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
  output$chicago_month1 <- renderPlot({
    
    if(input$FeridosMonth == "Sim"){feridos_month = "injuries"}else{feridos_month="none"}
    
    # Meses
    month <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_month) %>%
      mutate(mes = month(crash_date, label = T, abbr = T))
    
    # Hist
    ggplot(month, aes(x = sort(mes))) + 
      geom_bar(fill = "tomato") +
      labs(x = "Meses", y = "Acidentes") + 
      theme_minimal()
    
  })
  
  output$chicago_month2 <- renderPlot({
    
    if(input$FeridosMonth == "Sim"){feridos_month = "injuries"}else{feridos_month="none"}
    mes <- month(as.Date(paste0(input$Month_escolhido, "-01")))
    ano <- year(as.Date(paste0(input$Month_escolhido, "-01")))
    
    # Mês Escolhido
    month <- crash %>%
      filter(latitude > 0 & 
               month(crash_date) == mes & 
               year(crash_date) == ano &
               injuries == feridos_month)
    
    # Mapa dos Acidentes com o Mês Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = month$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
}

# App
shinyApp(ui, server)