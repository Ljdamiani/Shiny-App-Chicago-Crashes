
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

load(file = "crashes.RData")

# Últimos dois anos
#years_ago <- today() - years(2)

# Data
#crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
#crash_raw <- as_tibble(read.socrata(crash_url))

# Selecionando as colunas - SOMENTE LESOES
# crash <- crash_raw %>%
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
#    crash_type
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

# Data manipulation
crash <- crash %>%
  filter(year(crash_date) == 2022) %>%
  mutate(day_of_week = factor(crash_day_of_week,
                              labels = c("Domingo", "Segunda-feira",
                                         "Terca-feira", "Quarta-feira",
                                         "Quinta-feira", "Sexta-feira",
                                         "Sabado")))

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
              tabItem("Day",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme o Dia da Semana"), 
                        p("Abaixo podemos verificar os acidentes através do dia da semana e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosDay", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Day_escolhido",
                                               label = "Escolha o Dia da Semana:",
                                               choices = sort(unique(crash$day_of_week)))
                                 ), br(),
                                 box(title = "Acidentes Por Dia da Semana", solidHeader = T, status = "primary",
                                     plotOutput("chicago_day1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_day2", width = "100%", height = "600"), width = 12),
                          )
                        )
                      )
              ),
              tabItem("Hour",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme a Hora do Dia"), 
                        p("Abaixo podemos verificar os acidentes através da hora do dia e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosHour", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Hour_escolhida",
                                               label = "Escolha a hora do dia:",
                                               choices = sort(unique(crash$crash_hour)))
                                 ), br(),
                                 box(title = "Acidentes Por Hora do Dia", solidHeader = T, status = "primary",
                                     plotOutput("chicago_hour1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_hour2", width = "100%", height = "600"), width = 12),
                          )
                        )
                      )
              )
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
              tabItem("Speed",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme o limite de Velocidade da Estrada"), 
                        p("Abaixo podemos verificar os acidentes através do limite de velocidade e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosSpeed", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Speed_filter",
                                               label = "Escolha a velocidade limite:",
                                               choices = sort(unique(crash$posted_speed_limit)))
                                 ), br(),
                                 box(title = "Acidentes Por Velocidade Limite", solidHeader = T, status = "primary",
                                     plotOutput("chicago_speed1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_speed2", width = "100%", height = "600"), width = 12),
                          )
                        )
                      )
              ),
              tabItem("Damage",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme o valor dos danos estimados"), 
                        p("Abaixo podemos verificar os acidentes através do valor dos danos e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosDamage", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Damage_filter",
                                               label = "Selecione o valor dos danos estimados:",
                                               choices = sort(unique(crash$damage)))
                                 ), br(),
                                 box(title = "Acidentes Por Valor dos Danos", solidHeader = T, status = "primary",
                                     plotOutput("chicago_damage1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_damage2", width = "100%", height = "600"), width = 12),
                          )
                        )
                      )
              ),
              tabItem("Light",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme Condição de Luz no local"), 
                        p("Abaixo podemos verificar os acidentes através da condição de luz no momento e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosLight", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Light_filter",
                                               label = "Selecione a condição de luz no local:",
                                               choices = sort(unique(crash$lighting_condition)))
                                 ), br(),
                                 box(title = "Acidentes Por Condição de Luz", solidHeader = T, status = "primary",
                                     plotOutput("chicago_light1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_light2", width = "100%", height = "600"), width = 12),
                          )
                        )
                      )
              ),
              tabItem("Type",
                      fluidPage(
                        titlePanel("Variação dos Acidentes Conforme Tipo de Primeira Colisão"), 
                        p("Abaixo podemos verificar os acidentes através do tipo de primeira colisão e do mapa dos locais.",
                          style = "font-size:20px"), br(), br(),
                        fluidRow(
                          column(4,
                                 box(
                                   title = "Filtros", solidHeader = T, status = "primary", width = 12, 
                                   "Selecione os filtros que serão aplicados.", br(),
                                   selectInput("FeridosType", "Acidentes com Feridos?", choices = c("Sim", "Não")),
                                   selectInput("Type_filter",
                                               label = "Selecione o tipo da primeira colisão:",
                                               choices = sort(unique(crash$first_crash_type)))
                                 ), br(),
                                 box(title = "Acidentes Por Tipo de Primeira Colisão", solidHeader = T, status = "primary",
                                     plotOutput("chicago_type1", height = "300"), width = 12)
                          ),
                          column(8, 
                                 box(title = "Acidentes em Chicago", solidHeader = T, status = "primary",
                                     plotOutput("chicago_type2", width = "100%", height = "600"), width = 12),
                          )
                        )
                      )
              )
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
  
  ###### Day
  output$chicago_day1 <- renderPlot({
    
    if(input$FeridosDay == "Sim"){feridos_day = "injuries"}else{feridos_day="none"}
    
    # Meses
    day <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_day) %>%
      mutate(dia = day_of_week)
    
    # Hist
    ggplot(day, aes(x = sort(as.character(crash_day_of_week)))) + 
      geom_bar(fill = "tomato") +
      labs(x = "Dia da semana", y = "Acidentes") + 
      theme_minimal()
    
  })
  
  output$chicago_day2 <- renderPlot({
    
    if(input$FeridosDay == "Sim"){feridos_day = "injuries"}else{feridos_day="none"}
    dia <- input$Day_escolhido
    
    # Dia Escolhido
    day <- crash %>%
      filter(latitude > 0 & 
               day_of_week == dia &
               injuries == feridos_day)
    
    # Mapa dos Acidentes com o Mês Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = day$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
  ###### Hour
  output$chicago_hour1 <- renderPlot({
    
    if(input$FeridosHour == "Sim"){feridos_hour = "injuries"}else{feridos_hour="none"}
    
    # Meses
    hour <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_hour) %>%
      mutate(hora = crash_hour)
    
    # Hist
    ggplot(hour, aes(x = (sort(hora)))) + 
      geom_bar(fill = "tomato") +
      scale_x_continuous(breaks = hour$hora) +
      labs(x = "Hora do dia", y = "Acidentes") +
      theme_minimal()
    
  })
  
  output$chicago_hour2 <- renderPlot({
    
    if(input$FeridosHour == "Sim"){feridos_hour = "injuries"}else{feridos_hour="none"}
    hora <- input$Hour_escolhida
    
    # Mês Escolhido
    hour <- crash %>%
      filter(latitude > 0 & 
               crash_hour == hora &
               injuries == feridos_hour)
    
    # Mapa dos Acidentes com o Mês Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = hour$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
  ###### Speed
  output$chicago_speed1 <- renderPlot({
    
    if(input$FeridosSpeed == "Sim"){feridos_speed = "injuries"}else{feridos_speed="none"}
    
    # Meses
    speed <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_speed) %>%
      mutate(velocidade = posted_speed_limit)
    
    # Hist
    ggplot(speed, aes(x = sort(velocidade))) + 
      geom_bar(fill = "tomato") +
      labs(x = "Velocidade Limite", y = "Acidentes") + 
      theme_minimal()
    
  })
  
  output$chicago_speed2 <- renderPlot({
    
    if(input$FeridosSpeed == "Sim"){feridos_speed = "injuries"}else{feridos_speed="none"}
    velocidade <- input$Speed_filter
    
    # Mês Escolhido
    speed <- crash %>%
      filter(latitude > 0 & 
               posted_speed_limit == velocidade &
               injuries == feridos_speed)
    
    # Mapa dos Acidentes com o Mês Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = speed$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
  ###### Damage
  output$chicago_damage1 <- renderPlot({
    
    if(input$FeridosDamage == "Sim"){feridos_damage = "injuries"}else{feridos_damage="none"}
    
    # Meses
    damage <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_damage) %>%
      mutate(danos = damage)
    
    # Hist
    ggplot(damage, aes(x = sort(danos))) + 
      geom_bar(fill = "tomato") +
      labs(x = "Valor dos Danos estimados", y = "Acidentes") + 
      theme_minimal()
    
  })
  
  output$chicago_damage2 <- renderPlot({
    
    if(input$FeridosDamage == "Sim"){feridos_damage = "injuries"}else{feridos_damage="none"}
    danos <- input$Damage_filter
    
    # Valor de dano Escolhido
    damage <- crash %>%
      filter(latitude > 0 & 
               damage == danos &
               injuries == feridos_damage)
    
    # Mapa dos Acidentes com o valor de danos Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = damage$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
  ###### Light
  output$chicago_light1 <- renderPlot({
    
    if(input$FeridosLight == "Sim"){feridos_light = "injuries"}else{feridos_light="none"}
    
    # Condicao de Luz
    light <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_light) %>%
      mutate(luz = lighting_condition)
    
    # Hist
    ggplot(light, aes(x = sort(luz))) + 
      geom_bar(fill = "tomato") +
      labs(x = "Condição de Luz no local", y = "Acidentes") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
    
  })
  
  output$chicago_light2 <- renderPlot({
    
    if(input$FeridosLight == "Sim"){feridos_light = "injuries"}else{feridos_light="none"}
    luz <- input$Light_filter
    
    # Condicao de luz escolhida
    light <- crash %>%
      filter(latitude > 0 & 
               lighting_condition == luz &
               injuries == feridos_light)
    
    # Mapa dos Acidentes com a condicao de luz Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = light$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
  
  ###### Type
  output$chicago_type1 <- renderPlot({
    
    if(input$FeridosType == "Sim"){feridos_type = "injuries"}else{feridos_type="none"}
    
    # Tipo de primeira colisao
    type <- crash %>%
      filter(latitude > 0 & 
               injuries == feridos_type) %>%
      mutate(tipo = first_crash_type)
    
    # Hist
    ggplot(type, aes(x = sort(tipo))) + 
      geom_bar(fill = "tomato") +
      labs(x = "Tipo de primeira colisão", y = "Acidentes") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
    
  })
  
  output$chicago_type2 <- renderPlot({
    
    if(input$FeridosType == "Sim"){feridos_type = "injuries"}else{feridos_type="none"}
    tipo <- input$Type_filter
    
    # Tipo de primeira colisao escolhida
    type <- crash %>%
      filter(latitude > 0 & 
               first_crash_type == tipo &
               injuries == feridos_type)
    
    # Mapa dos Acidentes com o tipo de primeira colisao Acima
    ggmap(chi_basemap) +
      geom_sf(data = chicago_city, color = "red", alpha = 0, inherit.aes = FALSE) +
      geom_sf(data = type$geometry, size = 1.5, color = "deeppink4", inherit.aes = FALSE) +
      coord_sf(crs = st_crs(4326))
    
  })
}

# App
shinyApp(ui, server)