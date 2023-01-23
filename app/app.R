
###################################################
#
##### MAT02040 - ESTATÍSTICA ESPACIAL #############
# 
##### App em Shiny para Acidentes com Lesões ######
#
# Alunos: Giordano, João, Kevin, Leonardo.
#
###################################################

# https://mastering-shiny.org/basic-intro.html
# https://engineering-shiny.org/index.html

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


dados <- mtcars
dados <- tibble::rownames_to_column(dados, "Carros")

qta = c("mpg", "disp", "hp", "drat", "wt", "qsec")

dados.n <- dados[names(dados) %in% qta]
dados.q <- dados[!names(dados) %in% qta]

subq = names(dados.q)
dados.q[subq] = lapply(dados.q[subq], as.factor)
dados <- as.data.frame(cbind(dados.n, dados.q))
str(dados)

#ui

ui <- dashboardPage(
  skin = "black",
  header <- dashboardHeader(title= "Curso Shiny",
                            dropdownMenu(type = "messages",
                                         messageItem(
                                           from = "Aluno",
                                           message = "Aplicativo para o curso Shiny"
                                         ))
  ),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Interface", tabName = "Interface"),
      menuItem("Tipos", tabName = "Tipos", icon = icon("calculator"),
               menuSubItem("Graficos", tabName = "graficos"),
               menuSubItem("Tabela", tabName = "Tabela"))
    )
  ),
  body <- dashboardBody(
    tabItems(
      tabItem("Interface",
              fluidPage(
                titlePanel("Aplicativo para o curso Shiny"),
                fluidRow(
                  p("Nesse aplicativo, vamos apresentar um pouco sobre shiny.", style = "font-size:20px")
                )
              )
      ),
      tabItem("graficos",
              fluidPage(
                titlePanel("Apresentacao de graficos no Shiny"),
                fluidRow(
                  p("Abaixo sera mostrado alguns graficos no shiny.", style = "font-size:20px"),
                  box(plotOutput("grafico"), width = 12),
                  
                  p("Abaixo, sera mostrado um exemplo de grafico reativo", style = "font-size:20px"),
                  sidebarPanel(
                    h3("Filtros"),
                    selectizeInput("Carros",
                                   label = "Escolha os carros",
                                   choices = levels(dados$Carros),
                                   multiple = T,
                                   selected = levels(dados$Carros)),
                    selectizeInput("Quanti",
                                   label = "Escolha as variaveis numericas",
                                   choices = names(dados.n),
                                   multiple = FALSE,
                                   selected = names(dados.n)[1]),
                    selectizeInput("Cate",
                                   label = "Escolha as variaveis categoricas",
                                   choices = names(dados.q)[2:6],
                                   multiple = FALSE,
                                   selected = names(dados.q)[2]),
                    width = 12
                  ),
                  p("Abaixo, sera mostrado um grafico de boxplot", style = "font-size:20px"),
                  box(plotOutput("boxplot"), width = 12)
                )
              )
      ),
      tabItem("Tabela",
              fluidPage(
                titlePanel("Apresentacao de uma tabela no Shiny"),
                fluidRow(
                  p("Abaixo, sera mostrado um exemplo de tabela reativa", style = "font-size:20px"),
                  sidebarPanel(
                    h3("Filtros"),
                    selectizeInput("Carros_tabela",
                                   label = "Escolha os carros",
                                   choices = levels(dados$Carros),
                                   multiple = F,
                                   selected = levels(dados$Carros)[1]),
                    width = 12
                  ),
                  box(dataTableOutput("tabela"), width = 12)
                )
              )
      )
    )
    
  )
)


dashboardPage(header, sidebar, body)


# Server

server <- function(input, output) {
  
  output$grafico <- renderPlot({
    
    ggplot(dados) +
      geom_bar(aes(cyl), fill = "black")+
      labs(title = "Grafico das cilindradas",
           subtitle = "Utilizando o banco mtcars",
           x = "Cilindradas",
           y = "Contagem")
    
  })
  
  output$boxplot <- renderPlot({
    
    dados_filtrados1 <- dados %>%
      filter(Carros %in% input$Carros) %>%
      select(Carros, input$Quanti)
    
    dados_filtrados2 <- dados %>%
      filter(Carros %in% input$Carros) %>%
      select(Carros, input$Cate)
    
    dados_filtrados <- full_join(dados_filtrados1, dados_filtrados2, by = "Carros")
    dados_filtrados <- dados_filtrados[, c(2,3)]
    
    ggplot(dados_filtrados) + 
      geom_boxplot(aes(x = dados_filtrados[, 2], y = dados_filtrados[,1], groups = dados_filtrados[,2]), fill = "orange") +
      labs(title = "Boxplot",
           subtitle = "Utilizando o banco mtcars",
           x = input$Cate,
           y = input$Quanti)
    
  })
  
  output$tabela <- renderDataTable({
    dados_carros <- dados %>%
      filter(Carros %in% input$Carros_tabela)
    
    datatable(dados_carros)
  })
}

# App
shinyApp(ui, server)
