library(tidyverse)
library(highcharter)
library(shiny)
library(openxlsx)
library(zoo)
library(knitr)
library(kableExtra)
library(leaflet)
library(tablerDash)


source("funcoes.R", encoding = "UTF-8")


# Tratando os dados -------



--------------------------------------------------------------------------------
  
ui <- tags$html(
  
  tags$body(
    
    tablerDashPage(
      body = tablerDashBody(
        tablerTabItems(
          tablerTabItem(
            tabName = "home",
            leafletOutput('plot_mapa_regiao_global', height = "400px")
          )
        )
      )
    )
    
  )
  
)


server <- function(input, output, session) {
  
  output$plot_mapa_regiao_global <- renderLeaflet({
    
    
    
  })
  
}

shinyApp(ui, server)
