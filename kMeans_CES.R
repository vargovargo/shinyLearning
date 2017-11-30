rm(list = ls())

library(tidyverse)
library(shiny)

setwd("~/CapAndTradeProceeds/CalfireUrbanForestry/")



CES2 <- read.csv("CES3_tracts.csv", header = T) %>%
  mutate(tractNumber = Census.Tract) %>%
  select(tractNumber, Total.Population, CES.3.0.Percentile)

HPI2 <- read.csv("HDI1_tracts.csv", header = T) %>%
  mutate(tractNumber = CensusTract) %>%
  select(tractNumber, pop2010, hdi_pctile, ces2_pctile)


full2 <- full_join(CES2, HPI2)  %>%
  select(CES.3.0.Percentile, hdi_pctile, ces2_pctile) %>% na.omit()

names(full2) <- c("CES 3.0","HDI 1.1","CES 2.0")

ui <- pageWithSidebar(
  headerPanel('Plot Options'),
  sidebarPanel(
    selectInput(
      'xcol',
      'X Variable',
      names(full2)),
      selectInput('ycol', 'Y Variable', names(full2),
                  selected = names(full2)[[2]]),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9)
    ),
    mainPanel(plotOutput('plot1'))
  )
  
  server <- function(input, output, session) {
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
      full2[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
      palette(
        c(
          "#E41A1C",
          "#377EB8",
          "#4DAF4A",
          "#984EA3",
          "#FF7F00",
          "#FFFF33",
          "#A65628",
          "#F781BF",
          "#999999"
        )
      )
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(
        selectedData(),
        col = clusters()$cluster,
        pch = 20,
        cex = 3
      )
      points(clusters()$centers,
             pch = 4,
             cex = 4,
             lwd = 4)
    })
    
  }
  
  shinyApp(server = server, ui = ui
  )