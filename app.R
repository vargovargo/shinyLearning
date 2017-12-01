rm(list=ls())

library(tidyverse)
library(shiny)

foo <- read.csv("https://raw.githubusercontent.com/vargovargo/CHVIr/master/heatCHVIcounty.csv", header=T)

stateTable <- foo %>% group_by(reportyear) %>% 
  summarise(stateAvg = mean(estimate, na.rm=T))

avg2050 <- as.numeric(stateTable[1,2])
avg2085 <- as.numeric(stateTable[2,2])

ui <- pageWithSidebar(
  headerPanel('Projected Number of Extreme Heat Days'),
  sidebarPanel(
    selectInput(inputId = "co1", label = "Select a county", choices = unique(foo$county_name)),
    selectInput(inputId = "co2", label = "Select a second county", choices = unique(foo$county_name)),
    selectInput(inputId = "co3", label = "Select a third county", choices = unique(foo$county_name))
  ),
  mainPanel(plotOutput('plot1'))
)
  
server <- function(input, output, session) {
  

  output$plot1 <- renderPlot({
    
  foo %>% filter(county_name %in% c(input$co1, input$co2, input$co3)) %>%
      ggplot(aes(x=reportyear, y=estimate, fill=county_name)) + geom_bar(stat="identity", position="dodge") +
      geom_hline(yintercept = avg2050, color="blue", alpha=0.4, linetype="dashed") + 
      geom_hline(yintercept = avg2085, color="red", alpha=0.4, linetype="dashed") +
      ylab("Annual Extreme Heat Days")
  
    
  })
  
}




shinyApp(server = server, ui = ui)
