rm(list=ls())

library(tidyverse)
library(shiny)
library(ggvis)

ui <- fluidPage(
  sliderInput(inputId = "CEScutoff",
              label = "Threshold for CalEnviroscreen",
              value = 75, min = 0, max = 100),
  sliderInput(inputId = "HDIcutoff",
            label = "Threshold for Health Disadvantage Index",
            value = 75, min = 0, max = 100), 
  plotOutput(outputId = "hist")

)


server <- function(input, output, session) {
  
  output$hist <- renderPlot({
    
    full_join(CES2, HPI2)  %>%
      gather(CES.3.0.Percentile, ces2_pctile, key = CESind, value = percentile) %>% 
      mutate(venn = ifelse(percentile >= input$CEScutoff & hdi_pctile >= input$HDIcutoff, "Both CES and HPI DAC",
                           ifelse(percentile >= input$CEScutoff & hdi_pctile < input$HDIcutoff, "CES DAC only",
                                  ifelse(percentile < input$CEScutoff & hdi_pctile >= input$HDIcutoff, "HPI DAC only","Not a DAC")))) %>%
      ggplot(aes(x=hdi_pctile, y=percentile, color=venn)) + geom_point(size=1, alpha=0.5) + 
      geom_hline(yintercept = input$CEScutoff, size=2, alpha=0.5) + 
      geom_vline(xintercept = input$HDIcutoff, size=2, alpha=0.5)
    
    
    
  })
    
  
}




shinyApp(server = server, ui = ui)
