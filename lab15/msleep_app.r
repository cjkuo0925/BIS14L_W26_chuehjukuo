
library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)

ui <- fluidPage(
  
  theme=shinythemes::shinytheme("cyborg"),
  
  selectInput("y", 
              "Select Sleep Variable",
              choices= c("sleep_cycle",
                         "awake",
                         "sleep_rem",
                         "sleep_total"),
              selected="sleep_total"),
  
  plotOutput("plot", 
             width="600px",
             height="400px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    
    msleep%>%
      filter(vore!="NA")%>%
      ggplot(aes(x="vore",
                 y=.data[[input$y]],
                 fill=vore))+
      geom_boxplot(apha=0.75)+
      labs(title="Sleep Variables by Vore Type",
           x="vore", 
           fill="vore type")+
      theme_minimal()
  })
}

shinyApp(ui, server)