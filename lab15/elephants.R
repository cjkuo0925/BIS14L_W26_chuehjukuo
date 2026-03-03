ui <- dashboardPage(
  
  dashboardHeader(title="Distribution of Age and Height by Sex"),
  
  dashboardSidebar(
    
    selectInput("y", 
                "Select Variables",
                choices= c("age", 
                           "height"),
                selected="age")),
  
  dashboardBody(
    
    plotOutput("plot", 
               width="500px",
               height="400px")
    
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    elephants %>% 
      ggplot(aes(x=sex,
                 y=.data[[input$y]],
                 fill=sex))+
      geom_boxplot()
    
  })
}

shinyApp(ui, server)