
# Create server -------------------------------------------------------
server <- function(input, output){
  dat <- reactive({
    toxicity_val %>%
      filter(State == input$StateInput)
  })
  
  output$toxicityplot <- renderPlot({
    ggplot(dat(), aes(x = long, y = lat,
                      group = group, fill = value))+
      geom_polygon(color = "gray90", size = 0.1)+
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
      ggtitle("Toxicity value measured by lbs on Strawberry in the US")
  })
}