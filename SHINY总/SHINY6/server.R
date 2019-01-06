##SHINY6
##showing and hiding control with conditionalpanel
# Partial example
datasetInput <- reactive({
  switch(input$dataset,
         "rock" = rock,
         "pressure" = pressure,
         "cars" = cars)
})

output$nrows <- reactive({
  nrow(datasetInput())
})