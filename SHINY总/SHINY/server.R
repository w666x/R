##server.R
##定义一个简单的服务端实现
library(shiny)
library(datasets)
# we tweak the "am" field to have nicer factor label
# since this doesn't rely on any user inputs we
# can do this once at start up and then use the 
#value throughout the lifetime of the application.
mpgdata=mtcars
head(mpgdata)
#define the am as factor
mpgdata$am=factor(mpgdata$am,labels = c("Auto",
                                        "Maunal"))
#define server logic required to plot various variables
# against mpg
shinyServer(function(input,output){
  #compute the forumla text in a reactive expression
  # since it is shared by the output$caption and 
  # output$mpgplot expression
  formulatext=reactive({
    paste("mpg~",input$variable)
  })
  #return the formula text for printing  as a caption
  output$caption=renderText({
    formulatext()
  })
  #generate a plot of requested variable against 
  #mpg and only include outliers if requested
  output$mpgplot=renderPlot({
    boxplot(as.formula(formulatext()),
            data=mpgdata,
            outline = input$outliers)
  })
})
