##UI.R
##定义用户接口
library(shiny)
##define UI for miles per gallon application
shinyUI(pageWithSidebar(
  #application title
  headerPanel("miles per gallon"),
  #sidebar with control to select the variable
  #to plot against mpg and to 
  #specify whether outliers should be included
  sidebarPanel(
    selectInput("variable","variable:",
                list("cylinders"="cyl",
                     "transmission"="am",
                     "gears"="gear")),
    checkboxInput("outliers","show ouliers",F)
  ),       ##有选择的输出，默认情况下为F
  #show the caption and plot of the requested 
  #variable against mpg
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("mpgplot")
  )
))