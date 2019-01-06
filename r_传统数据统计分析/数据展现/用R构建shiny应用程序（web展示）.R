##用R构建shiny应用程序（web展示）
library(shiny)
#一个简单的应用程序，这个程序可以生成正态分布的随机数
#而且，随机数的个数可由用户自己设置，绘制直方图
runExample("01_hello")

##用户界面定义的源程序
library(shiny)
# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Hello "),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs",
                "Number of observations:",
                min = 0,
                max = 1000,
                value = 500)
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))

##应用程序的定义
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot
  ##此处定义一个displot
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
})


##示例二
runExample("02_text")

#示例三
runExample("03_reactivity")
##解析
#反应式设计
#反应式编程基础
#分解一:基于yoghurt在表单中的选项来返回R数据框
datasetInput <- reactive({
  switch(input$dataset,
         "rock" = rock,
         "pressure" = pressure,
         "cars" = cars)
})
#分解二：赋值给输出值的函数
output$view <- renderTable({
  head(datasetInput(), n = input$obs)
})

##原代码
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Reactivity"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"),
    
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("caption")), 
    
    verbatimTextOutput("summary"), 
    
    tableOutput("view")
  )
))

#服务端脚本
library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #  3) When the inputs change and the expression is re-executed, the
  #     new result is compared to the previous result; if the two are
  #     identical, then the callers are not notified
  #
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # The output$caption is computed based on a reactive expression that
  # returns input$caption. When the user changes the "caption" field:
  #
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive expression below don't 
  # depend on input$caption, those expression are NOT called when 
  # input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive expression, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # The output$view depends on both the databaseInput reactive expression
  # and input$obs, so will be re-executed whenever input$dataset or 
  # input$obs is changed. 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})

