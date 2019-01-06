##SHINY4
##上传文件
##UI.R
shinyUI(pageWithSidebar(
  headerPanel("CSV Viewer"),
  sidebarPanel(   
##input由此处导入
    fileInput('file', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    ##tags$hr表示空出一行的位置，或者说加大行间距 
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),  #默认值为common
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote') #默认值为double
  ),
  mainPanel(  #输出contents
    tableOutput('contents')
  )
))
