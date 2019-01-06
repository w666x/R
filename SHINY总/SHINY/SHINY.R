##中文教程：用R构建shiny应用教程
##UI和server，UI和服务器
##构建基本的输入和输出信息
##运行和调试
library(shiny)
runApp("SHINY")

#打印标准输出，和标准的错误信息
cat("foo\n")
cat("bar\n",file = stderr())
#使用调试浏览器,在代码的某一个地方无条件停止
# Stop execution when the user selects "am"
browser(expr = identical(input$variable, "am"))
#建立一个自定义的错误处理器
# Immediately enter the browser when an error occurs
options(error = browser)
# Call the recover function when an error occurs
options(error = recover)

##定制滑动条
library(shiny)
runApp("05_slidersr")
##个人定制成功
library(shiny)
runApp("SHINY1")

##定制选项卡
library(shiny)
runExample("06_tabsets")
#个人定制成功
library(shiny)
runApp("SHINY2")

##UI增强，更多小工具
library(shiny)
runExample("07_widgets")
##添加解释文本，添加控件控制更新进程
runApp("SHINY3")

##上传文件
library(shiny)
runExample("09_upload")
##
library(shiny)
runApp("SHINY4")

##下载数据
library(shiny)
runExample("10_download")
###
library(shiny)
runApp("SHINY5")

##HTML UI
##using the standard HTML page defining a shiny 
library(shiny)
runExample("08_html")

##动态UI/Dynamic UI
##showing and hiding control with conditionalpanel
library(shiny)
runApp("SHINY6")
traceback() ##往回递归，寻找错误地方
##creating control on the Fly with renderUI
library(shiny)
runApp("SHINY7")

