####运行和调试
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
