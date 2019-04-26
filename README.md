# R_相关
  * 程序进程中获取用户的指令，终端调试不错     

           #传参控制进程,变量 
           switch(menu(c("coutinue", "break")),
           cat("continue the process\n"), stop("we are stoping the process")) 
           #传参给变量 
           cat("输入第二行数字，以空格间隔，以回车终止")
           line2 <- scan()
           #传参给变量 
           input <- readline("please enter your input:")

  * 记录程序运行时间
          
          system.time(expr)

          
  * 终端传参
  
         Args <- commandArgs() #需要在脚本第一行加入此命令。输入的参数从第六个和第七个开始
         # 在unix、windows外部需要调用R脚本执行，然后又需要输入不同的参数，类似shell脚本的命令行参数输入，可以使用Rcript命令实现。
         Rscript [options] [-e expression] file [args] #命令格式
         #file表示需要执行的脚本，[options] [-e expression] 可以有也可以不用。

   * 将字符串转化成可执行命令
   
           x <- 1:10
           a <- "print(x)"
           class (a)
           eval(parse(text = a))
* 可以产生进度条的相关函数，在有多步循环过程中加入这个东西，来判断执行的进程是很好用的
#####  产生进度条
    -   k=10  # k为循环的个数，例如要循环 n次，我们就将k设置为n
        library(plyr)
        pbar=create_progress_bar('text') #显示运行步骤
        pbar$init(k) 
        # 在这之前都放到循环外，后面的放到循环中
        pbar$step()   

  * 【chapter one】[1基础](https://github.com/w666x/R/tree/master/r_1基础应用)
    * 安装R包的方法
    * 字符串函数详解
    * r并行化基础及提高
  * 【chapter two】[2清洗](https://github.com/w666x/R/tree/master/r_2数据清洗)
    * 数据读取和存储
    * 数据预处理及概览
    * 数据的描述性统计分析及探索性统计分析
  * 【chapter three】[3绘图](https://github.com/w666x/R/tree/master/r_3绘图)
    * ggpl
