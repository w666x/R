#R语言中文社区
setwd("E:/Rexercise1/R语言中文社区")
#交互式热力图，heatmaply和plotly
install.packages("heatmaply")
library(heatmaply)
heatmaply(mtcars,k_col=2,k_row=3)%>% 
  layout(margin=list(l=130,b=40))
#自行操作
heatmaply(mtcars)
heatmaply(mtcars,k_col=2,k_row=3)%>%
  layout(margin=list(l=100,b=100))
#k_col、k_row设定色彩种类,数值不应该超过所涉变量的范围
#Pipe操作连接margin函数以调整显示尺寸
heatmaply(cor(mtcars), k_col = 2, k_row = 2,limits = c(-1,1)) %>% 
  layout(margin = list(l =40, b = 40),colors = heat.colors(100))
#插入相关系数函数作为热力数值，limits作为显示范围从负相关到正相关.
#colors函数用以调整色彩，其他命令如下
heatmaply(cor(mtcars),
          scale_fill_gradient_fun = 
            ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 5, 
                                          limits = c(-1, 1)),k_col = 2, k_row = 3) %>% 
  layout(margin = list(l = 100, b = 100))
#上述已经生成了交互式热力图了，网端实现
library(plotly)
plotly_username=""  #你的用户名
plotly_api_key="" #你的API
library(heatmaply) #加载包
a<-heatmaply(mtcars, k_col = 2, k_row = 3) %>% 
  layout(margin = list(l = 130, b = 40))
plotly_POST(a, filename = "heatmap", fileopt = "new",
            sharing = "public")
