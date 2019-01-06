###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第二讲，语法的突破
library(ggplot2)
data1=mpg
head(data1);dim(data1)
tail(data1);str(data1)
#简单绘制散点图
qplot(displ,hwy,data = data1,colour=factor(cyl)) #发动机排量~每加仑行驶英里数
qplot(displ,hwy,data = data1,colour=factor(cyl),geom = "line") 
qplot(displ,hwy,data = data1,colour=factor(cyl),geom = "point") 
#可以将数据的单位转化成电脑可以识别的屋里单位（像素或者颜色）
#由数据；标度和坐标系；图形注释

#图形示例二
qplot(displ,hwy,data = mpg,facets = .~year)+geom_smooth()  #以year分面，facets

#图层语法的组件
p=qplot(displ,hwy,data = data1,colour=factor(cyl))
summary(p)
save(p,file = "plot.rdata") #保存图形对象
load("plot.rdata") #读取图形对象
p
ggsave("plot.png",width = 5,height = 5) #将图片保存为png格式
