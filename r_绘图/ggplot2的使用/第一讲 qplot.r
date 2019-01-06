###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
####第一讲 qplot
?qplot
###如何将变量映射到图形属性（颜色，大小，形状）上面
###如何通过制定不同的几何对象来创建不同类型的图形，以及如何将他们组合在一张图中
###分面的运用，将数据拆分为子集
###通过设定基本的选项来调整图形的外观
datas=diamonds
summary(datas)
dim(datas);str(datas) #显示维数以及各变量的基本属性
set.seed(1410)
dsmall=datas[sample(nrow(datas),100),] #只抽取100组构成小样本进行绘图分析
qplot(carat,price,data = datas) #作出重量和价格之间的关系
qplot(log(carat),log(price),data = datas) 
qplot(carat,x*y*z,data = datas) #作出重量和体积之间的关系

#添加颜色，大小，形状或者其他图形属性
qplot(carat,price,data = dsmall,colour=color)
qplot(carat,price,data = dsmall,colour=I("red")) #手动设定图形颜色
qplot(carat,price,data = dsmall,shape=cut)
qplot(carat,price,data = dsmall,size=I(4)) #手动设定图形大小
qplot(carat,price,data = datas,alpha=I(1/10)) #设置图形透明度，可以看出大部分的重叠位置
qplot(carat,price,data = datas,alpha=I(1/100)) 

#几何对象#画出其他类型的图像，geom
#向图中添加平滑曲线
qplot(carat,price,data = dsmall,geom = c("point","smooth"),asp = .8)
qplot(carat,price,data = dsmall,geom = c("smooth"))#虽然默认point，但是，没有的话，还是挺尴尬
qplot(carat,price,data = datas,geom = c("point","smooth"))
qplot(carat,price,data = datas,geom = c("point","smooth"),asp=1)
library(mgcv)
qplot(carat,price,data=dsmall,geom = c("point","smooth"))
#箱形图和扰动点图
qplot(color,price/carat,data = datas,geom = "jitter",alpha=I(1/5)) #扰动点图，以颜色为条件的每克拉价格的分布
qplot(color,price/carat,data = datas,geom = "jitter",alpha=I(1/50))
qplot(color,price/carat,data = datas,geom = "boxplot",alpha=I(1/5))
#直方图和密度曲线图
qplot(carat,data = datas,geom = "histogram",binwidth=1)
qplot(carat,data = datas,geom = "histogram",binwidth=.1)
qplot(carat,data = datas,geom = "histogram",binwidth=.01)
qplot(carat,data = datas,geom = "density")
qplot(carat,data = datas,geom = "histogram",fill=color)
qplot(carat,data = datas,geom = "density",colour=color) #将某一个分类变量映射到图形属性上面
#条形图
qplot(color,data = datas,geom = "bar")
qplot(color,data = datas,geom = "bar",weight=carat)+scale_y_continuous("carat")#按重量加权的条形图
#时间序列中的线条图和路径图
head(economics)
qplot(date,unemploy/pop,data = economics,geom = "line")
qplot(date,uempmed,data = economics,geom = "line")
year=function(x) as.POSIXlt(x)$year+1990  #画出路径图
qplot(unemploy/pop,uempmed,data = economics,geom = c("point","path")) 
qplot(unemploy/pop,uempmed,data = economics,geom = "path",colour=year(date))

#分面图
qplot(carat,data = datas,facets = color~., #以颜色为条件的重量的直方图
      geom = "histogram",binwidth=.1,xlim = c(0,3))
qplot(carat,..density..,data = datas,facets = color~., #以颜色为条件的重量的密度直方图
      geom = "histogram",binwidth=.1,xlim = c(0,3))

#其他选项
qplot(carat,price,data = dsmall,xlab = "price($)",ylab = "weight(carats)",
      main = "Price-weight relationship")
qplot(carat,price/carat,data = dsmall,ylab = expression(frac(price,carat)),
      xlab = "weight(carats)",
      main = "Small diamonds",
      xlim = c(.2,1))
qplot(carat,price,data = dsmall)
qplot(carat,price,data = dsmall,log = "xy")#对xy同时取对数