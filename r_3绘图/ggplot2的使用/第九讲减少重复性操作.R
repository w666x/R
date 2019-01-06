###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第9讲，编写自己的函数，减少重复性工作，主要介绍三种方式
#在之前图形上面迭代进行修改
#具有复制处理机制，只需定义一次就可多次使用的模板
##迭代法
qplot(x,y,data = diamonds,na.rm=T)
last_plot()+xlim(3,11)+ylim(3,11) #更改坐标的标度区间
last_plot()+xlim(4,10)+ylim(4,10)+geom_abline()
last_plot()+xlim(4,5)+ylim(4,5)
last_plot()+xlim(4,4.5)+ylim(4,4.5)
last_plot()+geom_abline(colour="red") #添加拟合线
#最终的图形代码
qplot(x,y,data = diamonds,na.rm=T)+
  geom_abline(colour="red")+xlim(4,4.5)+ylim(4,4.5)

##绘图模板
#构建可以重用的组件来自动执行某些常用的任务
#建立一个颜色标度并应用在两个图形中
gradient_rb=scale_color_gradient(low = "red",high="blue") #颜色标度
qplot(cty,hwy,data = mpg,colour=displ)+gradient_rb
qplot(bodywt,brainwt,data = msleep,colour=awake,log = "xy")+gradient_rb
#创建一个命令列表，ggplot中逐个添加命令和添加命令列表结果一样
#取消坐标轴标签和刻度的两个连续列表
xquiet=scale_x_continuous("",breaks = NULL)
yquiet=scale_y_continuous("",breaks = NULL)
quiet=list(xquiet,yquiet) #命令需以list方式存储
qplot(mpg,wt,data = mtcars)
qplot(mpg,wt,data = mtcars)+quiet #去除了标签和刻度
qplot(displ,cty,data = mpg)
qplot(displ,cty,data = mpg)+quiet #去除了标签和刻度
#创建一个改变图层默认设置的函数
#想在一个图形中添加线性模型的函数
geom_lm=function(formula=y~x){
  geom_smooth(formula = formula,se=F,method = "lm")
}
qplot(mpg,wt,data = mtcars)
qplot(mpg,wt,data = mtcars)+geom_lm() #添加一个平滑线
library(splines)
qplot(mpg,wt,data = mtcars)+geom_lm(y~ns(x,3))

###绘图函数，定义的函数列表中包含很多个命令组件
#使用aes_string代替aes，虽然二者功能是一样的，但是，导入自变量使用字符串形式效果更好
#通常情况下，将绘图代码分为两个函数：数据变换和处理；绘图
#实例：运用9.2.2中的代码，封装成平行坐标图绘图函数
##请事先加载reshape2,plyr包，并运行rang01函数
library(plyr);library(reshape2)
rang01=function(x){  #定义无量纲化的函数
  rng=range(x,na.rm = T)  #求出数据的最小最大值，即数据的作用区间
  (x-rng[1])/diff(rng)
}
pcp_data=function(df){  #定义数据处理函数
  numeric=laply(df,is.numeric)
  #将每一列的数值调整到相同的范围
  df[numeric]=colwise(rang01)(df[numeric])
  #行名作为行识别信息
  df$.row=rownames(df)
  #使用melt将非数值变量作为id.vars
  dfm=melt(df,id=c(".row",names(df)[!numeric]))
  #给数据框添加pcp类
  class(dfm)=c("pcp",class(dfm))
  dfm
}
pcp=function(df,...){ #最终的函数，包括绘图以及数据处理部分
  df=pcp_data(df)
  ggplot(df,aes(variable,value))+geom_line(aes(group=.row))
}
###运行我们所封装的最终的函数，得结果如下
pcp(mpg) #绘制平行坐标图
pcp(mpg)+aes(colour=drv)
