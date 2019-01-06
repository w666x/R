###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第10讲，不同语法之间的变换
#qplot和ggplot间的语法
#从基础图形系统转换
#从lattice图形系统转换
#GPL(Graphics Production Library)转换

##qplot和ggplot转换
head(diamonds) #本文所用数据集均为diamond
qplot(x,y,data =diamonds)
ggplot(diamonds,aes(x,y))+geom_point()
#图形属性
qplot(x,y,data = diamonds,shape=cut,colour=I("red")) #图例里面不显示颜色项
ggplot(diamonds,aes(x,y))+geom_point(colour="red")
#图层
qplot(x,y,data = diamonds,geom = "line")
ggplot(data = diamonds,aes(x,y))+geom_line()
qplot(x,y,data = diamonds,geom = c("point","smooth")) #当有几个几何参数时，按顺序添加
ggplot(diamonds,aes(x,y))+geom_point()+geom_smooth()
qplot(x,y,data = diamonds,stat = "bin")  #统计变换和几何对象是分开的
ggplot(data = diamonds,aes(x,y))+geom_point(stat = "bin")
qplot(x,y,data = diamonds,geom = c("point","smooth")) #忽略无用参数
ggplot(data = diamonds,aes(x,y))+geom_point()+geom_smooth(method = "lm") #有部分无用的参数其无法忽略
#标度和坐标轴
qplot(x,y,data = diamonds,xlim = c(1,5),xlab = "my label")
ggplot(diamonds,aes(x,y))+geom_point()+scale_x_continuous("my label",limits = c(1,5))
qplot(x,y,data=diamonds,xlim = c(1,5),ylim = c(0,20))
ggplot(diamonds,aes(x,y))+geom_point()+scale_x_continuous(limits = c(1,5))+
  scale_y_continuous(limits = c(0,20))
#对坐标轴进行坐标变换
qplot(x,y,data = diamonds,log = "xy")
ggplot(diamonds,aes(x,y))+geom_point()+scale_x_log10()+scale_y_log10()
#绘图选项
qplot(x,y,data = diamonds,main = "title",asp = 1)
ggplot(diamonds,aes(x,y))+geom_point()+labs(title="title")+coord_fixed(ratio=.5) #长宽比为0.5

##基础图形系统
#绘制完整的图和在图形上面添加元素即高级绘图和低级绘图
#高级绘图
attach(diamonds)
plot(x,y);dotchart(x,y);stripchart(x~y);qplot(x,y)
plot(x,y,type = "l");qplot(x,y,geom = "line")
plot(x,y,type = "s");qplot(x,y,geom = "step")
plot(x,y,type = "b");qplot(x,y,geom = c("point","line"))
boxplot(x,y);qplot(x,y,geom = "boxplot")
hist(x);qplot(x,geom = "histogram")
cdplot(x,y);qplot(x,fill=y,geom = "density",position = "fill")
barplot(table(x));qplot(x,geom = "bar",fill="color")
barplot(x);qplot(names(x),x,geom = "bar",stat = "identity")#image和contour需要矩阵形式的参数
plot(x,y,col="red",cex=1)
qplot(x,y,colour="red",size=1)
qplot(x,y,colour=I("red"),size=I(1)) #改变默认参数并覆盖默认是，需使用I()
#低级绘图
plot(x,y);lines(x,y)
qplot(x,y)+geom_line()
qplot(x,y);last_plot()+geom_line()
#调色板
palette(rainbow(5))
plot(1:5,1:5,col=1:5,pch=19,cex=4)
qplot(1:5,1:5,col=factor(1:5),size=4)
last_plot()+scale_color_manual(values = rainbow(5))+geom_point(size=I(10)) #输入size有用
qplot(0:100,0:100,col=0:100,size=I(4))+
  scale_color_gradientn(colours=rainbow(7)) #键入连续性的调色板
last_plot()+scale_color_gradientn(colours = terrain.colors(7))

##lattice图形设备,以下每行皆是lattice和ggplot的对比作图
#lattice的图形公式是基于公式的，而ggplot则不是
library(lattice)
xyplot(x~color,data=diamonds);qplot(color,x,data = diamonds)
xyplot(x~color|cut+clarity,data = diamonds)
qplot(color,x,data=diamonds,facets = ~cut+clarity) #等价于按cut和clarity分面
qplot(color,x,data = diamonds,facets = cut~clarity) #和上图等价但是很明显这种表示方法较好一些
stripplot(~table,data = diamonds,jitter.data=T);qplot(table,1,data = diamonds,geom = "jitter")
histogram(~table,data = diamonds);qplot(table,data = diamonds,geom = "histogram")
bwplot(color~x,data = diamonds);qplot(factor(color),x,data = diamonds,geom="boxplot")
xyplot(wt~mpg,mtcars,type=c("p","smooth"));qplot(mpg,wt,data = mtcars,geom = c("point","smooth"))
xyplot(wt~mpg,mtcars,type=c("p","r"));qplot(mpg,wt,data = mtcars,geom = c("point","smooth"))
#ggplot与lattice的标度处理方式相似，仅仅在语法上有些不同
xyplot(wt~mpg|cyl,mtcars,scales = list(y=list(relation="free")))#按cyl分类
qplot(mpg,wt,data = mtcars)+facet_wrap(~cyl,scales = "free") #按cyl分面，并释放标度范围
xyplot(wt~mpg|cyl,mtcars,scales = list(log=10))
qplot(mpg,wt,data = mtcars,log = "xy")+facet_wrap(~cyl)
xyplot(wt~mpg|cyl,mtcars,scales = list(log=2)) #对数化，标度
qplot(mpg,wt,data = mtcars)+scale_x_log10()+scale_y_log10()
xyplot(wt~mpg,data = mtcars,groups = cyl,auto.key = T) #这个挺不错的哦
#直接将图形属性映射到颜色，大小，形状
qplot(mpg,wt,data = mtcars,colour=cyl)
xyplot(wt~mpg,mtcars,xlim = c(20,30)) #lattice的作用对象为公式
qplot(mpg,wt,data = mtcars,xlim = c(20,30))
#类似的选项来控制图形中的标签
xyplot(wt~mpg,mtcars,xlab = "Miles per gallon",ylab = "weight",
       main="Weight-efficienty tradeoff")
qplot(mpg,wt,data = mtcars,xlab = "Miles per gallon",ylab = "weight")
xyplot(wt~mpg,mtcars,aspect = 1)
qplot(mpg,wt,data = mtcars,asp=2) #貌似也是比例的

##GPL
#此处没有进行下去啊，，感觉有点不懂了
source("demographics")
demographics=transform(demographics,bd=pmax(birth-death,0))

###用grid操作图形
library(ggplot2)
library(gtable)
library(grid)##需要editGrob函数
p=qplot(wt,mpg,data = mtcars,colour=cyl,main = "Title text")
p
#修改图形元件，将图形题目字体改为斜体红色
g=ggplotGrob(p)
idx=which(g$layout$name=="title")
g$grobs[[idx]]=editGrob(g$grobs[[idx]],gp=gpar(fontface="italic",col="red"))
#重新绘制
grid.draw(g)
