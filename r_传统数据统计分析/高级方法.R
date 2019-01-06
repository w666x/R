setwd("E:/Rexercise1")
###处理缺失数据的高级方法
###识别缺失数据
###缺失数据的可视化
###完整案例分析
###缺失数据的多重插补法
library(VIM)
library(mice)
###导入数据以及列出没有数据缺失的行和有数据缺失的行
data(sleep,package = "VIM")
head(sleep)
sleep[complete.cases(sleep),]
dim(sleep)
sleep[!complete.cases(sleep),]
###提取有关缺失数据的有用信息,提取TRUE的数量
sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream))

###探索缺失值模式
###列表显示缺失值
library(mice)
data(sleep,package = "VIM")
md.pattern(sleep)
###图形探究缺失数据,三种作图包
library("VIM")
install.packages("minqa")
aggr(sleep,prop=F,numbers=T)
matrixplot(sleep)
marginplot(sleep[c("Gest","Dream")],pch = c(20),
           col = c("darkgray","red","blue"))
###用相关性探索缺失值
x=as.data.frame(abs(is.na(sleep)))
head(sleep,n=5)
head(x,n=5)
tail(x)

###理解缺失数据的来由和影响
###缺失数据生成的潜在机制，评价缺失数据对回答实质问题的影响
###暨此即可找到何种统计方法可以用来处理你的数据了
###多重插补
library(mice)
data(sleep,package = "VIM")
imp=mice(sleep,seed = 1234)
fit=with(imp,lm(Dream~Span+Gest))
pooled=pool(fit)
summary(pooled)
imp
imp$imp$Dream  ###观察到实际的插补值
complete(imp,action = 3) ##制定m个完整数据集中的任意一个，

###高阶图形进阶
library(lattice)
library(ggplot2)
###示例一
head(singer)
tail(singer,n=8)  ###栅栏型直方图
histogram(~height|voice.part,data = singer,
          main="Distribution of Height by Voice Pitch",
          xalb="Height (inches)")
###标签参数的设置,主要是lattice包的应用
###示例二
attach(mtcars)
gear=factor(gear,levels = c(3,4,5),
            labels = c("3year","4year","5year"))
cyl=factor(cyl,levels = c(4,6,8),
           labels = c("4cylinders","6cylinders","8cylinders"))
cyl;gear
head(mtcars)
densityplot(~mpg,main="Density Plot",xlab="Miles per Gallon")
bwplot(cyl~mpg|gear,
       main="Box Plots by Cylinders and Gears",
       xlab = "Miles per Gallon",ylab = "Cylinders")
xyplot(mpg~wt|gear*cyl,
       main="Scatter Plots by Cylinders and Gears",
       ylab = "Miles per Gallon",xlab = "Car weight")
cloud(mpg~wt*qsec|cyl,
      main="3D Scatter Plots by Cylinders")
dotplot(cyl~mpg|gear,
        main="Dot Plots by Number of Gears and Cylinders",
        xlab="Miles Per Gallon")
splom(mtcars[c(1,3,4,5,6)],
      main="Scatter Plot Matrix for mtcars Data")
detach()
###可以储存图形到Mygraph对象中，用plot可以显现
mygraph=densityplot(~height|voice.part,data = singer)
plot(mygraph)
###updata修改图形对象,改变已经汇出图形的性质
update(mygraph,col="red",pch=16,cex=.8,jitter=.05,lwd=2)

###条件变量，lattice包的强大之处
###添加的经过处理的displacement即为条件变量
displacement=equal.count(mtcars$disp,number=3,overlap=0)
xyplot(mpg~wt|displacement,data = mtcars,
       main="Miles per Gallon vs. Weight by Engine Displacement",
       xlab = "Weight",ylab = "Miles per Gallon",
       layout=c(3,1),aspect = 1.5)

###面板函数
###自定义的面板函数的xyplot
displacement=equal.count(mtcars$disp,number=3,overlap=0)
###自定义函数
mypanel=function(x,y){
  panel.xyplot(x,y,pch = 19)
  panel.rug(x,y) ##添加小的刻度，即轴须线
  panel.grid(h=-1,v=-1) ##添加网格线
  panel.lmline(x,y,col="red",lwd=1,lty=2)
}
###面板函数的使用
xyplot(mpg~wt|displacement,data = mtcars,
       layout=c(3,1),aspect = 1.5,
       main="Miles per Gallon vs. Weight by ",
       xlab = "Weight",ylab = "Miles per Gallon",
       panel = mypanel)

###示例二
mtcars$transmission=factor(mtcars$am,levels = c(0,1),
                           labels = c("Automatic","Manual"))
head(mtcars)
###自定义函数
panel.smoother=function(x,y){
  panel.grid(h=-1,v=-1)
  panel.xyplot(x,y)
  panel.loess(x,y) ##画出非参数拟合曲线
  panel.abline(h=mean(y),lwd = 2,lty = 2,col = "green")
}    ###在各水平下添加了mpg的均值
xyplot(mpg~disp|transmission,data = mtcars,
       scales = list(cex=.8,col="red"),
       panel = panel.smoother,
       xlab = "Displacement",ylab = "Miles per Gallon",
       main="MGP vs Displacement by Transmission Type",
       sub="Dotted lines are group Means",aspect = 1)

###分组变量
mtcars$transmission=factor(mtcars$am,levels = c(0,1),
                           labels = c("Automatic","Manual"))
###以group分组，进而绘图
###auto.key表示的是图例等的位置以及内容
densityplot(~mpg,data = mtcars,groups = transmission,
            main="MPG Distribution by Transmission Type",
            xlab = "Miles per Gallon", 
            auto.key =list(space="right",colsums=1,
                           title="Transmission"))

###自定义图例并含有分组变量的核密度曲线图
mtcars$transmission=factor(mtcars$am,levels = c(0,1),
                           labels = c("Automatic","Manual"))
###定义颜色，线和点类型
colors=c("red","blue")
lines=c(1,2)
points=c(16,17)
###自定义图例
key.trans=list(title="Transmission",columns=2,
               space="bottom",cex=0.9,
               text=list(levels(mtcars$transmission)),
               points=list(pch=points,col=colors),
               lines=list(col=colors,lty=lines),
               cex.title=1)
###自定义密度图
densityplot(~mpg,data = mtcars,groups = transmission,
            main="MPG Distribution by Transmission Type",
            xlab = "Miles per Gallon",pch=points,
            lty=lines,col=colors,lwd=2,jitter=.005,
            key=key.trans)

###示例三，分组变量和条件变量同时存在
library(lattice)
colors="darkgreen"
symbols=c(1:12)
linetype=c(1:3)
key.species=list(title="Plant",
                 space="right",
                 text=list(levels(CO2$Plant)),
                 points=list(pch=symbols,col=colors))
xyplot(uptake~conc|Type*Treatment,data=CO2,
       group=Plant,type="o",pch=symbols,col=colors,
       lty=linetype,main="Carbon Uptake \n in Platts",
       key=key.species,
       xlab = expression(paste("connection",
                               bgroup("(",italic(frac(mL,L)),")")),
                         ylab = expression(paste("uptake",
                                                 bgroup("(",italic(frac("umol","m"^2)),")")))))

###图形参数的设置
trellis.par.get()
show.settings()  ###显示图形参数的设置
mysetting=trellis.par.get()
mysetting$superpose.symbol ###查看叠加点的默认设置值
mysetting$superpose.symbol$pch=c(1:10)
trellis.par.set(mysetting) ##新的参数设置
show.settings()
##页面摆放
library(lattice)
graph1=histogram(~height|voice.part,data = singer,
                 main="Height of Choral Singers by 
                 Voice Part")
graph2=densityplot(~height,data = singer,
                   groups = voice.part,
                   plot.points=F,auto.key = list(col=4))
plot(graph1,split=c(1,1,1,2))
plot(graph2,split=c(1,2,1,2),newpage=F)
levels(singer$voice.part)
plot(graph1,index.cond=list(c(2,4,6,8,1,3,5,7)))

###ggplot2绘图包
###应用实例一
library(ggplot2)
mtcars$cylinder=as.factor(mtcars$cyl)
head(mtcars)
qplot(cylinder,mpg,data = mtcars,geom = c("boxplot",
                                          "jitter"),
      fill=cylinder,main = "Box plot with superimposed",
      xlab = "Number of Cylinders",ylab = "Miles per Gallon")
###应用实例二:::添加了平滑拟合曲线和置信区间带
library(ggplot2)
transmission=factor(mtcars$am,levels = c(0,1),
                    labels = c("Automatic","Maunal"))
head(mtcars)
qplot(wt,mpg,data = mtcars,shape=transmission,
      color=transmission,geom = c("point","smooth"),
      xlab = "Weight",
      ylab = "Miles Per Gallon",
      main = "Regression Example")
help("qplot")
###应用实例三:在am以及cyl的条件下画出wt以及mpg的hp图
library(ggplot2)
mtcars$cyl=factor(mtcars$cyl,levels = c(4,6,8),
                  labels = c("four","six","eight"))
mtcars$am=factor(mtcars$am,levels = c(0,1),
                 labels = c("AUTO","MANU"))
head(mtcars)
qplot(wt,mpg,data = mtcars,facets = am~cyl,size=hp)
###应用实例四::画出了单变量的密度曲线图，以voice为
###条件变量，填充方式亦以voice不同
library(ggplot2)
data(singer,package = "lattice")
qplot(height,data = singer,geom = c("density"),
      facets = voice.part~.,fill=voice.part)

###交互式图形：提升了图形的交互能力
###单击图上的点后，就可对其进行标注了
plot(mtcars$wt,mtcars$mpg)
identify(mtcars$wt,mtcars$mpg,labels = row.names(mtcars))
###playwith包
library(playwith)
search()
library(lattice)
playwith(
  xyplot(mpg~wt|factor(cyl)*factor(am),
         data = mtcars,subscripts = T,type=c("r","p"))
)
###latticist包
library(latticist)

###iplot包
library(iplots)
