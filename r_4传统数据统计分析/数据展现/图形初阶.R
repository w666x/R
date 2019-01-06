setwd("E:/Rexercise1")
###a simple example
dose=c(20,30,40,45,60)
drugA=c(16,20,27,40,60)
drugB=c(15,18,25,31,40)
plot(dose,drugA,type = "b")
###添加图形参数
###复制了一份当前的图形参数
opar=par(no.readonly = T)
par(lty=2,pch=17)
plot(dose,drugA,type = "b")
par(opar)
###绘图参数设置二,可以直接在绘图函数中添加参数名加数值
plot(dose,drugA,type = "b",lty=2,pch=17)
plot(dose,drugA,type = "b",lwd=3,pch=15,cex=2)
###颜色
n=10
mycolors=rainbow(n)
pie(rep(1,n),labels = mycolors,col=mycolors)
mygray=gray(1:n/n)
pie(rep(1:n),labels = mygray,col = mygray)
###字体和字号，字样的参数
par(font.lab=3,cex.lab=1.5,font.main=4,cex.main=2)
###确定字体族的字体的设置,而后就可以作为family的应用项
windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"))
names(pdfFonts())
pie(rep(1:n),labels = mygray,family="A")
###确定图形尺寸和边界尺寸
par(pin=c(4,3),mai=c(1,.5,1,.2))
###综合绘图,保存了图形参数设置，日后可恢复的
opar=par(no.readonly = T)
par(pin=c(2,3))
par(cex.axis=.75,font.axis=3)
plot(dose,drugA,type = "b",pch=19,lty=2,col="red")
plot(dose,drugB,type = "b",pch=23,lty=6,col="blue")
par(opar)
###添加文本，自定义坐标轴和图例
plot(dose,drugA,type = "b",
     col="red",lty=2,pch=2,lwd=2,
     main="Clinical Trials for Drug A",
     sub = "This is hypothetical data",
     xlab="Dosage",ylab = "Drug Response",
     xlim = c(0,60),ylim =c(0,70))
###添加标题，以及标题的图形参数的设置
title(main = "My title",col.main="red",
      sub="MY sub-title",col.sub="blue",
      xlab = "x-axis label",ylab = "y-axis label",
      col.lab="green",cex.lab=0.75)
###坐标轴的参数设置
###自定义坐标轴
x=c(1:10)
y=x
z=10/x
opar=par(no.readonly = T)
par(mar=c(5,4,4,8)+0.1) ##增加边界的大小
plot(x,y,type = "b",pch=21, ##绘制x对y的图形
     col="red",yaxt="n",lty=3,ann = F)
lines(x,z,type = "b",pch=22,col="blue",lty=2)  ##增加曲线
axis(2,at=x,labels = x,col.axis="red",las=2)
axis(4,at=z,labels=round(z,digits = 2),##绘制自己的坐标轴
     col.axis="blue",las=2,cex.axis=0.7,tck=-.01)
mtext("y=10/x",side = 1,line = 3,  ##添加标题和文本
      cex.lab=1,las=1,col = "blue")
title("An Example of creative Axes",
      xlab = "X valueS",
      ylab = "Y=X")
par(opar)

###次要刻度线
###次要刻度线
###载入程序包Hmisc
install.packages("Hmisc")
library(Hmisc)
minor.tick(nx=n,ny=n,tick.ratio = n)

###参考线
abline(h=c(1,5,7))
abline(v=seq(1,10,2),lty=2,col="blue")

###图例
legend(2,10,"hello")

###实例操作
dose=c(20,30,40,45,60)
drugA=c(16,20,27,40,60)
drugB=c(15,18,25,31,40)
opar=par(no.readonly = T)
par(lwd=2,cex=1.5,font.lab=2)
plot(dose,drugA,type = "b",
     pch=15,lty=1,col="red",
     ylim = c(0,60),main = "Drug A vs. Drug B",
     xlab = "Drug Dosage",ylab = "Drug Response")
lines(dose,drugB,type = "b",pch=17,
      lty=2,col="blue")
abline(h=c(30),lwd=1.5,lty=2,col="gray")
library(Hmisc)
minor.tick(nx=3,ny=3,tick.ratio = 0.5)
legend("topleft",inset = .05,title = "Drug Type",
       c("A","B"),lty = c(1,2),pch = c(15,17),col = c("red","blue"))
par(opar)
###文本标注
attach(mtcars)
plot(wt,mpg,main = "Mileage vs. Car Weight",
     xlab = "Weight",ylab = "Mileage",
     pch=18,col="blue")
text(wt,mpg,row.names(mtcars),
    cex=0.6,pos=4,col="red")
detach()
###展示不同字体族的代码
opar=par(no.readonly = T)
par(cex=1.5)
plot(1:7,1:7,type = "n")
text(3,3,"Example of default text")
text(4,4,family="mono","Example of mono-spaced text")
text(5,5,family="serif","Example of serif text")
par(opar)
paste("x1","y1","z1")

###图形的组合
###示例一
attach(mtcars)
opar=par(no.readonly = T)
par(mfrow=c(2,2))
plot(wt,mpg,main = "Scatterplot of wt vs. mpg")
plot(wt,disp,main = "Scatterplot of wt vs. disp")
hist(wt,main = "Histogram of wt")
boxplot(wt,main="Boxplot of wt")
par(opar)
detach()
###示例二
attach(mtcars)
opar=par(no.readonly = T)
par(mfrow=c(3,1))
hist(wt,ann=FALSE)
hist(mpg)
hist(disp)
par(opar)
detach()
###图形组合示例二
###layout 函数
attach(mtcars)
layout(matrix(c(1,2,3,3),2,2,byrow = T))
hist(wt)
hist(mpg)
hist(disp)
detach()
###精度操作，控制图形大小
attach(mtcars)
###定义的行列宽度不是倍数关系，而是比他多几倍
layout(matrix(c(1,1,2,3),2,2,byrow = T),
       widths = c(0.3,0.1),heights =c(0.1,0.2))
hist(wt)
hist(mpg)
hist(disp)
detach()
####图形布局的精细控制
opar=par(no.readonly = T)
par(fig=c(0,.8,0,.8))  ##设置散点图
plot(mtcars$wt,mtcars$mpg,  
     xlab = "Miles Per Gallon",
     ylab = "Car Weight")
par(fig=c(0,0.8,0.55,1),new=T)
boxplot(mtcars$wt,horizontal = T,axes=F) #在上方添加箱线图
par(fig=c(0.65,1,0,0.8),new=T)
boxplot(mtcars$mpg,axes=F)  ##在右侧添加箱线图
mtext("Enhanced Scatterplot",side=1,outer=T,line=-3)
par(opar)
