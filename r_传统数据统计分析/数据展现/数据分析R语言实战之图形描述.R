###数据分析R语言实战之图形描述
##获取原始数据
##对绘图区域进行设置，分割
##绘制图形；标识图形；设置图形格式；保存导出图形
###实战案例，数据地图
##实战案例一
library(ggplot2)
install.packages("map_data")
state_map=map_data("state")

##实战案例二
library(sp)
##从gadm.org网站上得到中国的省区地理数据
date1=url("http://gadm.org/data/rda/CHN_adm1.RData")
load(date1)
load(url("http://gadm.org/data/rda/CHN_adm1.RData"))
help("load")

###两个实例由于没有找到相应的开源数据，故没有进行程序再现

###数据的图形描述
##展示二维，三维图形的示例
demo(graphics)
demo(persp)

##绘图区域的分割
par(no.readonly = T)
par(mfrow=c(1,3));
layout(matrix(c(1,1,2,3,2,3),2,3));layout.show(3);layout.show(1);
split.screen(C(2,1));split.screen(c(1,2),screen = 2);erase.screen();close.screen(all=T)

##二维图形的绘制
#多元数据的绘图
#示例一
#首先生成数据
y=data.frame(x1=1:5,x2=rnorm(5,0,1),x3=rgamma(5,2,3))
matplot(y,type = "l",col = 1:3,lwd = 2)
#添加图例
legend(1,5,col = 1:3,pch = "————",legend = c(paste("x",1:3,sep = "")))

#示例二
data("warpbreaks")
head(warpbreaks)
coplot(breaks~1:54|wool*tension,data = warpbreaks,col="red",bg="pink",
       pch = 21,bar.bg = c(fac="light blue"))

##低级绘图函数以及图形美化
x=rnorm(1000)
x=x[x<0]
hist(x,xlim = range(x),main = "hist of x",freq = F,nclass = 30,density = 20,
     adj=1,angle = 45,bg="blue",font.main=3)
lines(density(x),col="blue")
lines(-3:0,dnorm(-3:0,mean(x),sd(x)),col="red")
legend(-2.5,1,pch = c(15,-1,-1),lty = c(-1,1,1),col = c("gray","blue","red"),
       legend = c("histgram","density line","normal density line"),cex =0.8)
##图形美化
help("par")

##交互式绘图命令
x=rnorm(10)
plot(x)
locator(5,"o",col="red")
locator(2)
##标记点（4,1），（6,0）
identify(c(4,6),c(1,0),labels = c("a","b"),col="red")

##三维图形
demo(image)
demo(persp)
demo(contour)
#调出程序包，作三维绘图
library(rgl)
plot3d(x,y)

###lattice程序包
library(lattice)
library(ggplot2)
##读取数据
data(diamonds,package =" ggplot2")
diamonds=ggplot2::diamonds
head(diamonds)
dim(diamonds)
##绘图，随机抽取1000个样本,group为分组变量，auto.key为图例
##绘出二维散点图
sample=diamonds[sample(nrow(diamonds),1000),]
xyplot(price~carat,data=sample,groups=cut,
       auto.key=list(corner=c(1,0)),type=c("p","smooth"),
       span=0.7,main="Price VS. Carat")
##绘出箱线图
bwplot(color~price|cut,data = diamonds,main="Box-and-Whisker")
##画出直方图
histogram(~price|color,data = diamonds,layout=c(2,3))
##绘制三维图形的函数
x=seq(-pi,pi,len=20)
y=seq(-pi,pi,len=20)
g=expand.grid(x=x,y=y) ##构造一个数据框
head(g)
g$z=sin(sqrt(g$x^2+g$y^2)) ##在数据框内添加变量z
wireframe(z~x*y,data = g,drape=T,aspect=c(2,1),
          colorkey=T,main=expression(z=sin(x^2+y^2)))

###ggplot2程序包
library(ggplot2)
##快速绘图
sample=diamonds[sample(nrow(diamonds),200),]
qplot(carat,price,data = sample,shape=cut,color=color)
qplot(carat,price,data = sample,shape=cut,color=color,
      geom="line")
##添加平滑曲线一
qplot(carat,price,data = sample,geom = c("point","smooth")
     ,asp =2)
##直方图
qplot(carat,data = diamonds,geom = "histogram",
      binwidth=.2,xlim = c(0,3),fill=color)

##分图层绘图
#将第一图层对图形的初始化
##ggplot定义第一个图层
sample=diamonds[sample(nrow(diamonds),1000),]
p=ggplot(data = sample,mapping = aes(x=carat,y=price,
                                     color=clarity))
##几何对象，开始绘图：添加散点图以及添加平滑曲线
p+geom_point()+geom_smooth()
##仅第二层分类画点，即第三层中，平滑曲线即为相对于全体数据的
p=ggplot(data = sample,aes(x=carat,y=price))##不再指定分类变量
p+geom_point(aes(color=clarity))+geom_smooth() ##仅第二层分类

##散点图
sample=diamonds[sample(nrow(diamonds),100),]
p=ggplot(data = sample,aes(x=carat,y=price))
##设置三个分类变量
##非参数alpha控制透明度
##position="jitter"对散点增加扰动，防止点的过度重叠
p+geom_point(aes(color=color,shape=cut,size=clarity),
             alpha=.4,position = "jitter")

##标度
##控制图形的属性，参数的设置
##绘制重量的直方图
p=ggplot(data = diamonds,aes(x=carat))
p+geom_histogram()+scale_x_continuous(limits = c(0,3))
length(is.na(diamonds)==T)
dim(diamonds)
##绘图二
p=ggplot(data = diamonds,aes(x=carat,fill=color))
p+geom_histogram()+xlim(0,3)+scale_colour_manual(
  values = rainbow(7))

###统计变换
##用stat_smooth对数据作loess平滑，在carat-price散点图上
##添加非线性回归线
sample=diamonds[sample(nrow(diamonds),1000),]
##第二层添加散点，第三层坐标变换，第四层作平滑的统计变换
ggplot(sample,aes(x=carat,y=price))+geom_point()+
  scale_y_log10()+stat_smooth()

##分面
##分类变量的若干性质分图显示
ggplot(sample,aes(x=carat,y=price))+geom_point(
  aes(colour=cut))+scale_y_log10()+stat_smooth()+
  facet_wrap(~cut,ncol = 3)##按cut分类，三列排列

##坐标系统
ggplot(sample)+geom_bar(aes(x=factor(1),fill=cut))+
  coord_polar(theta = "y")

##图形保存
png(file="pie.png",bg="transparent")
dev.off() 
##ggplot2包中的图片保存命令
ggsave(filename = ("pie.pdf"))