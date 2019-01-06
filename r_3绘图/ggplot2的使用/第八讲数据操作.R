###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第8讲：数据操作
#使用plyr包实现ggplot2内部的统计变换过程
#长数据（时间序列图和品行坐标图）
#使用R对象作图，以及ggplot2进行线性模型诊断
##plyr，可同时在数据的多个子集上面做统计汇总
#统计实例1
#subset函数，进行数据提取
library(plyr)
dim(diamonds);str(diamonds)
ddply(diamonds,.(color),subset,carat==min(carat)) #取出各个颜色内最小的钻石
ddply(diamonds,.(color),subset,order(carat)<=2) #取出各个颜色内最小的两颗钻石
ddply(diamonds,.(color),subset,carat>quantile(carat,0.99)) #选取每组内大小为前1%的钻石
#transform函数，进行数据转换
ddply(diamonds,.(color),transform,price=scale(price)) #将每个颜色内的钻石价格标准化
ddply(diamonds,.(color),transform,price=price-mean(price)) #减去组均值
#clowise用来向量化一个普通函数，返回结果为一个新的函数
nmissing=function(x) sum(is.na(x)) #自定义一个函数，统计某一个向量缺失值的函数
nmissing(msleep$name)
nmissing(msleep$brainwt) #计算数据框中单一列的缺失值
nmissing(msleep) #计算总共的缺失值
nmissing_df=colwise(nmissing)
nmissing_df(msleep) #向量化后，就可以算出数据框内每一列的缺失值了
#更快捷的方式
colwise(nmissing)(msleep)
##特例
msleep2=msleep[,-6]
numcolwise(median)(msleep2,na.rm=T)
numcolwise(quantile)(msleep2,na.rm=T)
numcolwise(quantile)(msleep2,probs=c(.25,.75),na.rm=T) #缺失值此时必须处理
#统计实例2
ddply(msleep2,.(vore),numcolwise(median),na.rm=T)
##拟合多个模型
#演示stat_smooth生成平滑数据的过程
#统计实例3
qplot(carat,price,data = diamonds,geom = "smooth",colour=color) #按color分组
dense=subset(diamonds,carat<2)
head(dense)
qplot(carat,price,data=dense,geom = "smooth",
      colour=color,fullrange=T)
#绘图实例4
library(mgcv)
smooth=function(df){
  mod=gam(price~s(carat,bs="cs"),data = df) #模型拟合
  grid=data.frame(carat=seq(.2,2,length=50)) #构造一个grid出来
  pred=predict(mod,grid,se=T) #对每一个grid，求其相应模型下面预测值
  grid$price=pred$fit #将预测值也保存到grid下面
  grid$se=pred$se.fit #将残差也保存到grid下面
  grid
}
smoothes=ddply(dense,.(color),smooth) #对color进行函数处理
head(smoothes)
qplot(carat,price,data = smoothes,colour=color,geom = "line") #对储存在grid内的数据进行绘图
qplot(carat,price,data = smoothes,colour=color,geom = "smooth", #以及画出相应的误差带
      ymax=price+2*se,ymin=price-2*se)
##添加分组变量
mod=gam(price~s(carat,bs="cs")+color,data = dense)
grid=with(diamonds,expand.grid(  #将分组变量也考虑进去了
  carat=seq(.2,2,length=50),
  color=levels(color)
))
grid$pred=predict(mod,grid)
qplot(carat,pred,data = grid,colour=color,geom = "line")


###把数据化“宽”为“长”
#reshape2包里面的melt函数以及cast函数
#data；id.vars;measure.vars
head(economics);str(economics)
#在同一张图上画出两个时间序列的趋势图;首先将数据变成一个“长”的格式
#下面是以两种方式实现作图，但是由于两变量取值之间差异太大，所以某一组成为了一条直线
ggplot(economics,aes(date))+geom_line(aes(y=unemploy,colour="unemploy"))+
  geom_line(aes(y=uempmed,colour="uempmed"))+
  scale_color_hue("variable")
require(reshape2)
emp=melt(economics,id="date",measure=c("unemploy","uempmed")) #在economic数据中，以date为ID，处理两列
head(emp)
qplot(date,value,data = emp,geom = "line",colour=variable)
#改进；将数据标度调整到相同的范围或者使用自由标度
rang01=function(x){  #定义无量纲化的函数
  rng=range(x,na.rm = T)  #求出数据的最小最大值，即数据的作用区间
  (x-rng[1])/diff(rng)
}
emp2=ddply(emp,.(variable),transform,value=rang01(value)) #按variable分类别各自进行标准化
qplot(date,value,data =emp2,geom = "line",colour=variable,linetype=variable) #改进绘图一，更改数据
qplot(date,value,data = emp,geom = "line")+facet_grid(variable~.,scales ="free_y") #分面后，调整y轴

library(XML)
library(reshape2)
##平行坐标图
#将数据化为“长”数据之后，就可以做平行坐标图了
#透明度和聚类
###绘图实例5
head(diamonds);str(diamonds)
expen=subset(diamonds,price>10000)  #选取其中价格大于10000的钻石
dim(expen)  #由于对所有的数据都进行了长化处理，故数据成了5220*10这么多了
expen$.row=expen[,4] #以clarity作为id进行长化处理
head(expen)
molten=melt(expen,id=".row")
head(molten);dim(molten)
#化为长数据之后，画出折线,增加扰动以及增加透明度来更好展示数据分布
pcp=ggplot(molten,aes(variable,value,group=.row))
pcp+geom_line()
pcp+geom_line(colour="black",alpha=1/20) #添加透明度
jit=position_jitter(width = .25,height = 2.5)
pcp+geom_line(position = jit) #添加随机扰动项
pcp+geom_line(colour="black",alpha=1/20,position = jit) #透明度外加随机扰动
#图形改进，对电影进行聚类
head(expen)
cl=kmeans(expen[,8:10],5) #聚为5类
expen$cluster=reorder(factor(cl$cluster),expen$price)#按价格对cluster进行排序
head(expen)
levels(expen$cluster)=seq_along(levels(expen$cluster)) 
molten=melt(expen,id=c(".row","cluster"))
head(molten);dim(molten)
##可视化聚类结果,将不同的类标为不同的颜色
pcp_cl=ggplot(molten,aes(variable,value,group=.row,colour=cluster))
pcp_cl+geom_line(position = "jitter",alpha=1/5)
library(devtools)
pcp_cl+stat_summary(aes(group=cluster),fun.y=mean,geom="line")
#查看聚类的结果
pcp_cl+geom_line(position = jit,colour="black",alpha=1/5)+
  facet_wrap(~cluster)  #采用分面绘图，按cluster结果进行分面

###ggplot方法
#对于非数据框类别的数据，通过fortify协助完成整个绘图过程
#fortify即把模型和数据结合起来的想法：模型预测，扩展数据，数据修补，诊断模型等
#线性模型
library(stats)
plot(lm(cty~displ,data=mpg)) #一个简单线性模型的输出结果
qplot(displ,cty,data = mpg)+geom_smooth(method = "lm")
#画出标准化残差图以及用点的大小代替cook距离
mod=lm(cty~displ,data = mpg)
basic=ggplot(mod,aes(.fitted,.resid))+  #绘制残差拟合图
  geom_hline(yintercept = 0,colour="grey50",size=.5)+  #添加一条直线y=0
  geom_point()+
  geom_smooth(size=.5,se=F) #画出平滑带
basic
basic+aes(y=.stdresid) #更改，以标准化残差代替残差作图
basic+aes(size=.cooksd)+scale_size_area("Cook's distance") #以点的大小代替cook距离
#增补数据集，改进模型
full=basic%+%fortify(mod,mpg)  #将mpg数据和mod模型结合,并放进basic图中
head(full)
full+aes(colour=factor(cyl))
full+aes(displ,colour=factor(cyl)) #以displ作为x轴

###编写自己的fortify方法，实例实例
fortify.Image=function(model,data,...){  #将一个已有的图片添加到图中
  colours=channel(model,"x11")
  colours=colours[,rev(seq_len(ncol(colours)))]
  melt(colours,c("x","y"))
}
#安装R包
library(ebimag)
library(reshape2)
library(ggplot2)
img=readImage("http://had.co.nz/me.jpg")
qplot(x,y,data = img,fill=value,geom = "tile")+
  scale_fill_identity()+coord_equal()
