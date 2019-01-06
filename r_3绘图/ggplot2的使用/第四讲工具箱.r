###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第四讲工具箱
##列举了大量的统计变换和几何对象的一部分，辅助构建表现力更强的图形
#基本的图形类型；展示分布；应对散点图中的遮盖绘制问题；绘制曲线图；统计汇总；
#绘制地图；揭示数据中的不确定性和误差；为图形添加注解；绘制加权的数据
##用以展示数据本身
##用以展示数据的统计摘要
##用以添加额外的元数据，上下文信息和注解

###基本图形类型
df=data.frame(
  x=c(3,1,5),
  y=c(2,4,6),
  label=c("a","b","c")
)
p=ggplot(df,aes(x,y))+xlab(NULL)+ylab(NULL) #绘制底层
p
p+geom_area()+labs(title="geom_area") #面积图
p+geom_bar(stat = "identity")+labs(title="geom_bar(stat=\"identity\")") #此处需要加上斜杠
p+geom_line()+labs(title="线条图")
p+geom_point()+labs(title="散点图")
p+geom_polygon()+labs(title="绘制多边形")
p+geom_path()+labs(title="路径图")
p+geom_text(aes(label=label))+labs(title="添加标签")
p+geom_tile()+labs(title="绘制深色图或者水平图")

##展示数据分布
#分布的维数，分布的类型，以及条件分布或者联合分布
depth_dist=ggplot(diamonds,aes(depth))+xlim(58,68)
depth_dist+geom_histogram(binwidth = .1)
depth_dist+geom_histogram(aes(y=..density..),binwidth = .1)+facet_grid(cut~.)#进行分布的跨组比较一
depth_dist+geom_histogram(aes(fill=cut),binwidth = .1,position = "fill")#绘制条件密度图
depth_dist+geom_freqpoly(aes(y=..density..,colour=cut),binwidth=.1)#使用频率多边形，进行跨组比较二
#箱线图也可以对连续型变量取条件
library(plyr)
qplot(cut,depth,data = diamonds,geom = "boxplot") #按cut分类,此即为类别型变量
qplot(carat,depth,data = diamonds,geom = "boxplot", #对于连续型变量carat，我们需要对其进行封箱
      group=round_any(carat,.1,floor),xlim = c(0,3)) #即对carat以.1个单位为大小封箱
#应对数据遮盖问题
#geom_jitter=position_jitter+geom_point
qplot(class,cty,data = mpg,geom = "jitter") #绘制添加了随机扰动的散点图
qplot(class,cty,data = mpg)
qplot(class,drv,data = mpg)  #添加随机扰动是为了应对散点图中的数据遮盖问题
qplot(class,drv,data = mpg)+geom_jitter() #绘制添加了随机扰动的散点图
#核平滑方法得到频率多边形
qplot(depth,data = diamonds,geom = "density",xlim = c(54,70))
qplot(depth,data = diamonds,geom = "density",xlim = c(54,70),fill=cut,alpha=I(.2)) #按cut绘色

##处理遮盖绘制问题
#小规模的遮盖绘制问题（通过调整点的大小或者中空的符号）
df=data.frame(x=rnorm(2000),y=rnorm(2000))
norm=ggplot(df,aes(x,y))
norm+geom_point()
norm+geom_point(shape=1)
norm+geom_point(shape=".")
#对于更大的数据集（通过调整透明度）
norm+geom_point(colour="black",alpha=1/3)
norm+geom_point(colour="black",alpha=1/5)
norm+geom_point(colour="black",alpha=1/10)
#如果数据具有一定的离散型（通过加入一定的随机扰动项）
td=ggplot(diamonds,aes(table,depth))+xlim(50,70)+ylim(50,70)
td+geom_point()
td+geom_jitter()#添加随机扰动项
jit=position_jitter(width = .5) #设置扰动参数为.5
td+geom_jitter(position = jit)
td+geom_jitter(position = jit,colour="black",alpha=1/10) #随机扰动项和透明度混合作用
td+geom_jitter(position = jit,colour="black",alpha=1/50) #随机扰动项和透明度混合作用

#将遮盖绘制问题转化为二维核密度估计问题
#将点分箱并统计每个箱中点的数量，然后通过某种方法可视化这个数量
d=ggplot(diamonds,aes(carat,price))+xlim(1,3)+theme(legend.position="none")#不显示图例
d+stat_bin2d()
d+stat_bin2d(bins = 10)
d+stat_bin2d(binwidth = c(.02,200)) #binwidth控制箱的大小
library(hexbin)
d+stat_binhex() #六边形箱的效果
d+stat_binhex(bins = 10)
d+stat_binhex(binwidth = c(.02,200))
#作二维密度估计，并辅之以等高线。或以着色瓦片；或使用大小与分布密度成比例的点
d=ggplot(diamonds,aes(carat,price))+xlim(1,3)+theme(legend.position="none")
d+geom_point()
d+geom_point()+geom_density2d()
d+stat_density2d(geom = "point",aes(size=..density..),contour = F)+scale_size_area()
d+stat_density2d(geom = "tile",aes(fill=..density..),contour = F)
last_plot()+scale_fill_gradient(limits=c(1e-5,8e-4))

###绘制地图
library(maps)
data("countyMapEnv")
data("us.cities")
str(us.cities)
head(us.cities);dim(us.cities)
big_cities=subset(us.cities,pop>500000);dim(big_cities)
qplot(long,lat,data = big_cities) #borders是展示了地图数据
qplot(long,lat,data = big_cities)+borders("state",size=.5)
tx_cities=subset(us.cities,country.etc=="TX");dim(tx_cities);head(tx_cities) #选取tx
ggplot(tx_cities,aes(long,lat))+borders("county","texas",colour="grey70")+
  geom_point(colour="black",alpha=.5)
##绘制等值线图
#将地图数据转化为数据框,数据框通过merge操作可以进行数据融合，最终画出等值线
library(maps)
?map_data ##将地图数据转化为数据框
states=map_data("state");head(states);dim(states)
arrests=USArrests;head(arrests);dim(arrests)
names(arrests)=tolower(names(arrests)) #将名字都换成小写的
arrests$region=tolower(rownames(USArrests)) #将行名转化成数据框中的一列
head(arrests)
choro=merge(states,arrests,by="region") #将数据融合,以merge
#由于绘制多边形时，涉及顺序问题
#而且，使用merge函数时，破坏了原始顺序，故将行重新排序
choro=choro[order(choro$order),] #可以用图形很好的展示数据
qplot(long,lat,data = choro,group=group,fill=assault/murder,geom = "polygon") #fill函数，对图形上色
qplot(long,lat,data = choro,group=group,fill=assault,geom = "polygon")
###进一步处理
library(plyr)
ia=map_data("county","iowa") #将地图数据转化为数据框
mid_range=function(x) mean(range(x,na.rm = T))
centres=ddply(ia,.(subregion),colwise(mid_range,.(lat,long)))
ggplot(ia,aes(long,lat))+geom_polygon(aes(group=group),fill=NA,colour="grey60")+ #多边形图
  geom_text(aes(label=subregion),data = centres,size=2,angle=45) #加入图例

###揭示不确定性
#双因素含交互效应的回归模型，展示了如何提取边际效应以及条件效应，以及将其可视化
d=subset(diamonds,carat<2.5& rbinom(nrow(diamonds),1,.2)==1) 
d$lcarat=log10(d$carat)
d$lprice=log10(d$price)
head(d)
###消除整体的线性趋势
detrend=lm(lprice~lcarat,data = d)
d$lprice2=resid(detrend)
head(d)
mod=lm(lprice2~lcarat*color,data = d) #使用祛除了趋势性的数据作线性回归
library(effects)
effect.df=function(...){
  suppressWarnings(as.data.frame(effect(...)))
}
color=effect.df("color",mod)
both1=effect.df("lcarat:color",mod)
carat=effect.df("lcarat",mod,default.levels=50)
both2=effect.df("lcarat:color",mod,default.levels=3)
##开始作图一
qplot(lcarat,lprice,data = d,colour=color) #祛除了非线性性
qplot(lcarat,lprice2,data = d,colour=color) #祛除了数据的线性趋势
##开始作图二
fplot=ggplot(mapping = aes(y=fit,ymin=lower,ymax=upper))+ylim(range(both2$lower,both2$upper))
fplot%+%color+aes(x=color)+geom_point()+geom_errorbar() #展示结果中变量color的不确定性
fplot%+%both2+   #carat的不同水平下，color的条件效应
  aes(x=color,colour=lcarat,group=interaction(color,lcarat))+
  geom_errorbar()+geom_line(aes(group=lcarat))+scale_color_gradient()
##开始作图三
fplot%+%carat+aes(x=lcarat)+geom_smooth(stat = "identity") #展示结果中，carat的边际效应
##开始作图四
ends=subset(both1,lcarat==max(lcarat))
head(ends)
fplot%+%both1+aes(x=lcarat,colour=color)+
  geom_smooth(stat = "identity")+scale_color_hue()+
  theme(legend.position="none")+geom_text(aes(label=color,x=lcarat+.02),ends)


##统计摘要
#单独的摘要计算函数  ###(这个地方差点东西的啊)
midm=function(x) mean(x,trim = .5)
d=subset(diamonds,carat<2.5& rbinom(nrow(diamonds),1,.2)==1) 
head(d)
m2=ggplot(aes(carat,price),data = diamonds)
m2+stat_summary(aes(colour="trimmed"),fun.y = midm,geom = "point")+
  stat_summary(aes(colour="raw"),fun.y = mean,geom = "point")+scale_color_hue("mean")
#统一的摘要计算函数
iqr=function(x,...){
  qs=quantile(as.numeric(x),c(.25,.75),na.rm = T)
  names(qs)=c("ymin","ymax")
  qs
}
m2+geom_point()
m2+stat_summary(fun.data = "iqr",geom = "ribbon")

##天假图形注解
#注解仅仅是额外的数据而已；以逐个添加或者批量添加的方式
unmep=qplot(date,unemploy,data = economics,geom = "line",
            xlab = "",ylab = "No.unemployed(1000s)")
presidential=presidential[-c(1:3),]
head(presidential)
yrng=range(economics$unemploy) #失业比例
xrng=range(economics$date)  #时间
unmep+geom_vline(aes(xintercept=as.numeric(start)),
                 data = presidential)
library(scales)  ##总统任期分期
unmep+geom_rect(aes(NULL,NULL,xmin=start,xmax=end,fill=party),
                ymin=yrng[1],ymax=yrng[2],data = presidential,alpha=.2)+
  scale_fill_manual(values = c("blue","red"))
##在上一幅图中导入总统名字
last_plot()+geom_text(aes(x=start,y=yrng[1],label=name),data = presidential,size=3,
                      hjust=0,vjust=0)+geom_point(data = highest,size=3,colour="red",alpha=.5)
##添加标题
caption=paste(strwrap("Unemployment rates in the US have varied a lot over the years",40),
              collapse = "\n")
unmep+geom_text(aes(x,y,label=caption),data = data.frame(x=xrng[2],y=yrng[2]),
                hjust=1,vjust=1,size=4)
##标记出最大值
highest=subset(economics,unemploy==max(unemploy))
unmep+geom_point(data = highest,size=3,colour="red",alpha=.5)


###含权数据
#使用size来改变点的大小显示权重
qplot(percwhite,percbelowpoverty,data = midwest)
qplot(percwhite,percbelowpoverty,data = midwest,size=poptotal/1e6)+ #size显示权重
  scale_size_area("population\n(millions)",breaks=c(.5,1,2,4)) #图例
qplot(percwhite,percbelowpoverty,data = midwest,size=poptotal/1e6)
#使用weight图形属性来修改权重
