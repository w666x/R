###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第五讲：标度，坐标轴和图例
#标度定义，控制着数据到图形属性的映射，例如大小，颜色，位置或者形状，坐标轴和图例
#经过三步执行标度；变换，训练和映射
#标度四种类型：位置标度；颜色标度；手动离散型标度和同一型标度
#坐标轴和图例
data(msleep);str(msleep);summary(msleep)
#对数据取对数或者开根号， 基本的统计变换是要有的，继而计算统计摘要
#训练：通过学习得到标度的定义域
#映射：执行映射函数
###实例
summary(mpg);str(mpg)
plot=qplot(cty,hwy,data = mpg)
plot
#更改x轴
plot+aes(x=drv)
#更改Y轴，另外，scale函数此处影响貌似不是很大
plot+aes(y=drv)+scale_y_discrete()
###实例2
#添加不同的标度或者修改默认标度的某些特征
p=qplot(sleep_total,sleep_cycle,data = msleep,colour=vore) #x,y,数据，按vore着色
p
#显式添加默认标度
p+scale_color_hue()
#修改默认标度的参数，改变图例的外观
p+scale_color_hue("What does\nit eat?",
                  breaks=c("herbi","carni","omni",NA),
                  labels=c("plants","meat","both","don't know"))
#使用一种不同的标度
p+scale_color_brewer(palette = "Set1")

###标度详解
##通用参数
#name,设置坐标轴或者图例上出现的标签
str(mpg)
p=qplot(cty,hwy,data = mpg,colour=displ)
p
p+scale_x_continuous("City mpg")
p+xlab("City mpg1")
p+ylab("Highway mpg")
p+labs(x="city mpg",y="highway",colour="Displacement")
p+xlab(expression(frac(miles,gallon)))
#limits固定标度的定义域，影响显示在图形上面的元素
#breaks,labels;breaks控制显示在坐标轴或图例上面的值，labels确定了显示的标签
##绘图实例3
p=qplot(cyl,wt,data = mtcars)
p
p+scale_x_continuous(breaks = c(5.5,6))#控制显示坐标轴上面的值
p+scale_x_continuous(limits = c(5.5,6)) #控制显示图形
p=qplot(wt,cyl,data = mtcars,colour=cyl)
p
p+scale_colour_gradient(breaks = c(5.5,6))#控制显示颜色上面的值
p+scale_colour_gradient(limits = c(5.5,6)) #控制显示图形的颜色

###位置标度
#ggplot2提供了离散型，连续型以及日期型标度
p=qplot(cyl,wt,data = mtcars)
p
p+xlim(5,6) #快捷方式，修改坐标轴的范围
p+xlim("a","b","c") #快捷方式，离散型标度
p+xlim(as.Date(c("2008-05-01","2008-08-01"))) #一个从五月一号到八月一号的日期型标度
#连续型（可通过trans变换来修改，trans对任意连续型标度均有效）
##scale_x_log10()与scale_x_continuous(trans = "log10") 是等价的，前一个是后一个的快捷方式
##绘图实例4
qplot(log10(carat),log10(price),data = diamonds) #更改了相应的坐标轴名
qplot(carat,price,data = diamonds)+scale_x_log10()+scale_y_log10()#没有更改坐标轴名
#日期和时间（仅支持date类以及POSIXit类时间值）
#major以及minor确定时间的单位以及断点；format确定刻度标签的格式
#绘图实例5
str(economics);summary(economics)
library(scales)
plot=qplot(date,psavert,data = economics,geom = "line")+
  ylab("Persenal savings rate")+geom_hline(yintercept=0,colour="grey50") #添加了一条灰色的线
plot
plot+scale_x_date(breaks = date_breaks("10 years")) #设置了新的时间计量单位
plot+scale_x_date(
  limits = as.Date(c("2004-01-01","2005-01-01")), #取出其中2004年的这一段绘图
  labels = date_format("%Y-%m-%d") #设置了新的格式
)

###颜色标度
#色相，为0到360之间的额一个值
#明度，指颜色的明暗程度
#彩度，指色彩的纯度
#绘图实例6
str(faithful);summary(faithful)
f2d=with(faithful,MASS::kde2d(eruptions,waiting,h=c(1,10),n=50)) #生成的是一个grid项
class(f2d);length(f2d)
df=with(f2d,cbind(expand.grid(x,y),as.vector(z))) #数据类型处理
names(df)=c("eruptions","waiting","density")
erupt=ggplot(df,aes(waiting,eruptions,fill=density))+ #按照z作为填充的元素
  geom_tile()+scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
erupt+scale_fill_gradient(limits=c(0,.04))
erupt+scale_fill_gradient(limits=c(0,.04),low = "white",high = "black")
erupt+scale_fill_gradient2(limits=c(-.04,.04),midpoint = mean(df$density))
##绘图实例7
library(vcd)
library(colorspace) #添加颜色包
fill_gradn=function(pal){
  scale_fill_gradientn(colours=pal(7),limits=c(0,.04))
}
erupt+fill_gradn(rainbow_hcl)
erupt+fill_gradn(diverge_hcl)
erupt+fill_gradn(heat_hcl)

#离散型
#离散数据可以自动选择颜色或者从手工甄选中的颜色集中选择颜色
RColorBrewer::display.brewer.all() #列出所有的调色板
#绘图实例8
str(msleep);summary(msleep)
point=qplot(brainwt,bodywt,data = msleep,log = "xy",colour=vore)
area=qplot(log10(brainwt),data = msleep,fill=vore,binwidth=1)
point+scale_color_brewer(palette = "set1")
point+scale_color_brewer(palette = "set2") #调用不同的调色板作图
point+scale_color_brewer(palette = "Pastel1")
area+scale_fill_brewer(palette = "set1")
area+scale_fill_brewer(palette = "Set1") #区分大小写，这两个不一样哦
area+scale_fill_brewer(palette = "Pastel1")

##手动离散型标度
#手动创建自定义颜色标度
##绘图实例9
levels(as.factor(msleep$vore))
plot=qplot(brainwt,bodywt,data = msleep,log = "xy")
plot+aes(colour=vore)+scale_color_manual(values = c("red","orange", #默认情况下按照水平的先后次序进行排序
                                                    "yellow","green","blue"))
colours=c(carni="red","NA"="orange",insecti="yellow",herbi="green",omni="blue")#确定了顺序
plot+aes(colour=vore)+scale_color_manual(values=colours)
plot+aes(shape=vore)+scale_shape_manual(values = c(1,2,6,0,23))
#绘图实例10；展示多个变量，并显示一个有用的图例
huron=data.frame(year=1875:1972,level=LakeHuron)
head(huron)
ggplot(huron,aes(year))+geom_line(aes(y=level-5),colour="blue")+
  geom_line(aes(y=level+5),colour="red")
#添加图例
huron=data.frame(year=1875:1972,level=LakeHuron)
ggplot(huron,aes(year))+geom_line(aes(y=level-5,colour="below"))+
  geom_line(aes(y=level+5,colour="above"))
#改进(自定义更改绘图颜色)
ggplot(huron,aes(year))+geom_line(aes(y=level-5,colour="below"))+
  geom_line(aes(y=level+5,colour="above"))+scale_color_manual("Direction",
                                                              values = c("below"="blue","above"="red"))

###坐标轴和图例
