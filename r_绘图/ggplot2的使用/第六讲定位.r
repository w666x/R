###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第6讲；定位
##主讲页面布局和坐标系如何工作
#位置调整；位置标度；分面；坐标系
###对于分面；网格型；封装型
#使用mpg数据集为例
head(mpg);summary(mpg);str(mpg)
mpg2=subset(mpg,cyl!=5 & drv %in% c("4","f")) #选取部分数据，其中drv应该为4，f
head(mpg2);dim(mpg2)
levels(factor(mpg2$drv))
#网格分面
#不进行分面
qplot(cty,hwy,data = mpg2)
qplot(cty,hwy,data = mpg2)+facet_null() #二者效果一样，不进行分面
#一行多列
qplot(cty,hwy,data = mpg2)+facet_grid(.~cyl) #按照cyl分列，一行多列
#一列多行
qplot(cty,data = mpg2,geom = "histogram",binwidth=2)+
  facet_grid(cyl~.)
#多行多列
qplot(cty,hwy,data = mpg2)+facet_grid(drv~cyl) 

##边际图
#类似于列联表中的边际和
p=qplot(displ,hwy,data = mpg2)+geom_smooth(method = "lm",se=F)
p+facet_grid(cyl~drv)
p+facet_grid(cyl~drv,margins = T) #绘出边际图
#调整(绘制彩色平滑线)
qplot(displ,hwy,data = mpg2)+geom_smooth(aes(colour=drv),method = "lm",se=F)+
  facet_grid(cyl~drv,margins = T)

###封装分面 facet_warp
library(ggplot2)
library(scales)
library(plyr)

#标度控制
p=qplot(cty,hwy,data = mpg)
p+facet_wrap(~cyl)
p+facet_wrap(~cyl,scales = "free") #允许标度变化
##绘图实例2
library(reshape2)
em=melt(economics,id="date")
head(em);dim(em);str(em)
qplot(date,value,data = em,geom = "line",group=variable)+ #按variable分群
  facet_grid(variable~.,scales = "free_y") #y可以自由变动，x不行，这是网格分面所不能办到的
##绘图实例3
mpg3=within(mpg2,{ 
  model=reorder(model,cty)  ##可以使model和manufacture按照cty重新排序
  manufacturer=reorder(manufacturer,cty)
})
head(mpg3)
models=qplot(cty,model,data = mpg3)
models
models+facet_grid(manufacturer~.,scales = "free",  #解放了scale值，space高度与标度范围成正比
                  space = "free")+theme(strip.text.y=element_text())

#分面变量缺失，即当一个新添加的图层没有你所需要的分面的变量时
#分组和分面，以及两者优缺点的互补
xmaj=c(.3,.5,1,3,5)
xmin=as.vector(outer(1:10,10^c(-1,0))) #求取外积
ymaj=c(500,1000,5000,10000)
ymin=as.vector(outer(1:10,10^c(2,3,4))) #求取外积
dplot=ggplot(subset(diamonds,color %in% c("D","E","G","J")), #选取数据
             aes(carat,price,colour=color))+
  scale_x_log10(breaks=xmaj,labels=xmaj,minor=xmin)+ #按照xmaj标度
  scale_y_log10(breaks=ymaj,labels=ymaj,minor=ymin)+
  scale_color_hue(limits=levels(diamonds$color))+
  theme(legend.position="none")
dplot+geom_point()   #总的图
dplot+geom_point()+facet_grid(.~color) #按color分面
dplot+geom_smooth(method=lm,se=F,fullrange=T)  #回归线平滑拟合
dplot+geom_smooth(method = lm,se=F,fullrange=T)+facet_grid(.~color) #按颜色对拟合的平滑回归线分面

###并列和分面
#区别：标注方式上面的差异：分面图形在上方有颜色的标注，下面有切工的标注
#并列图形，下放有颜色的标注，但是无切工的标注
qplot(color,data = diamonds,geom = "bar",fill=cut,position = "dodge") #画出并列图
qplot(cut,data = diamonds,geom = "bar",fill=cut)+ #分面外加切工的标注信息
  facet_grid(.~color)+theme(axis.text.x=element_text(angle=90,hjust = 1,size = 8 #画出分面图
                                                     ,colour = "grey50"))
#当变量交叉且有缺失值时
mpg4=subset(mpg,manufacturer %in% c("audi","volkswagen","jeep"))
head(mpg4);str(mpg4)
mpg4$manufacturer=as.character(mpg4$manufacturer)
mpg4$model=as.character(mpg4$model) #此处的数据操作貌似没有多大用处
head(mpg4);str(mpg4)
base=ggplot(mpg4,aes(fill=model))+  #构建基础图层
  geom_bar(position = "dodge")+ #绘出条形图
  theme(legend.position="none")
base+aes(x=model)+facet_grid(.~manufacturer) #分面绘图
last_plot()+facet_grid(.~manufacturer,scales = "free_x",space = "free")#相比上图而言，x的坐标更随意
base+aes(x=manufacturer)  #分列绘图

###连续型变量
##对于连续型变量分面，我们首先将其离散化
#将数据分为n个长度相同的部分 cut_interval(x,n=10)
#将数据分为n个有相同数目点的部分 cut_number(x,n=10)
#绘图实例3
mpg2=subset(mpg,cyl!=5 & drv %in% c("4","f")) #选取部分数据，其中drv应该为4，f
mpg2$disp_ww=cut_interval(mpg2$displ,length = 1)
mpg2$disp_wn=cut_interval(mpg2$displ,n=6)
mpg2$disp_nn=cut_number(mpg2$displ,n=6)
head(mpg2)
plot=qplot(cty,hwy,data = mpg2)+labs(x=NULL,y=NULL)
plot+facet_wrap(~disp_ww,nrow = 1) #按上述情形离散化后画出的相应数据对应散点图的结果
plot+facet_wrap(~disp_wn,nrow = 1)
plot+facet_wrap(~disp_nn,nrow = 1)


###坐标系
#命令方式为 coord_可用坐标系名
##变换;坐标系变换将改变图形的几何形状
#几何形状的参数变化只依据定位，不考虑维度（即仅考虑可确定其的点的坐标）
#另外，可以假设在很短的距离内，直线坐标变换之间还是直线，这样，我们就可以将直线先“分割再组合”
#笛卡尔坐标系
#coord_cartesian() coord_equal() coord_flip() coord_trans()
#设置坐标系范围
#绘图实例4
p=qplot(disp,wt,data = mtcars)+geom_smooth()
p
p+scale_x_continuous(limits = c(325,500)) #范围外的数据都被删除了
p+coord_cartesian(xlim = c(325,500)) #使用的仍然是所有的数据，只不过图形放大了
d=ggplot(diamonds,aes(carat,price))+stat_bin_2d(bins = 25,colour="grey70")+
  theme(legend.position="none")  #绘制基本图层
d
d+scale_x_continuous(limits = c(0,2)) #使用标度范围设置
d+coord_cartesian(xlim = c(0,2))
#坐标轴翻转
qplot(displ,cty,data = mpg)+geom_smooth() 
qplot(cty,displ,data = mpg)+geom_smooth()  #仅仅将图形旋转90度
qplot(cty,displ,data = mpg)+geom_smooth()+coord_flip() #坐标轴翻转，以y为条件变量刻画x
#变换
#在标度层面和坐标系层面都可以进行数据变换coord_trans，
#其中坐标系发生在计算统计量之后，会改变几何形状
qplot(carat,price,data = diamonds,log = "xy")+geom_smooth(method = "lm") #标度对数变换后的散点图
library(scales)
last_plot()+coord_trans(x=exp_trans(10),y=exp_trans(10)) #坐标系变换

###非笛卡尔坐标系
#极坐标和地图投影
#堆叠条形图
pie=ggplot(mtcars,aes(x=factor(1),fill=factor(cyl)))+geom_bar(width = 1)
pie 
#饼图
pie+coord_polar(theta = "y")
#牛眼图
pie+coord_polar()
