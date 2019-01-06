###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第三讲使用图层构建图像
###对于图层，几何对象，统计变换，位置调整如何调用和自定义
library(ggplot2)
p=ggplot(diamonds,aes(carat,price,colour=cut))
#p=p+layer(geom = "point",stat = "identity", position = "identity")
p+geom_point() #上述绘图函数的快捷函数此处可以直接作出散点图,另外可以根据geom_point的调整绘图结果


###绘图实例一
##layer(geom,params,stat,params,data,mapping,position)   参数展示
p=ggplot(diamonds,aes(x=carat)) #注意，参数有略微调整，故需以以下可运行部分为准
p+layer(
  geom = "bar",
  #  geom_params=list(fill="steelblue"),
  stat = "bin",params = list(fill="steelblue",binwidth=1),
  #  stat_params=list(binwidth=2)
  position = "identity"
)
p=ggplot(diamonds,aes(x=carat)) #注意，参数由略微调整，故需以以下可运行部分为准
p+geom_histogram(binwidth = 2,fill="steelblue") ##上述绘图的快捷函数

###运行实例2
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggplot(mpg, aes(displ, hwy)) +
  layer(geom = "point", stat = "identity", position = "identity",
        params = list(na.rm = FALSE)
  )
ggplot(mpg, aes(displ, hwy)) +
  layer(geom = "point", stat = "identity", position = "identity",
        data = head, params = list(na.rm = FALSE)
  )



###图层的定义，内容深究
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()
#等价
qplot(sleep_rem/sleep_total,awake,data=msleep)
##给gplot添加图层
qplot(sleep_rem/sleep_total,awake,data=msleep)+geom_smooth()
#等价
qplot(sleep_rem/sleep_total,awake,data=msleep,geom=c("point","smooth"))
#等价
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()+geom_smooth()

##查看图形对象的结果而不绘图
p=ggplot(msleep,aes(sleep_rem/sleep_total,awake))
summary(p)
p=p+geom_point()
summary(p)
##将图层储存到变量中应用实例
##创建一个图层，然后需要时调用
library(scales)
bestfit=geom_smooth(method = "lm",se=F,
                    colour=alpha("steelblue",.5),size=2)
qplot(sleep_rem,sleep_total,data = msleep)+bestfit
qplot(awake,brainwt,data = msleep,log = "y")+bestfit
qplot(bodywt,brainwt,data=msleep,log = "xy")+bestfit

###数据#数据必须是数据框
p=ggplot(mtcars,aes(mpg,wt,colour=cyl))+geom_point()
p
mtcars=transform(mtcars,mpg=mpg^2)  ##数据转换
p%+%mtcars  ##添加不同的数据集绘图

###图与图层
p=ggplot(mtcars)
summary(p)
p=p+aes(wt,hp)
summary(p)
###实例二
p=ggplot(mtcars,aes(x=mpg,y=wt))
p+geom_point()
p+geom_point(aes(colour=factor(cyl)))
p+geom_point(aes(y=disp)) #一个图层的设定仅对该图层起作用

###设定和映射
p=ggplot(mtcars,aes(mpg,wt))
p+geom_point(colour="darkblue") #将颜色设定为新颜色
p+geom_point(aes(colour="darkblue")) #将颜色映射为新颜色
###分组，分组图形属性
library(nlme)
head(Oxboys)
p=ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()
p
p=ggplot(Oxboys,aes(age,height,group=1))+geom_line() ##错误分组
p
#不同图层上面的不同分组
p=ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()
p+geom_smooth(aes(group=Subject),method = "lm",se=F) #错误绘图
p+geom_smooth(aes(group=1),method = "lm",se=F)
##修改默认分组
boysbox=ggplot(Oxboys,aes(Occasion,height))+geom_boxplot() #当对两个作图时，一般都是箱形图
boysbox
boysbox+geom_line(aes(group=Subject),colour="#3366FF")
##匹配图形属性和图形对象
xgrid=with(df,seq(min(x),max(x),length.out = 50)) #定义一个数据框
interp=data.frame(
  x=xgrid,
  y=approx(df$x,df$y,xout = xgrid)$y
)
qplot(x,y,data = df,colour=colour,size=I(5))+geom_line(data = interp,size=2) #对特殊的数据匹配以不同的line

###几何对象（geom）
##见ggplot2:数据分析与图形艺术59页


###统计变换(stat)
##以某种给定的方式对数据信息进行汇总(乘车变量的名用"..  .."围起来)
ggplot(diamonds,aes(carat))+geom_histogram(aes(y=..density..),binwidth = .1)
##密度直方图
qplot(carat,..density..,data = diamonds,geom = "histogram",binwidth=.1)
p=stat_bin(data = diamonds,binwidth = .1)

###位置的调整（对图形位置进行微调）
qplot(carat,..density..,data = diamonds,geom = "histogram",binwidth=.1,position = "dodge")
qplot(carat,..density..,data = diamonds,geom = "histogram",binwidth=.1,position = "fill")

###整合
#结合几何对象和统计变换
d=ggplot(diamonds,aes(carat))+xlim(0,3)
d
d+stat_bin(aes(ymax=..count..),binwidth = .1,geom = "area") #频率多边形
d+stat_bin(aes(size=..density..),binwidth = .1,geom = "point",position = "identity")
d+stat_bin(aes(fill=..count..),binwidth = .1,geom = "tile",position = "identity") #热图
#显示已计算过的统计量
###一个较为全面的例子
library(nlme)
#对于数据nlme中的oxboys数据集我们，拟合一个截距和斜率都包含随机效应的混合模型
require(nlme,quiet=T,warn.conflicts = F)
model=lme(height~age,data = Oxboys,random = ~1+age|Subject)
summary(model)  #以预测的生长轨迹和实际的生长轨迹对比
oplot=ggplot(Oxboys,aes(age,height,group=Subject))+geom_line()
oplot
###生成一个网格数据框，包含所有年龄和个体组合的
age_grid=seq(-1,1,length.out = 10)
subjects=unique(Oxboys$Subject)
preds=expand.grid(age=age_grid,Subject=subjects) #生成一个数据框
preds
preds$height=predict(model,preds) #将预测值导入到数据框中
head(preds)
###绘图作比较,看模型拟合的好坏
oplot+geom_line(data = preds,colour="#3366FF",size=.4)
###观察残差，比较模型拟合的好坏 
Oxboys$fitted=predict(model) #导入拟合值到原始数据框中
Oxboys$resid=with(Oxboys,fitted-height) #导入残差到原始数据框中
oplot%+%Oxboys+aes(y=resid)+geom_smooth(aes(group=1)) #作出残差图，并加上一条光滑曲线


###将模型添加一个二次项效应，进一步分析
model2=update(model,height~age+I(age^2))  #更新模型，添加一个二次项
Oxboys$fitted=predict(model2) #导入拟合值到原始数据框中
Oxboys$resid=with(Oxboys,fitted-height) #导入残差到原始数据框中
oplot%+%Oxboys+aes(y=resid)+geom_smooth(aes(group=1)) #作出残差图，并加上一条光滑曲线

