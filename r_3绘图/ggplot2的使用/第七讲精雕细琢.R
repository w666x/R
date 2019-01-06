###ggplot2包的使用
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/ggplot2的使用")
library(ggplot2)
###我们使用图层layer来逐个定义绘图太过繁琐，所以我们有事通过快捷函数绘图
###第7讲：精雕细琢
#如何绘制高质量的图形
#图形的视觉呈现是由数据和非数据部分决定的；我们将看到非数据展示部分，默认参数更改
#将图形转化到LaTeX，word等软件中，另外，单页显示多幅图形
##主题
#主题系统包括标题，坐标轴标签，图例标签的文字调整，网格线，背景，轴须的颜色搭配
#内置主题theme_gray以及thtme_bw
#绘图实例1
theme_set(theme_gray()) #主题设置为灰白
p=qplot(disp,wt,data = mtcars)+geom_smooth()
p
theme_set(theme_bw()) #主题设置为白黑
p
previous_theme=theme_set(theme_bw())  #将设置的一个主题保存到previous中
p
theme_set(previous_theme)  ##永久性储存初始主题
#主题元素和元素函数
#控制主题元素外观的函数为元素函数
#element_text绘制标签和标题
#绘图实例2，主要是更改其字体设置
p=qplot(disp,wt,data = mtcars)+geom_smooth()
p=p+labs(title="This is a smooth")
p
p+theme(plot.title=element_text(size = 20)) #更改字体大小
p+theme(plot.title=element_text(size = 20,colour="red")) #更改颜色
p+theme(plot.title=element_text(size = 20,hjust = 0)) #字体缩进0
p+theme(plot.title=element_text(size = 20,face = "bold"))
p+theme(plot.title=element_text(size = 20,angle = 180)) #字体调整180度
#element_line，绘制线条或者线段,对相关的主题元素运用相应的函数
p+theme(panel.grid.major=element_line(colour = "red")) #添加红色网格线
p+theme(panel.grid.major=element_line(size=2)) #网格线间距变大
p+theme(panel.grid.major=element_line(linetype = "dotted"))  #此处修改的都是对应有标度的网格线
p+theme(axis.line=element_line()) #直线和坐标轴
p+theme(axis.line=element_line(colour = "red"))
p+theme(axis.line=element_line(size = .5,linetype = "dashed"))
#element_rect绘制主要供背景使用的矩形
p+theme(plot.background=element_rect(fill = "grey80",colour = "red")) #绘制灰色背景
p+theme(plot.background=element_rect(size =2))
p+theme(plot.background=element_rect(colour = "red")) #设置边框颜色
p+theme(plot.background=element_rect())
p+theme(plot.background=element_rect(colour = NA))
p+theme(plot.background=element_rect(linetype = "dotted",colour = "red")) #边框颜色及类型设置
#element_blank表示空主题,可以删除部分绘图元素
p#逐步删除非数据元素
last_plot()+theme(panel.grid.minor=element_blank())  #对次网格线进行处理，即消除
last_plot()+theme(panel.grid.major=element_blank())  #对主网格线进行处理
last_plot()+theme(panel.background=element_blank())  #消除面板背景
last_plot()+theme(axis.title.x=element_blank(),axis.title.y=element_blank())  #消除坐标轴
last_plot()+theme(axis.line=element_line()) 
#theme_get可以获得当前主题
old.theme=theme_update(  #定义一个新的主题
  plot.background=element_rect(fill = "#3366FF"), #图形背景
  panel.background=element_rect(fill = "#003DF5"), #面板背景
  axis.text.x=element_text(colour = "#CCFF33"),  #x轴标签
  axis.text.y=element_text(colour = "#CCFF33",hjust = 1),
  axis.title.x=element_text(colour = "#CCFF33",face = "bold"),  #水平轴标题
  axis.title.y=element_text(colour = "#CCFF33",face = "bold",angle = 90) 
)
qplot(cut,data = diamonds,geom = "bar")
qplot(cty,hwy,data = mpg)
theme_set(old.theme)
theme_set(theme_bw()) #将主题设置为黑白主题


###自定义标度和几何对象
theme_set(theme_gray())
p=qplot(mpg,wt,data = mtcars,colour=factor(cyl))
p
scale_color_discrete=scale_color_brewer
p
#几何对象和统计变换
#update_geom_defaults和update_stat_defaults定义几何对象和统计变换
update_geom_defaults("point",aes(colour="darkblue")) #设置为点以及蓝色
qplot(mpg,wt,data = mtcars)
update_stat_defaults("bin",aes(y=..density..)) #设置统计变换为bin
qplot(rating,data = movies,geom = "histogram",binwidth=1)


###存储以及输出
#矢量图和光栅图
qplot(mpg,wt,data = mtcars)
ggsave(file="图形存储.pdf")
qplot(mpg,wt,data = mtcars)
dev.new()


###一页多图
#视图窗口；创建图形并将图形赋值为变量
a=qplot(date,unemploy,data = economics,geom = "line")
b=qplot(uempmed,unemploy,data = economics)+geom_smooth(se=F)
c=qplot(uempmed,unemploy,data = economics,geom = "path")
##子图；将子图嵌入到主图的顶部
##绘图实例
library(grid)
##占据整个图形设备的视图窗口
vp1=viewport(width = 1,height = 1,x=.5,y=.5)
vp1=viewport()
#占了一半图形设备的视图窗口;定位在图形中间位置
vp2=viewport(width = .5,height =.5,x=.5,y=.5)
vp1=viewport(width = .5,height = .5)
#一个2*3的视图窗口，定位在设备的中心
vp3=viewport(width = unit(2,"cm"),height = unit(3,"cm"))
#实例2,绘制子图，并保存到PDF中
pdf("主子图实例.pdf",width = 4,height = 4) #开始保存到PDF中
subvp=viewport(width = .4,height = .4,x=.75,y=.35)
b  #主图
print(c,vp=subvp) #添加子图
dev.off()  #此命令是将图形保存到PDF中的执行命令之一
ggsave(file="主子图实例.pdf") #这个仅仅保存上面最新出现的图形
#实例3，图形微调
csmall=c+theme_gray(9)+  #c小图,颜色为灰色，去掉坐标轴
  labs(x=NULL,y=NULL)+theme(plot.margin=unit(rep(0,4),"lines"))
pdf("主子图实例新.pdf",width=4,height=4)  #此处的命名貌似不能含有字母或者数字
b
print(csmall,vp=subvp)
dev.off()

##矩形网格
#grid.layout 设置了一个任意高和宽的视图窗口网格
pdf("矩形网格绘图.pdf",width = 8,height = 6)
grid.newpage()  #在新的一页上面操作
pushViewport(viewport(layout = grid.layout(2,2))) #定义一个2*2的网格
vplayout=function(x,y)  #定义后面的视图是以何种方式选取的
  viewport(layout.pos.row = x,layout.pos.col = y)
print(a,vp=vplayout(1,1:2)) #x=1,y=1:2
print(b,vp=vplayout(2,1))  #x=2,y=1
print(c,vp=vplayout(2,2))  #x=2,y=2
dev.off()
