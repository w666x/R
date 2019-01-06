##R语言构建配对交易量化模型
#
set.seed(1) #设置随机种子
dates=as.Date('2010-01-01')+1:100 #100个日期
head(dates)
x=round(rnorm(100,50,40),2) #随机生成x产品，100个正态分析的收盘价
y=round(rnorm(100,59,40),2) #随机生成y产品
df=data.frame(dates,x,y)
head(df)

#把数据进行可视化
library(ggplot2)
library(scales)
library(reshape2)
#数据转型
df2=melt(df,c('dates'))
head(df2)

#画图
g=ggplot(data = df2,aes(x=dates,y=value,colour=variable))
g=g+geom_line()
g=g+scale_x_date(date_breaks = "1 week",date_labels='%m-%d')
g=g+labs(x='date',y='price')
g

#计算差价
df$diff=df$x-df$y
#找到差值大于10的点
idx=which(df$diff>10)
idx=idx[-which(diff(idx)==1)-1]
#打印差价的索引值
idx

#打印前20个数据
head(df,20)

#当差价大于10时，做空x，当差价小于0时，平仓
xprof4it=df$x[4]-df$x[6];xprof4it

#查看x,y的差价图
plot(df$diff,type = 'l')

##用R语言实现配对交易
#加载工具包
library(xts)
library(TTR)

# 读取CSV数据文件
#读取数据的时候，可以自己构造模型，将后续的一些
#基础的数据整理步骤纳入其中
read<-function(file){ 
  df<-read.table(file=file,header=FALSE,sep = ",", na.strings = "NULL")  # 读文件
  names(df)<-c("date","Open","High","Low","Close")                       # 设置列名
  dl<-split(df,format(as.POSIXct(df$date),'%Y-%m-%d'))                   # 按日期分组
    
  lapply(dl,function(item){                                              # 换成xts类型数据
      xts(item[-1],order.by = as.POSIXct(item$date))
  })
}
cu=read("配对交易数据.txt")
cu

##配对交易模型的建立
#数据整理，对空值进行处理
# 用前值替换空值
xdf<-na.locf(xdf)

names(cu)
