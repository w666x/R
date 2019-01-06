#R语言中文社区
setwd("E:/Rexercise1/R语言中文社区")
#R环境下Echart的8种可视化
library(devtools)
install_github('yihui/recharts') #全局设定，下载了某些东西
Sys.setlocale("LC_CTYPE","Chs")
source("E:\\Rexercise1\\R语言中文社区\\数据\\echartR.R") #全局设定
library(knitr)
knitr::opts_chunk$set(message=FALSE,warning=FALSE,results='asis')

##读取数据以及数据预处理
#数据集说明：汽车贷款违约数据
#使用的变量N:数值变量/T:因变量
##[N]fico_score信用评分
##[N]purch_price 汽车价格
##[N]loan_amt  贷款金额
##[T]bad_ind    是否违约
#读取数据与数据预处理
data=read.csv('数据/accepts.csv')
head(data);dim(data);str(data);summary(data)
#将部分数据因子化
data$bad_ind=as.factor(data$bad_ind)
data$bankruptcy_ind=as.factor(data$bankruptcy_ind)
data$used_ind=as.factor(data$used_ind)
#缺失值填补(均值/众数填补)
tianbu=function(data){
  for (i in 1:ncol(data)){
    if (sum(is.na(data[,i]))!=0){ #即存在空值
      if (class(data[,i])=='factor'){ #如果是因子
        a=(as.data.frame(table(data[,i])))
        b=as.numeric(max(mode)) #统计后找出众数对应的数据
        mode=as.character(a[which(a$Freq==b),1])
        data[which(is.na(data[,i])=='TRUE'),i]=mode[1]
      } #因子使用众数填补
      else {
        data[which(is.na(data[,i])=='TRUE'),i]=mean(data[,i],na.rm=T)
      } #非因子使用平均数填补
    }
    else 
      warning(paste('变量',colnames(data)[i],'无缺失无需填补',sep=''))}
}
#进行填补
tianbu(data = data)
warnings() #即有部分变量无需填补
summary(data) #判断填补效果

##开始可视化分析
###多系列散点图
#echart语法见https://github.com/madlogos/recharts
#标注线(标注线性归回拟合线)
Line=t(c(1,'lm',"lm",F))
#标注点(标注两个异常值点)
Points=rbind(c(1,NA,1,111696,111166,F),c(1,NA,2,63700,28700,F))
#横纵轴设定
x=list(lab='汽车金额',color='blue',rotate=45)
y=list(lab='贷款额',color='blue',rotate=45)
#制图
echartR(data = data, x = ~purch_price, y = ~loan_amt,
        type = 'scatter', palette='aetnaorange',
        title = '散点图-汽车金额vs贷款额', 
        subtitle = "(source: 汽车贷款数据)",
        xAxis = x, yAxis = y,
        markLine=Line,
        markPoint=Points)

###多系列散点图
#标注点(标注两个异常值点)
Points=rbind(c(2,NA,1,760,96692,F),c(1,NA,2,632,111554,F))
#横纵轴设定
x=list(lab='信用评分',color='blue')
y=list(lab='贷款额',color='blue')
#制图
echartR(data = data, x = ~fico_score, y = ~loan_amt, series = ~bad_ind,
        type = 'scatter', palette='aetnaorange', symbolList='circle',
        scale=F, xAxis = x,yAxis = y,
        title = '多系列散点图-信用评分vs贷款额', 
        subtitle = "(source: 汽车贷款数据)",
        markPoint=Points)

###气泡图
#数据集说明：中国各省人口GDP和人均寿命数据
#使用的变量N:数值变量
#Prov:省份
#GDP:GDP
#LIFE:平均寿命
#POPULATION:人口
#读取数据
China=read.csv('数据/gdp.csv')
#Bubble 气泡图
echartR(data = China, x = ~Life, y = ~GDP, 
        weight =Population, series = ~Prov, 
        symbolList=c('circle'), 
        type = 'bubble', palette='aetnaorange',
        title = '部分省人均寿命-GDP-人口', 
        subtitle = '(source: GDP)', xlab = '平均寿命', ylab = 'GDP',
        pos=list(title=6,toolbox=3))


##柱状图
ibrary(reshape2)
#数据描述：某化妆品公司数据集
#dis:大区(series)
#type:产品名(x)
#amount销量(y)
#读取数据与数据预处理
data=read.csv('数据/col.csv')
head(data)
#Tiled Column柱状图
echartR(data = data, x = ~type, y = ~amount,  
        series = ~dis,stack=F,
        type = 'bar', palette='aetnaorange',
        title = "大区-产品-销量柱状图", 
        subtitle = '(source: col)',
        xlab = 'Parameter', ylab = '销售额')


###饼图
#数据准备
data=read.csv('数据/col.csv')
#1.ECHART PIE 饼图
echartR(data, x = ~type,  y = ~amount, type='pie',
        palette='aetnaorange', 
        title='化妆品产品饼图',
        subtitle = '(source: col)')
###环图
echartR(data, x = ~type,  y = ~amount, type='ring',
        palette='aetnaorange', 
        title='化妆品产品饼图',
        subtitle = '(source: col)')


###地图
#数据准备
dtgdp1=read.csv('数据/CHINA-GDP-12-14.csv')[2:4]
#区域标注
echartR(dtgdp1, x = ~Prov, y = ~GDP, series= ~Year, 
        type=c('map','china','area'),
        title="title",
        subtitle='(source: title)',
        dataRangePalette='heat(5)',
        dataRange=c('High',"Low"),splitNumber=5,
        pos=list(toolbox=3))
#区域标注
worldgdp=data.frame(
  country=c('United States of America','China','Japan','Germany',
            'United Kingdom','France','Brazil', 'Italy','India','Russia',
            'Canada','Australia','South Korea','Spain','Mexico','Indonesia',
            'Netherlands','Turkey','Saudi Arabia','Switzerland'),
  GDP=c(17418925,10380380,4616335,3859547,2945146,2846889,2353025,2147952,
        2049501,1857461,1788717,1444189,1416949,1406855,1282725,888648,866354,
        806108,752459,712050))

echartR(worldgdp, x = ~country, y = ~GDP, type=c('map','world','area'),
        title="Nations with top 20 GDPs, 2014 (Million USD)",
        subtitle = '(source: Wikipedia)',
        dataRangePalette='heat(5)', dataRange=c("High","Low"), 
        splitNumber=10, pos=list(toolbox=3))


#Point 点标注
#数据读取
chinapm=read.csv('数据/chinapm25.csv',stringsAsFactors=F,header=T,sep=',')[,2:5]
#点标注
top5=head(chinapm[order(chinapm$PM,decreasing=T),],5)
top5$Name="Top 5"
top5$effect=T
top5=top5[,c(5,1,2,4,3,6)]
#制图
echartR(chinapm, x=~City, y=~PM25, xcoord=~xcoord, ycoord=~ycoord,
        type=c('map','china','point'),
        title='中国各市PM2.5情况',
        subtitle="(source: chinapm)",
        dataRange=c("High","Low"), pos=list(toolbox=3), 
        dataRangePalette='heat(7)',
        splitNumber=7,
        markPoint=top5)

#Line 线标注
flight=read.csv('数据/flight.csv',stringsAsFactors=F,header=T,sep=',')
head(flight)
flight$y=''
names(flight)
echartR(flight, x=~From, x1=~To, y=~y, series=~From, xcoord=~Xcoord.x, ycoord=~Ycoord.x,
        xcoord1=~Xcoord.y, ycoord1=~Ycoord.y, type=c('map','china','line'),
        pos=list(toolbox=3), title="南方航空公司主要航班线路")

###词云
#获取数据
santi=read.csv('数据/santi.csv')
santi
#构建词云
echartR(santi[1:30,], x=~word, y=~TFIDF, type="wordcloud", 
        title="《三体》特征词", palette='aetnaorange', 
        subtitle="来源:三体")

###力导向布局图
wujiandao=read.csv('数据/netLink.csv',stringsAsFactors=F)
wujiandao
echartR(wujiandao,
        x=~Link,
        y=~weight,
        x1=~NodeVal,
        series=~Series,type='force',
        title='无间道',
        pos=list(title=5,legend=10),
        palette=c('orange','lightblue','orange','lightblue','orange','lightblue'))
