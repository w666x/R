#R语言中文社区
setwd("E:/Rexercise1/R语言中文社区")
#对非平稳时间序列进行建模
#ARIMA模型
#时间序列平稳性的必要条件：
#常数均值稳定在t。
#常数方差稳定在t。
#两个观察值之间的自协方差仅由两个观测值之间的距离来决定，它可用log（h)来表示。
require(astsa)
data("gtemp");head(gtemp);length(gtemp)
#转化成时间序列，并画出时序图
data=ts(gtemp,start =c(1880),frequency = 1)
plot.ts(data)
#这个就是典型的自己作图，low
plot(gtemp,xlab="year",ylab="Deviation",
     main="Global Temperature Deviations,1880-2009")
#作自相关图，偏相关图
pacf(data,plot = T)
acf(data,plot = T)
#在上述基础上，判断其是否为平稳时间序列
acf2(data)
#进行一阶差分,
plot(diff(data)) #一阶差分后，序列平稳了
acf2(diff(data))
#进行模型拟合，，，sarima(q,d,p)
sarima(data,0,1,1)
sarima.for(data,5,0,1,1) #可以作未来5年的预测
