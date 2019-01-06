setwd("E:/Rexercise1")
###时间序列简要的应用
###时间序列分析在数据分析R语言实战中的第十三章，十五章中都提到过很多
##平稳性的证明
##就是用R先时间序列建模ARMA模型,然后用分位数回归
##我要有单位根检验结果图，自相关，偏相关分析图,分位数回归图,还有回归系数图，最后方程曲线拟合图
RT=read.table("房价.txt",header = T)
head(RT)
M=ts(RT[,2],frequency = 12,start = c(2010,1))
M
plot.ts(M) #画出时序图
##目标时间序列呈现的为趋势性外加随机波动，无季节性影响

##非季节性数据分解，提取趋势部分
library(TTR)
##三阶平滑，去掉部分随机波动
M.sma3=SMA(M,n=3) 
plot(M.sma3)
##五阶平滑，去掉部分随机波动
M.sma5=SMA(M,n=5) 
plot(M.sma5)
##12阶平滑，去掉部分随机波动
M.sma12=SMA(M,n=12) 
plot(M.sma12)

##指数平滑法预测分析，此处拟合非季节性模型
M.pre=HoltWinters(M,gamma=F)
M.pre
plot(M.pre)
##未来结果的短期预测
library(forecast)
M.pre2=forecast.HoltWinters(M.pre,h=10) ##未来十期的
plot.forecast(M.pre2,col=2)
##残差的白噪声检验
#自相关图检验
acf(M.pre2$residuals,lag.max = 20)
acf(M.pre2$residuals,lag.max = 12)
pacf(M.pre2$residuals,lag.max = 12)
pacf(M,lag.max = 12)
acf()
plot(M.pre2$residuals)
#Q统计量
Box.test(M.pre2$residuals,lag = 20,type = "Ljung-Box")

###分位数回归图
library(quantreg)
fit1=rq(M[-60]~M[-1],data = M)
fit1
summary(fit1)
fit2=rq(M[-60]~M[-1],data = M,tau = c(0.1,0.3,.5,.7))
fit2
summary(fit2)
plot(fit2)
plot(fit2$residuals)
fit3=rq(M.pre$coefficients,tau=1:9/10)

x=1:10
y=10:19
LML2=rq(y~x+I(x^2)+I(x^3),tau = c(0.1,0.5,0.8))
plot(LML2)
LM1=lm(y~x+I(x^2)+I(x^3))
plot(x,y)
abline(LML2)
rq(LM1)
LM1
M.pre

library(tseries)
pp.test(M)
