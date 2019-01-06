setwd("E:/Rexercise1")
###用R做时间序列分析
data("airline passenger")
passenger=AirPassengers
head(passenger)
length(passenger)
p=unlist(passenger) ##将数据以向量的形式展现
pt<-ts(p,frequency=12,start=2001) #将数据转化为时间序列
pt
plot(pt)
plot.ts(pt)
###将数据分成了两段，12年之前以及之后的
###一组作为训练数据，一组作为验证数据
train<-window(pt,start=2001,end=2011+11/12)
train  ##保留了部分数据
test<-window(pt,start=2012)
test
##根据实验数据，预测
library(forecast)
pred_meanf<-meanf(train,h=12)
pred_meanf
##与预留的检验数据作比较,计算均方根误差
##首先定义函数
rmse=function(a,b){
c=sqrt(sum((a-b)^2)/length(a)  )
c
}

###检验预测值同检验值之间的差别，即均方根误差
##检验各种预测值同检验值之间的差异
rmse(test,pred_meanf$mean)
#226.2657
pred_naive<-naive(train,h=12)
rmse(pred_naive$mean,test)
#102.9765
pred_snaive<-snaive(train,h=12)
rmse(pred_snaive$mean,test)
#50.70832
pred_rwf<-rwf(train,h=12, drift=T)
rmse(pred_rwf$mean,test)
#92.66636
pred_ses <- ses(train,h=12,initial='simple',alpha=0.2)
rmse(pred_ses$mean,test) 
#89.77035
pred_holt<-holt(train,h=12,damped=F,initial="simple",beta=0.65)
rmse(pred_holt$mean,test)
#76.86677  without beta=0.65 it would be 84.41239
pred_hw<-hw(train,h=12,seasonal='multiplicative')
rmse(pred_hw$mean,test)
#16.36156
fit<-ets(train)
accuracy(predict(fit,12),test)
#24.390252
pred_stlf<-stlf(train)
rmse(pred_stlf$mean,test)
#22.07215
#Seasonal Decomposition of Time Series by Loess
plot(stl(train,s.window="periodic"))  

fit<-auto.arima(train)
accuracy(forecast(fit,h=12),test) 
#23.538735
ma = arima(train, order = c(0, 1, 3),   seasonal=list(order=c(0,1,3), period=12))
p<-predict(ma,12)
accuracy(p$pred,test)
#18.55567
BT = Box.test(ma$residuals, lag=30, type = "Ljung-Box", fitdf=2)
BT
##auto.arima函数，可以自动去挑选一个最恰当的算法去分析数据
auto.arima(train)
