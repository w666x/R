#ch13 时间序列分析
#13.1 时间序列概述
#将原始数据读入到R
#将这些数据存储到一个时间序列对象中
#用这些数据绘制时间序列图
#分解时间序列
#作时间序列的预测
data=read.table("E:/Rexercise1/原始数据包/M1.txt",header=T)
head(data)
M=ts(data$M1,frequency=12, start=c(2002,1))
M
is.ts(M)
plot.ts(M)
plot(M)

#13.2.1 分解非季节性数据
library(TTR)
M.SMA3=SMA(M,n=3) ##简单移动平均，跨度为3
plot(M.SMA3)
M.SMA5=SMA(M,n=5) ##跨度为5的简单移动平均
plot(M.SMA5)


#13.2.2 分解季节性数据
data=c(362,385,432,341,382,409,498,387,
        473,513,582,474,544,582,681,557,
        628,707,773,592,627,725,854,661)
sales=ts(data,frequency=4,start=c(2004,1))
sales
plot.ts(sales)
##分解季节性数据一
components=decompose(sales)
components
options(digits=3)  #显示小数点后3位有效数字
components$seasonal  #季节性部分
plot(components) #画出，原始图，季节性，趋势性，随机图
##分析季节性数据二
components1=stl(sales,s.window="periodic")
components1
plot(components1)#画出原始时序图，季节性，趋势，以及残差的自相关图


#13.3.1 简单指数平滑法
data=scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)  #skip=1表示跳过第一行（基本信息）再开始读入数据
head(data) #网络链接导入数据
rain=ts(data,start=1813)  #start设置时间序列从1813年开始
plot.ts(rain)  #画时序图

rain.pre=HoltWinters(rain,beta=FALSE,gamma=FALSE) #作简单指数平滑
rain.pre  
rain.pre$fitted  #提取预测结果
plot(rain.pre) #画出原始数据图和预测图
rain.pre$SSE  #残差平方和作为预测效果的准确计量
##作出今后时点的预测；未来十年的预测
library(forecast)
rain.pre2=forecast.HoltWinters(rain.pre, h=10) 
plot.forecast(rain.pre2) #作出预测图，


#13.3.2 残差的白噪声检验，检验模型的效果怎么样
acf(rain.pre2$residuals,lag.max=20)  #计算残差，并作出残差的自相关图
Box.test(rain.pre2$residuals,lag=20,type="Ljung-Box") #Q统计量检验


#13.3.3 Holt双参数线性指数平滑法
#若数据有增长或者趋势，非季节性，并可以作加法模型
data=read.table("E:/Rexercise1/原始数据包/M1.txt",header=T)
M=ts(data$M1,frequency=12, start=c(2002,1))
M.pre=HoltWinters(M,gamma=FALSE)
M.pre
M.pre$fitted
M.pre$SSE
plot(M.pre)

M.pre2=forecast.HoltWinters(M.pre,h=20)  #20个季度，所以h=20,近期预测
plot.forecast(M.pre2)
acf(M.pre2$residuals,lag.max=10)
Box.test(M.pre2$residuals,lag=10, type="Ljung-Box")


#13.3.4 Winters线性和季节性指数平滑法
#模型具有增长趋势，存在季节性，并且可以用加法模型描述
data=c(362,385,432,341,382,409,498,387,
+       473,513,582,474,544,582,681,557,
+       628,707,773,592,627,725,854,661)
sales=ts(data,frequency=4,start=c(2004,1))
sales.pre=HoltWinters(sales) #参数的默认值均为TRUE
sales.pre$fitted
sales.pre
sales.pre$SSE
plot(sales.pre)

sales.pre2=forecast.HoltWinters(sales.pre,h=4) #预测下一年各季度的预测值
plot.forecast(sales.pre2)  #这里使用函数plot.forecast()与plot()是一样的
acf(sales.pre2$residuals,lag.max=8)
Box.test(sales.pre2$residuals,lag=8, type="Ljung-Box")


#13.4.2 时间序列的平稳化处理
#差分自回归移动平均模型
#将非平稳时间序列转化为平稳时间序列
####操作步骤
#时间序列品文化处理
#根据自相关图以及偏自相关图选择阶数，确定模型
#进行参数估计
#利用好的模型进行预测，并诊断检验
data=read.table("E:/Rexercise1/原始数据包/M1.txt",header=T)
M=ts(data$M1,frequency=12, start=c(2002,1))
plot.ts(M)

#当数据具有非平稳性质的时候，需作差分处理
#时间序列的拟合以及预测
M.diff=diff(M)  #首先作差分处理
head(M.diff)
plot(M.diff)  #绘制差分序列图

#最近时期的波动性较大，说明有异方差性
logM.diff=diff(log(M))
op=par(mfrow=c(1,2))
plot(diff(logM.diff))  #log消除部分异方差性后再做差分，并作出自相关图
acf(logM.diff,lag.max=10)


#13.4.3 建立适当的ARIMA模型
layout(1)
pacf(M.diff,lag.max=10)  #偏自相关图
#或者，直接用forecast来发现合适的模型
library(forecast)
auto.arima(M.diff,ic="bic") #选择合适的模型，以bic为准则


#13.4.4 ARIMA模型的参数估计
M.arima=arima(log(M),order=c(1,1,1)) #order设置p,d,q的值
M.arima
options(digits=5)


#13.4.5 模型预测及检验
M.pre3=forecast.Arima(M.arima,h=12)
M.pre3
plot.forecast(M.pre3)

acf(M.pre3$residuals,lag.max=5) #残差序列纯随机性检验
Box.test(M.pre3$residuals,lag=5,type="Ljung-Box")
plot(M.pre3$residuals)
