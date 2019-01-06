##R在金融数据分析中的应用
#模拟退火法是什么东西来着
#朴素法又是什么东西

#投资组合最优化实例
#马克维茨投资组合理论：均值 标准差 模型
#导入数据
data=read.csv("原始数据包/Dalyr.csv",header=T)
dim(data)
head(data)
code=unique(data$Stkcd) #提取股票的唯一识别码，股票的代码
n=length(code)
#整理数据
#计算三十支股票的协方差阵和平均收益
Dalyr=matrix(0,220,n) #生成220行，30列的矩阵
head(Dalyr)
for (i in 1:n)
  Dalyr[,i]=data$Dretnd[which(data$Stkcd==code[i])] #将第i支股票的所有dretnd情况都记录到矩阵中
head(Dalyr)
r.cov=cov(Dalyr) #计算协方差阵
r.mean=apply(Dalyr,2, mean) #计算平均收益期望
options(digits = 2)
r.mean;head(r.cov)
#将平均收益率用散点图表示出来
plot(r.mean,main = "30支股票期望收益率",pch=8,col=2)
abline(h=0)

#建立均值方差模型
t11=Sys.time()
num=100000
rp=numeric(num);sigmap=numeric(num);#定义俩变量，存放均值以及标准差
for (i in 1:num){
  x1=runif(30) #仅仅是产生一个权重而已
  x=x1/sum(x1)
  rp[i]=sum(r.mean*x) 
  sigmap[i]=t(x) %*% r.cov %*% x
}
plot(sigmap,rp,pch='.',main = "随机打点法有效边界") #绘制十万次的模拟的投资组合散点
#计算有效边缘
rp=round(rp,4)  #仅取四位有效数字
rp.uni=unique(rp) #取出不同的值
nn=length(rp.uni)
sigma.min=numeric(nn)
for (i in 1:nn)
  sigma.min[i]=min(sigmap[which(rp==rp.uni[i])]) #找到对应的rp.uni中最小的那一个
rp.sort=sort(rp.uni)
order=order(rp.uni)
lines(rp.sort~sigma.min[order],col="red") #画出对应点的连线
t12=Sys.time()
time.used=t12-t11;time.used #朴素法使用的时间

#模拟退火算法
#加温过程
#等温过程
#冷却过程
#产生新解-计算目标函数差-判断是否接受-接受或者舍弃
###simulated annealing###
t21=Sys.time()
temp0=100000  #确定初温
temp=temp0
x0=c(1,runif(29)) #产生30个数的一向量
x=x0/sum(x0)  #求出其权重
L=50  #循环次数
M=1000  #惩罚因子
R=0.0025  #目标收益率
f2=function(x){t(x)%*%r.cov%*%x+M*max(0,R-sum(r.mean*x))}  #乘法函数法
f1=function(x){(R-sum(r.mean*x))/sqrt(t(x)%*%r.cov%*%x)}  #正态假设法

f=f1(x)  #记录每次退温的最优解
f=as.numeric(f)
k=1  #记录退温次数
a=data.frame(x)#记录权重
e=0.00001
while(temp>0.001)
  {
       for(i in 1:L)
        {
             x1=x+runif(30)  #加上新的随机数
             x1=x1/sum(x1)
             deltaf=f1(x1)-f1(x)
             pr=exp(-deltaf/temp)  #Meropolis判断概率，两种判断二选一即可
             if(deltaf<0) {x=x1;f[k+1]=f1(x1)} else {if(pr>runif(1)) x=x1;f[k+1]=f1(x1)}
           }
       k=k+1
       a[k]=x
      temp=temp0/(k^3) #退温
}

#退火法选取的投资组合
r.sa=0;sigma.sa=0
for (i in 1:length(a))
  {
       r.sa[i]=sum(r.mean*a[[i]])
       sigma.sa[i]=t(a[[i]])%*%r.cov%*%a[[i]]
     }
plot(sigma.sa,r.sa,main='模拟退火法有效边界')
#有效边缘
r.sa=round(r.sa,4)
rsa.uni=unique(r.sa)
nn=length(rsa.uni)
sigma.min=numeric(nn)
for (i in 1:nn)
   {
       sigma.min[i]=min(sigma.sa[which(r.sa==rsa.uni[i])])
     }
rsa.sort=sort(rsa.uni)
order=order(rsa.uni)
lines(rsa.sort~sigma.min[order],col='red',lwd=2)
legend(0.000101,-4e-04,legend='正态假设法')
t22=Sys.time()
time.used2=t22-t21;time.used2#退火法使用的时间


##构造投资组合的有效前沿
#输入数据
#将数据加工处理，得到收益率矩阵
#将收益率矩阵作为输入变量，计算得到有效前沿

#计算分析
#读取数据
library(quantmod)
getSymbols(c('IBM','SPY','YHOO')) #读取数据
head(IBM)
head(YHOO)
head(SPY)
#计算收益矩阵
IBM_ret=dailyReturn(IBM)
syp_ret=dailyReturn(SPY)
yhoo_ret=dailyReturn(YHOO)
head(IBM_ret)
data=merge(IBM_ret,syp_ret,yhoo_ret) #合并收益率，构成收益率矩阵
#计算投资组合的有效前沿
library(timeSeries)
data=as.timeSeries(data) #转换对象
head(data)
library(fPortfolio)
Frontier=portfolioFrontier(data) #计算有效前沿
Frontier
#绘出有效前沿图
plot(Frontier)

##股票聚类分析
#K-means

##R在数据预测中的应用
#回归分析预测、时间序列预测
#回归分析预测

#数据的处理和描述
mortality=read.csv('原始数据包/swedish mortality.csv',header=T)
head(mortality)
dim(mortality)
mortality=na.omit(mortality) #处理缺失数据
dim(mortality)
mortality=mortality[mortality$q_male>0,] #处理异常值
mortality=mortality[mortality$q_male<=1,]
dim(mortality)
attach(mortality)
library(rgl)
plot3d(Year,Age,q_male,col = 'red',type = "p",zlim = 1) #绘出三维图
#绘出二维散点图
par(mfrow=c(1,2))
plot(Age,log(q_male),main='年龄与死亡率（对数）')
plot(Year,log(q_male),main="年份与死亡率")
layout(1) #取消图形区域拆分
plot(Age,L_male_exp,main="年龄与对数生存人数")

#拟合研究对象的分布类型
hist(Male_death,freq = F,breaks = 100) #绘制概率直方图，得到死亡人数分布的直观感受
hist(log(Male_death),freq = F,breaks = 100) #取对数后，再作概率直方图
#初步拟合
lg.md=log(Male_death)
#将数据分为两部分
data1=lg.md[lg.md<6];data2=lg.md[lg.md>6]
p1=length(data1)/(length(data1)+length(data2))  #求第一个正态分布的比重
library(MASS)
para1=fitdistr(data1,'normal')$estimate  #拟合第一个正态分布的参数
para1
para2=fitdistr(data2,'normal')$estimate
para2
#计算样本的双峰混合分布的拟合值
p=p1*dnorm(lg.md,para1[1],para1[2])+(1-p1)*dnorm(lg.md,para2[1],para2[2])
#绘出图形，经验值和拟合值作比较
hist(log(Male_death),freq = F,breaks = 100,main = "经验值和拟合值",ylim=c(0,0.33))
points(lg.md,p) #描出拟合值的点
#用KS检验，检验双峰分布的拟合效果
#计算双峰分布的累积分布函数
F.mix=p1*pnorm(lg.md,para1[1],para1[2])+(1-p1)*pnorm(lg.md,para2[1],para2[2])
ks.test(lg.md,F.mix)

##普通线性回归
ols=lm(Male_death~Age+Year+L_male_exp)
summary(ols)
#广义线性回归模型
m.nb=glm.nb(Male_death~factor(Age)+factor(Year)+offset(L_male_exp))
summary(m.nb)
#检验各因子的显著性水平,以及异常值点诊断
anova(m.nb,test = 'Chisq')
par(mfrow=c(2,2))
plot(m.nb)
detach()
#去除异常值点，进行模型改进
location=c(1,102,307,5414,5626)
mortality=mortality[-location]
attach(mortality)
#引入正交多项式，进行模型改进
model.final=glm.nb(Male_death~poly(Age,25)+poly(Year,4)+offset(L_male_exp))
options(digits = 2)
summary(model.final)
#模型的拟合结果
pre.final=predict(model.final) #计算拟合值
layout(1)
plot(Male_death,exp(pre.final),xlab='观测值',ylab='拟合值',main='正交多项式改进模型')
abline(0,1,col='red')
#模型的预测
q_pre=exp(pre.final)/Male_Exp #死亡率的拟合值
plot(Age,log(q_male),pch='.',main='对数死亡率')
points(Age,log(q_pre),pch='.',col=2)
legend(70,-7,legend = c('拟合值','预测值'),lty=1,col=c(1,2))


##时间序列预测
#【案例2】CPI的向量自回归模型
data=read.table("原始数据包/cpi_data.txt",header=T)
dat=ts(data,start=c(2005,1),frequency=12)  #根据原始数据构造时间序列
#由于是月度数据，因此设置frequency为12，以2005年1月为序列起点
dat=log(dat)  #取数据的对数值
head(dat)
plot.ts(dat,main='time series of cpi')  #绘制时间序列图

#一阶差分，消除自相关
dat_dif=diff(dat)  #得到一阶差分序列
plot.ts(dat_dif^2) #取一阶差分序列的平方值

#平稳性检验
library(tseries)
cpi=ts(data$CPI,frequency=12, start=c(2005,1))  #构造CPI的单变量时间序列
cpi_dif=diff(log(cpi))  #对cpi取对数后，再进行一阶差分
pp.test(cpi_dif)

#数据建模
library(vars)
options(digits=2)  #计算结果至少显示2位小数
result=VAR(dat_dif,p=2)
plot(result)
result$varresult$CPI  #varresult给出模型中各参数的估计结果，用”$”提取CPI的部分
summary(result)$varresult$CPI  #给出模型中各变量的显著性

dat_mod=dat_dif[,c(-2,-4)]  #剔除M2和PPI分别对应的第二列和第四列
result2=VAR(dat_mod,p=2)  #对提出变量后的数据建立VAR模型
summary(result2)$varresult$CPI

plot(result2)
pre=predict(result2,n.ahead=12)
CPI.pre=pre$fcst$CPI  #提出对CPI预测的结果
CPI.pre

options(digits=5)  #修改小数显示格式
LCPI.pre=log(cpi[119])+cumsum(CPI.pre[,1])  #计算CPI对数的预测值
CPI.pre=exp(LCPI.pre)  #计算CPI预测值
CPI.pre
#plot(CPI.pre)
