##脏数据的定义
#数据中存在的有缺失值的事项
#数据中存在有异常值的事项
#数据的不一致性

#缺失值的处理
#剔除法，多重插补法

#首先生成一批含有缺失值的数据集
set.seed(1234)
Tel=13812341000:13812341999
length(Tel)
Sex <- sample(c('F','M'), size = 1000, replace = T,
              prob = c(0.4,0.6))
Age <- round(runif(n = 1000, min = 18, max = 60))
Freq <- round(runif(n = 1000, min = 1, max = 368))
Amount <- rnorm(n = 1000, mean = 134, sd = 10)
ATV <- runif(n = 1000, min = 23, max = 138)
df <- data.frame(Tel = Tel, Sex = Sex, Age = Age, 
                 Freq = Freq, Amount = Amount, ATV = ATV)
head(df)
summary(df)
#随机参数某行某列的下标
set.seed(1234)
i=sample(1:6,size = 100,replace = T)
j=sample(1:1000,size = 100)
index=as.matrix(data.frame(j,i))
df=as.matrix(df)
#将随机参数的行列赋值为NA，即生成了缺失值
df[index]=NA
head(df)
summary(df)
#将新矩阵重新转换为数据框
df2=as.data.frame(df)
#转换变量类型
df2$Age=as.integer(df2$Age)
df2$Freq=as.integer(df2$Freq)
df2$Amount <- as.numeric(df2$Amount)
df2$ATV <- as.numeric(df2$ATV)
#再一次查看赋予缺失值后的数据框概要
summary(df2)

#查看缺失值的分布情况，
library(VIM)
aggr(df2,prop=F,numbers=T)

#缺失值处理
#下面对Tel变量缺失的观测进行剔除；
#对Sex变量的缺失值用众数替换；
#Age变量用平均值替换；
#Freq变量、Amount变量和ATV变量用多重插补法填充

#剔除Tel变量的缺失观测
df3 <- df2[is.na(df2$Tel)==FALSE,]
#分别用众数和均值替换性别和年龄
#性别的众数
Sex_mode <- names(which.max(table(df3$Sex)))
#年龄的均值
Age_mean <- mean(df3$Age, na.rm = TRUE)
library(tidyr)
df3 <- replace_na(df3,replace = list(Sex = Sex_mode, 
                                     Age = Age_mean))
summary(df3)

#对缺失数据进行插补法
#对于数值型数据，默认使用随机回归添补法(pmm)；
#对二元因子数据，默认使用Logistic回归添补法(logreg)；
#对多元因子数据，默认使用分类回归添补法(polyreg)
library(mice)
#对缺失值部分进行五次的多重插值
imp=mice(data = df3,m=5)
imp$imp

#计算5重插补值的均值
Freq_imp <- apply(imp$imp$Freq,1,mean)
Amount_imp <- apply(imp$imp$Amount,1,mean)
ATV_imp <- apply(imp$imp$ATV,1,mean)

#并用该均值替换原来的缺失值
df3$Freq[is.na(df3$Freq)] <- Freq_imp
df3$Amount[is.na(df3$Amount)] <- Amount_imp
df3$ATV[is.na(df3$ATV)] <- ATV_imp

#再次查看填补完缺失值后的数据集和原始数据集概况
summary(df3)
summary(df2)

##异常值的处理
#识别异常值
#一般通过绘制盒形图来查看哪些点是离群点，而离群点的
#判断标准是四分位数与四分位距为基础。即离群点超过上
#四分位数的1.5倍四分位距或低于下四分位数的1.5倍四分位距

#随机产生一组数据
set.seed(1234)
value=c(rnorm(100,mean = 10,sd=3),runif(20,min = .01,
                                        max = 20),
        rf(30,df1=5,df2=20))
plot(value)

#绘制箱线图,并用红色的方块标出异常值
library(ggplot2)
ggplot(data = NULL,mapping = aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red',outlier.shape = 15,
               width=1.2)

#找出异常值
#计算下四分位数、上四分位数和四分位距
QL <- quantile(value, probs = 0.25)
QU <- quantile(value, probs = 0.75)
QU_QL <- QU-QL
QL;QU;QU_QL
which(value > QU + 1.5*QU_QL)
value[which(value > QU + 1.5*QU_QL)]

#异常值点的处理
#用离异常点最近的点替换，即最大的非异常点
test01 <- value
out_imp01 <- max(test01[which(test01 <= QU + 1.5*QU_QL)])
test01[which(test01 > QU + 1.5*QU_QL)] <- out_imp01

#用上四分位数的1.5倍四分位距或下四分位数的1.5倍
#四分位距替换
test02 <- value
out_imp02 <- QU + 1.5*QU_QL
test02[which(test02 > QU + 1.5*QU_QL)] <- out_imp02

#对比替换前后的数据概览
summary(value)
summary(test01)
summary(test02)

##数据的不一致性处理
##一般是由不同的数据源所致，可以通过数据变换得到 一致
##的数据，只有在数据一致的情况下，统计分析的工作才有效