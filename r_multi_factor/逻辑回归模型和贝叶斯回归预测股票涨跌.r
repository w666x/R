#R语言社区
setwd("E:/Rexercise1/R语言")
#逻辑回归模型预测股票涨跌
#贝叶斯判别分析在此预测股票涨跌情况
#逻辑回归是一个分类器
#sigmoid函数，逻辑斯谛模型
#贝叶斯判别分析的优势
#当类别的区分度高的时候，逻辑回归的参数估计不够稳定
#如果样本量很小时，且每一类相应变量中预测变量x服从正态分布，那么线性判别更稳定
#线性判别分析更加的普遍
help(package="ISLR") #查看函数
library(ISLR)
data(Smarket)
summary(Smarket)
head(Smarket);str(Smarket)
#查看一下各变量的相关系数
library(corrplot)
corrplot(corr = cor(Smarket[,-9]),order = "AOE",
         type = "upper",tl.pos = "d")
corrplot(corr = cor(Smarket[,-9]),add = T,type = "lower",
         method = "number",order = "AOE",diag = F,tl.pos = "n",cl.pos = "n")

#训练兵使用逻辑回归模型
attach(Smarket)
#05年之前作为训练集，05年之后作为测试集
train=Year<2005
#对训练集进行逻辑斯谛回归
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial,subset = train)
summary(glm.fit)
anova(glm.fit)
#对训练好的模型在测试集中进行预测，type='response'表示只返回概率值
glm.probs=predict(glm.fit,newdata = Smarket[!train,],type = "response")
head(glm.probs)
#根据概率进行涨跌分类
glm.pred=ifelse(glm.probs>.5,"Up","Down")
head(glm.pred)
#2005年实际的涨跌状况
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
#求得预测的准确率
mean(glm.pred==Direction.2005)

#模型调整
summary(glm.fit)
glm.step=step(glm.fit)
predict(glm.fit,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

###贝叶斯判别分析再次预测股票涨跌情况
#贝叶斯分类器
library(ISLR)
library(MASS)
attach(Smarket)
lda.fit=lda(Direction~Lag1+Lag2,data = Smarket,subset = Year<2005)
lda.fit
table(Smarket[Year<2005,9])/nrow(Smarket[Year<2005,])
plot(lda.fit)#LDA训练模型在up和down类的直方图
##比较验证
library(dplyr)
Lag1_1 <- Smarket %>% filter(Year<"2005", Direction=="Down") %>% select(Lag1)
Lag2_1 <- Smarket %>% filter(Year<"2005", Direction=="Down") %>% select(Lag2) 
Lag1_2 <- Smarket %>% filter(Year<"2005", Direction=="Up") %>% select(Lag1) 
Lag2_2 <- Smarket %>% filter(Year<"2005", Direction=="Up") %>% select(Lag2) 
lm_1 <- (-0.6420190*Lag1_1-0.5135293*Lag2_1)[,1]
lm_2 <- (-0.6420190*Lag1_2-0.5135293*Lag2_2)[,1]
par(mfrow=c(2,1))
hist(lm_1,breaks=16,freq = F,col="lightblue")
hist(lm_2,breaks=16,freq = F,col="lightblue")

###在测试集中验证LDA模型
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
class(lda.pred)
names(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)


#练习使用qda
qda.fit=qda(Direction~Lag1+Lag2,data = Smarket,subset = train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
