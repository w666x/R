###复杂数据统计方法—基于R的应用
##第二章，横截面数据回归：经典方法(只看到第2.3节)
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/数据集")
###回顾，做简单拟合
w=read.table("COfreewy.txt",header = T)
head(w)
dim(w)
str(w)
a=lm(CO~.,w) #利用全部自变量作线性回归
summary(a)
b=step(a,direction = "backward") #向后逐步回归
summary(b)
shapiro.test(b$residuals) #残差的正态性检验
###对数据的进一步分析
qqnorm(b$residuals)
qqline(b$residuals)
plot(w)
pairs(w)
attach(w)
##计算相关系数（CO，与Traffic，平方项，立方项等）
cor(cbind(CO,Traffic,Tsq=Traffic^2,Tcub=Traffic^3,
          Hour,Hsq=Hour^2,Hcub=Hour^3,Wind,
          Wsq=Wind^2,Wcub=Wind^3))
b=cbind(CO,Traffic,Tsq=Traffic^2,Tcub=Traffic^3,
        Hour,Hsq=Hour^2,Hcub=Hour^3,Wind,
        Wsq=Wind^2,Wcub=Wind^3)
head(b);head(w)
###作CO与所有变量的线性回归
a=lm(CO~Traffic+Wind+I(Wind^2)+I(Wind^3)+sin((2*pi/24)*Hour)+
       cos((2*pi/24)*Hour)+sin((4*pi/24)*Hour)+cos((4*pi/24)*Hour))
summary(a)
##逐步回归，按AIC选择变量
b1=step(a)
summary(b1)
anova(b1)
shapiro.test(b1$residuals)
###进一步调整
b2=lm(CO~Traffic+Wind+I(Wind^2)+cos((2*pi/24)*Hour)+cos((4*pi/24)*Hour))
summary(b2)
anova(b2)#方差分析表
shapiro.test(b2$residuals) #残差的正态性检验
qqnorm(b2$residuals)
qqline(b2$residuals)

###小例一:伪回归（毫无相关性的数据有了回归结果）
##任何检验都是为了否定而设定的，非为了肯定什么设定
##人们永远也不能说，这个回归没有任何问题
set.seed(441010)
x=c(rnorm(100),50);y=c(rnorm(100),-50)
a=lm(y~x);summary(a)
shapiro.test(a$residuals)
plot(x~y)
plot(x)
plot(y)

###损失函数及分位数回归简介
##最小一乘回归（使残差的绝对值的和最小）即是分位数回归的特例
##分位数回归
library(quantreg)
par(mfrow=c(1,2))
data("engel")
dim(engel)
head(engel)
plot(engel)
plot(log10(foodexp)~log10(income),data = engel,main="'engel' data (log10 -tranformed)")
#分位数回归
taus=c(.15,.25,.5,.75,.95,.99)
rqs=as.list(taus)
for (i in seq(along=taus)){
  rqs[[i]]=rq(log10(foodexp)~log10(income),
  tau=taus[i],data=engel)
  lines(log10(engel$income),fitted(rqs[[i]]),col=i+1)
}
legend("bottomright",paste("tau=",taus),inset = .04,
       col = 2:(length(taus)+1),lty =1,text.width =.05,cex = .5,lwd =1.5)


###简单线性模型中的指数变换
###读取数据，作简要的数据处理
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/数据集")
w=read.csv("pharynx.csv") 
head(w);dim(w)
str(w);summary(w)
w=w[,-c(1,11)] #去掉cast和日期
all(is.na(w)==F)
w=w[w$COND!=9 & w$GRADE!=9,] ##去掉两列NA,此处NA用9表示
dim(w)
##把cond变成两个水平
w$COND[w$COND==3|w$COND==4]=2
w$COND[w$COND==0]=1
##保存在pharynx1.csv中
write.csv(w,"pharynx1.csv",quote = F,row.names = F)

##首先作简单的线性回归
u=read.csv("pharynx1.csv")
head(u);dim(u);str(u)
##把定性变量从数值型转为因子型
x=1:11;x=x[-c(5,11)]
for (i in x) u[,i]=factor(u[,i])
#线性回归
a=lm(TIME~.,data = u)
summary(a)
names(w)

###对数据作指数变换
library(MASS)
b=boxcox(TIME~.,data=u)
I=which(b$y==max(b$y))
I
b$x[I] ##找到lambda的值，以此lambda值作指数代换
#指数变换后，作线性回归
a=lm(TIME^.4~INST+SEX+TX+AGE+COND+T_STAGE+N_STAGE+STATUS,data=u)
summary(a)
a1=lm(TIME^.4~.,data = u)
summary(a1)
#作逐步回归
b=step(a);
summary(b)
anova(b)
shapiro.test(b$residuals)
par(mfrow=c(1,1))

###生存分析数据的Cox回归模型
head(w)
##处理横截面生存数据的cox比例危险回归模型
library(survival)
fit=survfit(Surv(TIME,as.numeric(STATUS))~TX,data = u)
plot(fit,lty = 1:2,ylab = "S(t)",xlab = "t",main="Survival Function")
legend(1500,1,c("TX=1","TX=2"),lty = 1:2,cex =.8) #图例
