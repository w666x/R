###非线性曲线拟合
###基本用法
par(mfrow=c(1,1))
t=c(0:10)
y=rnorm(11,mean = 5*exp(-t/5),sd=.2)
plot(y~t)
##对数据建模
nlsout=nls(y~A*exp(-alpha*t),start = c(A=2,alpha=.05))
summary(nlsout)
###寻找初值
juul2=ISwR::juul2
head(juul2)
attach(subset(juul2,age<20 & age>5 & sex==1))
plot(height~age)
###通过查找资料，我们发现了描述生长的常用函数曲线
###通过对函数的分析以及对数据的变换，得到结果如下
plot(log(5.3-log(height))~age)
lm(log(5.3-log(height))~age)
##将求取的初值带入到nls中，拟合曲线
fit=nls(height~alpha*exp(-beta*exp(-gamma*age)),
        start = c(alpha=exp(5.3),beta=exp(0.42),
                  gamma=.15))
summary(fit)
###将拟合结果叠加在散点图上
plot(age,height)
newage=seq(5,20,length=100)
lines(newage,predict(fit,newdata = data.frame(age=newage)),
      lwd=2)
###模型改进,在对数刻度下进行拟合
fit=nls(log(height)~log(alpha*exp(-beta*exp(-gamma*age))),
        start = c(alpha=exp(5.3),beta=exp(.12),gamma=.12))
summary(fit)
###将拟合结果叠加在散点图上
plot(age,log(height))
lines(newage,predict(fit,newdata = data.frame(age=newage))
      ,lwd=2)

###自启动模型
###自动计算初值
summary(nls(height~SSgompertz(age,Asym,b2,b3)))
##错误的变换一
nls(log(height)~log(SSgompertz(age,Asym,b2,b3)))
##基本的变换
cf=coef(nls(height~SSgompertz(age,Asym,b2,b3)))
summary(nls(log(height)~
              log(as.vector(SSgompertz(age,Asym,b2,b3))),
            start = as.list(cf)))

###剖面分析
par(mfrow=c(3,1))
plot(profile(fit))
confint(fit)
confint.default(fit)
nls(height~SSgompertz(age,Asym,b2,b3),trace = T)
