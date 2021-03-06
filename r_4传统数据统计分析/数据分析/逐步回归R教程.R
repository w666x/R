#逐步回归R教程
cement=data.frame(x1=c(7,1,11,11,7,11,3,1,2,21,1,11,10),
x2=c(26,29,56,31,52,55,71,31,54,47,40,66,68),
x3=c(6,15,8,8,6,9,17,22,18,4,23,9,8)
,x4=c(60,52,20,47,33,22,6,44,22,26,34,12,12),
y=c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4))
lm.sol=lm(y~x1+x2+x3+x4,data=cement)
summary(lm.sol)
#step处理
lm.step=step(lm.sol)
summary(lm.step)
#lm.step的改进
newsol=lm(y~x1+x2,data=cement)
summary(newsol)
drop1(lm.sol)
drop1(lm.step)
#虚假回归的例子
anscombe=data.frame(x123=c(10,8,13,9,11,14,6,4,12,7,5),
y1=c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68),
y2=c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.1,9.13,7.26,4.74),
y3=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.44,5.73),
x4=c(rep(8,7),19,rep(8,3)),
y4=c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.5,5.56,7.91,6.8))
attach(anscombe)
lm.sol1=lm(y1~x123)
lm.sol2=lm(y2~x123)
lm.sol3=lm(y3~x123)
lm.sol4=lm(y4~x4)
summary(lm.sol1)
summary(lm.sol2)
summary(lm.sol3)
summary(lm.sol4)
plot(y4,x4)
abline(lm.sol4)
#残差图
y.res=residuals(lm.sol1)
y.res2=lm.sol2$residuals
x=c(1:11)
plot(x,y.res,type='l')
plot(x,y.res2)
lines(y.res2)
shapiro.test(y.res2)
#白噪声检验
forbes=read.table('dataexample605.txt')
lm.sol=lm(log100~F,data=forbes)
y.res=residuals(lm.sol)
shapiro.test(y.res)
ks.test(y.res2,'pnorm',mean(y.res2),sd(y.res2))
#异方差检验
newsol=lm(y~x1+x2,data=cement)
summary(newsol)
rexamples6_10=residuals(newsol)
cm=as.matrix(rexamples6_10,c(length(rexamples6_10),1))
write.table(cm,'residualofexample6_10.txt')#保存残差成为文件
y.rst=rstandard(newsol)
y.fit=predict(newsol)
plot(y.rst~y.fit)
getwd()
#探测强影响点
x=matrix(c(194.5,20.79,1.3179,131.79,194.3,20.79,1.3179,131.79,197.9,22.40,1.3502,135.02,198.4,22.67,1.3555,135.55,199.4,23.15,1.3646,136.46,199.9,23.35,1.3683,136.83,200.9,23.89,1.3782,137.82,201.1,23.99,1.3800,138.00,201.4,24.02,1.3806,138.06,201.3,24.01,1.3805,138.05,203.6,25.14,1.4004,140.04,204.6,26.57,1.4244,142.44,209.5,28.49,1.4547,145.47,208.6,27.76,1.4434,144.34,210.7,29.04,1.4630,146.30,211.9,29.88,1.4754,147.54,212.2,30.06,1.4780,147.80),ncol=4,byrow=T,dimnames=list(1:17,c('F','h','log','log100')))
#以上为数据录入工作
forbes=as.data.frame(x)
p<-1
n<-nrow(forbes)
d=dffits(lm.sol)
cf=1:n
cf[d>2*sqrt((p+1)/n)]
#
p=1;n=nrow(forbes)
d=cooks.distance(lm.sol)
cf=1:n
cf[d>2*sqrt((p+1)/n)]
#离1最远原则
p=1
n=nrow(forbes)
d=covratio(lm.sol)
#多重共线性
collinear=data.frame(y=c(10.006,9.737,15.087,8.422,8.625,16.289,5.958,9.313,12.96,5.541,8.756,10.937),
x1=rep(c(8,0,2,0),c(3,3,3,3)),
x2=rep(c(1,0,7,0),c(3,3,3,3)),
x3=rep(c(1,9,0),c(3,3,6)),
x4=rep(c(1,0,1,10),c(1,2,6,3)),
x5=c(0.541,0.13,2.116,-2.397,-0.046,0.365,1.996,0.228,1.38,-0.798,0.257,0.44),
x6=c(-0.099,0.07,0.115,0.252,0.017,1.504,-0.865,-0.055,0.502,-0.399,0.101,0.432)
)
xx=cor(collinear[2:7])
kappa(xx,exact=TRUE)
e=eigen(xx)
e$vectors[,6]
