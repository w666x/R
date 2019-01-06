####线性模型
###多项式回归
cystfibr=ISwR::cystfibr
head(cystfibr)
dim(cystfibr)
attach(cystfibr)
summary(lm(pemax~height+I(height^2)))
pred.frame=data.frame(height=seq(110,180,2))
lm.pemax.hq=lm(pemax~height+I(height^2))
predict(lm.pemax.hq,interval = "pred",newdata = pred.frame)
head(pred.frame)
pp=predict(lm.pemax.hq,interval = "pred",newdata = pred.frame)
pc=predict(lm.pemax.hq,interval = "conf",newdata = pred.frame)
plot(height,pemax,ylim = c(0,200))
matlines(pred.frame$height,pp,lty = c(1,2,2),col = "black")
matlines(pred.frame$height,pc,lty = c(1,3,3),col = "black")
detach()

###过原点的回归分析
x=runif(20)
y=2*x+rnorm(20,0,0.3)
summary(lm(y~x))
summary(lm(y~x-1))
anova(lm(y~x))
anova(lm(y~x-1))

###设计矩阵和虚拟变量
model.matrix(pemax~height+weight)
lm(pemax~height+weight)
###组间的共线性
fake.trypsin=ISwR::fake.trypsin
attach(fake.trypsin)
dim(fake.trypsin)
summary(fake.trypsin)
head(fake.trypsin)
tail(fake.trypsin)
anova(lm(trypsin~grpf))
anova(lm(trypsin~grp))
###简单线性模型和各组具有独立均值的模型的比较
model1=lm(trypsin~grp)
model2=lm(trypsin~grpf)
anova(model1,model2)
###
anova(lm(trypsin~grp+grpf))
###绘图
###对trypsin以grpf为因子求均值
xbar.trypsin=tapply(trypsin,grpf,mean)
stripchart(trypsin~grp,method="jitter",
           jitter=.1,vertical=T,pch=20)
lines(1:6,xbar.trypsin,type = "b",pch=4,cex=2,lty=2)
abline(lm(trypsin~grp))
###对数据进行方差分析
n=c(32,137,38,44,16,4)
tryp.mean=c(128,152,194,207,215,218)
tryp.sd=c(50.9,58.5,49.3,66.3,60,14)
gr=1:6
anova(lm(tryp.mean~gr+factor(gr),weights = n))
###
sum(tryp.sd^2*(n-1))
sum(n-1)
sum(tryp.sd^2*(n-1))/sum(n-1)

###交互效应
summary(lm(trypsin~grp*grpf))
summary(lm(trypsin~grp+grpf+grp:grpf))
detach()

###可重复的双因素反差分析
coking=ISwR::coking
head(coking)
dim(coking)
attach(coking)
anova(lm(time~width*temp))
summary(lm(time~width*temp))
tapply(time,list(width,temp),mean)
detach()

###协方差分析
hellung=ISwR::hellung
head(hellung)
tail(hellung)
dim(hellung)
summary(hellung)
###因子转换
hellung$glucose=factor(hellung$glucose,labels = c("yes","no"))
summary(hellung)
attach(hellung)
###图形描述
plot(conc,diameter,pch=as.numeric(glucose))
###确定图例的位置，为鼠标第一次点击的位置
legend(locator(n=1),legend = c("gulcose","no gulcose"),pch = 1:2)
###图形描述二，对x取对数，
plot(conc,diameter,pch=as.numeric(glucose),log = "x")
###图形描述三，对x,y均取对数
plot(conc,diameter,pch=as.numeric(glucose),log="xy")
###图形描述四，添加回归线
tethym.gluc=hellung[glucose=="yes",]
tethym.nogluc=hellung[glucose=="no",]
lm.nogluc=lm(log10(diameter)~log10(conc),data = tethym.nogluc)
lm.gluc=lm(log10(diameter)~log10(conc),data = tethym.gluc)
abline(lm.nogluc)
abline(lm.gluc)

###比较回归直线，
summary(lm(log10(diameter)~log10(conc),data = tethym.gluc))
summary(lm(log10(diameter)~log10(conc),data = tethym.nogluc))
###
summary(lm(log10(diameter)~log10(conc)*glucose))
summary(lm(log10(diameter)~log10(conc)+glucose))
var.test(lm.gluc,lm.nogluc)
anova(lm(log10(diameter)~log10(conc)*glucose))
anova(lm(log10(diameter)~glucose+log10(conc)))
anova(lm(log10(diameter)~log10(conc)+glucose))
t.test(log10(diameter)~glucose)
detach()

###模型诊断
thuesen=ISwR::thuesen
head(thuesen)
dim(thuesen)
attach(thuesen)
options(na.action = "na.exclude")
lm.velo=lm(short.velocity~blood.glucose)
opar=par(mfrow=c(2,2),mex=0.6,mar=c(4,4,3,2)+.3)
plot(lm.velo,which=1:4)
par(opar)
###模型诊断二
opar=par(mfrow=c(2,2),mex=0.6,mar=c(4,4,3,2)+.3)
plot(rstandard(lm.velo))
plot(rstudent(lm.velo))
plot(dffits(lm.velo),type = "l")
matplot(dfbeta(lm.velo),type = "l",col="black")
lines(sqrt(cooks.distance(lm.velo)),lwd=2)
par(opar)
###根据诊断结果进行处理后继续分析
summary(lm(short.velocity~blood.glucose,subset = -13))
detach()
###诊断三
attach(cystfibr)
cookd=cooks.distance(lm(pemax~height+weight))
cookd=cookd/max(cookd)
cook.colors=gray(1-sqrt(cookd))
plot(height,weight,bg=cook.colors,pch=21,cex=1.5)
points(height,weight,pch=1,cex=1.5)
