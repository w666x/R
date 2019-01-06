###系统设置阶段，挺重要的呢
list(ls())
rm(list=ls())
options(digits = 4)
par(mar=c(4,4,2,1)+0.1,cex=0.8)
setwd("E:/Rexercise1")
library()
library(class)
install.packages(dstat)
library(dstat)
##将所运行的结果输出到指定文件中
zz <- file("all.Rout", open = "wt")
sink(zz)
sink(zz, type = "output")
a=c(1:10)
try(log(a))
## back to the console
sink(type = "output")
sink()
file.show("all.Rout")
unlink("all.Rout")
###读取数据
UG=read.csv("3_17.csv")
UG
###统计分析
summary(UG)
attach(UG)
t.test(EXP,AMB)
t.test(EXP,AMB,data = UG)
colnames(UG)
fm=lm(LC~EXP)
summary(fm)
plot(EXP~LC,main = "散点图一")
abline(fm)
X=rnorm(100)
curve(dnorm(x),add = T)

##数据收集过程
#还可以从剪切板中直接提取
UG=read.table("clipboard",header = T)
UG
class(UG)
head(UG)


##数据处理步骤
###数据选择（观测，变量），数据转换，数据整理
##数据选择
help("read.csv")
getwd()
install.packages("Rstat")
library(Rstat)
library()
dim(UG)
names(UG)
head(UG)
head(UG,-3)
tail(UG)
tail(UG,3)
##选取观测值
UG[ID<10,]
subset(UG,ID<10)
##选取变量值
UG[,5]
mean(ID)
plot(ID,LC)
lm(ID~LC)
with(UG,{
  mean(ID)
  lm(ID~LC)
})
vars=names(UG)
names(UG)=paste("X",1:9,seq="")
names(UG)
names(UG)=vars
names(UG)
UG$x=log(LC)
UG
UG=transform(UG,X=log(LC))
is.data.frame(UG)
UG$Y=as.integer(X)
UG$X1[x<2]="LOW"
UG$X1[x>2]="BIG"
UG
UG=within(UG,{
  UG$X2[LC<4]="LOW"
  UG$X2[LC>4]="BIG"
})
table(cut(1:110,breaks = c(0,5,50,100)))
head(UG)
UG$ID1=cut(UG$AA,breaks = c(0,3,6,max(UG$AA)))
###数据整理
UG=UG[,c(1:6)]
head(UG)
UG[order(UG$FL),]
UG[order(UG$ID,decreasing=T),]
UG[order(UG$FL,UG$AA),]
UG1=UG[,c(1,2,3)]
UG2=UG[,c(1,4,5)]
UG1
UG2
UG3=cbind(UG1,UG2)
head(UG3)
UG4=rbind(UG1,UG2)
merge(UG1,UG2,by="ID")
UG.NA=UG[,1:5]
UG.NA
names(UG.NA)
UG.NA$AA[2]=NA
UG.NA$LA[5]=NA
UG.NA
head(UG.NA)
is.na(head(UG.NA$AA))
is.na(UG.NA$LA)
summary(UG.NA)
attach(UG.NA)
lm(FL~ID)
UG_NA=na.omit(UG.NA)
head(UG_NA)
detach()
detach()

###基本绘图处理
attach(UG)
op=par(mfrow=c(1,2))
plot(LA,xlab = "序号",ylab = "LA",main = "练习一",ylim = c(0,8),xlim = c(0,50),pch=4)
plot(1:24,pch=1:24)
text(1:24,adj = -1)
box()
plot(LA,type = "l")
title("hellp")
legend(2,4,"hi")
text(4,6,"you")
polygon(x,y)
rect(1,2,40,8)
plot(LA,type = "h")
lines(LA);abline(h=5,lty=1)
points(LA);text(LA,cex = 0.5)
plot(LA,type = "b")
par(mfrow=c(1,2))
plot(LA,AA)
plot(LA,AA,pch=as.numeric(ID))
legend(4,4,"levels(LA)",pch = 1:3,bty="n")
par(mar=c(4,4,2,1)+0.1,cex=0.75)
par(mfrow=c(1,1))
layout(matrix(1:4,2,2))
layout.show(4)
##这是怎么回事
layout(matrix(c(1,1,1,2,3,4,2,3,4),nr=3,byrow = T))
hist(rnorm(100));hist(rnorm(20));hist(rnorm(40));hist(rnorm(60))
layout(1)

detach()

colors()


###单变量数据分析
UG$CON=cut(UG$AA,breaks=c(0,4,max(UG$AA)))
head(UG)
attach(UG)
summary(UG)
table(UG$AA)
summary(table(UG$AA))
##条图
par(mfrow=c(1,2))
barplot(table(CON),ylim = c(0,35))
barplot(table(CON),ylim = c(0,35),col = c("red","blue"))
par(mfrow=c(1,1))
hist(AA,prob=T)
lines(density(AA))

##饼图
par(mfrow=c(1,3))
pie(table(CON))
pie(table(CON),col = c("red","blue"))
pct=round(table(CON)/sum(table(CON))*100,1)
pct
LBS=paste(names(table(CON)),pct,"%",seq="")
pie(table(CON),LBS)
par(mfcol=c(1,1))

#构建自己的分析函数
EDA.stat=function(x){
cat('n=',length(x),'\n')  
cat('min=',min(x),'\n')
}
save(EDA.stat,file = "EDA.R")
EDA.stat(LA)
EDA.stat2=function(x){
c(n=length(x),min=min(x))  
}
EDA.stat2(LA)
EDA.stat2
#探索性统计图
EDA.plot(LA)

##数据框数据分析
UG$K=cut(UG$FL,breaks = c(0,5,100))
UG$N=cut(UG$APP,breaks = c(0,3,6,10),
labels=c("little","middle","big"))
head(UG)
table(UG$N)
tsr=table(UG$CON,UG$N)
tsr
tsr1=table(UG$CON,UG$N,UG$K)
tsr1
k=ftable(UG$CON,UG$N,UG$K)
write.table(k,file = "k.txt",row.names = T,col.names = T)
read.table("k.txt",head=T)
prop.table(tsr,1)
prop.table(tsr,2)
barplot(tsr,ylim=c(0,25),legend.text = levels(UG$CON))
barplot(tsr,ylim=c(0,20),beside = T,legend.text = c(levels(UG$CON)))
##
attach(UG)
barplot(table(CON,UG$N),legend.text = levels(CON))
detach()
rm(N)
attach(UG)
par(mfrow=c(1,2),cex=0.8)
barplot(table(CON,N),ylim = c(0,40),col =2:4)
legend('topleft',levels(CON),pch = 15,col = 2:4)
par(mfrow=c(1,1))
detach()

head(UG)
attach(UG)
plot(FL,AA)
pairs(UG[,c(3:5)],gap = 0)

##
attach(UG)
bism=by(AA,CON,mean)
bism
barplot(bism,ylim = c(0,20),col = 2:3)
legend('topleft',levels(CON),pch = 15,col = 2:3)
by(AA,CON,summary)
boxplot(AA~CON)
boxplot(AA~CON,notch=T,col=1:2)
stripchart(AA~CON,pch=19)
stripchart(AA)
plot(AA,LA);text(AA,LA,col=1:2,adj = -0.5,cex = 0.75)
plot(AA,LA);text(AA,LA,CON,col=1:2,adj = -0.5,cex = 0.75)
coplot(AA~LA|CON)
coplot(AA~LA|CON,rows = 1)
detach()

##应用类函数的应用
UG1=UG[,c(1:5)]
apply(UG1,2,mean)
lapply(UG1, function(x) list(mean=mean(x),sd=sd(x)))
sapply(UG1, function(x) list(mean=mean(x),sd=sd(x)))
tapply(UG1$FL, INDEX=UG$CON, FUN=mean)
aggregate(UG1,by=list(UG$CON),FUN=mean)
aggregate(UG1,by = list(UG$CON,UG$K),FUN = mean)


###基本统计推断方法
hist(UG1$FL,breaks=15,prob=T,main="hello")
lines(density(UG1$FL))
qqnorm(UG1$FL)
qqline(UG1$FL)
shapiro.test(UG1$FL)
t.test(UG1$FL,mu=0)
t.test(UG1$FL,UG1$AA)
var.test(UG1$FL,UG1$AA)
#非参数检验方法
ks.test(rnorm(100),"pnorm")
qqnorm(rnorm(100))
qqline(rnorm(100))


#常用统计分析模型
cor(UG$FL,UG$AA)
attach(UG)
plot(APP~ID)
names(UG)
library(lattice)
xyplot(FL~AA|CON)
h=cor.test(~FL+AA,data=UG[UG$N=='little',])
h
save(h,file="h.txt")
cor.test(~FL+AA,K="middle")
cor.test(~FL+AA,K="big")
head(UG1)
N
y=ifelse(UG$FL<5,0,1)
y

##函数包以及数据包的下载以及应用
round(12.3455667,4)
search()
packages(all.available=T)
find("mean")
args('mean')
apropos('mean')
apropos('sd')
summary()
summary
methods(summary)
summary.table

##特殊的统计绘图
x=1:10
y=runif(10)
symbols(x,y,circles = y/2,inches = F,bg=x)
library(lattice)
library(ggplot2)
n=seq(5,40,5)
x=rnorm(sum(n))
y=factor(rep(n,n),labels = paste("n=",n))
densityplot(~x |y,
            panel = function(x,...){
              panel.densityplot(x,col="Dark01iveGreen",...)
              panel.mathdensity(dmath = dnorm,args = list(mean=mean(x),sd=sd(x)),col = "darkblue")
            })

plot(data=UG,AA~LA)
p=ggplot(data = UG,aes(x=AA))
p+geom_point()
p+geom_histogram()
p+geom_line(ase(y=LA))
ggplot(UG,ase(x=AA))

qplot(LA,data=UG,binwidth=0.8,geom = c("histogram"),colour=I("green"),fill=I("white"))
ggplot(UG,aes(x=LA))+geom_histogram()
qplot(UG$N,data = UG,geom = "bar",ylim = c(0,35),fill=region)
qplot(UG$N,LA,data = UG,geom = c("boxplot","jitter"),alpha=I(0.5),colour=N)
qplot(UG$LA,UG$AA,geom = c('point','smooth'),method='lm',colour=UG$N,se=F)
qplot(LA,AA,data = UG,geom = c("point","smooth"),method='lm',facets = N~.,se=F)

##模拟
n=100
x=cumsum(rnorm(n))
plot(x,type = 'l')
plot(x)
length(x)
y=cumsum(rnorm(n))
plot(x,y,type = 'l')

ls()
rm(m,f,p,n)
##抛硬币模拟
Bernoulli=function(m=500){
  f=rep(0,m)
  p=rep(0,m)
  f[1]=sample(c(0,1),1,rep=T)   ##此处f[1]是向量，首先n此处没有赋值，另外，当n大于1时，f[1]放的下吗
  p[1]=f[1]/1
  for (n in 2:m) {
    f[n]=sum(sample(c(0,1),n,rep=T))#n大于1了嘛，如何赋值成功来着
    p[n]=f[n]/n
  }
  plot(p,type='l',xlab='i',ylim=c(0,1));abline(h=0.5)
}
Bernoulli(m=50)
Bernoulli(5000)

plot(seq(-3,3,0.1),dnorm(seq(-3,3,0.1)),type = "l",xlab = 'x',ylab = expression(phi(x)))
text(-3,0.3,adj = c(0,1),expression( phi(x) == frac(1,sqrt(2*pi))~e^-frac(x^2,2)))
abline(v=c(-1,1),lty=3)
text(0,0.1,expression(integral(phi(x)*dx,-1,1)%~~%0.68))

###模拟相关应用
h=function(x){
  sin(x^2+x^3)+cos(1/2+x^0.5)
}
par(mar=c(2,2,2,1),mfrow=c(2,1),cex=0.75)
curve(h,0,1,xlab = "Function",ylab = "",lwd=2)
integrate(h,0,1)##正常求积分
##模拟技术求积分
m=1000
x=h(runif(m))
estint=cumsum(x)/(1:m)
estint[m]
##模拟求积分二
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint,type = 'l')
lines(estint+2*esterr,col=3)
lines(estint-2*esterr,col=3)
par(mfrow=c(1,1))


#模拟函数的建立
f=function(n=10,p=0.5){
  s=rbinom(1,n,p)
  (s-n*p)/sqrt(n*p*(1-p))
}
x=sim.fun(100,f)
hist(x,prob=T)

##模拟的进一步认识
##t统计量
t.stat=function(x,mu){
  (mean(x)-mu)/(sqrt(var(x)/length(x)))
}
result=c()
for (i in 1:200)
  result[i]=t.stat(rexp(100,1/mu),mu)
hist(result)
boxplot(result)
qqnorm(result)
qqline(result)


##t统计量
result=c()
for (i in 1:200)
  result[i]=t.stat(rexp(8,1/mu),mu)
hist(result)
boxplot(result)
qqnorm(result)
qqline(result)

##t统计量
result=c()
for (i in 1:200)
  result[i]=t.stat(rt(8,5),0)
hist(result)
boxplot(result)
qqnorm(result)
qqline(result)
qqplot(result,rt(200,7))


###数据库与调查分析
install.packages("RODBC")
library(RODBC)
Rxls=odbcconnectExcel("UnderGradute.xls")
UG=sqlFetch(Rxls,"sheet1")
close(Rxls)

