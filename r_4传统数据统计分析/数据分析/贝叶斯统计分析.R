setwd("E:/Rexercise1")
###贝叶斯统计分析
###统计分为贝叶斯统计分析和经典统计分析即频率学派
##贝叶斯估计：未知参数的估计对先验分布的选择相当敏感，将未知参数视为随机变量也很难被接受
##分析时可以先通过模糊先验以及敏感性分析等方法弱化先验分布对结果的影响
##优点
#结合了数据的信息和参数的先验信息，不断通过样本数据更新先前的认知
#与经典统计分析相比，无需复杂的假设以及数学推导
#可以对缺失数据，截尾数据进行简明处理，还能对模型进行全面而稳健的估计

###建立一个完整的概率模型，包括先验分布和观测数据的抽样分布
###对数据进行条件化后得到的后验分布，计算后验分布，并对他进行合理的解释
###对模型的拟合和后验结果进行评估

##相同数据下，不同的先验分布对贝叶斯分析的影响
x=seq(0,1,.01)
n=5;z=dbeta(x,1,n+1)
z0=dbeta(x,1,n+1)
z1=dbeta(x,1+1,n-1+1)
z2=dbeta(x,2+1,n-2+1)
z3=dbeta(x,3+1,n-3+1)
z4=dbeta(x,4+1,n-4+1)
z5=dbeta(x,5+1,n-5+1)
z.df=data.frame(cbind(z,z0,z1,z2,z3,z4,z5))
matplot(x,z.df,ylim = c(0,4),xlab = "x",ylab = "",
        col = 1:6,type = "l",lwd = 2)
text(.2,3,"y=0")
text(.25,2.5,"y=1")
text(.08,1.1,"prior")
abline(h=1)



###随着观测信息的增加，不同先验对贝叶斯分析的影响
x=seq(0,1,0.01)
par(mfrow=c(1,3))
# 左侧图形 -- 先验
z1=dbeta(x,1,1);
z2=dbeta(x,5,2);
z3=dbeta(x,1,10)
z.df=data.frame(cbind(z1,z2,z3))
head(z.df)
matplot(x,z.df, xlab="y", ylab="",
        col=c("black", "red", "blue"),
        type="l", lty=1:3, lwd=2)
text(0.8,6,"Priors")
# 中间图形 -- 后验: n=5, y=1
n=5
y=1
z1=dbeta(x,y+1,n-y+1);
z2=dbeta(x,y+5,n-y+2);
z3=dbeta(x,y+1,n-y+10)
z.df=data.frame(cbind(z1,z2,z3))
head(z.df)
matplot(x,z.df, xlab="y", ylab="",
          col=c("black", "red", "blue"),
          type="l", lty=1:3, lwd=2)
text(0.8,3.5,"Posterios")
text(0.8,3.3,"n=5, y=1")
# 右侧图形 -- 后验: n=50, y=10
n=50
y=10
z1=dbeta(x,y+1,n-y+1);
z2=dbeta(x,y+5,n-y+2);
z3=dbeta(x,y+1,n-y+10)
z.df=data.frame(cbind(z1,z2,z3))
matplot(x,z.df, xlab="y", ylab="",
          col=c("black", "red", "blue"),
          type="l", lty=1:3, lwd=2)
text(0.8,4.5,"Posterios")
text(0.8,4.3,"n=50, y=10")


###我们在980例因非正常受孕而导致胎盘位置过低的分娩中，
#437例为女婴，能否判断，此类非正常分娩中，女婴的概率

###首先计算后验均值，标准差，中位数，以及95%置信区间
alpha <- 438
beta <- 544
postmean<-alpha/(alpha+beta)
print("The posterior mean is")
print(postmean)
poststd<-sqrt(alpha*beta/(alpha+beta)^2/(alpha+beta+1))
print("The posterior standard deviation is")
print(poststd)
postmedian<-qbeta(0.5, alpha, beta)
print("The median based on
        posterior distribution is")
print(postmedian)
CI_95<-c(qbeta(0.025,alpha, beta), qbeta(0.975, alpha, beta))
print("The 95% posterior confidence interval is")
print(CI_95)

##然后采用随机模拟的方法，产生1000个符合分布的随机数，计算均值，标准差等
alpha <- 438
beta <- 544
theta <- rbeta(1000, alpha, beta)
sort_theta <- sort(theta)
spostmean <- mean(theta)
spoststd <-sd(theta)
spostmedian <- sum(sort_theta[500:501])/2
approxCI_95 <-c(spostmean-1.96*spoststd, spostmean+1.96*spoststd)
print(spostmean)
print(spoststd)
print(spostmedian)
print("The 95% confidence interval of theta
        based on normal approximation is")
print(approxCI_95)

##其次，基于随机模拟，九三两种变换的均值，标准差等
alpha <- 438; beta <- 544
theta <- rbeta(1000, alpha, beta)
logit_theta <- log(theta/(1-theta))
sort_logit_theta <- sort(logit_theta)
slogit_median <- sum(sort_logit_theta[500:501])/2
slogit_postmean <- mean(logit_theta)
slogit_poststd<-sd(logit_theta)
L <- slogit_postmean-1.96*slogit_poststd
U<-slogit_postmean+1.96*slogit_poststd
approxlogit_CI=c(L, U)
approx_CI=c(exp(L)/(1+exp(L)), exp(U)/(1+exp(U)))
print(slogit_postmean)
print(slogit_poststd)
print(slogit_median)
print("The 95% confidence interval of logit(theta)
        based on normal approximation is")
print(approxlogit_CI)
print("The 95% confidence interval of theta
        based on normal approximation is")
print(approx_CI)
# phi的推断
  phi<-(1-theta)/theta
sort_phi <- sort(phi)
sphi_median <- sum(sort_phi[500:501])/2
sphi_postmean<-mean(phi)
sphi_poststd <- sd(phi)
L<-sphi_postmean-1.96*sphi_poststd
U<-sphi_postmean+1.96*sphi_poststd
approxphi_CI<-c(L, U)
print(sphi_postmean)
print(sphi_poststd)
print(sphi_median)
print("The 95% confidence interval of phi=(1-theta)/theta is" )
print(approxphi_CI)

###最后，作出theta,logit(theta),phi的频数直方图
alpha <- 438; beta <- 544
theta <- rbeta(1000,alpha,beta)
par(mfrow=c(1,3))
#Fig(1,1)-- histogram of theta
   par(mar=c(5,4,2,1))
hist(theta, breaks = seq(0.35,0.55,0.005),
       xlim = c(0.35,0.55),
       main="", xlab=quote(theta),
       probability="T")
#Fig(1,2) -- histogram of log(theta)
  logit_theta <- log(theta/(1-theta))
 breaks <- quantile(logit_theta, 0:20/20)
par(mar=c(5,4,2,1))
hist(logit_theta, breaks = seq(-0.5,0.1,0.01),
       xlim = c(-0.5,0.1), main="",
       xlab=quote(logit(theta)==log(theta/(1-theta))),
       probability=T)
 #Fig(1,3) -- histogram of phi=(1-theta)/theta
 phi=(1-theta)/theta
breaks <- quantile(phi, 0:20/20)
par(mar=c(5,4,2,1))
hist(phi, breaks = seq(0.8,1.6,0.01),
       xlim = c(1.0,1.6),
       main="", xlab=quote(phi==(1-theta)/theta),
       probability=T)


###多参数贝叶斯统计分析
##通过模拟得到编辑后验分布的样本
###多参数贝叶斯分析的模型建立

##回归分析
logit<-function(x){
  y=log(x/(1-x))
  return(y)
}
bioassay<-data.frame(
  x <- c(-0.86, -0.30, -0.05, 0.73),
  n <- c(5, 5, 5, 5),
  y <- c(0.01, 1, 3, 4.99),
  r <- logit(y/n))
plot(x,r)
lm.bioassay<-lm(formula = r~x)
abline(lm.bioassay)
summary(lm.bioassay)

##贝叶斯估计
bioassay.post<-function(alpha=0.1,beta=5){
  k<-4
  x <- c(-0.86, -0.30, -0.05, 0.73)
  n <- c(5, 5, 5, 5)
  y <- c(0, 1, 3, 5)
  prod<-1
  prod <- prod((exp(alpha+beta*x)/(1+exp(alpha+beta*x[i])))^y
                 *(1/(1+exp(alpha+beta*x)))^(n-y))
  return(prod)}
mlpost<-function(alpha=0.1,beta=5){-log(bioassay.post(alpha,beta))}
library(stats4)
mle(mlpost)


##后验密度及离散化抽样
modedensity<-bioassay.post(0.87,7.91)
alphax<-seq(-5,10,length=1000)
betay<-seq(-10,40,length=1000)
post<-outer(alphax,betay, "bioassay.post")
par(mfrow=c(1,2))
contour(alphax,betay,post,
          levels=seq(0.05,0.95,length=10)
          *modedensity, xlim=c(-5,10),
          ylim=c(-10,40), xlab=quote(alpha),
          ylab=quote(beta), drawlabels= FALSE)

####获得1000个样本点，及相应的散点图
post<-post/sum(post)
posta<-apply(post,MARGIN=1,FUN=sum)
w<-posta/sum(posta)
n<-1000
ra<-rep(0,n)
rb<-rep(0,n)
for(j in 1:n){
  ra[j]<-sample(alphax,1,replace=T,prob=w)
  postb<-bioassay.post(ra[j],betay)
  wb<-postb/sum(postb)
  rb[j]<-sample(betay,1,replace=T,prob=w)
}
plot(ra, rb, xlim=c(-5,10), ylim=c(-10,40),
       xlab=quote(alpha), ylab=quote(beta))

