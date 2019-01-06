###逆变换方法产生连续型密度函数的随机观测值
###首先导出其分布函数的逆变换函数
###然后从均匀分布中产生一随机数，代入逆变换函数中即可
n<-1000
u<-runif(n)
x<-u^(1/3)
hist(x,prob=TRUE,main=expression(f(x)==3*x^2))
y<-seq(0,1,0.01)
lines(y,3*y^2)
###使用逆变换方法产生指数分布的随机数
n=1000
u=runif(n)
lmbda=4
x=-log(u)/lmbda
hist(x,probability = T)
y=seq(0,2,0.01)
lines(y,lmbda*exp(-lmbda*y))

###离散型场合产生随机数
##逆变换方法产生0-1分布的随机数
n<-1000
p<-0.4
u<-runif(n)
x<-ceiling(log(1-u)/log(1-p))
x<-as.integer(u>1-p)
k<-floor(log(u)/log(1-p))+1
mean(x) #理论值p
var(x) ##理论值p(1-p)
##逆变换方法产生几何分布的随机数
n<-1000
p<-0.4
u<-runif(n)
x<-ceiling(log(1-u)/log(1-p))
###用逆变换方法产生泊松分布的随机数
rlogarithmic<-function(n,theta){
  #return a random logarithmic(theta) sample size n
  u<-runif(n)
  #set the initial length of cdf vector
  N<-ceiling(-16/log10(theta))
  k<-1:N
  a<--1/log(1-theta)
  fk<-exp(log(a)+k*log(theta)-log(k))
  Fk<-cumsum(fk)
  x<-integer(n)
  for(i in 1:n){
    x[i]<-as.integer(sum(u[i]>Fk))
    while(x[i]==N){
      logf<-log(a)+(N+1)*log(theta)-log(N+1)
      fk<-c(fk,exp(logf))
      Fk<-c(Fk,Fk[N]+fk[N+1])
      N<-N+1
      x[i]<-as.integer(sum(u[i]>Fk))
    }
  }
  x+1
}
rlogarithmic(10,0.5)

###舍选法产生X的随机数
###产生1000个目标的随机数，需要循环j次
f3=function(n=10,j=0,k=0){
  y=numeric(n)
  while (k<n){
    u=runif(1)
    j<-j+1
    x<-runif(1) #从g中产生一个随机数
    if(x*(1-x)>u) {
      k<-k+1
      y[k]<-x
    }}
  j
}
f3(1000)
debug(f3)
f3(n=10)

###变换法产生随机变量
rlogarithmic<-function(n,theta){
  stopifnot(all(theta>0 & theta <1))
  th<-rep(theta,length=n)
  u<-runif(n)
  v<-runif(n)
  x<-floor(1+log(v)/log(1-(1-theta)^u))
  return(x)
}
rlogarithmic(10,0.5)
traceback()


###随机变量的和以及混合是一种特殊类型的变换
n=1000
nu=2
X=matrix(rnorm(n*nu),n,nu)^2
##方法一
y=rowSum(X)
y=apply(X,MARGIN =1,FUN=sum)
####分布的混合
n<-1000
k<-sample(1:5,size=n,replace=TRUE,prob=(1:5)/15)
rate<-1/k
x<-rgamma(n,shape=3,rate=rate)
#画出混合的密度以及分量的密度
plot(density(x),xlim=c(0,40),ylim=c(0,.3),lwd=3,xlab="x",main="")
for(i in 1:5)
  lines(density(rgamma(n,3,1/i)))
###画出混合函数的程序
f<-function(x,lambda,theta){
  sum(dgamma(x,3,lambda)*theta)
}
x<-seq(0,8,length=200)
dim(x)<-length(x) ##使用apply函数的需要
p<-c(.1,.2,.2,.3,.2)
lambda<-c(1,1.5,2,2.5,3)
###计算混合密度在x处的值
###apply函数通过调用f函数，以及相关参数值，计算x处对应的值
y<-apply(x,1,f,lambda=lambda,theta=p)
plot(x,y,type="l",ylim=c(0,.85),lwd=3,ylab="Density")
for(j in 1:5){
  y<-apply(x,1,dgamma,shape=3,rate=lambda[j])
  lines(x,y)
}
###泊松分布和伽马分布的混合
#Poisson-Gamma Mixture
n<-1000
r<-4
beta<-3
lambda<-rgamma(n,r,beta) #lambda是随机的
x<-rpois(n,lambda)
#compare with negative binomial
mix<-tabulate(x+1)/n
negbin<-round(dnbinom(0:max(x),r,beta/(1+beta)),3)
se<-sqrt(negbin*(1-negbin)/n)
round(rbind(mix,negbin,se),3)
###联合分布函数求随机数
###第一步：产生标准正态分布随机变量Z1，Z2，...
###第二步：计算分解式Σ = Q T Q
###第三步：应用变换 X = ZQ + Jµ T 
###谱分解方法产生随机数
mu <- c(0, 0)
Sigma <- matrix(c(1, .9, .9, 1), nrow = 2, ncol = 2)
rmvn.eigen <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
###谱分解法，eigen，计算值
    ev <- eigen(Sigma, symmetric = TRUE)
    lambda <- ev$values
    V <- ev$vectors
###谱分解法计算方差
    R <- V %*% diag(sqrt(lambda)) %*% t(V)
    Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
###计算出X值
    X <- Z %*% R + matrix(mu, n, d, byrow = TRUE)
    X
  }
# generate the sample
X <- rmvn.eigen(1000, mu, Sigma)
plot(X, xlab = "x", ylab = "y", pch = 20)
print(colMeans(X))
print(cor(X))

###奇异值分解法
rmvn.svd <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
###奇异值分解
    S <- svd(Sigma)
###奇异值法产生随机数
    R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v) #sq. root Sigma
    Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
    X <- Z %*% R + matrix(mu, n, d, byrow=TRUE)
    X
  }
###Choleski分解方法产生随机数
rmvn.Choleski <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
###Choleski产生随机数
    Q <- chol(Sigma) # Choleski factorization of Sigma
    Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
    X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE)
    X
  }

###比较各种生成器的时间复杂性
library(MASS)
n <- 100 #sample size
d <- 30 #dimension
N <- 2000 #iterations
mu <- numeric(d)
set.seed(100)
system.time(for (i in 1:N)
  rmvn.eigen(n, mu, cov(matrix(rnorm(n*d), n, d))))
set.seed(100)
system.time(for (i in 1:N)
  rmvn.svd(n, mu, cov(matrix(rnorm(n*d), n, d))))
set.seed(100)
system.time(for (i in 1:N)
  rmvn.Choleski(n, mu, cov(matrix(rnorm(n*d), n, d))))
set.seed(100)
system.time(for (i in 1:N)
  mvrnorm(n, mu, cov(matrix(rnorm(n*d), n, d))))
set.seed(100)
system.time(for (i in 1:N)
  rmvnorm(n, mu, cov(matrix(rnorm(n*d), n, d))))
set.seed(100)
system.time(for (i in 1:N)
  cov(matrix(rnorm(n*d), n, d)))
detach(package:MASS)

###多元正态的混合
library(MASS) #for mvrnorm
loc.mix <- function(n, p, mu1, mu2, Sigma) {
  #generate sample from BVN location mixture
  n1 <- rbinom(1, size = n, prob = p)
  n2 <- n - n1
  x1 <- mvrnorm(n1, mu = mu1, Sigma)
  x2 <- mvrnorm(n2, mu = mu2, Sigma)
  X <- rbind(x1, x2) #combine the samples
  return(X[sample(1:n), ]) #mix them
}
###实例
x <- loc.mix(1000, .5, rep(0, 4), 2:5, Sigma = diag(4))
r <- range(x) * 1.2
par(mfrow = c(2, 2))
for (i in 1:4)
  hist(x[ , i], xlim = r, ylim = c(0, .3), freq = FALSE,
       main = "", breaks = seq(-5, 10, .5))
par(mfrow=c(1,1))
###
runif.sphere <- function(n, d) {
  # return a random sample uniformly distributed
  # on the unit sphere in R ^d
  M <- matrix(rnorm(n*d), nrow = n, ncol = d)
  L <- apply(M, MARGIN = 1,
             FUN = function(x){sqrt(sum(x*x))})
  D <- diag(1 / L)
  U <- D %*% M
U
}
###产生200个2维圆周上的均匀分布随机数散点图
X <- runif.sphere(200, 2)
par(pty = "s")
plot(X, xlab = bquote(x[1]), ylab = bquote(x[2]))
par(pty = "m")

###齐次泊松过程