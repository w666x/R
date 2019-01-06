###蒙特卡罗法计算积分
m=10000
x=runif(m,min = 2,max = 4)
theta.hat=mean(exp(-x))*2
print(theta.hat)
print(exp(-2)-exp(-4))
###蒙特卡罗法计算无穷积分
###方法一
x <- seq(.1, 2.5, length = 10)
m <- 10000
u <- runif(m)
cdf <- numeric(length(x))
for (i in 1:length(x)) {
  g <- x[i] * exp(-(u * x[i])^2 / 2)
  cdf[i] <- mean(g) / sqrt(2 * pi) + 0.5
}
Phi <- pnorm(x)
print(round(rbind(x, cdf, Phi), 3))
###方法二
x <- seq(.1, 2.5, length = 10)
m <- 10000
z <- rnorm(m)
dim(x) <- length(x)
p <- apply(x, MARGIN = 1,
           FUN = function(x, z) {mean(z <= x)}, z = z)
Phi <- pnorm(x)
print(round(rbind(x, p, Phi), 3))
###蒙特卡罗积分的误差界，置信区间求解
x <- 2
m <- 10000
z <- rnorm(m)
g <- (z <= x) #the indicator function
v <- mean((g - mean(g))^2) / m
cdf <- mean(g)
c(cdf, v)
c(cdf - 1.96 * sqrt(v), cdf + 1.96 * sqrt(v))
all(4>rnorm(1000))

###何种求积分的方法更有效率
###对偶变量求积分，前提要求是函数t是单调的，
###可以只需求出一般的随机变量，剩下一半由1-u得到
MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
  u <- runif(R/2)
  if (!antithetic) v <- runif(R/2) else
    v <- 1 - u
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] * exp(-(u * x[i])^2 / 2)
    cdf[i] <- mean(g) / sqrt(2 * pi) + 0.5
  }
  cdf
}
MC.Phi(100)
##和简单的蒙特卡罗模拟pnorm作比较
x <- seq(.1, 2.5, length=5)
Phi <- pnorm(x)
set.seed(123)
MC1 <- MC.Phi(x, anti = FALSE)
set.seed(123)
MC2 <- MC.Phi(x)
print(round(rbind(x, MC1, MC2, Phi), 5))
##方差减少量
m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 1.95
for (i in 1:m) {
  MC1[i] <- MC.Phi(x, R = 1000, anti = FALSE)
  MC2[i] <- MC.Phi(x, R = 1000)
}
print(sd(MC1))
print(sd(MC2))
print((var(MC1) - var(MC2))/var(MC1))

###蒙特卡罗模拟中控制变量减少方差
x <- seq(.1, 2.5, length=5)
Phi <- pnorm(x)
set.seed(123)
MC1 <- MC.Phi(x, anti = FALSE)
set.seed(123)
MC2 <- MC.Phi(x)
print(round(rbind(x, MC1, MC2, Phi), 5))
###确定方差的减少量
m <- 10000
a <- - 12 + 6 * (exp(1) - 1)
U <- runif(m)
T1 <- exp(U) #simple MC
T2 <- exp(U) + a * (U - 1/2) #controlled
mean(T1)
mean(T2)
(var(T1) - var(T2)) / var(T1)
###使用控制变量法的蒙特卡罗法求积分
###输入g(x)以及足够接近g（x）的控制函数f(x)
f <- function(u) exp(-.5)/(1+u^2)
g <- function(u) exp(-u)/(1+u^2)
set.seed(510) #needed later
u <- runif(10000)
B <- f(u)
A <- g(u)
a <- -cov(A,B) / var(B) #est of c*
m <- 100000
u <- runif(m)
T1 <- g(u)
T2 <- T1 + a * (f(u) - exp(-.5)*pi/4)
c(mean(T1), mean(T2))
c(var(T1), var(T2))
(var(T1) - var(T2)) / var(T1)

###对偶控制变量的蒙特卡罗方法求积分
###控制变量和回归，使用回归方法估计积分
set.seed(510)
u <- runif(10000)
f <- exp(-.5)/(1+u^2)
g <- exp(-u)/(1+u^2)
L <- lm(g~f)
c.star <- - L$coeff[2] # beta[1]
mu <- exp(-.5)*pi/4
c.star
theta.hat <- sum(L$coeff * c(1, mu)) #pred. value at mu
theta.hat
summary(L)$sigma^2
summary(L)$r.squared

###重要性抽样方法确定积分
###重要性函数的选择
m <- 10000
theta.hat <- se <- numeric(5)
g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}
x <- runif(m) #using f0
fg <- g(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)
x <- rexp(m, 1) #using f1
fg <- g(x) / exp(-x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)
x <- rcauchy(m) #using f2
i <- c(which(x > 1), which(x < 0))
x[i] <- 2 #to catch overflow errors in g(x)
fg <- g(x) / dcauchy(x)
theta.hat[3] <- mean(fg)
se[3] <- sd(fg)
u <- runif(m) #f3, inverse transform method
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
theta.hat[4] <- mean(fg)
se[4] <- sd(fg)
u <- runif(m) #f4, inverse transform method
x <- tan(pi * u / 4)
fg <- g(x) / (4 / ((1 + x^2) * pi))
theta.hat[5] <- mean(fg)
se[5] <- sd(fg)
rbind(theta.hat, se)

###由强大数定理可知，我们将要求的积分分成几个积分之和
M <- 20 #number of replicates
T2 <- numeric(4)
estimates <- matrix(0, 10, 2)
g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1) }
for (i in 1:10) {
  estimates[i, 1] <- mean(g(runif(M)))
###将【0,1】区间分为四段分别求积分值
  T2[1] <- mean(g(runif(M/4, 0, .25)))
  T2[2] <- mean(g(runif(M/4, .25, .5)))
  T2[3] <- mean(g(runif(M/4, .5, .75)))
  T2[4] <- mean(g(runif(M/4, .75, 1)))
  estimates[i, 2] <- mean(T2)
}
estimates
apply(estimates, 2, mean)
apply(estimates, 2, var)
###分层抽样应用二
###没有调用特殊的辅助函数，仅仅是将抽样方式分层进行
M <- 10000 #number of replicates
k <- 10 #number of strata
r <- M / k #replicates per stratum
N <- 50 #number of times to repeat the estimation
T2 <- numeric(k)
estimates <- matrix(0, N, 2)
g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}
for (i in 1:N) {
  estimates[i, 1] <- mean(g(runif(M)))
  for (j in 1:k)
    T2[j] <- mean(g(runif(M/k, (j-1)/k, j/k)))
  estimates[i, 2] <- mean(T2)
}
apply(estimates, 2, mean)
apply(estimates, 2, var)
###分层抽样在重要性抽样方法中的应用
