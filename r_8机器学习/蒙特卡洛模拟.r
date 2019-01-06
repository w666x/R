#R语言中文社区
setwd("E:/Rexercise1/R语言中文社区")
#蒙特卡洛模拟相关实例
#正态分布
#二项分布-扔硬币运动
#估计pi的值
#A/B Test & Beta distribution
#机率，赌转盘
#股票预测

#正态分布的模拟
set.seed(1)
cord.x <- c(0, seq(0, 5, 0.1), 5) 
cord.y <- c(0, dnorm(seq(0, 5, 0.1), 0, 5), 0) 
curve(dnorm(x, 0, 5), xlim = c(-15,15), main = 'Normal Distribution, PDF', col="darkgreen", xlab="", ylab="Density",
      type="l", lwd=2, cex=2,cex.axis=.8) 
polygon(cord.x, cord.y, col='skyblue') #多边形绘制
runs <- 1000
xs <- rnorm(runs,mean = 0, sd = 5)
sum(xs >= 0 & xs <= 5)/runs #从其中找出符合上述条件的随机数的个数

#扔硬币运动
runs <- 1000
one.trial <- function(){
  sum(sample(c(0, 1), 10, replace = T)) > 5 #0,1 随机数中出现1的个数是否大于5
}  #返回值为T或者F
sum(replicate(runs,one.trial()))/runs #重复1000次one.trial函数,其中返回的T是可以叠加的
pbinom(q = 5, size = 10, prob = 0.5, lower.tail=FALSE)

#Estimate pi
runs <- 10000
xs <- runif(runs, min = -5, max = 5)
ys <- runif(runs, min = -5, max = 5)
in.circle <- xs^2 + ys^2 <= 5^2 #提取出圆内中出现的随机数
(sum(in.circle)/runs)*4 #如果在圆内则为蓝色，否则为橙色
plot(xs,ys,pch='.',col=ifelse(in.circle,"blue","orange"), xlab='',ylab='',asp=1,
     main=paste("MC Estimate of Pi =", (sum(in.circle)/runs)*4))

#A/B Test & Beta distribution
x = seq(0,1,length=100) #构造两个参数不同的beta分布
y = dbeta(x,25,75) #y是beta分布的
plot(x,y, type="l", col="darkgreen", lwd=2, cex=2,cex.axis=.8, ylim = c(0,12))
text(0.24,11, 'A', col = 'darkgreen')
curve(dbeta(x,37,63), type="l", col="blue", lwd=2, cex=2,cex.axis=.8, add = T)
text(0.34,11, 'B', col = 'blue')
runs <- 1000 #开始执行随机模拟
a.samples <- rbeta(runs,25,75)
b.samples <- rbeta(runs,37,63)
sum(a.samples > b.samples)/runs
# statistically significant #横坐标为ratio
hist(b.samples/a.samples, main = 'Difference ratio between two simulated\n distribution to show average improvement')

#赌转盘，机率
runs <- 1000
game <- function(){ #可重复抽样
  results <- sample(c(-1, 0, 1 , 2), 10, replace=T)
  return(sum(results) > 5) #返回是否大于5
}
sum(replicate(runs, game()))/runs #求得大于5即T的比例

#股票预测
days <- 200
changes <- rnorm(200,mean=1.001,sd=0.005) #产生随机数,作为波动率
plot(cumprod(c(20,changes)), type='l', ylab="Price", xlab="Day", main="StockA Closing Price", 
     col = 'darkgreen', lwd=2, cex=2,cex.axis=.8)
runs <- 1000
price <- function(){
  days <- 200
  changes <- rnorm(200,mean=1.001,sd=0.005)
  sample.path <- cumprod(c(20,changes))
  closing.price <- sample.path[days]
  return(closing.price)
}
mc.closing <- replicate(runs, price())
hist(mc.closing)
median(mc.closing)
# upper and lower 95th percentiles
quantile(mc.closing,0.95) #= 27.35
quantile(mc.closing,0.05) #= 21.69
path <- function(days, reps){
  sample.path <- matrix(NA, reps, days+1, byrow = T)
  for (i in 1:reps){
    changes <- rnorm(days, mean=1.001, sd=0.005)
    sample.path[i,] <- cumprod(c(20, changes))}
  sample.path <- data.frame(t(sample.path))
  colnames(sample.path)=1:reps
  sample.path$day = as.numeric(rownames(sample.path))
  return(sample.path)
}
library(tidyr)
library(ggplot2)
sample=path(10,3)
sample %<>% gather(scenario,price,1:3) %>% ggplot(aes(x=day, y=price)) + geom_smooth() + 
  stat_summary(fun.y = mean, geom = "point", col = 'red') + ggtitle('MC Simulated Stock Scenario')
