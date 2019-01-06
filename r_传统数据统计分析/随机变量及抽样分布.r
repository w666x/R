##随机变量及其分布
#二项分布
n=20;p=0.5;x=1:5;
p=choose(n,x)*p^x*(1-p)^(n-x);
plot(x,p,'h')
#超几何分布
N=100;M=5;n=10;x=1:5;
P=choose(N-M,n-x)*choose(M,x)/choose(N,n);
plot(x,P,'h')
#
m=10;n=7;k=8;
x=0:(k+1);
px=phyper(x,m,n,k)
dx=dhyper(x,m,n,k)
cbind(px,dx)
#泊松分布
lamda=6;x=0:5;
P=lamda^x*exp(-lamda)/factorial(x)
plot(x,P,'h')
##连续型随机变量
#均匀分布
x=0:1;
y=c(1,1)
plot(x,y,ylim=c(0,1.5),type='l')
#正态分布
x=seq(-4,4,0.1)
y=1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type='l')
P=pnorm(2)  #正态分布的累积概率
alpha=qnorm(0.95) #正态概率的分位数
#指数分布
x=0:50;
y=0.3*exp(-0.3*x)
plot(x,y,type='l')
P=pexp(2) #指数分布的累积概率
alpha=qexp(0.85) #指数分布的分位数
##随机数与随机抽样
#二项分布随机数
rbinom(10,1,0.5)
rbinom(10,5,0.35)
n=100;p=0.25
c=rbinom(n,50,p)
hist(c,prob=T,main=paste("n=",n))
xn=0:n
points(xn,dbinom(xn,n,p),type="h",lwd=3)
#超几何随机分布数
rhyper(15,10,5,3) 
#泊松分布随机数
rpois(10,lambda=4)
##连续型随机变量
runif(5,0,1)
runif(3,1,3)
runif(5)
#做出10000个均匀分布随机数，做出其概率直方图
x=runif(100000,0,1)
hist(x,prob=T,ylim=c(0,1.5),main='uniform(0,1)')
curve(dunif(x,0,1),add=T)  #添加均匀分布的密度函数线
#正态分布随机数
rnorm(5,10,5)
rnorm(5)
#做出1000个标准正态分布随机数，做出其概率直方图
x=rnorm(1000)
hist(x,prob=T,ylim=c(0,0.5),main='N(0,1)')
curve(dnorm(x),add=T)
#指数分布随机数
rexp(10,1)
#生成1000个均值为1/10的指数分布随机数
x=rexp(1000,1/10)
hist(x,prob=T,ylim=c(0,0.1),main="exp(1/10)")
curve(dexp(x,1/10),add=T)
#抽样分布
sample(c("H","T"),10,rep=T)
sample(100,10,rep=T)
##抽样分布的基本性质
#卡方分布
x=seq(0,20,0.1)
curve(dchisq(x,2),0,20,ylab="p(x)")  #作出了自由度为2的卡方分布，Y轴为p(x)值
curve(dchisq(x,4),add=T,lty=2) #加自由度为4的卡方分布，线条类型为2，下同
curve(dchisq(x,6),add=T,lty=3)
curve(dchisq(x,8),add=T,lty=4)
curve(dchisq(x,10),add=T,lty=5)
legend(13,0.4,c("n=2","n=4","n=6","n=8","n=10"),lty=1:5,bty="n")
#t分布
x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",lty=1)
for (i in c(1,5,10))
points(x,dt(x,df=i),type="l",lty=i+1)
legend(2,0.3,c("N(0,1)","t(10)","t(5)","t(1)"),lty=1:4,bty="n")
#F分布
x=seq(0,6,0.1)
plot(x,df(x,3,3),type="l",ylim=c(0,1.2),ylab="p(x)")
curve(df(x,5,5),add=T,lty=3)
curve(df(x,10,10),0.6,add=T,lty=4)
curve(df(x,20,20),add=T,lty=5)
curve(df(x,30,30),add=T,lty=6)
legend(4,1,c("n=3","n=5","n=10","n=20","n=30"),lty=2:6,bty="n")  #此处的4和1是显示图例的位置的
#中心极限定理的应用
m=100 #m次模拟次数
n=10;p=0.25;
z=rbinom(m,n,p)  #产生100个二项随机数
x=(z-n*p)/sqrt(n*(1-p)*p)  #对100个二项随机数标准化
hist(x,prob=T,main=paste("n=",n))
curve(dnorm(x),add=T) #增加正态曲线
##抽样分布的临界值的表格
#标准正态分布概率表
u0=seq(0,3,by=0.1) #临界表的行值
u.0=seq(0,0.1,by=0.01)  #临界表的列值
u=u0+matrix(u.0,31,11,byrow=T) #临界分位数
p=pnorm(u)  #临界表概率值
colnames(p)<-u.0  #临界表列标记
rownames(p)<-u0  #临界表行标记
p  #形成正态分布临界表
#t分布的临界值的表格
a=c(0.01,0.025,0.05,0.1,0.5,0.95,0.975,0.99)  #尾部概率
n=2:20  #样本数
cbind(n,"0.01"=qt(0.01,n),"0.025"=qt(0.025,n))
