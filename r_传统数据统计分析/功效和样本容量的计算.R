###功效和样本容量的计算
###样本容量的计算方法
###功效是指拒绝错误原假设的可能性大小
###配对样本t检验的功效
setwd("E:/Rexercise1")
###累积分布图
###ncp值即为非中心参数v
curve(pt(x,25,ncp=3),from = 0,to=6)
###画出拒绝域的那条垂线
abline(v=qt(.975,25))
###计算检验统计量落入接受域的概率,小于0.975分位点的概率
pt(qt(.975,25),25,ncp = 3)
###样本容量n，功效beta，显著性水平下标准差sigma，相关差异（即偏差）theta
###确定样本容量
x=rt(25,ncp = 3,n=100)
sigma=sd(x)
alpha=0.5;beta=0.8;theta=0.01;ncp=3
n=ncp^2*sigma^2/theta
pt(0,25)
qt(0.975,25)
pt(0.18,25,ncp = 3)
help("pt")

###两样本t检验的功效
###两样本问题
###delta表示真实差异，sd表示标准差，显著性水平为0.01，功效为0.9
###这是求样本量至少为多少
power.t.test(delta = 0.5,sd=2,sig.level = 0.01,power = 0.9)
help("power.t.test")
power.t.test
###样本量等为450，求功效
power.t.test(n=450,delta = 0.5,sd=2,sig.level = 0.01)
###求功效二
delta=0.5;sd=2;sig.level=0.01;n=450;
ncp=delta*sqrt(n)/sigma
curve(pt(x,25,ncp=ncp))
abline(v=qt(0.9,25))
pt(qt(0.9,25),25,ncp = ncp)
###求样本容量三
power.t.test(delta = 0.5,sd=2,sig.level = 0.01,power = 0.9,
             alternative = "one.sided")

###单样本问题及配对样本检验
power.t.test(delta = 10,sd=10*sqrt(2),power = 0.85,type = "paired")
###比例的比较
power.prop.test(power = 0.85,p1=0.15,p2=0.3)


#####功效分析（实验设计中）并有功效图
###判断所需样本量
###计算效应值
###评价统计功效
install.packages("pwr")
library(pwr)
pwr.2p.test
example("pwr.p.test")
##举例
pwr.t.test(d=.8,sig.level = .05,power = .9,
           type = "two.sample",alternative = "two.side")
pwr.t.test(n=20,d=.5,sig.level = .01,type = "two.sample",
           alternative = "two.side")
pwr.t2n.test(n1=30,n2=40,sig.level = .05,
             power = .9,alternative = "two.side")

###方差分析
pwr.anova.test(k=5,f=.25,sig.level = .05,power = .8)
###相关性
pwr.r.test(r=.25,sig.level = .05,power = .9,
           alternative = "greater")
###线性模型
pwr.f2.test(u=3,f2=.0769,sig.level = .05,power = .9)
##比例检验
pwr.2p.test(h=ES.h(.65,.6),sig.level = .05,power = .9,
            alternative = "greater")
##卡方检验
prob=matrix(c(.42,.28,.03,.07,.1,.1),byrow = T,nrow = 3)
ES.w2(prob)
pwr.chisq.test(w=.1853,df=2,sig.level = .05,
               power =.9)
###在新情况中选择合适的效应值
es=seq(.1,.5,.01)
nes=length(es)
samsize=NULL
for (i in 1:nes) {
  result=pwr.anova.test(k=5,f=es[i],sig.level = .05,
                        power = .9)
  samsize[i]=ceiling(result$n)
  
}
plot(samsize,es,type = "l",lwd=2,col="red",
     ylab = "Effect Size",
     xlab = "Sample Size (per cell)",
     main = "One Way Anova with power=.9 and alpha=.5")

###绘制功效分析图形
r=seq(.1,.5,.01)
nr=length(es)
p=seq(.4,.9,.1)
np=length(p)
samsize=array(numeric(nr*np),dim = c(nr,np))
for (i in 1:np) {
  for (j in 1:nr){
    result=pwr.r.test(n=NULL,r=r[j],sig.level=.05,
                      power = p[i],alternative="two.side")
    samsize[j,i]=ceiling(result$n)
  }
}
##创建图形 ,创建绘图区域
xrange=range(r)
yrange=range(range(samsize))
colors=rainbow(length(p))
plot(xrange,yrange,type = "n",
     ylab = "sample size (n)",
     xlab = "Correlation Coefficient (r)")
###添加功效曲线
for (i in 1:np){
  lines(r,samsize[,i],type = "l",lwd=2,col=colors[i])
}
###添加注释
abline(v=0,h=seq(0,yrange[2],50),lty=2,col="grey89")
abline(h=0,v=seq(xrange[1],xrange[2],.02),lty=2,
       col="gray89")
title("sample size estimation for correlation studies\n
      sig=.05 (two-tailed)")
legend("topright",title = "Power",as.character(p),
       fill = colors,cex = 0.6)
