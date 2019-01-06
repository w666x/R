###中级绘图
###二元变量和多元变量关系的可视化
###绘制散点图和折线图
###理解相关图
###学习马赛克图和关联图
##添加了最佳拟合曲线的散点图
attach(mtcars)
plot(wt,mpg,
     main = "Basic Scatter plot of MPG vs. Weight",
     xlab = "Car Weight (lbs/1000)",
     ylab = "Milee Per Gallon",pch=19)
abline(lm(mpg~wt),col="red",lwd=2,lty=2)
lines(lowess(wt,mpg),col="blue",lwd=2,lty=2)
###car包的应用
install.packages("car")
library(car)
scatterplot(mpg~wt | cyl,data=mtcars,lwd=2,
            main="Scatter Plot of MPG vs.Weight by # Cylinders",
            xlab="Weight of car (lbs/1000)",
            ylab="Miles Per Gallon",
            legend.plot=T,
            id.method="identify",
            labels=row.names(mtcars),
            boxplots="xy"
)
detach()

###散点图矩阵
pairs(~mpg+disp+drat+wt,data = mtcars,
      main="Basic Scatter Plot Matrix",
      lower.panel=NULL)
library(car)
scatterplotMatrix(~mpg+disp+drat+wt,data=mtcars,
                  spread=F,lty.smooth=2,
                  main="Scatter Plot Matrix via car Package")
library(car)
scatterplotMatrix(~mpg+disp+drat+wt | cyl,data=mtcars,
                  spread=F,diagonal="histgram",
                  by.groups=T,
                  main="Scatter Plot Matrix via car package")

###gclus包生成的散点图矩阵
install.packages("gclus")
library(gclus)

###高密度散点图
set.seed(1234)
n=10000;c1=matrix(rnorm(n,mean = 0,sd=0.5),ncol=2)
c2=matrix(rnorm(n,mean = 3,sd=2),ncol = 2)
mydata=rbind(c1,c2)
mydata=as.data.frame(mydata)
names(mydata)=c("x","y")
dim(mydata)
with(mydata,
     plot(x,y,pch=19,main = "Scatter Plot with 10000 Observations"))
with(mydata,  ##用核密度估计生成颜色密度表示点的分布
     smoothScatter(x,y,main="Scatterplot colored"))

###三维散点图,基本绘图
install.packages("scatterplot3d")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg,
              main = "Basic 3D Scatter Plot",
              pch=16,highlight.3d=T,
              type = "h")
s3d=scatterplot3d(wt,disp,mpg,pch=16,
                  highlight.3d = T,type = "h",
                  main = "3D Scatter Plot with")
fit=lm(mpg~wt+disp)
s3d$plane3d(fit)

###气泡图
###散点的大小也是可以说明信息的
r=sqrt(disp/pi)
symbols(wt,mpg,circles = r,inches = .3,
        fg="white",bg="lightblue",
        main = "Bubble Plot with point size",
        ylab = "Miles Per Gallon",
        xlab = "Weight of Car (lbs/1000)")
text(wt,mpg,rownames(mtcars),cex = .6)
detach(mtcars)

###折线图
opar=par(no.readonly = T)
par(mfrow=c(1,2))
t1=subset(Orange,Tree==1)
plot(t1$age,t1$circumference,xlab = "Age",
     ylab = "Circumference",main = "Orange Tree")
plot(t1$age,t1$circumference,xlab = "Age",type = "b",
     ylab = "Circumference",main = "Orange Tree")
par(opar)
###复杂折线图的生成
Orange$Tree=as.numeric(Orange$Tree)
ntress=max(Orange$Tree)
##创建图形框架以及参数设计
xrange=range(Orange$age)
yrange=range(Orange$circumference) 
plot(xrange,yrange,type = "n",xlab = "Age",
     ylab = "Circumference")
colors=rainbow(ntress)
linetype=c(1:ntress)
plotchar=seq(18,18+ntress,1)
###添加线条
for (i in 1:ntress){
  tree=subset(Orange,Tree==i)
  lines(tree$age,tree$circumference,
        type = "b",lwd=2,lty=linetype[i],
        col=colors[i],pch=plotchar[i])
}
###添加图例
title("Tree Growth","example of line plot")
legend(xrange[1],yrange[2],
       1:ntress,cex = .6,col = colors,
       pch = plotchar,lty = linetype,title = "Tree")

###相关图（相关关系的可视化）
options(digits = 2)
cor(mtcars) ##生成相关矩阵
###导入数据包，生成相关图
install.packages("corrgram")
library(corrgram)
search()
corrgram(mtcars,order = T,lower.panel = panel.shade,
         upper.panel = panel.pie,text.panel = panel.txt,
         main="Correlogram of mtcars intercorrelations")
corrgram(mtcars,order = T,lower.panel = panel.ellipse,
         upper.panel = panel.pts,text.panel = panel.txt,
         diag.panel = panel.minmax,
         main="Correlogram of mtcars data using scatter plot")

###马赛克图
ftable(Titanic)
install.packages("vcd")
install.packages("grid")
library(vcd)
mosaic(Titanic,shade=T,legend=T)
mosaic(~Class+Sex+Age+Survived,data = Titanic,
       shade=T,legend=T)


###重抽样和自助法
###理解置换检验的逻辑
###在线性模型中应用置换检验
###利用自助法获得置信区间
install.packages(c("coin","lmPerm"))
library(coin)
library(survival)
score=c(40,57,45,55,58,57,64,55,62,65)
treatment=factor(c(rep("A",5),rep("B",5)))
###传统t检验方法
mydata=data.frame(treatment,score)
t.test(score~treatment,data = mydata,var.equal=T)
###单因素置换检验
oneway.test(score~treatment,data = mydata,
            distribution='exact')
help("oneway_test")
traceback()
###wilcox检验双样本是否有显著差异
library(MASS)
head(UScrime)
UScrime=transform(UScrime,So=factor(So))
wilcox.test(Prob~So,data = UScrime,
            distribution="exact")
###K样本检验，单因素方差分析
library(multcomp)
head(cholesterol)
set.seed(1234)
oneway_test(response~trt,data = cholesterol,
            distribution=approximate(B=9999))

###列联表中的独立性
library(coin)
library(vcd)
Arthritis=transform(Arthritis,
                    Improved=as.factor(as.numeric(Improved)))
head(Arthritis)
set.seed(1234)
chisq_test(Treatment~Improved,data=Arthritis
          ,distribution=approximate(B=9999))

###数值变量间的独立性
###两样本和K样本相关性检验coin包 
vignette("coin")
UseMethod("oneway_test")

###简单回归和多项式回归
install.packages("lmPerm")
library(lmPerm)
set.seed(1234)
fit=lmp(weight~height,data=women,perm="Prob")


###自助法包及其应用实例
###单个统计量的自助法
install.packages("boot")
library(boot)
###写出一个目标函数，获取目标信息的目标函数
rsq=function(formula,data,indices){
  d=data[indices,]
  fit=lm(formula,data = d)
  return(summary(fit)$r.square)
}
###执行自助抽样,获得大量的R方
library(boot)
set.seed(1234)
results=boot(data = mtcars,statistic = rsq,R=1000,
             formula=mpg~wt+disp)
print(results)
plot(results)
###获取其置信区间
boot.ci(results,type = c("perc","bca"))

###多个统计量的自助法
bs=function(formula,data,indices){
  d=data[indices,]
  fit=lm(formula,data = d)
  return(coef(fit))
}
###执行自助抽样，获得大量的回归系数的结果
set.seed(1234)
results=boot(data = mtcars,statistic = bs,
             R=1000,formula=mpg~wt+disp)
print(results)
plot(results,index = 2)
plot(results,index = 1)
###获取其置信区间
boot.ci(results,type = "bca",index = 1)
