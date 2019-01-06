###横截面数据回归：机器学习方法
##横截面数据无法作任何分布假定，故介绍机器学习中的算法
#决策树回归(决策树本身受数据影响比较大，但是许多决策树结合效果就很好了）
#boosting回归，bagging回归，随机森林回归（以众多决策树作为基本决策单元）

###boosting回归方法
#若一个弱学习器的犯错误率为0.49，则10001个弱学习器的犯错概率为1-pbinom(5000,10001,0.49)
#前提是弱学习器的错误率小于0.5

#支持向量机回归，人工神经网络回归等
#介绍了通过交叉验证，比较了各种回归的预测效果
#讨论了稳定性以及过拟合的现象
####当数据类型出了问题的时候，可以选择先将其转储为csv文件，然后再读取

setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/数据集")
w=read.table("mg.txt")#读取数据
#head(w)
w=matrix(w,ncol = 13,byrow = T)
#head(w)
w=w[,c(1,3,5,7,9,11,13)]
head(w)
dim(w)
w=as.matrix(w)
w=as.data.frame(w)
names(w)=c("y",paste("x",1:6,sep = ""))
class(w)
class(w$y)
head(y)
str(w)
###当数据类型出了问题的时候，可以选择先将其转储为csv文件，然后再读取
write.csv(w,file = "mg.csv")
w=read.csv("mg.csv")
head(w)
class(w) 
w=w[,-1]
a=lm(y~.,data=w) #作简单线性回归
summary(a)
cor(w) #计算相关系数表
pairs(w)
###简单线性回归效果不佳，相关系数和两两散点图看不出变量之间的模式，故尝试机器学习
##使用几种不同算法建模，并使用交叉验证（均方误差和标准化均方误差）对各种方法进行比较

###使用十折交叉验证的方法（按照随机原则，建立十个训练集建立十个模型，对测试集分别得到十个标准均方误差）
#构造训练集和测试集如下
CV=function(n,Z=10,seed=888){ #n为样本量，Z为折的数目
  z=rep(1:Z,ceiling(n/Z))[1:n]; ##仅取前n个数
  set.seed(seed);z=sample(z,n); ##设置种子
  mm=list();
  for (i in 1:Z) 
   mm[[i]]=(1:n)[z==i]; #提取n个数据中的对应为i的位置
  return(mm) ##设置下标集
}
##提取训练集如下
w=read.csv("mg.csv")
n=nrow(w)
Z=10;
mm=CV(n,Z)
D=1
###计算10折交叉验证的测试集的NMSE
MSE=rep(0,Z) #建立一个向量储存结果
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=lm(y~.,w[-m,]) #训练集作简单线性回归
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
mean(MSE) #测试集的NMSE



###决策树回归，回归树部分
dim(w)
head(w)
w=w[,-1]
library(rpart.plot)
a=rpart(y~.,w);a #计算并展示输出决策树的细节
rpart.plot(a,type = 2,faclen = T)  #画出决策树的图
#交叉验证，(十折交叉验证)通过十个数据集构建十棵决策树，并计算测试集的NMSE
###计算10折交叉验证的测试集的NMSE
MSE=rep(0,Z) #建立一个向量储存结果
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=rpart(y~.,w[-m,]) #训练集作决策树回归
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
MSE
mean(MSE) #测试集的NMSE

###boosting回归方法
#若一个弱学习器的犯错误率为0.49，则10001个弱学习器的犯错概率为1-pbinom(5000,10001,0.49)
#前提是弱学习器的错误率小于0.5
##boosting回归，十折交叉验证方法检验
##此处使用的程序包为mboost
library(mboost)
MSE=rep(0,Z)
set.seed(1001);
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=mboost(y~btree(x1)+btree(x2)+btree(x3)+btree(x4)+btree(x5)+btree(x6),data = w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
MSE
mean(MSE)


###bagging回归
library(ipred)
MSE=rep(0,Z)
set.seed(1001);
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=bagging(y~.,data = w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
MSE
mean(MSE)


###随机森林回归
library(randomForest)
MSE=rep(0,Z)
set.seed(1001);
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=randomForest(y~.,data = w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
summary(a$forest)
plot(a) #可以直接直观的确定随机森林的个数
MSE
mean(MSE)
#对全部数据作随机森林回归，可以输出重要性度量
library(randomForest)
set.seed(1010)
SS=randomForest(y~.,data=w,importance=T,proximity=T)
SS$importance


###支持向量机回归，交叉验证法检验
library(rminer)
set.seed(1001);MSE=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=fit(y~.,data = w[-m,],model = "svm")
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
summary(a)
MSE
mean(MSE)
###支持向量机回归二
library(e1071)
set.seed(1001);MSE=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=svm(y~.,data = w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
summary(a);names(a)
MSE
mean(MSE)
###支持向量机三
library(kernlab)
set.seed(1001);MSE=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=ksvm(y~.,data = w[-m,],model = "svm")
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
summary(a)
MSE
mean(MSE)



###人工神经网络回归（一般由交叉验证法选择隐藏层的节点个数）
#为了寻找合适的隐藏层节点数目,此处就可以找出最优的隐藏层节点数目以及decay权重衰减参数了
require(nnet)
require(caret)
set.seed(1001) #由于神经网络一般只处理(0,1)之间的数据，故除以最大值标准化
mygrid=expand.grid(.decay=c(.5,.1),.size=c(4,5,6))
nnetfit=train(y/max(w[,D])~.,data = w,method="nnet",
              maxit=1000,tuneGrid=mygrid,trace=F)
print(nnetfit)
###神经网络交叉验证法
set.seed(1001);MSE=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2)
  a=nnet(y/max(w[,D])~.,data = w[-m,],size=4,decay=.1)
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求取测试集的NMSE
}
summary(a)
MSE
mean(MSE)
###人工神经网络二(由于neuralnet函数的公式中不能有运算符)
ny=(1:ncol(w))[-D]
nn1=paste(names(w)[D],"~",names(w)[ny[1]],sep = "")
for (i in (1:ncol(w))[-D][-1]){
  nn1=paste(nn1,"+",names(w)[i],sep = "")
}
  v=w;v[,D]=v[,D]/max(w[,D]) #此处处理w,标准化为v

#上述的nn1即为对数据V的公式
##实施交叉验证
library(neuralnet)
set.seed(1001);MSE=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];M=mean((w[m,D]-mean(w[m,D]))^2) #对训练集进行操作
  a=neuralnet(nn1,data=v[-m,],err.fct="sse",hidden=4,linear.output=F)
  MSE[i]=mean((w[m,D]-compute(a,v[m,-D])$net.result*max(w[,D]))^2)/M  #求取测试集的NMSE
}
summary(a)
MSE
mean(MSE)



###十折交叉验证结果汇总及方法的稳定性讨论
##对于mg数据，我们使用不同数目的随机森林，以及不同隐藏节点数的神经网络来拟合
##此处采用的是二折交叉验证
w=read.csv("mg.csv")
head(w)
w=w[,-1]
head(w);dim(w)
m=sample(1:nrow(w),ceiling(nrow(w)/2)) #训练集和测试集各占一半
library(randomForest)
n=100;NMSE=rep(0,n)->NMSE0
set.seed(1010)
for (i in 1:n){
  A=randomForest(y~.,data=w[-m,],ntree=i)
  y0=predict(A,w[-m,]);y1=predict(A,w[m,])
  NMSE0[i]=mean((w$y[-m]-y0)^2)/mean((w$y[-m]-mean(w$y[-m]))^2) #训练误差
  NMSE[i]=mean((w$y[m]-y1)^2)/mean((w$y[m]-mean(w$y[m]))^2)
}
library(neuralnet)
set.seed(1010);n1=40;nmse=rep(0,n1)->nmse0
for (i in 1:n1){
  a=neuralnet(nn1,data=v[-m,],err.fct="sse",hidden=i,linear.output=F)
  y0=compute(a,v[-m,-D])$net.result*max(w[,D])
  y1=compute(a,v[m,-D])$net.result*max(w[,D])
  nmse0[i]=mean((w$y[-m]-y0)^2)/mean((w$y[-m]-mean(w$y[-m]))^2)
  nmse[i]=mean((w$y[m]-y1)^2)/mean((w$y[m]-mean(w$y[m]))^2)
}
###作图
par(mfrow=c(1,2))
plot(1:n,NMSE,type = "l",ylim=c(min(NMSE,NMSE0),max(NMSE,NMSE0)),lty=2); #测试误差
lines(1:n,NMSE0) #训练误差
legend("topright",lty = 1:2,c("training set","testing set"),cex = .4,bty = "n")
plot(1:n1,nmse,type = "l",ylim=c(min(nmse,nmse0),max(nmse,nmse0)),lty=2);
lines(1:n1,nmse0)
legend("topright",lty = 1:2,c("training set","testing set"))
