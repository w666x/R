###在因变量为分类，而自变量含有多个分类变量或分类变量水平较多
##决策树（分类树），ababoost，bagging,随机森林，SVM，最邻近方法
###决策树进行分类
#决策树数字意义的说明是在输出的第二行中

###adaboost分类（adaptive boosting）
#以分类树作为基本分类器（对异常值和离群点较为敏感，对过拟合不敏感）

###bagging分类（自助整合法）
#对样本进行许多次的有放回抽样，每次抽样建立一分类树

###随机森林分类（每个节点变量的产生有很大的随机性）
##和bagging相似，也是有放回的自助抽样基础上，建立决策树
#优点：不惧怕大的维数，样本量很少，变量个数很多的情况下，效果也很好，颠覆了传统统计

###最邻近方法分类（交叉验证后，我们才可以知道k的最优值）
##连续情况下，在空间中，以欧氏距离，平方欧氏距离定义一个距离
##离散情况下，采取多数投票方法，即该点最邻近的k个点中，大多数属于的类型

getwd()
setwd("E:/Rexercise1/复杂数据统计方法_基于R的应用/数据集")
w=read.csv("CTG.csv")
##数据集的预处理
#head(w)
w=w[-1,]
#head(w);dim(w)
w=w[,c(7:17,19:28,39,40)]
dim(w);str(w)
names(w)
head(w)
##采取十折交叉验证的方法
##首先将因变量，NSP三种类型每一种都随机的分成十类，采用Fold()函数
##定义fold函数，其用来进行随机抽样，因变量中的每一种类型都随机抽样
Fold=function(Z=10,w,d,seed=7777){
  n=nrow(w);d=1:n;dd=list()
  e=levels(w[,D])
  T1=length(e);set.seed(seed)
  for (i in 1:T1) {
    d0=d[w[,D]==e[i]];j=length(d0)
    ZT=rep(1:Z,ceiling(j/Z))[1:j]
    id=cbind(sample(ZT,length(ZT)),d0);dd[[i]]=id
  }
  mm=list();
  for (i in 1:Z){
    u=NULL;
    for (j in 1:T){
      u=c(u,dd[[j]][dd[[j]][,1]==i,2])
      mm[[i]]=u
    }
  }
  mm  ##输出Z个下标集
}

##数据处理，生成交叉验证数据集
dim(w)
F1=21:23;for(i in F1) w[,i]=factor(w[,i]) #将分类变量因子化
str(w)
D=23;Z=10;n=nrow(w);#定义因变量的位置，以及交叉验证的折数
mm=Fold(Z,w,D,8888)
dim(mm)


###决策树进行分类
#决策树数字意义的说明是在输出的第二行中
library(rpart.plot)
a=rpart(NSP~.,w) #使用决策树进行拟合全部数据
rpart.plot(a,type = 2,extra = 4)
a
summary(a)
##预测
wp=predict(a,w,type = "class")
z=table(w[,D],wp)
z
sum(w[,D]!=wp,na.rm = T)/nrow(w)
##采用十折交叉验证法验证
library(rpart)
E=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];n1=length(m);
  a=rpart(NSP~.,w[-m,])
  E[i]=sum(w[m,D]!=predict(a,w[m,],type = "class"),na.rm =T)/n1
}
mean(E)
E


###adaboost分类（adaptive boosting）
##以分类树作为基本分类器（对异常值和离群点较为敏感，对过拟合不敏感）
head(w);str(w);summary(w);tail(w)
w=w[-c(2127,2128),]
tail(w)
dim(w)
library(adabag)
set.seed(4410)
a=boosting(NSP~.,w)  ##使用boosting对全部数据来一次拟合
wp=predict(a,w)$class
z=table(w[,D],wp)
z
sum(w[,D]!=wp)/nrow(w)
barplot(a$importance,cex.names = .4)  #画出变量的重要性图
rpart.plot(a$trees[[100]],type = 2,extra = 4) ##打印第100棵决策树
##预测（使用rpart包）
newdata=w[11,]
a=rpart(NSP~.,w)
predict(a,newdata[-D],type = "class")
##预测（使用adabag包）
newdata=w[11,]
b=boosting(NSP~.,w)
predict(b,newdata)$class
##预测（失败了，使用adabag包）
predict(b,newdata[-D])$class
newdata[,D]=factor(3)
predict(b,newdata)$class
##预测（需使用完整数据）
newdata=w[1:20,]
newdata[,D]=factor(rep("1",20)) ##随意赋水平内的值
levels(newdata[,D])=levels(w[,D])
predict(b,newdata)$class
##交叉验证的方法
##采用十折交叉验证法验证
set.seed(1010)
E=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];n1=length(m);
  a=boosting(NSP~.,w[-m,])
  E[i]=sum(as.character(w[m,D])!=predict(a,w[m,])$class)/n1
}
mean(E)
E


###bagging分类（自助整合法）
#对样本进行许多次的有放回抽样，每次抽样建立一分类树
#拟合全部数据
set.seed(1010)
D=23
dim(w)
a=bagging(NSP~.,w)
wp=predict(a,w)$class
z=table(w[,D],wp)
z
sum(w[,D]!=wp)/nrow(w)
barplot(a$importance,cex.names = 0.5) #画出变量的重要性图
rpart.plot(a$trees[[2]],type = 2,extra = 4) #画出第二棵树
#交叉验证
##采用十折交叉验证法验证
set.seed(1010)
E=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];n1=length(m);
  a=bagging(NSP~.,w[-m,])
  E[i]=sum(as.character(w[m,D])!=predict(a,w[m,])$class)/n1
}
mean(E)
E


###随机森林分类（每个节点变量的产生有很大的随机性）
##和bagging相似，也是有放回的自助抽样基础上，建立决策树
#优点：不惧怕大的维数，样本量很少，变量个数很多的情况下，效果也很好，颠覆了传统统计
library(randomForest)
set.seed(1010)
a=randomForest(NSP~.,w,importance=T,proximity=T)
wp=predict(a,w)
z=table(w[,D],wp);z
E0=(sum(z)-sum(diag(z)))/sum(z)
E0
#各个变量的相对重要性
par(mfrow=c(3,1))
matplot(importance(a)[,1:3],type = "o",pch = 1:3,lty = 1:3,col = 1,
        xlab = "Variable Number",ylab = "Importance")
title("Variable Importance for three levels of Response")
legend("topleft",legend = paste("NSP=",1:3,sep = ""),pch = 1:3,lty = 1:3,col = 1,cex = .2)
barplot(importance(a)[,4],cex.name = .6)
title("Variable Importance according to mean decrease accuracy")
barplot(importance(a)[,5],cex.name=.6)
title("Variable Importance according to mean decrease Gini")
par(mfrow=c(1,1))
##基于OOB（out of bag）的结果
a
a$confusion
print(a)
###交叉验证
##采用十折交叉验证法验证
set.seed(1010)
E=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];n1=length(m);
  a=randomForest(NSP~.,w[-m,])
  E[i]=sum(w[m,D]!=predict(a,w[m,]))/n1
}
mean(E)
E


###支持向量机分类
library(e1071)
a=svm(NSP~.,data = w,kernal="sigmoid")
wp=predict(a,w)
z=table(w[,D],wp);z
##交叉验证
library(e1071)
##采用十折交叉验证法验证
set.seed(1010)
E=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];n1=length(m);
  a=svm(NSP~.,data = w[-m,],kernal="sigmoid")
  E[i]=sum(w[m,D]!=predict(a,w[m,]))/n1
}
mean(E)
E


###最邻近方法分类（交叉验证后，我们才可以知道k的最优值）
##连续情况下，在空间中，以欧氏距离，平方欧氏距离定义一个距离
##离散情况下，采取多数投票方法，即该点最邻近的k个点中，大多数属于的类型
library(kknn)
a=kknn(NSP~.,k=6,train = w,test = w) #k是交叉验证得到的
z=table(w[,D],a$fitted);z
##交叉验证法
##采用十折交叉验证法验证
set.seed(1010)
E=rep(0,Z)
for (i in 1:Z){
  m=mm[[i]];n1=length(m);
  a=kknn(NSP~.,k=6,train = w[-m,],test = w[m,])
  E[i]=sum(w[m,D]!=a$fitted.values)/n1
}
mean(E)
E



###神经网络分类
#拟合
library(nnet)
a=nnet(NSP~.,data = w,subset = 1:n,size=2,
       rang=.01,decay=5e-4,maxit=200)
wp=predict(a,w[m,],type = "class")
sum(w[m,D]!=wp)/length(m)
##神经网络对于不同参数的拟合效果的分析
#固定delay=.01,变化size
set.seed(1010);d1=sample(1:n,n/2);d2=setdiff(1:n,d1) #1:n中和d1不同的数列
ww=matrix(0,20,2);
for (s in 1:20){
  a=nnet(NSP~.,data = w,subset = d1,size=s,rang=.01,
         decay=.01,maxit=200)
  ww[s,1]=sum(w[d1,D]!=predict(a,w[d1,],type = "class"))/length(d1)
  ww[s,2]=sum(w[d2,D]!=predict(a,w[d2,],type = "class"))/length(d2)
}
  matplot(1:20,ww,type = "o",lty = 2:1,pch = c(16,18),
          ylab = "Error rage",xlab = "Size")
  legend("topleft",c("Training set","Testing set"),pch = c(16,18),lty = 2:1)
  title("Error rate for different size of hidden layer in nnet")

