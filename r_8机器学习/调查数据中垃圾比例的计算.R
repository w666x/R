getwd()
setwd("E:/Rexercise1")
####调查数据中垃圾比例的计算
###吓人的巨型问卷
###可以计算最小抽样的样本量1068（但是这是对整体的比例计算得到的，无法支持以部分变量的水平组合的比例）
###谁生产了这些问卷，又产生了什么结论
#将近六十人的团队里面，仅有几个作为打工仔的合作成员是统计师生；真正的调查组成员才十个人
#所得的结果中，没有一个有样本量，置信区间，或者置信度之类的
###问卷调查数据处理中的弊端。过程不透明，可信度不高

##此处仅讨论，一个数据会产生多少垃圾，纯粹数据的问题
##如何精确的计算出问卷中垃圾的所有可能的比例
##如何将数据中有用的比例和没用的比例都提取出来

##问卷的问题是否是越多越好，选项是否是越多越好
##一个问卷的理论比例是多少（即可以从问卷中所提炼的信息--比例）
#理论比例为（任何变量的水平组合的交的观测值数目作为分母，
#交于其他任何变量的任何水平组合的观测数目为分子）

##一个问卷的各种有效比例中，有多少有意义，什么是有意义
##一个问卷中为什么会产生那么多无意义的比例
library(arules)
data("IncomeESL")
##解剖这个数据集
head(IncomeESL);dim(IncomeESL);str(IncomeESL);summary(IncomeESL)
IncomeF=IncomeESL[complete.cases(IncomeESL),]
Incomf1=na.omit(IncomeESL)  ##删除缺失值，生成数据一
names(IncomeF)
IncomeF2=Incomf1[sample(1:nrow(IncomeF),2000),c(1,5,6,12,13)]
dim(IncomeF)
head(IncomeF)
write.csv(IncomeF,file = "复杂数据统计方法_基于R的应用/数据集/IncomeF.csv")
write.csv(IncomeF2,file = "复杂数据统计方法_基于R的应用/数据集/IncomeF2.csv")
dim(IncomeF2);head(IncomeF2)


##数据的理论比例有多少个
##计算理论比例的R程序
T1=function(w){
  m=ncol(w);Y=NULL;
  for (i in 1:m){
    Y=c(Y,length(levels(w[,i])))
  }
  Y1=Y+1;G=prod(Y1)-1;
  for (i in 1:m) G=G+Y[i]*(prod(Y1[-i])-1)#单独范畴为分母
  for (k in 2:(m-1)) {
    M=combn(1:m,k);J=ncol(M) #多范畴之交为分母
    for (j in 1:J) G=G+prod(Y[M[,j]])*(prod(Y1[-M[,j]])-1)
  }
  return(G)
}
##计算理论比例(不好的问卷调查数据，理论比例会是非常大的)
w=read.csv("复杂数据统计方法_基于R的应用/数据集/IncomeF.csv")
n=nrow(w);m=ncol(w);dim(w)
T1(w)
##错误调试部分
debug(T1(w)) 
undebug(T1(w))

##计算出数据的一些比例以及95%置信区间
###分母为观察值总数，分子为所有可能的分母子集的观测值数
#求收入7.5万美元以上的人，占所有人的比例
summary(w$income)
table(w$income)[9]
x1=table(w[,2])[9];x1
x1/n;binom.test(x1,n)$con ##计算比例
###分母为一个问题的某个水平的观测值数，分子为多有分母子集的观测值数
#求收入7.5万美元以上的人在太平洋岛民后裔的比例
summary(w$ethnic.classification)
x2=table(w[,c(2,14)])[9,7]
x2
x3=table(w[,14])[7]
x3;
x2/x3;binom.test(x2,x3)$con
###分母为两个问题的水平的交集，分子为所有分母子集的观测值数
#求太平洋后裔年龄大于65岁中的收入在7.5万美元的比例
x4=table(w[,c(2,14,5)])[9,7,7]
x4
summary(w$age)
x5=table(w[,c(14,5)])[7,7]
x5
x4/x5;binom.test(x4,x5)$con
###分母为三个问题的水平的交集，分子为所有可能的分母子集的观测值
#住公寓的欧裔，大于65岁的人群收入在7.5万美元以上的比例是多少
x6=table(w[,c(13,14,5,2)])[2,8,7,9]
x6;x7=table(w[,c(13,14,5)])[2,8,7]
x7;x6/x7;binom.test(x6,x7)$con

###可以计算最小抽样的样本量（但是这是对整体的比例计算得到的，无法支持以部分变量的水平组合的比例）
w1=read.csv("复杂数据统计方法_基于R的应用/数据集/IncomeF2.csv")
T1(w1)


###计算抽样调查数据中的各种比例以及置信区间
##数据预处理
w1=read.csv("复杂数据统计方法_基于R的应用/数据集/IncomeF2.csv")
n=nrow(w1);m=ncol(w1)
head(w1)
Y=NULL;dim(w1)
for (i in 1:m){
  Y=c(Y,length(levels(w1[,i])))
}  #生成各个变量的水平数
Y
Y1=Y+1;prod(Y1)-1;

###将各个变量的水平数字化
for (i in 1:m){
  levels(w1[,i])=1:Y[i]
}
str(w1)
###每个变量都加一个空水平存在p中
p=list()
for (i in 1:m){
  p[[i]]=as.character(c(levels(w1[,i]),Y[i]+1))
}
head(p)
###各变量水平的组合
pp=expand.grid(p);pp=pp[-nrow(pp),] ##减去最后一行空集组合
for (i in 1:ncol(pp)) pp[,i]=as.integer(pp[,i]) ##将pp变成整数
head(pp);dim(pp);tail(pp)
###主要函数
WW=function(pp1=pp1,Y8=Y8,w1=w1,n1=n1){
  RR1=NULL
  for (i in 1:nrow(pp1)){
    a=T;for (j in 1:ncol(pp1)){
      ind=pp1[i,j]  ##prod函数，求解连乘的
      if (length(dim(Y8))==1) B=(ind!=Y8+1) else B=(ind!=Y8[j]+1)
      if (prod(dim(w1))==1){if(B) a=(a&(w1==levels(w1)[ind]))} else
        if(B) a=(a&(w1[,j]==levels(w1[,j])[ind]))
        ##上述a为pp1的第i行所有非空列组合的T-F向量
    }
    if (is.numeric(pp1)) A=(sum(pp1[i]==Y8+1)!=length(Y8)) else
      A=(sum(pp1[i,]==Y8+1)!=length(Y8))
    if (A){
      p=sum(a)/n1;len=diff(binom.test(sum(a),n1,con=.95)$con);
      RR1=rbind(RR1,c(n1,sum(a),p,len))
    }
  }
  RR1=data.frame(RR1);names(RR1)=c("n","x","p","len")
  return(RR1)
}
###使用
#举例：我们想得到同时获得具有w第三个变量，第二个水平；第四个变量第三个水平
#第二个变量第五个水平的观测值数目作为分子
x=c(3,4,2,2,3,5);x=matrix(x,ncol = length(x)/2,b=T);x
a0=T
for (i in 1:ncol(x))
  a0=a0&(w1[,x[1,i]]==levels(w1[,x[1,i]])[x[2,i]])
w2=w1[a0,-x[1,]] #保留a0的行，去掉分母变量的列
n1=sum(a0) #w1的行数，即比例的分母
pp1=unique(pp[,-x[1,]]) #把pp中相应的分母列去掉，把重复的行去掉
Y8=Y[-x[1,]] #把Y中相应于分母的列的元素去掉
RT=WW[pp1,Y8,w2,n1]
debug(WW[pp1,Y8,w2,n1])

