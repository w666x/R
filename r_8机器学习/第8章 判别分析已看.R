###判别分析，根据已经掌握的每个类别若干样本的数据信息
###总结出客观的规律性，建立判别公式和准则，备新样本使用
###费希尔（线性判别分析，及一般性的衍生算法）
#选出合适的投影轴（保证类内离差尽可能小，类间大）
#MASS/lda(),qda()
###贝叶斯（朴素贝叶斯分类算法）（要求各属性之间无关）
#无惧噪声和无关变量
#klaR/NaiveBayes()
###距离判别法（k最邻近算法）
#在临近的k个样本点中，哪一类型的样本点所占比例越大
#则，新近样本点则更有可能分配到此类中
#当赋予权重后，就近似为先验的信息分布导入，分类更准确
#class/knn();kknn/kknn()

###R中的实现
##导入数据集
# data #
library(kknn)
data(miete)
head(miete)
dim(miete)
summary(miete)
str(miete)
#write.csv(miete,"miete.csv")

##数据预处理
library(sampling)
n=round(2/3*nrow(miete)/5)
n
sub_train=strata(miete,stratanames="nmkat",size=rep(n,5),method="srswor")
head(sub_train)
dim(sub_train)
data_train=getdata(miete[,c(-1,-3,-12)],sub_train$ID_unit)  
data_test=getdata(miete[,c(-1,-3,-12)],-sub_train$ID_unit) 
dim(data_train);dim(data_test)
head(data_test)

##线性判别分析
# lda #
# lda(formula, data, ..., subset, na.action)
# lda(x,grouping,prior=proportions,tol=1.0e-4,method, CV = FALSE, nu, ...)
# lda(x,grouping, ...,subset,na.action)
#install.packages("MASS")
library(MASS)
names(data_train)
fit_lda1=lda(nmkat~.,data_train)
names(fit_lda1)
fit_lda1$prior #输出使用的先验概率
fit_lda1$counts #输出各类别的样本量
fit_lda1$means #输出各变量在每一类别中的均值
fit_lda1$scaling 
fit_lda1$lev
fit_lda1$svd
fit_lda1$N     
fit_lda1$call
fit_lda1$terms
fit_lda1$xlevels
fit_lda1 #输出判别分析的各项结果


##线性判别分析二
fit_lda2=lda(data_train[,-12],data_train[,12])
fit_lda2
#判别结果的可视化
plot(fit_lda1)
plot(fit_lda1,dimen=1) #输出一个判别式的图形
plot(fit_lda1,dimen=2) #输出两个判别式的图形
##对测试集待判别变量取值进行预测
pre_lda1=predict(fit_lda1,data_test)
pre_lda1$class #输出预测结果
pre_lda1$posterior #输出后延概率
#输出预测值和实际值的混淆矩阵
table(data_test$nmkat,pre_lda1$class)
error_lda1=sum(as.numeric(as.numeric(pre_lda1$class)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_lda1 ##计算出错误率

# qda #
fit_qda=qda(data_train[,-12],data_train[,12])
fit_qda


##朴素贝叶斯分类
# bayes #
# ## S3 method for class ’formula’
 # NaiveBayes(formula, data, ..., subset, na.action = na.pass)
  ## Default S3 method:
 # NaiveBayes(x, grouping, prior, usekernel = FALSE, fL = 0, ...)

#install.packages("klaR")
library(klaR)
library(e1071)
fit_Bayes1=naiveBayes(nmkat~.,data_train)
fit_Bayes1=NaiveBayes(nmkat~.,data_train)
names(fit_Bayes1)
fit_Bayes1$apriori
fit_Bayes1$tables #输出所有变量在各类别下的条件概率
fit_Bayes1$levels
fit_Bayes1$call
fit_Bayes1$usekernel
fit_Bayes1$varnames

fit_Bayes2=NaiveBayes(data_train[,-12],data_train[,12])
fit_Bayes2
##各类别下，变量密度的可视化
plot(fit_Bayes1,vars="wfl",n=50,col=c(1,"darkgrey",1,"darkgrey",1)) # 占地面积
plot(fit_Bayes1,vars="mvdauer",n=50,col=c(1,"darkgrey",1,"darkgrey",1)) # 租赁期
plot(fit_Bayes1,vars="nmqm",n=50,col=c(1,"darkgrey",1,"darkgrey",1)) # 每平方米净租金

##对测试集待测变量进行预测
pre_Bayes1=predict(fit_Bayes1,data_test)
pre_Bayes1
#输出混淆矩阵
table(data_test$nmkat,pre_Bayes1$class)
error_Bayes1=sum(as.numeric(as.numeric(pre_Bayes1$class)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_Bayes1

###k最邻近算法
# knn #
# knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

#install.packages("class")
library(class)

fit_pre_knn=knn(data_train[,-12],data_test[,-12],cl=data_train[,12])
fit_pre_knn #训练集，测试集，训练集中的判别变量
table(data_test$nmkat,fit_pre_knn)
error_knn=sum(as.numeric(as.numeric(fit_pre_knn)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_knn
##调整K值，作k最邻近法
error_knn=rep(0,20)
for(i in 1:20)
{ fit_pre_knn=knn(data_train[,-12],data_test[,-12],cl=data_train[,12],k=i)
  error_knn[i]=sum(as.numeric(as.numeric(fit_pre_knn)!=as.numeric(data_test$nmkat)))/nrow(data_test)}
error_knn
plot(error_knn,type="l",xlab="K")

###加权k最邻近法
# kknn #
# kknn(formula = formula(train), train, test, na.action = na.omit(),
 # k = 7, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
 # contrasts = c(’unordered’ = "contr.dummy", ordered = "contr.ordinal"))

#install.packages("kknn")
library(kknn) #公式，选年级，测试集，k值
fit_pre_kknn=kknn(nmkat~.,data_train,data_test[,-12])
fit_pre_kknn
summary(fit_pre_kknn)
fit=fitted(fit_pre_kknn)
fit
##混淆矩阵
table(data_test$nmkat,fit)
error_kknn=sum(as.numeric(as.numeric(fit)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_kknn
###找出最优K值
error_kknn=rep(0,20)
for(i in 1:20)
{ fit_pre_kknn=kknn(nmkat~.,data_train,data_test[,-12],k=i)
  error_kknn[i]=sum(as.numeric(as.numeric(fitted(fit_pre_kknn))!=as.numeric(data_test$nmkat)))/nrow(data_test)}
error_kknn
plot(error_kknn,type="l",xlab="K")

sub=matrix(0,4,30);dim(sub)
for(i in 1:4)  sub[i,]=sample(which(miete$nmkat==i),30)
SUB=sample(which(miete$nmkat=="5"),200)
subb=matrix(0,5,20)
for(i in 1:5)  subb[i,]=sample(which(miete$nmkat==i),20)
data_train=miete[c(sub,SUB),c(-1,-3,-12)]  
data_test=miete[subb,c(-1,-3,-12)] 
dim(data_train);dim(data_test)


###从这儿就没看了啊
# 一个案例 #

setwd("E:\\Rexercise1/数据分析R语言实战代码数据包/原始数据包")
data=read.table("u.data.txt")
data=matrix(data,ncol = 4,byrow = T)
head(data)
dim(data)
data=data[,-4]
  colnames(data)=c("userid","itemid","rating")
head(data)
data=data.frame(data)
head(data)
class(data)
head(data,10)
data[data$userid==1,]
dim(data[data$userid==1,])


###编写函数用户对某部电影的评分值
# 函数 #
Userid=1;Itemid=61;n=50;K=10 ##设定参数取值
###定义函数
MovieLens_KNN=function(Userid,Itemid,n,K) {
  
  sub=which(data$userid==Userid) #获取获取行标签
  if(length(sub)>=n) sub_n=sample(sub,n) #随机抽取n个，当样本量小于n时，全部抽中
  if(length(sub)<n) sub_n=sample(sub,length(sub))
  known_itemid=data$itemid[sub_n] #获取已评分的电影ID
  unknown_itemid=Itemid #从函数所给的参数中，获得待测ID
  unknown_itemid;as.numeric(known_itemid)#查询部分信息
 #找出已经对该ID电影评分的用户ID
   unknown_sub=which(data$itemid==unknown_itemid)
  user=data$userid[unknown_sub[-1]]
  user
  #设置data.all矩阵
  data_all=matrix(0,1+length(user),2+length(known_itemid))
  data_all=data.frame(data_all)
  names(data_all)=c("userid",paste("unknown_itemid_",Itemid),paste("itemid_",known_itemid,sep=""))
  item=c(unknown_itemid,known_itemid)
  data_all$userid=c(Userid,user)
  data_all
  ##对data_all中的有相应取值的位置赋值，缺失值赋0
  for (i in 1:nrow(data_all))
  { #将data中，id号等于data_all中的数据提取出来
    data_temp=data[which(data$userid==data_all$userid[i]),]
    for (j in 1:length(item))
    {  if(sum(as.numeric(data_temp$itemid==item[j]))!=0) #判断该位置是否空值
    {data_all[i,j+1]=data_temp$rating[which(data_temp$itemid==item[j])]}
    } }

  data_test_x=data_all[1,c(-1,-2)] #获取测试集的已知部分
  data_test_y=data_all[1,2] #获取测试集的待预测值
  data_train_x=data_all[-1,c(-1,-2)] #获取已知部分取训练集的y
  data_train_y=data_all[-1,2] #获取训练集的待测部分
  #进行knn判别
  fit=knn(data_train_x,data_test_x,cl=data_train_y,k=K)
  list("data_all:"=data_all,"True Rating:"=data_test_y,"Predict Rating:"=fit,"User ID:"=Userid,"Item ID:"=Itemid)
}

# 使用函数 #
MovieLens_KNN(Userid=1,Itemid=61,n=50,K=10)


user1=NULL
for(Item in 1:20) 
user1=c(user1,MovieLens_KNN(Userid=1,Itemid=Item,n=50,K=10)$`True Rating:`)
user1

which(user1==5) #显示评分为5的电影ID

