#R语言社区
setwd("E:/Rexercise1/R语言")
#这里模拟了一个进度条，这个是很不错的
#使用neuralnet包在R中拟合一个神经网络模型
#创建神经网络模型，并创建一个线性回归模型进行比较
#模型拟合好坏的标准是均值方差
#导入mass下的数据集boston
#数据集说明：Boston郊区的房价的数据集
library(MASS)
set.seed(500)
data=Boston
head(data);str(data);dim(data)
#检查数据集中任何数据都不含缺失值
apply(data,2,function(x) sum(is.na(x)))
#以随机的方式将数据集划分为训练数据集和测试集
index=sample(1:nrow(data),round(0.75*nrow(data)))
train=data[index,] #训练样本集
test=data[-index,] #测试样本集
#使用广义线性模型拟合
lm.fit=glm(medv~.,data = train) #广义线性模型拟合
summary(lm.fit)
anova(lm.fit)
pr.lm=predict(lm.fit,test) #使用测试集测试
MSE.lm=sum((pr.lm-test$medv)^2)/nrow(test) #计算均值方差差
MSE.lm

#使用神经网络模型作相应的拟合
#对数据进行预处理(对数据进行准则化处理)
#最大最小值的方法，基于[0,1]来测量数据
#测量和分离数据集
maxs=apply(data,2,max)
mins=apply(data,2,min)
data.frame(maxs,mins)#最大最小准则化处理
scaled=as.data.frame(scale(data,center=mins,
                           scale=maxs-mins))
head(scaled)
#分离数据
train_=scaled[index,]
test_=scaled[-index,]

#设置神经网络模型相关参数
#没有几个绝对的准则能让我们知道要使用多少层和多少个神经元
#输入层有13个输入值
#两个隐藏层分别有5个和3个神经元
#输出层只有一个输出值，因为是作回归分析
library(neuralnet)
n=names(train_);n
#构造一个函数，可以让其生成函数的表达式类型
f=as.formula(paste("medv~",paste(n[!n %in% "medv"],
                                 collapse = "+")))
f
#y~.公式不可再neuralnet函数中使用
#linear.output指定我们是否需要进行回归分析
nn=neuralnet(f,data = train_,hidden = c(5,3),
             linear.output = T)
summary(nn);plot(nn)

#测试集检验，预测私有房屋的价格
pr.nn=compute(nn,test_[,1:13])
pr.nn_=pr.nn$net.result*(max(data$medv)-
                           min(data$medv))+
  min(data$medv)
test.r=(test_$medv)*(max(data$medv)-
                       min(data$medv))+min(data$medv)
head(pr.nn_)
MSE.nn=sum((test.r-pr.nn_)^2)/nrow(test_)
MSE.nn
print(paste(MSE.lm,MSE.nn))

##做交叉检验来使得结果更具有说服力
#基于测试数据集的神经网络模型和线性模型的第一步可视化操作的图像如下
#初步拟合效果可视化
par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col="red",main="Real vs predicted NN",pch=18,cex=.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n')
#进一步可视化比较(将两个拟合图放在同一个图中作比较)
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,bty = 'n',col=c('red','blue'))

#进一步交叉检验
#1.完成测试数据集分离
#2.基于训练数据集拟合一个模型
#3.用测试数据集测试模型
#4.计算预测误差
#5.重复这个过程K次
#然后，计算平均误差，它可以让我们获悉这个模型是怎样运作的。
#使用boot包的cv.glm函数来检验线性模型
library(boot)
set.seed(200)
lm.fit=glm(medv~.,data = data)
cv.glm(data,lm.fit,K=10)$delta[1]
#重复十次进行随机的90%训练数据集和10%的测试数据集
set.seed(450)
cv.error=NULL
k=10
library(plyr)
pbar=create_progress_bar('text') #显示运行步骤
pbar$init(k);pbar$step()
for (i in 1:k){
  index=sample(1:nrow(data), #数据集分离
               round(0.9*nrow(data)))
  train.cv=scaled[index,]
  test.cv=scaled[-index,]
  nn=neuralnet(f,data = train.cv,hidden = c(5,2),linear.output = T) #作神经网络模型，回归分析
  pr.nn=compute(nn,test.cv[,1:13]) #预测值
  pr.nn=pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv) #将数据还原，即乘以最大最小的区间然后加上最小值
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv) #将数据还原，在最小值的上面区间还原
  cv.error[i]=sum((test.cv.r-pr.nn)^2)/nrow(test.cv)
  pbar$step()
}
#计算MSE均值，并将结果可视化
mean(cv.error)
cv.error
#作图代码
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
