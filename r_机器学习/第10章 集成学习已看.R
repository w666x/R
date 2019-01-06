###集成学习
##经典的两个集成算法：bagging和adaboost
#众多的基分类器集成而成的分类器，但是，当分类器中大多数
#的分类结果均为错误的话，那个错误也应该会被认为是正确的吧
##bagging算法：通过bootstrap抽样得到若干不同的训练集，
#以这些训练集分别建立模型，得到一系列基分类器，经基分类器投票
#或者平均后（简单综合），就得到了集成分类
#bagging（）
##adaboost算法：同样的是若干基分类器基础上的集成算法，
#在构建基分类器的过程中，会根据上一个基分类器的对各样本的预测，
#自行调整本次各样本被抽中的概率。对上一分类器中误判的训练样本，
#给予较高的权重
#boosting（）

#数据集
# data #
##数据预处理，简要的分析数据的类型，了解数据所代表的含义
##构建出测试集和训练集
setwd("数据分析R语言实战代码数据包\\原始数据包\\Bank Marketing")
data=read.csv("bank.csv",header=TRUE,sep=";")
head(data)
dim(data)
summary(data)
str(data)
sum(data$y=="yes"); sum(data$y=="no")#统计数据中，是否的数量
#抽取data中，四分之一的样本
sub=sample(1:nrow(data),round(nrow(data)/4))
length(sub)
head(sub)
data_train=data[-sub,]
data_test=data[sub,]
dim(data_train);dim(data_test)

##应用案例-集成学习算法实现
# Bagging #

#install.packages("adabag")
#install.packages("rpart")
library(adabag)
library(rpart)
#生成五棵决策树，bagging算法
bag=bagging(y~.,data_train,mfinal=5)
#查看算法的输出项
names(bag)
bag$formula
bag$trees[2]#查看第二棵决策树的具体构成
bag$votes[105:115,] #查看投票情况
bag$prob[105:115,]#查看概率prob矩阵
bag$class[105:115]#所属类别的最终判断
bag$samples[105:115,]#抽样情况
bag$samples[1:11,]
bag$importance
barplot(bag$importance)#输入变量的相对重要性
bag$call

###模型修正之类的，优化
##决策树修枝问题，之前所输出的树过于茂盛
#控制分类树的大小最大为3，通过control参数控制
bag1=bagging(y~.,data_train,mfinal=5,control=rpart.control(maxdepth=3))
bag1$trees[2]#查询第二棵决策树的具体结构


##对测试集进行预测检验
pre_bag=predict(bag,data_test) #通过上面建立的模型预测
names(pre_bag) #显示预测结果的输出项名称
pre_bag$votes[1:10,]#前十个样本的投票
pre_bag$prob[1:10,]#前十个样本的归属概率
pre_bag$class[1:10]#测试集的预测类别
pre_bag$confusion#输出混淆矩阵
pre_bag$error#输出错误率

###实验结果中，yes项出现了不平衡数据问题，需改正
sub_minor=which(data_test$y=="yes")#取少数类在测试集中的编号
sub_major=which(data_test$y=="no")
length(sub_minor); length(sub_major)
#分别计算出测试总体的预测错误率（总的错误率以及各类别的错误率）
err_bag=sum(pre_bag$class!=data_test$y)/nrow(data_test)
err_minor_bag=sum(pre_bag$class[sub_minor]!=data_test$y[sub_minor])/length(sub_minor)
err_major_bag=sum(pre_bag$class[sub_major]!=data_test$y[sub_major])/length(sub_major)
err_bag; err_minor_bag; err_major_bag


###Adaboost算法
# Boosting #
##对训练集运行adaboost算法，并对测试集预测
boo=boosting(y~.,data_train,mfinal=5)
pre_boo=predict(boo,data_test)
#训练集，测试集都选择的是上面一样的，故此处不需调整
err_boo=sum(pre_boo$class!=data_test$y)/nrow(data_test)
err_minor_boo=sum(pre_boo$class[sub_minor]!=data_test$y[sub_minor])/length(sub_minor)
err_major_boo=sum(pre_boo$class[sub_major]!=data_test$y[sub_major])/length(sub_major)
err_boo; err_minor_boo; err_major_boo

