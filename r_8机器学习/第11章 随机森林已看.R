#install.packages ( "randomForest" )
library(randomForest)
##实质上是决策树的分类器集成算法
#在变量以及数据的使用上进行随机化，生成很多分类
#树，再汇总分类树的结果

##随机森林产生：元分类器由CART算法构建
#输出由：简单多数投票-分类模型
#简单平均-回归模型

##随机森林中，单棵树的分类强度越强，
#树与树之间的相关度越弱，效果越好

##树节点所选的变量个数、随机森林中树的个数

###R软件包的实现
###importance函数示例
#提取随机森林中各变量的重要性的度量结果
set.seed(4)
data(mtcars)
head(mtcars)
str(mtcars)
mtcars.rf=randomForest(mpg~.,data=mtcars,ntree=1000,importance=TRUE)
importance(mtcars.rf)	#默认情况下，两种度量均输出
importance(mtcars.rf, type=1)
importance(mtcars.rf, type=2)

###MDSplot函数示例
##随机森林的可视化分析（所产生的临近矩阵标准化后的坐标图）
set.seed(1)	# 设定产生随机数的初始值
data(iris)	# 调用数据集iris
str(iris)
iris.rf=randomForest(Species ~ ., iris, proximity=TRUE)# 基于数据集iris建立随机森林模型
MDSplot(iris.rf, iris$Species, palette=c(1:3), pch=as.numeric(iris$Species))# 绘制图像

###rfImpute函数示例
##利用临近矩阵对缺失值进行插值拟合
data(iris)	# 调用数据集iris
iris.na=iris	# 生成需要进行处理的数据集
iris.na[75,2];iris.na[125,3]
iris.na[75,2]=NA;iris.na[125,3]=NA;	  # 在第75号样本和第125号样本中设置缺失值
set.seed(111)				# 设置随机数生成器初始值
iris.imputed=rfImpute(Species ~ .,data=iris.na)	# 对数据集iris.na进行插值
list("real"=iris[c(75,125),1:4],"have-na"=iris.na[c(75,125),
                                               1:4],
     "disposed"=round(iris.imputed[c(75,25),2:5],1))


###treesize函数示例
##查看随机森林模型中，每棵树所具有的节点个数
iris.rf<- randomForest(Species ~ ., iris)	# 利用数据iris构建相关随机森林模型
hist(treesize(iris.rf))			# 绘制相应的柱状图
hist(treesize(iris.rf,terminal = F))			# 绘制相应的柱状图


###模型可视化示例
###确定在构建随机森林模型中该使用的决策树数量，
#可确定使误差最小时所对应的决策树的数量
data(airquality)		# 调用数据集airquality
set.seed(131)			# 设置随机数生成器初始值
str(airquality)
ozone.rf=randomForest(Ozone~.,data=airquality,mtry=3,importance=TRUE,na.action=na.omit)# 建立随机森林回归模型
plot(ozone.rf)			# 绘制相关图像
ozone.rf
ozone.rf$predicted #输出随机森林预测的结果
ozone.rf$importance #输出各个变量在模型中的重要程度
ozone.rf$call #输出模型的基本参数
ozone.rf$type
ozone.rf$ntree #森林中决策树的数量
ozone.rf$mtry


###实际案例
library(randomForest)
setwd("E:/Rexercise1")
wine=read.table("数据分析R语言实战代码数据包/原始数据包/wine2.txt",sep = ",",
                na.strings = "NA") # 本文默认数据以记事本格式存储于电脑D盘中
dim(wine)
class(wine)
class(wine$V12)
head(wine)
names(wine)=c("fixed acidity","volatile acidity","citric acid","residual sugar","chlorides","free sulfur dioxide","total sulfur dioxide","density","PH","sulphates","alcohol","quality")	# 为数据集wine各个变量命名
summary (wine)                # 获取wine数据集的概括信息
hist(wine$quality)

##数据预处理
##由quality直方图，将酒的品质quality定义为“差，中，好”三类
cha=0	# 设置中间变量对处理后的向量进行临时存储
for(i in 1:4898) # 针对每一个样本进行调整
{
	if(wine[i,12]>6)
	{
		cha[i]="good"	# 将品质大于6的样本品质定义为“good”
	}
	else if(wine[i,12]>5)
	{
		cha[i]="mid"	# 将品质大于5却不大于6的样本品质定义为“mid”
	}
	else
	{
		cha[i]="bad"	# 将品质不大于5的样本品质定义为“bad”
	}
}
wine[,12]=factor(cha)	# 将字符型变量转化为含有因子的变量并复制给数据集wine
summary(wine$quality)
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")
summary(wine)


##模型的建立
#一：以既定的公式建立模型(首先确定建立模型所使用的数据)
set.seed(71)			# 设置随机数生成器初始值
samp=sample(1:4898,3000)	# 从全部数据集中抽取3000个样本作为训练集
set.seed(111)			# 设置随机数生成器初始值
wine.rf=randomForest(quality~.,data=wine,importance=TRUE,proximity=TRUE,ntree=500,subset=samp)	 # 构建决策树为500棵的随机森林模型

x=wine[-samp,1:11]		# 利用构建模型剩下的样本作为测试集
pred=predict(wine.rf,x)		# 根据模型wine.rf对x数据进行预测
head(pred)
set.seed(100)
samp1=sample(1:1898,8)
list(pred=pred[samp1],exact=wine[samp1,12])		# 随机挑选8个预测结果进行展示


###二：根据所给的数据建立模型（提出响应变量和自变量）
x=subset(wine,select = -quality)#提取自变量
y=wine$quality #提取响应变量
set.seed(71)
samp2=sample(1:4898,3000) #提取数据作为训练集
xr=x[samp2,];yr=y[samp2]
set.seed(111)  #设置随机数生成器初始值
wine.rf=randomForest(xr,yr,importance = T,proximity =T,
                     ntree = 500) #构建随机森林模型
###使用第二种格式建立模型时，不用特别强调模型的形式
##结果分析
print(wine.rf)
#自变量的重要程度
importance(wine.rf)
MDSplot(wine.rf,wine$quality)


##模型优化（变量个数，决策树的数量）
###寻找模型最优节点变量数
n=ncol(wine)-1			# 计算数据集中自变量个数
rate=1				# 设置模型误判率向量初始值
for(i in 1:n)			# 依次逐个增加节点所选变量个数
{
	set.seed(222)		# 设置随机数生成器的初始值
	model=randomForest(quality~.,data=wine,mtry=i,importance=TRUE,ntree=1000)	# 建立随机森林模型
	rate[i]=mean(model$err.rate)				# 计算基于OOB数据的模型误判率均值
#	print(model)		# 展示模型简要信息
}
rate			

###寻找最优的决策树数量
set.seed(222)		# 设置随机数生成器初始值
model=randomForest(quality~.,data=wine,mtry=1,importance=TRUE,ntree=1000) # 构建随机森林模型
plot(model,col=1:1)
legend(800,0.215,"mid",cex=.9,bty="n")			#为图像添加图例
legend(800,0.28,"bad",cex=.9,bty="n")			#为图像添加图例
legend(800,0.37,"good",cex=.9,bty="n")			#为图像添加图例
legend(800,0.245,"total",cex=0.9,bty="n")		#为图像添加图例


###根据上面确定的最优变量数以及最优树数，构建模型
set.seed(222)		# 设置随机数生成器初始值
model=randomForest(quality~.,data=wine,mtry=1,proximity=TRUE,importance=TRUE,ntree=400)# 建立随机森林模型
hist(treesize(model))						 # 展示随机森林模型中每颗决策树的节点数
importance(model)			 # 展示在随机森林模型中各个变量对模型预测能力的影响
MDSplot(model,wine$quality, palette=rep(1, 3), pch=as.numeric(wine$quality))		 # 展示数据集在二维的情况下个类别的具体分布情况