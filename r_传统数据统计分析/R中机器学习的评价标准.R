##R中机器学习的评价标准
##主要展示用caret包中的评价标准来衡量机器学习算法

##准确率和Kappa系数
library(caret)
##library(mlbench)
data("longley")
head(longley)
##建立可重复抽样的模型
control=trainControl(method = "cv",number = 5)
set.seed(7) #设立随机种子
fit=train(Population~.,data=longley,method="glm",
          metric="Accuracy",trControl=control)
##展示结果
print(fit)

##均方误差和R方
# load libraries
library(caret)
# load data
data(longley)
# prepare resampling method
control <- trainControl(method="cv", number=5)
set.seed(7)   #metric设定的是均价的准则
fit <- train(Employed~., data=longley, method="lm", metric="RMSE", trControl=control)
# display results
print(fit)

##roc曲线面积
#灵敏度和准确度的度量
#数据集没法找到，没办法执行
# load libraries
library(caret)
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# prepare resampling method
control <- trainControl(method="cv", number=5,
                        classProbs=TRUE, summaryFunction=twoClassSummary)
set.seed(7)
fit <- train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="ROC", trControl=control)
# display results
print(fit)

##对数损失函数
# load libraries
library(caret)
# load the dataset
data(iris)
# prepare resampling method
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(7)
fit <- train(Species~., data=iris, method="rpart", metric="logLoss", trControl=control)
# display results
print(fit)
