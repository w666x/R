setwd("E:/Rexercise1")
###R语言中apply家族函数的用法及其比较
##apply函数为按行，列处理，可运行自定义函数
##lapply为按组件求出，输出结果为list
##sapply同lapply的处理方式一样，只不过输出结果为向量
##tapply()函数的作用是先将数据分为若干组，然后在每组数
  ##据调用指定的函数
##by函数，tapply函数的升级，是的传入的数据可以是一个
  ##数据框或者矩阵，使用更加灵活

###apply函数的使用
data("mtcars")
head(mtcars)
data(beavers)
head(beaver1);dim(beaver1)
head(beaver2)
head(t(beaver1)[1:4,1:10])#将数据转置并取出部分数据展示
apply(t(beaver1), 1, max) ##按行计算最大值
apply(beaver1,2,max)  ##按列计算最大值
apply(mtcars, 2,mean)  ##按列计算均值
head(apply(mtcars,2,function(x) x%%10)) ##将数据集按列应用自定义函数

###lapply函数的应用
l=list(a=1:10,b=11:20)
lapply(l, mean) ##最美一个组件求取均值
class(lapply(l, mean)) ##查看返回的结果的类型

###sapply函数
sapply(l, mean)

###tapply函数
str(mtcars$cyl) ##查看数据结构
levels(as.factor(mtcars$cyl))##生成因子类型
tapply(mtcars$mpg, mtcars$cyl,mean)

###by函数
data("iris") #导入数据以及查看数据类型
str(iris)
by(iris[,1:4],iris$Species,colMeans)
