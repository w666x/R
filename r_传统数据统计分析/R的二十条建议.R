###R的20条有用的建议
###添加行和列的总和
iris.num=iris[,-5]
head(iris)
dim(iris)
###一般都按列求均值，即求各个特性的均值之类的
colSums(iris.num)
colMeans(iris.num)
head(rowMeans(iris.num))
apply(iris.num,2,min)
apply(iris.num,2,max)
sapply(iris.num,min)
sapply(iris.num,max)
head(iris.num)

###格式化数据format(应该在最后一步进行)
format(12345.6789,digits = 9,decimal.mark = ",",
       big.mark = " ",small.mark = ".",small.interval = 3)
x=colMeans(mtcars[,1:4])
format(x,digits =2,nsmall = 2)
x=seq(0.5,0.55,.01)
##添加百分号，%.1f表示对第一个值格式化，小数点后保留一位
### %% 表示打印一个百分号
sprintf("%.1f %%",100*x)
##对货币进行格式化
set.seed(1)
x=runif(5)*1000
sprintf("$ %3.2f",x)
###可将任何变量转化为字符串
stuff=c("bread","cookies")
price=c(2.1,4)
sprintf("%s costed $ %3.2f",stuff,price)

###数据排序
#sort/order
with(mtcars,mtcars[order(hp),])

###使用if进行选择
mtcars=within(mtcars,
              mpgClass=ifelse( mpg<mean(mpg),"low","high"))
mtcars[mtcars$mpgClass == "high",]
traceback()

###计算条件总和
with(mtcars,mean(mpg))
with(mtcars,mean(mpg[hp <150]))
with(mtcars,mean(mpg[hp >150]))
with(mtcars,length(mpg))

###调换行和列
t

###查找唯一值和重复值
unique(mtcars$cyl)
##重复值
dupes=duplicated(iris)
head(dupes)
tail(dupes)
which(dupes==T)  ##找到重复值的那一行
iris[dupes,]
iris[!dupes,]
nrow(iris[!dupes,])

###使用检索表
index=match("Toyota Corolla",rownames(mtcars))
index
mtcars[20,]

###使用数据透视表reshape2包
with(mtcars,tapply(hp,list(cyl,gear),mean))
aggregate(hp~cyl+gear+am,mtcars,mean)

###使用单变量求解
sales=function(price1){100-0.5*price1}
revenue=function(price1){price1*sales(price1)}
opar=par(no.readonly = T)
par(mfrow=c(1,2))
curve(sales,from = 50,to=150,xname = "price1",
      ylab = "Sales",main="Sales")
curve(revenue,from = 50,to=150,xname = "price1",
      ylab = "revenue",main="Revenue")
par(opar)
optimise(revenue,interval = c(50,150),maximum = T)
