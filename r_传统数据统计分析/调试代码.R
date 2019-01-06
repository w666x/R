###调试代码
x=c(1:10)
y = if(x<5) 0 else 1
y
?"if"

###训练函数一
##check input and does logit calculation
logit=function(x){
  x=ifelse(x<0|x>1,"NA",x)
  log(x / (1-x)/100)
}
###transforms percentage to number and calls logit
logitpercent=function(x){
  x=gsub("%","",x)
  logit(as.numeric(x))
}
logitpercent('50%')
###追溯到问题出现的地方
traceback()
###调试函数来调试
debug(logit)
debugonce(logit)
print(n)
undebug(logit)

###函数调试二
logit=function(x){
  x=ifelse(x<0|x>1,"NA",x)
  browser()
  log(x /(1-x)/100)
}
logit(50)
undebug(logit)
###自定义信息，预防函数错误
logit=function(x){
  if( any (x<0|x>1)) stop('x not between 0 and 1')
  log(x / (1-x)/100)
}
logitpercent(c('50%','150%'))
###一些常见的错误
###数据结构出错
###数据格式出错
###一不小心被降维
rowsum.df=function(x){
  id = sapply(x,is.numeric) ##将数值型的列找出来
  rowSums(x[,id]) ###按行求和
}
rowsum.df(sleep) ##错误一
traceback()
id=sapply(sleep,is.numeric)
dim(sleep)
head(sleep)
tail(sleep)
rowsum.df(pressure) ##正确
id=sapply(pressure,is.numeric)
dim(pressure)
head(pressure)

###被列表给弄糊涂了，使用列表的时候要注意的
strsplit('this is a sentence','')[2]
strsplit('this is a sentence','  ')##这个单引号里面的空格大小，可以对输出结果产生影响
strsplit('this is a sentence',' ')[[1]][2]
##错误示例一
###姓名分离，即character分离而已
customer=c('Johan Delong','Marie Petit')
customer
namesplit=strsplit(customer,' ')
namesplit
###从列表中提取数据，单引号所得到的结果只能是列表
paste(namesplit[2],collapse = '.')
paste(namesplit[[2]],collapse = '.')

###被因子和数值向量弄糊涂了
cyl.factor=as.factor(mtcars$cyl)
cyl.factor
##错误使用
median(as.numeric(cyl.factor))
##正确使用
as.numeric(levels(cyl.factor))[cyl.factor]
