####多元数据的关联规则分析
##分类规则处理的都是二分类变量
##关联规则分析亦称关联规则挖掘，试图在一个很大的数据集中找出关联或者相关的关系
##主要应用于，交易数据，调查问卷等以分类变量为主的情况
#还可以把离散变量连续化来处理带有连续变量的情况

library(arules)
data("IncomeESL")
head(IncomeESL);dim(IncomeESL);str(IncomeESL)

#数据的初步处理,将分类变量水平简化
#IncomeF=IncomeESL[complete.cases(IncomeESL),]
IncomeESL=na.omit(IncomeESL)  ##删除缺失值，生成数据一
names(IncomeESL)
dim(IncomeESL);head(IncomeESL)
identical(a,IncomeESL) #测试a与incomeesl是否等同
IncomeESL$income=factor(
  (as.numeric(IncomeESL$income)>6)+1,
  levels = 1:2,labels = c("$0-$40,000","$40,000+")
)
IncomeESL$age=factor(
  (as.numeric(IncomeESL$age)>3)+1,
  levels = 1:2,labels = c("14-35","35+")
)
IncomeESL$education=factor(
  (as.numeric(IncomeESL$education)>4)+1,
  levels = 1:2,labels = c("no callege graduage","college graduage")
)
IncomeESL$`years in bay area`=factor(
  (as.numeric(IncomeESL$`years in bay area`)>4)+1,
  levels = 1:2,labels = c("1-9","10+")
)
IncomeESL$`number in household`=factor(
  (as.numeric(IncomeESL$`number in household`)>3)+1,
  levels = 1:2,labels = c("1","2+")
)
IncomeESL$`number of children`=factor(
  (as.numeric(IncomeESL$`number of children`)>1)+0,
  levels = 0:1,labels = c("0","1+")
)
head(IncomeESL);str(IncomeESL);summary(IncomeESL)
class(IncomeESL)

###理论准备（支持度，置信度，提升度）
income=as(IncomeESL,"transactions")
typeof(income);class(income)
dim(income);head(income)
income[1:5,1:5]
summary(income)
itemFrequencyPlot(income,support=.05,cex.names=.8) #图形中给出了频繁项集（0.05以上的）


###频数和规则
#求解各个项集的频率并展示出来
fsets=eclat(income,parameter = list(support=.05,maxlen=10))
inspect(sort(fsets,by="support")[1:11]) #按支持度的大小输出前十项
#求解各种规则，抽出部分子集
rules=apriori(income,parameter = list(support=.01,confidence=.01))
length(rules)
inspect(sort(rules,by="lift")[1:5])
x=subset(rules,subset=rhs %in% "sex=male" &lift>1.2)
inspect(sort(x,by="support")[1:5]) #按支持度输出前五条规则
inspect(sort(x,by="lift")[1:3])
length(x)
