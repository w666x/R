###数据预处理
#对不符合常理的情况进行处理
#不完整（缺少属性值）、含噪声（离群点）、不一致


###进行数据清理前的准备工作
##了解数据背后的内容，分辨变量的重要程度
#了解数据收集的目的，方法和途径
#install.packages(lattice)       
#install.packages(MASS)                                     
#install.packages(nnet)                                      
library(lattice)                                            
library(MASS)                                        
library(nnet)   

#install.packages(mice)
library(mice)
data(nhanes2)   
nrow(nhanes2);ncol(nhanes2)  
summary(nhanes2)
head(nhanes2)
str(nhanes2)
summary(nhanes2,na.tring=T)


###数据清理
##缺失值的处理
##删除法、插补法
#缺失值会影响分析工作的进行，仅仅对数据进行
#删除或者插补会影响数据规模和数据结构
sum(is.na(nhanes2)) #计算缺失的数量
dim(nhanes2)
sum( complete.cases(nhanes2)) #计算完整样本的数量
md.pattern(nhanes2) #观测缺失值的情况

###缺失值通过mice多重插补法拟合值代替缺失值
which(is.na(nhanes2[,2]))
imp=mice(nhanes2,m=4) #生成4组完整的数据并赋值给imp
head(imp)
fit=with(imp,lm(chl~age+hyp+bmi))#生成线性回归模型
pooled=pool(fit) #pool函数对建立的4个模型进行汇总              
summary(pooled) 

#插补法，缺失值处理
sub=which(is.na(nhanes2[,4])==TRUE) 
sub
dataTR=nhanes2[-sub,]    
dataTE=nhanes2[sub,] 
dataTE
dataTE[,4]=sample(dataTR[,4],length(dataTE[,4]),
                  replace = T) #简单抽样插补法
dataTE
#均值插补法
sub=which(is.na(nhanes2[,4])==TRUE) 
sub
dataTR=nhanes2[-sub,]    
dataTE=nhanes2[sub,] 
dataTE
dataTE[,4]=mean(dataTR[,4]) #均值插补法
dataTE

#回归插补法
sub=which(is.na(nhanes2[,4])==TRUE) 
sub
dataTR=nhanes2[-sub,]    
dataTE=nhanes2[sub,] 
dataTE
lm=lm(chl~age,data =dataTR)#建立回归模型
nhanes2[sub,4]=round(predict(lm,dataTE)) #回归插补法
head(nhanes2)

#热平台插补法
accept=nhanes2[which(apply(is.na(nhanes2),1,sum)!=0),] #存在缺失值的样本
accept
donate=nhanes2[which(apply(is.na(nhanes2),1,sum)==0),]#无缺失值的样本
donate
#热平台插补即是在donate中找到与该样本相似的样本，
#用相似样本的对应值代替该样本的缺失值
#热平台插补执行
sa=donate[which(donate[,1]==accept[2,1]&donate[,3]==
            accept[2,3]&donate[,4]==accept[2,4]),]
sa
accept[2,2]=sa[1,2] #用找到的样本的对应值替代缺失值
accept

##冷平台插补方法
#由于实际操作中，很难找到与需要插补的样本完全相同的样本，
#可按某变量，将数据分组，在层中对缺失值使用均值插补
level1=nhanes2[which(nhanes2[,3]=="yes"),]#按照hyp分层
level1
level1[4,4]=mean(level1[1:3,4]) #用群内均值代替第四个样本的缺失值
level1


###噪声数据处理（找出离群点）
#离群点还可以通过聚类方法进行检测
library(outliers)
set.seed(1);s1=.Random.seed #设置随机种子
y=rnorm(100) 
plot(y);head(y)
outlier(y)                                           
outlier(y,opposite=TRUE) #找出最远离群点相反的值
dotchart(y) #标记离群点
dim(y) <- c(20,5)  
outlier(y) 
outlier(y,opposite=TRUE)   
set.seed(1);s1=.Random.seed #设置随机种子
y=rnorm(10)  
outlier(y,logical=TRUE)  
plot(y)
#离群点的处理方法
##分箱方法对数据进行排序，通过近邻数据光滑有序数据
set.seed(1);s1=.Random.seed #设置随机种子
x=rnorm(12)
x=sort(x) 
dim(x)=c(3,4)  
x[1,]=apply(x,1,mean)[1] 
x[2,]=apply(x,1,mean)[2]   
x[3,]=apply(x,1,mean)[3]  
x  


##数据不一致的处理
#数据不一致的查询，vapply函数
x=list(a=1:10,beta=exp(-3:3),logic=c(T,F,T,F))
x
probs=c(1:3/4)
rt.value=c(0,0,0)
vapply(x,quantile,FUN.VALUE = rt.value,probs=probs)
probs=c(1:4/4)
vapply(x,quantile,FUN.VALUE = rt.value,probs=probs)
rt.value=rep(0,4)
vapply(x,quantile,FUN.VALUE = rt.value,probs=probs)
rt.value=c(0,0,0,"") ##主要是设置value来判断
vapply(x,quantile,FUN.VALUE = rt.value,probs=probs)


##数据集成
#首先考虑如何进行数据匹配
#使用相关性分析对数据集冗余进行检测
x=cbind(sample(c(1:50),10),sample(c(1:50),10))      
chisq.test(x) 
#相关系数判断数据是否冗余
x=cbind(rnorm(10),rnorm(10))   
cor(x)
cov(x) 

##检测数据是否重复
x=cbind(sample(c(1:10),10,replace=T),rnorm(10),rnorm(10))
head(x)
#是否重复检测
y=unique(x[,1]) ;y;length(y)
sub=rep(0,length(y))    
for(i in 1:length(y))
  sub[i]=which(x[,1]==y[i])[1]
x=x[sub,]
head(x)


##数据变换
#规范化处理(数据标准化处理)
set.seed(1);s1=.Random.seed #设置随机种子
a=rnorm(5);a
b=scale(a) 
b
#离散化处理(连续数据离散化)
set.seed(1)
a=rnorm(10)
n=length(a)
la=rep(0,n)
la[which(a>0.5)]=1
la
#数据合并，数据分层
city=sample(c(1:10),size = 10,replace = T)
city
n=length(city)
provice=rep(0,n) #因为此处赋值均为零，所以有后面的0值
provice[which(city>5)]=1
provice

##数据归约（接近于保持数据的完整性，但数据量小）
##移除不相关的属性，提高模型效率（维规约）
#AIC准则，LASSO，分类树/随机森林，小波变换/主成分
#LASSO进行维规约
x=matrix(rnorm(100*20),100,20) 
y=rnorm(100) 
library(glmnet)
fit1=glmnet(x,y)  #广义线性回归                              
b=coef(fit1,s=0.01)               
b 

predict(fit1,newx=x[1:10,],s=c(0.01,0.005))
