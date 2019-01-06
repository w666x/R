# start #
# C4.5 # 最后面 c4.5 调用失败，挺尴尬的哦
#Recursive Partitioning and Regression Trees
#分类树和回归树
#library(mvpart)版本有点老了
library(rpart) #建立分类树以及递归算法
library(rpart.plot) #对rpart绘制决策树
library(maptree)  #用来剪枝
library(RWeka)  #建立R与weka的连接
data(car.test.frame)
head(car.test.frame)
dim(car.test.frame)
#油耗指标处理
car.test.frame$Mileage=100*4.546/(1.6*car.test.frame$Mileage)
names(car.test.frame)=c("价格","产地","可靠性","油耗","类型","车重",
                        "发动机功率","净马力")
head(car.test.frame)
#对数据结构类型大致浏览
str(car.test.frame)
summary(car.test.frame)
#数据预处理（油耗变量将作为我们的目标变量）
#分组
Group_Mileage=matrix(0,60,1)
Group_Mileage[which(car.test.frame$"油耗">=11.6)]="A"
Group_Mileage[which(car.test.frame$"油耗"<=9)]="C"
Group_Mileage[which(Group_Mileage==0)]="B"
car.test.frame$"分组油耗"=Group_Mileage
car.test.frame[1:10,c(4,9)]
#建立训练集和测试集,两者之间的比例定为1:3
library(sampling)
a=round(1/4*sum(car.test.frame$"分组油耗"=="A"))
b=round(1/4*sum(car.test.frame$"分组油耗"=="B"))
c=round(1/4*sum(car.test.frame$"分组油耗"=="C"))
a;b;c
##按上面求取的抽样的个数进行分层抽样
sub=strata(car.test.frame,stratanames="分组油耗",size=c(c,b,a),method="srswor")
#使用strata函数对car.test.frame中的“分组油耗”分层抽样
sub
Train_Car=car.test.frame[-sub$ID_unit,] #训练集
Test_Car=car.test.frame[sub$ID_unit,]#生成测试集
nrow(Train_Car);nrow(Test_Car)
head(Train_Car)
str(Train_Car)
levels(Train_Car$类型)

# CART #
library(rpart)
library(rpart.plot)
library(maptree)

#对油耗建立回归树
formula_Car_Reg=油耗~价格+产地+可靠性+类型+车重+发动机功率+净马力
rp_Car_Reg=rpart(formula_Car_Reg,Train_Car,method="anova")
print(rp_Car_Reg)#导出回归树的数字结果
printcp(rp_Car_Reg) #导出回归树的cp表格
summary(rp_Car_Reg)#返回详细信息

##将分支包含的最小样本数由默认20改为10，做回归树
rp_Car_Reg1=rpart(formula_Car_Reg,Train_Car,method="anova",minsplit=10)
print(rp_Car_Reg1)
printcp(rp_Car_Reg1)
#设置模型复杂度
rp_Car_Reg2=rpart(formula_Car_Reg,Train_Car,method="anova",cp=0.1)
print(rp_Car_Reg2)
printcp(rp_Car_Reg2)
#对决策树按照cp进行剪枝
rp_Car_Reg3=prune.rpart(rp_Car_Reg,cp=0.1)
print(rp_Car_Reg3)
printcp(rp_Car_Reg3)

rp_Car_Reg4=rpart(formula_Car_Reg,Train_Car,method="anova",maxdepth=1)
print(rp_Car_Reg4)
printcp(rp_Car_Reg4)

##对油耗变量建立回归树——树形结果
rp_Car_Plot=rpart(formula_Car_Reg,Train_Car,method="anova",minsplit=10)
print(rp_Car_Plot)
rpart.plot(rp_Car_Plot)
rpart.plot(rp_Car_Plot,type=0)
rpart.plot(rp_Car_Plot,type=4,branch=1)
rpart.plot(rp_Car_Plot,type=4,fallen.leaves=TRUE)
#树形结果
draw.tree(rp_Car_Plot,col=rep(1,7),nodeinfo=TRUE)
plot(rp_Car_Plot,uniform=TRUE,main="plot: Regression Tree")
text(rp_Car_Plot,use.n=TRUE,all=TRUE)
post(rp_Car_Plot,file="",title.="post: Regression Tree") 
post(rp_Car_Plot,file="")

#对"分组油耗"变量建立分类树
formula_Car_Cla=分组油耗~价格+产地+可靠性+类型+车重+发动机功率+净马力
rp_Car_Cla=rpart(formula_Car_Cla,Train_Car,method="class",minsplit=5)
print(rp_Car_Cla)
rpart.plot(rp_Car_Cla,type=4,fallen.leaves=TRUE)
#对测试集预测目标变量
pre_Car_Cla=predict(rp_Car_Cla,Test_Car,type="class")
pre_Car_Cla
table(Test_Car$分组油耗,pre_Car_Cla)
(p=sum(as.numeric(pre_Car_Cla!=Test_Car$分组油耗))/nrow(Test_Car))

# C4.5 # 调用失败，挺尴尬的哦
library(RWeka)
names(Train_Car)=c("Price","Country","Reliability","Mileage",
                   "Type","Weight","Disp.","HP","Oil_Consumption") 
Train_Car$Oil_Consumption=as.factor(Train_Car$Oil_Consumption)
formula=Oil_Consumption~Price+Country+Reliability+Type+Weight+Disp.+HP
head(Train_Car)

C45_0=J48(formula,Train_Car)
C45_0
summary(C45_0)

C45_1=J48(formula,Train_Car,control=Weka_control(M=3))
C45_1
plot(C45_1)



