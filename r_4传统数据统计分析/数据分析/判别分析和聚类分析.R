#判别分析和聚类分析

##进行聚类分析的相关步骤
#读入数据，准备好数据矩阵
#使用dist函数计算n个样品两两之间的距离
#使用hclust对dist返回的结果进行聚类分析
#绘制谱系图
#根据图形的结果决定类的个数和类，并提取类

##判别分析
#距离判别的R实现
#导入原始数据，计算距离
B=read.table("原始数据包/bankruptcy.txt",header = T)
head(B)
dim(B)
mu=colMeans(B) #得到各列的均值
mu
sx=cov(B)
sx
distance=mahalanobis(B,mu,sx) #计算距离
options(digits = 3)
distance
#进行判别分析
library(WMDB)
G=c(rep(1,17),rep(2,21)) #生成训练样本的已知类别
G=as.factor(G)
wmd(B,G)
#待判样品的判别分析
newdata=data.frame(X1=c(0.04,-0.06,0.07,-0.13,0.15,0.16,0.29,0.54),
                      X2=c(0.01,-0.06,-0.01,-0.14,0.06,0.05,0.06,0.11),
                      X3=c(1.5,1.37,1.37,1.42,2.23,2.31,1.84,2.33),
                      X4=c(0.71,0.4,0.34,0.44,0.56,0.2,0.38,0.48))
head(newdata)
dim(newdata)
wmd(B,G,TstX = newdata) #待测样本的分类判别，但是有警告

#Fisher判别的R实现
B=read.table("原始数据包/bankruptcy.txt",header=T)
dim(B)
edit(B)
G=c(rep(1,17),rep(2,21)) #生成训练样本的已知类别
G=as.factor(G)
B$class=G
attach(B)
names(B) #显示数据中所有的对象
#判别分析
library(MASS)
B.lda=lda(class~X1+X2+X3+X4)
B.lda
#对样本的判别结果与真实类别作比较
class.pre=predict(B.lda)$class #选择预测结果中的对象class
table(class)
table(class.pre,class)
#判别分析效果的检验-卡方检验
chisq.test(class,class.pre)
#对未测的样本进行判别分析
head(newdata)
predict(B.lda,newdata = newdata)
#给出结果中会包括判别分析的直接结果；属于某一类的概率；
#以及现行判别函数的具体取值
detach()

#贝叶斯判别法
#选取平均误判损失最下的即可以作为归属类别
#R实现
library(WMDB)
B=read.table("原始数据包/bankruptcy.txt",header=T)
dim(B)
head(B)
G=c(rep(1,17),rep(2,21)) #生成训练样本的已知类别
dbayes(B,G)

#聚类分析及R实现
drink=read.table("原始数据包/drink.txt",header=T)
head(drink)
dim(drink)
drink=drink[,-1] #去掉第一列，即饮料的编号
head(drink)
#求取距离
d=dist(drink)
head(d)
length(d)
#各种方式聚类
hcl=hclust(d,method = "ward.D") #离差平方和
hc2=hclust(d,method = "single") #最短距离法
hc3=hclust(d,method = "complete") #最长举例法
opar=par(mfrow=c(1,3))
#作出谱系图，得到分类的一个直观的结果
plot(hcl,hang = -1);plot(hc2,hang =-1);plot(hc3,hang = -1);
par(opar) #释放绘图区域
#对数据进行分组
cutree(hc2,4) #将数据分成四组
cutree(hc3,3)
#绘制出谱系图
drink.hc=as.dendrogram(hc2) #将聚类分析的返回对象转换为谱系图对象
par(mfrow=c(1,2)) #type决定了谱系图的类型
plot(drink.hc,type = "rectangle",nodePar = list(pch=c(1,NA),lab.cex=.8))
plot(drink.hc,nodePar = list(pch=2:1,cex=.4*2:1,col=2:3),horiz = T)
plot(drink.hc,nodePar = list(pch=2:1,cex=.4*2:1,col=2:3),horiz =F)
par(mfrow=c(1,1))

#聚类分析R实例
dat=read.table("原始数据包/real estate.txt",header=T)
head(dat)
dim(dat)
dat=dat[,-1]
dim(dat)
d=dist(dat)
length(d)
hc=hclust(d,method = "ward.D2")
plot(hc,hang = -1)
cutree(hc,4)
cutree(hc,3)
cutree(hc,5)
