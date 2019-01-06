###观测样本数，变量数，以及变量的实际含义，对数据
###集的庞大程度和相对重要性做到心中有数。
##定性数据：定类数据（性别），定序数据（年龄），定距数据。。。
###R用R做简单数据处理
###数据抽样及R实现
###训练集和测试集

library ( MASS )                                  # 加载含有数据集的软件包MASS
data ( Insurance )                                          # 获取数据集Insurance
head(Insurance)
class(Insurance)
str(Insurance)
dim ( Insurance )                                             # 获取数据集的维度
dim ( Insurance[1:10, ] )                            # 获取数据集前10条数据的维度
dim ( Insurance[ ,2:4] )                 # 获取数据集仅含第2、3、4个变量部分的维度
dim ( Insurance ) [1]                     # 获取数据集维度向量的第一个元素，即行数
dim ( Insurance ) [2]                     # 获取数据集维度向量的第二个元素，即列数

vars = c ( "District", "Age" )     # 构造含有“District”和“Age”两个元素的字符向量vars
Insurance [ 20:25, vars ]                 # 筛选出District及Age变量的第20-25行数据
names ( Insurance )                                  # 输出Insurance数据集变量名
head ( names(Insurance), n=2 )                                # 仅输出前2个变量名
tail ( names(Insurance), n=2 )                                # 仅输出后2个变量名
head ( Insurance$Age )                              # 仅输出Age变量前若干条数据

class ( Insurance$District )                                # 显示District的变量类型
class ( Insurance$Age )                                      # 显示Age的变量类型
class ( Insurance$Holders )                                # 显示Holders的变量类型

levels ( Insurance$Age )                                # 显示Age变量的4个水平值
levels ( Insurance$Age) [1]                            # 显示Age变量的第1个水平值
levels ( Insurance$Age ) [1] = "young"        # 将Age变量的第1个水平值修改为“young”
head ( Insurance$Age )                          # 回看修改后Age变量前若干个取值

is.character ( Insurance$Age )                           # 判断Age是否为字符型变量
class ( Insurance$Claims )                                  # 显示Claims的变量类型
class ( as.numeric (Insurance$Claims) )         # 将Claims的数据类型强制转换为数值型


# 抽样技术 #选取测试集和训练集
##简单随机抽样
sub1=sample(nrow(Insurance),10,replace=T)
Insurance[sub1,]

sub2=sample(nrow(Insurance),10,replace=T,prob=c(rep(0,nrow(Insurance)-1),1))
Insurance[sub2,]

sub3=sample(nrow(Insurance),nrow(Insurance)+1)
sub31=sample(nrow(Insurance),nrow(Insurance))
Insurance[sub31,]

##分层抽样
library(sampling)
sub4=strata(Insurance,stratanames="District",size=c(1,2,3,4),method="srswor")
sub4 #抽取district分层抽样
getdata(Insurance,sub4) #获取分层抽样的数据

sub5=strata(Insurance,stratanames="District",size=c(1,2,3,4),description=TRUE)
sub5
getdata(Insurance,sub5)
##系统抽样，抽取概率为pik定义的值得大小
sub6=strata(Insurance,stratanames="District",size=c(1,2,3,4),method="systematic",pik=Insurance$Claims)
sub6
getdata(Insurance,sub6)

##整群抽样，size定义群的个数
sub7=cluster(Insurance,clustername="District",size=2,method="srswor",description=TRUE)
sub7
str(Insurance)

##
sub8=mstage(Insurance, stage = c("stratified","stratified"), varnames=c("District","Group"), size=list(c(16,16,16,16),2),description=TRUE)
sub8
getdata(Insurance,sub8)

train_sub=sample(nrow(Insurance),3/4*nrow(Insurance))
train_data=Insurance[train_sub,]
test_data=Insurance[-train_sub,]
dim(train_data);dim(test_data)
