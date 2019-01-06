##R语言社区
#就读取数据可以执行，剩下的都没有进行成功啊
setwd("E:\\Rexercise1\\R语言")
###多元统计分析之因子分析
library(readxl)
data=read_excel("因子分析数据.xlsx")
head(data);class(data)
data=as.data.frame(data)
head(data);str(data)
cor(data[,-1])  #计算相关系数矩阵
library(stats)
.libPaths() #查询我包的下载地址
source("mvstats/R/mvstats")
?factpc
?princomp
fac=princomp(data[,-1],4) #用主成分法作因子分析
fac
##作因子旋转
fa1=factanal(x,3,rot('varimax'))
