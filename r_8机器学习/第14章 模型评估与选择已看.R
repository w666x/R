# 安装程序包rattle
#install.packages("rattle", dependencies=TRUE)	
# 更新程序包rattle
#install.packages("rattle",repos=http://rattle.togaware.com)
library(rattle)	# 加载程序包
##查询数据包里面有哪些数据集
data(package="rattle")
rattle() 	# 调用程序包

##运用函数 predict
##混淆矩阵:
#误判率，精确度，敏感度，特异性
##风险图
##roc图像
##得分数据集
data("audit.csv")
rattle::acquireAuditData(write.to.file = "audit.csv")
help("rattle")
data("audit") ##此处导入数据，使rattle中，dataset里面可以读取数据
head(audit)
