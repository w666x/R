###基本图形
###导入数据包VCD，并安装
install.packages("vcd")
library(vcd)
###均值条形图
states=data.frame(state.region,state.x77)
head(states)
dim(states)
means=aggregate(states$Illiteracy,by=list(state.region),
                FUN=mean)
means  ##求得均值
means=means[order(means$x),]
means  ##均值排序后画出条形图，用names.arg展示标签
barplot(means$x,names.arg = means$Group.1)
title("Mean Illiteracy Rate")

##饼图
opar=par(no.readonly = T)
par(mfrow=c(2,2))
slices=c(10,12.4,16,18)
lbls=c("US","UK","Australia","Germany","France")
pie(slices,labels = lbls,main = "Simple Pie Chart")
##为饼图添加比例数值
pct=round(slices/sum(slices)*100)
lbls2=paste(lbls," ",pct,"%",sep = "")
lbls2
pie(slices,labels = lbls2,col = rainbow(length(lbls2)),
    main = "Pie Chart with Percentages")
##从表格创建饼图
install.packages("plotrix")
library(plotrix)
pie3D(slices,labels=lbls,explode=.1,main="pie")
##从表格创建饼图，\n表示换行
mytable=table(state.region)
mytable
lbls3=paste(names(mytable),"\n",mytable,sep = "")
lbls3
pie(mytable,labels = lbls3,
    main="Pie Chart from a Table\n (with sample sizes)")
par(opar)

rug(jitter(mtcars$mpg,amount = .01))
install.packages("sm")
library(sm)

boxplot.stats(mtcars$mpg)
boxplot(mtcars$mpg)
