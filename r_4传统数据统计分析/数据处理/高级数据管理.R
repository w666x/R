###高级数据管理
setwd("E:/Rexercise1")
###数据的标准化
mydata=matrix(c(1:12),4)
newdata=scale(mydata)
newdata
###数据处理难题的一套解决方案
options(digits = 2)
student = c("John Davis","Angela Williams","Bullwink Moose",
            "David Jones","Janice Markhammer","Cheryl Cushing",
            "Reuven Ytzrhak","Grep Knox","Joel England",
            "Mary Rayburn")
Math=c(502,600,412,358,495,512,410,625,573,522)
Science=c(95,99,80,82,75,85,80,95,89,86)
English=c(25,22,18,15,20,28,15,30,27,18)
roster=data.frame(student,Math,Science,English,
                  stringsAsFactors = T)
roster
###标准化
Z=scale(roster[,2:4])
Z
score=apply(Z,1,mean)
score
roster=cbind(roster,score)
roster
y=quantile(score,c(.8,.6,.4,.2))
y
###确定分位数后，即可对相应的学生成绩定义ABCD了
###抽取姓氏和名字
name=strsplit(as.character(roster$student),' ')
help("strsplit")
name
lastname=sapply(name, "[",2)
firstname=sapply(name, "[",1)
roster=cbind(firstname,lastname,roster[,-1])
roster
roster=roster[order(lastname,firstname),]
roster

###数据的整合
attach(mtcars)
aggdata=aggregate(mtcars,by=list(cyl,gear),FUN=mean,na.rm=T)
aggdata

###优秀的调试函数
#Debugging in R”（http://www.
#stats.uwo.ca/faculty/murdoch/software/debuggingR）。