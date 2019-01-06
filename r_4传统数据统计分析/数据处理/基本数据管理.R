setwd("E:/Rexercise1")
###基本数据管理
###示例一
manger=c(1,2,3,4,5)
date=c("10/24/08","10/28/08","10/1/08",
       "10/12/08","5/1/09")
gender=c("M","F","F","M","F")
country=c("US","US","UK","UK","UK")
age=c(32,45,25,39,99)
q1=c(5,3,3,3,2)
q2=c(4,5,5,3,2)
q3=c(5,2,5,4,1)
q4=c(5,5,5,NA,2)
q5=c(5,5,2,NA,1)
leadership=data.frame(manger,date,country,gender,age,
                      q1,q2,q3,q4,q5,stringsAsFactors=T)
leadership
###数据整合；不完整数据处理；分组变量的处理；异常值处理
leadership$age[leadership$age == 99] =NA
leadership$agecat[leadership$age >75] = "Elder"
leadership$agecat[leadership$age >=55 &
                    leadership$age <75] ="Middle Aged"
leadership$agecat[leadership$age <55]="Young"
leadership
###紧凑型的数据整合
leadership=within(leadership,{
  agecat1 = NA
  agecat1[age>75] = "Elder"
  agecat1[age >= 55 & age <= 75] = "Middle"
  agecat1[age <55] = "Young"
})
leadership
leadership$agecat2=cut(age,breaks=c(0,55,75,90),
                       labels =F)
leadership
###变量的重命名
fix(leadership)
leadership=edit(leadership)
leadership
library(reshape2)
leadership=rename(leadership,
                  c(manger="mangerID",date="testDate"))
names(leadership)
names(leadership)[2] = "testDate"
names(leadership)
names(leadership)[6:10]=paste("item",c(1:5),seq="")
leadership
###缺失值的处理的寻找
is.na(leadership[,6:10])
##在分析中排出缺失值
na.omit(leadership)
apply(leadership[,6:10],1,sum)
apply(leadership[,6:10], 2, sum,na.rm=T)
apply(leadership[,6:10],2,sum)
###日期值的处理,使之可以进行数值计算
mydates=as.Date(c("2007-06-22","2004-02-13"))
mydates
mydates[2]-mydates[1]
difftime(mydates[1],mydates[2],units = "weeks")
dates=as.Date(mydates,"%B/%d/%Y")
dates
today=Sys.Date()
date()
format(today,format="%A")
###将日期转为字符型变量
date1=as.character(mydates)
date1[1]-date1[2]
###类型变换
is.numeric(leadership[,6:10])
###数据排序
newdata=leadership[order(leadership$age),]
newdata
###数据删除
myvars = names(leadership) %in% c("item3","item4")
newdata=leadership[!myvars]
newdata
###数据选入规则
##示例一
newdata = leadership[1:3,]
newdata = leadership[which(leadership$gender=="M" &
                             leadership$age >30),]
newdata
##示例二
attach(leadership)
newdata=leadership[which(gender=="M" & age >30),]
detach()
newdata
###示例三(选取合适的数据出来)
leadership$testDate = as.Date(leadership$testDate,
                              "%m/%d/%Y")
startdate = as.Date("2009-01-01")
enddate = as.Date("2009-10-31")
newdata - leadership[which(leadership$testDate >= startdate & 
                             leadership$testDate<= enddate),]
###
newdata = subset(leadership,age>=35|age<24,
                 select = c(leadership$`item 1 `))
newdata
newdata=subset(leadership,gender="M" & age>25,
               select = gender:leadership$`item 4 `)
newdata
###使用SQL语句操作数据框
install.packages("sqldf")
