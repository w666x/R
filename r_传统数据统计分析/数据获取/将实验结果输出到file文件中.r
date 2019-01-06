getwd()
##将实验结果输出到file文件中
setwd("E:/Rexercise1")
sink("result.txt") #将结果保存到result.r中
cr=1;mr=2;cureprob=3;nai=4
cat("cr=",cr,fill=T) #输出cr值
cat("mr=",mr,fill=T) #输出mr值
cat("cureprob=",cureprob,fill=T)
cat("nai=",nai,fill=T) #输出nai值
sink()

