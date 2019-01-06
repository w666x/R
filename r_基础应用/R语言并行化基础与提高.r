#R语言中文社区
setwd("E:/Rexercise1/R语言中文社区")
##介绍了R的部分并行计算，并给出了一些常见的陷阱以及避免的小技巧
##采用并行计算的原因就是因为程序运行时间太长
##一般是通过browser/cat/print发现错误
##trycatch捕捉错误
##do.call,可以将列表按矩阵的方式输出
##Embarrassingly parallel
##Bootstrapping
##交叉验证（Cross-validation）
##拟合多元回归方程
#学习lapply为关键
lapply(1:3, function(x) c(x,x^2,x^3))
#可以添加额外的参数
lapply(1:3,round,digits=3)

#parallel包
library(parallel)
##claculate the number of cores #初始化集群
no_cores=detectCores()-1  #确定集群数量
#启动集群initiate cluster
cl=makeCluster(no_cores)
#使用并行版本的lapply,parlapply即可了
parLapply(cl, 2:4,
          function(exponent)
            2^exponent)
#当我们结束时，需要关闭集群，否则电脑内存会被占用
stopCluster(cl)

##变量作用域
cl=makeCluster(no_cores)
base=2
clusterExport(cl,"base") #将base变量引入到集群中
parLapply(cl,2:4,
          function(exponent)
            base^exponent)
stopCluster(cl)
#当clusterexport加载某些变量后，此类变量的任何变化都会被忽略
cl=makeCluster(no_cores)
clusterExport(cl,"base") #此处base是等于2
base=4
parLapply(cl,2:4,
          function(exponent)
            base^exponent)
stopCluster(cl)

#使用parsapply，返回一个向量或者矩阵
cl=makeCluster(no_cores)
clusterExport(cl,"base") 
base=2;
parSapply(cl,2:4,
          function(exponent)
            base^exponent)
#输出矩阵并显示行名和列名
parSapply(cl,as.character(2:4),
          function(exponent){
            x=as.numeric(exponent)
            c(base=base^x,self=x^x)
          })

#foreach包
#使用register注册集群
library(foreach)
library(doParallel)
cl=makeCluster(no_cores)
registerDoParallel(cl) #注册集群
foreach(exponent=2:4, #结果行排列
        .combine=cbind) %dopar% base^exponent
foreach(exponent=2:4,.combine=rbind) %dopar% base^exponent
                      #结果列排列
foreach(exponent =2:4, .combine = list,  #列表形式输出结果
        .multicombine =TRUE) %dopar%  base^exponent
foreach(exponent = 2:4,  #列表形式输出结果
        .combine = list)  %dopar%  base^exponent
stopImplicitCluster() #结束集群

#变量作用域
base <- 2
cl<-makeCluster(2)
registerDoParallel(cl)
foreach(exponent = 2:4, 
          .combine = c)  %dopar%  
  base^exponent
stopCluster(cl)
#对于父环境的变量不会加载
test <- function (exponent) {
  foreach(exponent = 2:4, 
          .combine = c)  %dopar%  
    base^exponent
}
test()
#改进,使用.export可以载入父环境中变量
base <- 2
cl<-makeCluster(2)
registerDoParallel(cl)
base <- 4
test <- function (exponent) {
  foreach(exponent = 2:4, 
          .combine = c,
          .export = "base")  %dopar%  
    base^exponent
}
test()

##内存控制
#windows下，我们使用PSOCK
#psock
library(pryr) # Used for memory analyses
icl<-makeCluster(no_cores)
clusterExport(cl, "a")
clusterEvalQ(cl, library(pryr))
parSapply(cl, X = 1:10, function(x) {address(a)}) == address(a)
stopCluster(icl)
#
cl=makeCluster(no_cores)
b <- 0
base=2
clusterExport(cl,"b")
parSapply(cl, X = 1:10, function(x) {b <- b + 1; b})
# [1] 1 1 1 1 1 1 1 1 1 1
parSapply(cl, X = 1:10, function(x) {b <<- b + 1; b})
# [1] 1 2 3 4 5 1 2 3 4 5
b
# [1] 0

##错误的捕捉
foreach(x=list(1, 2, "a"))  %dopar%  
{
  tryCatch({
    c(1/x, x, 2^x)
  }, error = function(e) return(paste0("The variable '", x, "'", 
                                       " caused the error: '", e, "'")))
}

#通过rbind在lapply进行conbine
out=lapply(1:3, function(x) c(x,x^2,x^3))
out
do.call(rbind,out) #通过矩阵展示结果
stopCluster(cl)

#创建一个文件输出，使结果输出到文件当中
cl=makeCluster(no_cores,outfile="debug.txt")
registerDoParallel(cl)
foreach(x=list(1, 2, "a"))  %dopar%  
{
  print(x)
}
stopCluster(cl)

##创建一个专用文件，如果数据集有某些问题，可以观测
cl<-makeCluster(no_cores, outfile = "debug.txt")
registerDoParallel(cl)
foreach(x=list(1, 2, "a"))  %dopar%  
{
  cat(dput(x), file = paste0("debug_file_", x, ".txt"))
} 
stopCluster(cl)


##partools包
#cathing当做一个大型计算时，我强烈推荐使用一些缓存
cacheParallel <- function(){
  vars <- 1:2
  tmp <- clusterEvalQ(cl, 
                      library(digest))
  
  parSapply(cl, vars, function(var){
    fn <- function(a) a^2
    dg <- digest(list(fn, var))
    cache_fn <- 
      sprintf("Cache_%s.Rdata", 
              dg)
    if (file.exists(cache_fn)){
      load(cache_fn)
    }else{
      var <- fn(var); 
      Sys.sleep(5)
      save(var, file = cache_fn)
    }
    return(var)
  })
}
#运行
system.time(out=cacheParallel())
