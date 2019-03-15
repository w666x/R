# i = 1
for (i in 1:10)
{  #传参控制进程,变量 
  print(i)
  test1 = matrix(i:(i+15),4,4)
  switch(menu(c("coutinue", "break")),
         cat("continue the process\n"), stop("we are stoping the process"))
  print(test1)
  #  尝试一：根据get给出名字来存储文件 for i in ls() print(get(i)+1)
  write.csv(test1,file=paste("test4_",i,".csv"))
  #  尝试二：根据eval来运行命令存储文件
  command_sentences = paste('write.csv(test1,file=paste("test4_",',i,',".csv"))')
  eval(parse(text = command_sentences))
  }
