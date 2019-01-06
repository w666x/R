setwd("E:/Rexercise1")
###R与MySQL的交互使用
library(RMySQL)
conn=dbConnect(MySQL(),dbname="rmysql",username="rmysql"
               ,password="rmysql")
users=dbGetQuery(conn,"SELECT*FROM t_user")
dbDisconnect(conn)
summary(MySQL(),verbose=T)

###建表并插入数据
t_demo<-data.frame(
  a=seq(1:10),
  b=letters[1:10],
  c=rnorm(10)
)
dbWriteTable(conn, "t_demo", t_demo)
###获得整个表数据
dbReadTable(conn, "t_demo")
###插入新数据
dbWriteTable(conn, "t_demo", t_demo, append=TRUE)
dbReadTable(conn, "t_demo")

t_demo
class(t_demo)
