###R中导入数据到MySQL
#密码是 wangxing
#查询，删除，插入，更新
setwd("E:/Rexercise1")

#创建数据库只能在MySQL中进行，R中目前做不到，
# create database db_admin;
#查询有多少个数据库，可以用SHOW DATABASES;执行
#删除数据库，也是在MySQL中执行的，drop database db_admin;

##数据表操作部分
#创建数据表
#use db_admin；create table tb_admin{}
#查看表的结构
#show columns from tb_admin from db_admin;desc tb_admin;
#修改表的结构
#alter table tb_admin add email varchar(50) not null;
#重命名表
#rename tb_admin to tb_user;
#删除表
#drop table tb_user;

#MySQL语句操作
#插入记录
# insert into tb_admin(user) values ('tsoft')
#查询记录
#select * from tb_user;
#修改记录或者更新
#update tb_admin set password='1234' where user='tsoft';
#删除记录
#delete from tb_admin where user='小新'；


#RMySQL上的对于数据库的相关操作
##数据库操作部分
##加载库类，建立本地连接，选择连接某一个数据库
library(RMySQL)
conn=dbConnect(MySQL(),dbname="rmysql",username="rmysql",
               password="rmysql",CLIENT_LONG_FLAG=
                 CLIENT_MULTI_STATEMENTS)
dbListTables(conn) #显示DB中有多少表
dbListFields(conn,"t_user")#显示表中有多少变量
user=dbGetQuery(conn,"select * from t_user") #在数据库中执行相关的查询工作
dbDisconnect(conn)
user;

#当RMySQL连接上数据库后，即可执行数据表的相关操作
#删除数据表
if(dbExistsTable(conn,'t_use')){ #如果某一数据库中存在某表，则删除该表
  dbRemoveTable(conn,'t_use')
}

##R中载入数据到数据库中
t_demo<-data.frame(
  a=seq(1:10),
  b=letters[1:10],
  c=rnorm(10)
)
dbWriteTable(conn,"t_demo",t_demo,overwrite=T) #载入数据
dbReadTable(conn,"t_demo") #读取数据
c=dbReadTable(conn,"t_demo") #数据的读取以及赋值
c
dbListTables(conn)

#查询一
user=dbGetQuery(conn,"select * from t_user") #在数据库中执行相关的查询工作
#查询二
rs <- dbSendQuery(conn, "SELECT * FROM t_demo where c>0")
class(rs)
d1 <- fetch(rs,n=3)
d1
dbClearResult(rs)

##R中载入数据到数据库中二
t_work=read.csv("3_7.csv",header = T)
dbWriteTable(conn,"t_work",t_work,overwrite=T)
dbReadTable(conn,"t_work")

##从数据库中读取该数据的前五行
work1=dbGetQuery(conn,"select * from t_work limit 5")
work1

##载入数据到数据库中，将数据变量分为两组
t_work1=t_work[,c(2:4)]
head(t_work1)
t_work2=t_work[,c(2,5,6)]
head(t_work2)
dbWriteTable(conn,"t_work1",t_work1,overwrite=T)
dbWriteTable(conn,"t_work2",t_work2,overwrite=T)
dbDisconnect(conn)
dbReadTable(conn,'t_work1')
dbListFields(conn,'t_work1')
dbListFields(conn,'t_work2')

#选取两个或者多个结果集的合并
ds=dbGetQuery(conn,'select name,sex from t_work1
              union 
              select name,sex from t_work')
ds
ds1=dbGetQuery(conn,'select name,sex from t_work1
              union all 
              select name,sex from t_work')
ds1


##读取数据集三
person=read.table("Persons.txt",header = T)
Order1=read.table("Orders.txt",header = T)
Order=as.data.frame(Order1)
Order1
person
##导入数据库
dbWriteTable(conn,"person",person,overwrite=T)
dbDisconnect(conn)
dbReadTable(conn,'Order1')
#制作备份文件
ds=dbGetQuery(conn,'select * from person where id_p=1')
ds
dbWriteTable(conn,'persons_backup',ds[,-1],overwrite=T)
dbReadTable(conn,'persons_backup')
dbListTables(conn)
#制作备份文件二
dbReadTable(conn,'t_work1')
dbListFields(conn,'t_work1')
dbReadTable(conn,'t_work2')
dbListFields(conn,'t_work2')
ds2=dbGetQuery(conn,'select t_work1.name,t_work1.age,t_work2.height
               from t_work1
               inner join t_work2
               on t_work1.name=t_work2.name')
ds2
dbWriteTable(conn,'ds2',ds2,overwrite=T)
dbListTables(conn)
dbGetQuery(conn,'select * from ds2 limit 5')
dbDisconnect(conn)
