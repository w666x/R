##R语言社区
setwd("E:\\Rexercise1\\R语言")
###R+MySQL
#直接下载数据集并解压；另外此处的数据库指的是MySQL
#dplyr包、、MySQL
library(dplyr)
library(ggplot2)
library(data.table)
library(magrittr)
##解析下载数据，并整合数据
data=read.csv("indi2015q2.csv")
data1=read.csv("indi2015q1.csv")
data2=read.csv("demo2015q1.csv")
data3=read.csv("demo2015q2.csv")
head(data);dim(data);dim(data1);dim(data2);dim(data3)
#显示数据类型,数据整合一,(将以demo开头的数据名都提取出来)
filenames=list.files(pattern = "^demo.*.csv",full.names = T)
head(filenames);filenames
demography=rbindlist(lapply(filenames,fread,select=
                              c("primaryid","caseid","age",
                                "event_dt","sex","wt","occr_country")))
head(demography);str(demography);dim(demography)
#数据整合二
filenames=list.files(pattern = "^indi.*.csv",full.names = T)
filenames
indication=rbindlist(lapply(filenames,fread,
                            select=c("primaryid","indi_drug_seq","indi_pt")))
head(indication);str(indication);dim(indication)

###R与MySQL的交互使用
library(RMySQL)
conn=dbConnect(MySQL(),dbname="rmysql",username="rmysql"
               ,password="rmysql")
users=dbGetQuery(conn,"SELECT*FROM demography")
head(users)
dbListTables(conn) #查询rmysql中的所有表
dbListFields(conn,"demography") #显示某一个表中的所有单元
##上传数据到数据库中
#users
dbWriteTable(conn,"demography",demography,overwrite=T)
dbWriteTable(conn, "indication",indication) #读取数据到数据库中
###数据库中查询
#use rmysql;
#select * from indication limit 5;

#查询一
indication=dbGetQuery(conn,"select * from indication") #在数据库中执行相关的查询工作
head(indication)
demography=dbGetQuery(conn,"select * from demography")
head(demography)
#查询二
rs <- dbSendQuery(conn, "SELECT * FROM indication where indi_drug_seq=1")
class(rs)
rs;
d1 <- fetch(rs,n=3) #导出数据
d1

#选取两个或者多个结果集的合并
#可以选取有相同内容的两列合并
ds=dbGetQuery(conn,'select primaryid indi_pt from indication
              union 
              select primaryid age from demography')
head(ds);dim(ds)
ds1=dbGetQuery(conn,'select primaryid indi_pt from indication
              union all 
               select primaryid age from demography')
head(ds1);dim(ds1)


##数据提取
demograhpy=dbReadTable(conn,"demography") #数据的读取以及赋值
head(demograhpy)
indication=dbReadTable(conn,"indication")
head(indication)

###将数据可视化,
#使用%>%将多重命令链接起来
##对demography数据集进行可视化
demography %>% group_by(country=occr_country) %>%
  summarize(total=n()) %>% arrange(desc(total)) %>%
  filter(country!='') %>% head(5)
##可视化
demography %>% group_by(country=occr_country) %>% summarize(total=n()) %>% arrange(desc(total)) %>% 
  filter(country!='') %>% head(5) %>% mutate(country = factor(country,levels = country[order(total)])) %>%
  ggplot(aes(x=country,y=total))+geom_bar(stat='identity',color='blue',fill='yellow')+xlab("")+
  ggtitle('Top Five Countries With Highest Number Of Adverse Events')+theme(plot.title=element_text(size=rel(1.6),  lineheight=.9, family="Times", face="bold.italic", colour="dark green"))+coord_flip()+ylab('Total Number Of Reports')+
  theme(axis.title.x=element_text(size=15, lineheight=.9, family="Times", face="bold.italic", colour="blue"))+
  theme(axis.text.y=element_text(size=12,family="Times", face="bold.italic", colour="blue"))
##进一步可视化分析 ,去除了美国的影响
demography %>% group_by(country=occr_country) %>% summarize(total=n()) %>% arrange(desc(total)) %>% 
  filter(country!='' & country!='US') %>% head(10) %>% mutate(country = factor(country,levels = country[order(total)])) %>%
  ggplot(aes(x=country,y=total))+geom_bar(stat='identity',color='blue',fill='orange')+xlab("")+
  ggtitle('Top Ten Non-US Countries')+theme(plot.title=element_text(size=rel(1.6),  lineheight=.9, family="Times", face="bold.italic", colour="dark green"))+coord_flip()+ylab('Total Number Of Reports')+
  theme(axis.title.x=element_text(size=15, lineheight=.9, family="Times", face="bold.italic", colour="blue"))+
  theme(axis.text.y=element_text(size=12,family="Times", face="bold.italic", colour="blue"))

##对indication数据集进行可视化
indication %>% group_by(indi_pt) %>% 
  summarize(count=n()) %>% arrange(desc(count())) %>% head(5)
##可视化（我们剔除了计数最多的一项）
indication %>% group_by(indi_pt) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(6) %>% tail(-1) %>% 
  mutate(indi_pt=factor(indi_pt,levels = indi_pt[order(desc(count))])) %>% ggplot(aes(x=indi_pt, y=count))+
  geom_bar(stat='identity',colour="#000099",fill="#000099")+ggtitle('Top Five Indication Counts') + xlab('') +
  ylab('')+theme(plot.title =element_text(size = rel(1.6), family="Times", face="bold", colour = "black"))+
  theme(axis.text.x=element_text(angle=90, size=12,family="Times", face="bold", colour="blue"))

###年龄的分布
#可以考虑使用基本分布函数ggplot，qplot
demography$age=round(as.numeric(demography$age))
demography %>% filter(!is.na(age) & age<100 & age>0) %>% select(age) %>% as.data.frame() %>% ggplot(aes(x=age))+
  geom_histogram(breaks=seq(0, 100, by =5), col="dark grey", aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red")+labs(title="Age Histogram") +labs(x="Age",y="")+
  theme(plot.title =element_text(size = rel(1.6), family="Times", face="bold", colour = "black"))+
  theme(axis.text.x=element_text(size=10,family="Times", face="bold", colour="black"))+
  theme(axis.title.x=element_text(size=12,family="Times", face="bold", colour="black"))

###合并数据集
joined=demography %>% inner_join(indication,by="primaryid",copy=T)
warnings()
head(joined);dim(joined)

##将其写入数据库中
dbWriteTable(conn,"demography_indication",joined)

##退出与数据库的连接
dbDisconnect(conn)
summary(MySQL(),verbose=T)
