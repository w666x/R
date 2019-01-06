###R与数据库，此处主要是sqlite数据库
#数据包准备
library(dplyr)
library(ggplot2)
library(data.table)
library(magrittr)
#下载数据
year=2015
for (m in (1:2) ){
##把数据地址定义为url1,url2等，然后分别下载数据集
  url1<-paste0("http://www.nber.org/fda/faers/",year,"/demo",year,"q",m,".csv.zip")
  download.file(url1,dest="data.zip") # Demography
  unzip ("data.zip")
  url2<-paste0("http://www.nber.org/fda/faers/",year,"/indi",year,"q",m,".csv.zip")
  download.file(url2,dest="data.zip") # Indication
  unzip ("data.zip")
}

##解析下载数据，构建人口统计信息和反应症状数据集
filenames <- list.files(pattern="^demo.*.csv", full.names=TRUE)
filenames
demography = rbindlist(lapply(filenames, fread, 
                              select=c("primaryid","caseid","age","event_dt","sex","wt","occr_country")))
str(demography)
head(demography)
##解析下载数据集二
filenames <- list.files(pattern="^indi.*.csv", full.names=TRUE)
filenames
indication = rbindlist(lapply(filenames, fread, 
                              select=c("primaryid","indi_drug_seq","indi_pt")))
str(indication)
head(indication)

##连接数据库
###R与MySQL的交互使用
library(RMySQL)
conn=dbConnect(MySQL(),dbname="rmysql",username="rmysql"
               ,password="rmysql")
users=dbGetQuery(conn,"SELECT*FROM t_user")
users
summary(MySQL(),verbose=T) #显示当前数据库的各种信息
#上载数据集到建好的数据库中
dbWriteTable(conn,"demography",demography)
dbWriteTable(conn,"indication",indication)
#建立与已有数据库的链接并检索所存的数据表
dbReadTable(conn,"demography")
dbListTables(conn)
dbListFields(conn,"demography")
work1=dbGetQuery(conn,"select * from demography limit 5")
work1
#断开R与数据库的连接
dbDisconnect(conn)

##数据分析++可视化ggplot
##数据可视化，查看数据的大致分布
##将数据降序求和，然后从大到小排列
demography %>% group_by(country=occr_country) %>% summarize(total=n()) %>%
  arrange(desc(total)) %>% filter(country!='') %>% head(6)

#数据整理，及可视化
demography %>% group_by(country=occr_country) %>%
  summarize(total=n()) %>% arrange(desc(total)) %>% 
  filter(country!='') %>% head(5) %>% mutate(country = factor(country,levels = country[order(total)]))%>%
  ggplot(aes(x=country,y=total))+geom_bar(stat='identity',color='blue',fill='yellow')+xlab("")+
  ggtitle('Top Five Countries With Highest Number Of Adverse Events')+theme(plot.title=element_text(size=rel(1.6),  lineheight=.9, family="Times", face="bold.italic", colour="dark green"))+coord_flip()+ylab('Total Number Of Reports')+
  theme(axis.title.x=element_text(size=15, lineheight=.9, family="Times", face="bold.italic", colour="blue"))+
  theme(axis.text.y=element_text(size=12,family="Times", face="bold.italic", colour="blue"))

##剔除美国后，再行观察其他国家的差异
demography %>% group_by(country=occr_country) %>% summarize(total=n()) %>% arrange(desc(total)) %>% 
  filter(country!='' & country!='US') %>% head(10) %>% mutate(country = factor(country,levels = country[order(total)])) %>%
  ggplot(aes(x=country,y=total))+geom_bar(stat='identity',color='blue',fill='orange')+xlab("")+
  ggtitle('Top Ten Non-US Countries')+theme(plot.title=element_text(size=rel(1.6),  lineheight=.9, family="Times", face="bold.italic", colour="dark green"))+coord_flip()+ylab('Total Number Of Reports')+
  theme(axis.title.x=element_text(size=15, lineheight=.9, family="Times", face="bold.italic", colour="blue"))+
  theme(axis.text.y=element_text(size=12,family="Times", face="bold.italic", colour="blue"))

##年龄的分布
demography$age <- round(as.numeric(demography$age))
demography %>% filter(!is.na(age) & age<100 & age>0) %>% select(age) %>% as.data.frame() %>% ggplot(aes(x=age))+
  geom_histogram(breaks=seq(0, 100, by =5), col="dark grey", aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red")+labs(title="Age Histogram") +labs(x="Age",y="")+
  theme(plot.title =element_text(size = rel(1.6), family="Times", face="bold", colour = "black"))+
  theme(axis.text.x=element_text(size=10,family="Times", face="bold", colour="black"))+
  theme(axis.title.x=element_text(size=12,family="Times", face="bold", colour="black"))


##数据集的操作，合并数据集
##以primaryid作为合并的依据
joined=demography %>% inner_join(indication, by='primaryid',copy = TRUE) 
head(joined,5)
