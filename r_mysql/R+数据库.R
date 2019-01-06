##R语言社区
setwd("E:\\Rexercise1\\R语言")
###R+数据库非常完美
#直接下载数据集并解压；另外此处的数据库指的是sqlite
#dplyr包、、sqlite
library(dplyr)
library(ggplot2)
library(data.table)
library(magrittr)
##下载数据(数据的下载方式)
year=2015
for (m in (1:2) ){
  url1<-paste0("http://www.nber.org/fda/faers/",year,"/demo",year,"q",m,".csv.zip")
  download.file(url1,dest="data.zip") # Demography
  unzip ("data.zip")
  url2<-paste0("http://www.nber.org/fda/faers/",year,"/indi",year,"q",m,".csv.zip")
  download.file(url2,dest="data.zip") # Indication
  unzip ("data.zip")
}
##解析下载数据，并整合数据
data=read.csv("indi2015q2.csv")
head(data);dim(data);str(data)
#显示数据类型,数据整合一
filenames=list.files(pattern = "^demo.*.csv",full.names = T)
head(filenames);filenames
demography=rbindlist(lapply(filenames,fread,select=
                              c("primaryid","caseid","age",
                                "event_dt","sex","wt","occr_country")))
head(demography);str(demography)
#数据整合二
filenames=list.files(pattern = "^indi.*.csv",full.names = T)
filenames
indication=rbindlist(lapply(filenames,fread,
                            select=c("primaryid","indi_drug_seq","indi_pt")))
head(indication);str(indication)

###创建数据库
my.db=src_sqlite("adverse.events",create = T) 
##上传数据到数据库中
copy_to(my.db,demography,temporary = F)
copy_to(my.db,indication,temporary = F)
##建立与已有数据库的链接并检索所存数据
my.db=src_sqlite("adverse.events",create = F) #建立链接
src_tbls(my.db) #显示数据库中的部分数据,其中有数据集以及相应的数据统计
#访问数据库
#首先从数据库中导入数据
demography=tbl(my.db,"demography")
head(demography)
head(indication)
indication=tbl(my.db,"indication")
head(indication)
sqlite_stat1=tbl(my.db,"sqlite_stat1")
head(sqlite_stat1)
FR=filter(demography,occr_country='FR')
FR$query #选取了部分数据
explain(FR)

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
head(joined)
