###网络爬虫之rvest
library(rvest)
library(sqldf)
library(gsubfn)
library(proto)
library(tcltk)
#creat a function
#建立一个函数，可以把之后我需要做的步骤都放一部分进去
extrafun <- function(i,non_pn_url){
  url <- paste0(non_pn_url,i)
  web <- read_html(url)
  papername<- web %>% html_nodes("dl.bbda dt.xs2 a") %>% html_text()%>% .[c(seq(2,20,2))] %>% as.character()
  paperlink<-gsub("\\?source\\=search","",web %>% html_nodes("dl.bbda dt.xs2 a") %>% html_attr("href"))%>% .[c(seq(2,20,2))]
  paperlink <- paste0("http://blog.sciencenet.cn/",paperlink) %>% as.character()
  posttime <- web %>% html_nodes("dl.bbda dd span.xg1") %>% html_text() %>% as.Date()#这里取每篇文章的发布时间
  count_of_read <- web %>% html_nodes("dl.bbda dd.xg1 a") %>% html_text()
  count_of_read <- as.data.frame(count_of_read)
  count_of_read <- sqldf("select * from count_of_read where count_of_read like '%次阅读'")
  data.frame(papername,posttime,count_of_read,paperlink)
}
#crawl data  数据爬取
final <- data.frame()
url <- 'http://blog.sciencenet.cn/home.php?mod=space&uid=556556&do=blog&view=me&page='
for(i in 1:40){
  extrafun(i,url)
  final <- rbind(final,extrafun(i,url))
}
dim(final)
head(final)


###数据整理
write.table(final,"final.csv",fileEncoding="GB2312")
#抓取的数据需要在Excel进一步加工，加工后读取进来，进一步做分析
a <- read.table("dai_shen_blog_0326.csv",header=TRUE,sep=";",fileEncoding="GB2312")#Mac OS 环境下，要sep＝";"
a$posttime <- as.Date(a$posttime)
a$paperlink <- as.character(a$paperlink)
a$papername <- as.character(a$papername)
a$count_of_read_NO. <- as.numeric(a$count_of_read_NO.)
library(ggplot2)
qplot(posttime,count_of_read_NO.,data=a,geom="point",colour=repost,size=6)


##爬虫二
#Crawl NBA player statistics from sina
#web http://nba.sports.sina.com.cn/playerstats.php?s=0&e=49&key=1&t=1
library(rvest)
library(stringr)
library(sqldf)
rm(NBAdata)
start <- seq(0,250,50)
end <- seq(49,299,50)
getdata <- function(i){  ##定义爬取函数，里面包括了所需要爬取的内容
  url <- paste0('http://nba.sports.sina.com.cn/playerstats.php?s=',start[i],'&e=',end[i],'&key=1&t=1')
  rank <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(1)") %>% html_text()%>%.[-1]%>%as.numeric()
  player <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(2)") %>% html_text()%>%.[-1]%>%str_sub(9,100)%>%as.character()
  team <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(3)") %>% html_text()%>%.[-1]%>%str_sub(9,100)%>%as.character()
  avg_score <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(4)") %>% html_text()%>%.[-1]
  total_score <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(5)") %>% html_text()%>%.[-1]
  total_shoot <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(6)") %>% html_text()%>%.[-1]
  three_point <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(7)") %>% html_text()%>%.[-1]
  punish_point <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(8)") %>% html_text()%>%.[-1]
  avg_time <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(9)") %>% html_text()%>%.[-1]
  total_involve <- url %>% html_session() %>% html_nodes("table") %>% .[[2]] %>% html_nodes("td:nth-child(10)") %>% html_text()%>%.[-1]
  data.frame(rank,player,team,avg_score,total_score,total_shoot,three_point,punish_point,avg_time,total_involve)
}
NBAdata <- data.frame()
for(i in 1:6){
  NBAdata <- rbind(NBAdata,getdata(i))
}
NBAdata <- sqldf("select distinct * from NBAdata")
write.table(NBAdata,"NBAdata.csv",sep=",",fileEncoding="GB2312")


a1r <- head(warpbreaks)
a1r
a1s <- sqldf("select * from warpbreaks limit 6")
a1s
identical(a1r, a1s)
