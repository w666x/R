##如何做商业信息的分析和展现

##数据获取，网络爬虫
######爬取的数据，中文都是乱码啊
#http://www.wfwj.gov.cn/
#http://www.stats.gov.cn/
#rcurl网络爬虫数据
#表格的下载
library(RCurl)
library(XML)
#http://www.stats.gov.cn/tjsj/zxfb/201604/t20160405_1339571.html
url="http://www.stats.gov.cn/tjsj/zxfb/201604/t20160405_1339571.html"
##首先判断url是否存在
url.exists("http://www.stats.gov.cn/tjsj/zxfb/201604/t20160405_1339571.html")
#####消息报头
##利用getURL查看相关信息
#debugfunction设置为d$update故可更新叠加
d = debugGatherer()
temp <- getURL(url,debugfunction=d$update,
               verbose =T)
cat(d$value()[3])#提交给服务器的头信息
cat(d$value()[1])#服务器地址以及端口号
cat(d$value()[2])#服务器端返回的头信息
##查看服务器端返回的头信息
##字符串形式展示
headers = basicHeaderGatherer()
txt=getURL(url,
           headerfunction = headers$update)
names(headers$value())#说明是字符串形式
headers$value()
##查看curl请求的访问信息
curl = getCurlHandle()
d=getURL(url, curl = curl)
getCurlInfo(curl)$response.code #返回状态码
getCurlInfo(curl) #返回详细信息

###下载表格
wp <- getURL(url,encoding="utf-8") ##访问该网页
#wp,
temp=getURL(url,httpheader=myheader,encoding="utf-8")
doc <- htmlParse(temp, asText = TRUE)
tables <- readHTMLTable(doc)
names(tables)
#tables
write.table(tables[[1]],"table.txt")
head(read.table("table.txt",header = T))

##表格的下载二
##应用的条件是纯表格，对于有其他注释内容的无法爬虫
url="http://www.stats.gov.cn/tjsj/zxfb/201604/t20160405_1339571.html"
temp<- getBinaryURL(url)
note <- file("table1.txt", open = "wb")
writeBin(temp,note)
close(note)

##表格的下载三：
##数据为英文格式
#http://www.stats.gov.cn/english/PressRelease/201604/t20160414_1343477.html
#http://www.stats.gov.cn/english/PressRelease/201604/t20160414_1343479.html
url="http://www.stats.gov.cn/english/PressRelease/201604/t20160414_1343479.html"
wp <- getURL(url,encoding="utf-8") ##访问该网页
#wp,
doc <- htmlParse(wp, asText = TRUE)
tables <- readHTMLTable(doc,header = T)
names(tables)
#tables
write.csv(tables[[1]],"table3.csv")


##网络爬虫二
library(RCurl)
library(XML)
#http://www.wfwj.gov.cn/wnewsView.jsp?id=40
url="http://www.wfwj.gov.cn/wnewsView.jsp?id=40"
wp <- getURL(url,encoding="utf-8") ##访问该网页
wp
doc <- htmlParse(wp, asText = TRUE,encoding="utf-8")
doc
tables <- readHTMLTable(doc)
names(tables)
#tables
write.table(tables[[1]],"table2.txt")
head(read.table("table.txt",header = T))

##数据清理，去重，去除异常值，等等

