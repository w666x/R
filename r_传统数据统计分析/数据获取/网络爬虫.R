##网络爬虫
library(RCurl)
##判断URL是否存在
url.exists()
url.exists("http://www.baidu.com")
url.exists("ftp.djhj.do")
url.exists("ftp://210.42.151.222")
url.exists("ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/")

###消息报头
##利用getURL查看相关信息
#debugfunction设置为d$update故可更新叠加
d = debugGatherer()
temp <- getURL("http://www.dataguru.cn/",debugfunction=d$update,
               verbose =T)
cat(d$value()[3])#提交给服务器的头信息
cat(d$value()[1])#服务器地址以及端口号
cat(d$value()[2])#服务器端返回的头信息
d
d$reset()
d
d$value
d$value()

##查看服务器端返回的头信息
##字符串形式展示
headers = basicTextGatherer()
txt=getURL("http://www.dataguru.cn/",
           headerfunction = headers$update)
names(headers$value())#说明是字符串形式
headers$value()

##查看服务器端返回的头信息
 ###列表形式
h = basicHeaderGatherer()
txtt=getURL("http://www.dataguru.cn/",headerfunction = h$update)
names(h$value())
h$value()

##查看curl请求的访问信息
curl = getCurlHandle()
d=getURL("http://www.dataguru.cn/", curl = curl)
getCurlInfo(curl)$response.code #返回状态码
getCurlInfo(curl) #返回详细信息

##武装自己的header
myheader <- c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
#使用自己已经改过的header来进入服务器
#temp=getURL("http://www.baidu.com",httpheader=myheader)
temp=getURL(url,httpheader=myheader)
#查看是否伪装成功
d=debugGatherer()
temp=getURL("http://www.baidu.com",verbose=T,
            httpheader=myheader,debugfunction=d$update)
cat(d$value()[3])

##列出curl的参数,看可以改变那些参数
listCurlOptions()

##getform
#探索搜索原理
#提取关键字符
#替换成新的搜索
#首先可以复制该网页的链接
#然后，getformparams读取
#其次，getform链接
#https://www.so.com/s?q=Rcurl&src=srp&fr=se7_newtab_new&psid=90a2a0df57fb3fe7b82fb2213420112d
chart=c("https://www.baidu.com/s?wd=RCurl&rsv_spt=1&rsv_iqid=0xb4e957930000a56a&issp=1&f=8&rsv_bp=0&rsv_idx=2&ie=utf-8&tn=baiduhome_pg&rsv_enter=1&rsv_sug3=5&rsv_sug1=4&rsv_sug7=100&rsv_t=5108T8b1GQlScSt5ppHLznvi2xUIemV7wCzbnjRF82LZaWOfQNhoM7Vqb%2FlR13mzy3c%2B&rsv_sug2=0&inputT=4087&rsv_sug4=4087")
getFormParams(chart)
#此处改变wd的名，即可改变链接对象
#getform是浏览器和服务器的交互
url=getForm("http://www.baidu.com/s",wd="团购电影",
            rsv_spt='1',rsv_iqid="0xb4e957930000a56a",
            issp="1",f="8",rsv_bp="0",rsv_idx="2",
            ie="utf-8",tn="baiduhome_pg",rsv_enter="1",
            rsv_sug3="5",rsv_sug1="4",rsv_sug7="100",
            rsv_t="5108T8b1GQlScSt5ppHLznvi2xUIemV7wCzbnjRF82LZaWOfQNhoM7Vqb%2FlR13mzy3c%2B",
            rsv_sug2="0",inputT="4087",rsv_sug4="4087")
getFormParams(url)
write.table(url,"url.txt")

##postform

##第二周
##curl部分参数的设置
#verbose;httpheader;encoding;headerfunction;params;dirlistonly;
#followocation;maxredirs

##仅读取ftp中目录的操作示例
##此处的操作失败
url="ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/"
filename=getURL(url,dirlistonly=T)
filename
#支持重定向，即当网址输入错误时，导向类似网址
curl=getCurlHandle()
destination=getURL("http://www.sina.com",curl = curl,
                   followlocation=F) #即页面无法访问成功
destination=getURL("http://www.sina.com",curl = curl,
                   followlocation=T) #访问成功
getCurlInfo(curl)$response.code
destination ##即获得了网页信息

#getbinaryurl下载文件
#下载表格，成功了
url="http://sd.offcn.com/dl/2014/0715/20140715092308897.xls"
temp<- getBinaryURL(url)
note <- file("hellodata.xls", open = "wb")
writeBin(temp,note)
close(note)


#下载表格
#失败了啊
url="ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/"
wp <- getURL(url)
doc <- htmlParse(wp, asText = TRUE)
tables <- readHTMLTable(doc)


##批量下载
#http://rfunction.com/code/1202/
#需要学会一下函数的使用

#系统切割，切割某一个函数
#切割函数的应用实例
strsplit
Sys.time() #显示系统时间
class(Sys.time())
#引号中的东西确定切割
strsplit(as.character(Sys.time()),' ')
strsplit(as.character(Sys.time()),'-')#引号中的东西确定切割
strsplit(as.character(Sys.time()),'')
strsplit(as.character(Sys.time()),'-')[2]
#是输出结果不呈现list形式 
unlist(strsplit(as.character(Sys.time()),'-'))[1]
destination
strsplit(destination,'\r\n')
##当需要提取某一个特殊部分时，就以unlist将其逃脱list
##的限制，就可以提取了

##lapply
##paste
#首先获取网页的全部源代码
html=getURL("http://rfunction.com/code/1202/")
html
#切割一
temp=strsplit(html,'<li><a href="')
##由于此处是以列表的形式展现，所以，应该取其第一项
temp=strsplit(html,"<li><a href=\"")[[1]]
temp
#切割二
files=strsplit(temp,"\"")
files

#提取关键部分
files=lapply(files, function(x){x[1]})
files
#去除结果的list格式，
files1=unlist(files)
files2=unlist(files)[c(-1,-2)]
files2

#使用for循环，开始批量下载
base="http://rfunction.com/code/1202/"
i=1
for (i in 1:length(files2)){
  #构建url，使用paste语句
  url=paste(base,files2[i],sep = "")
  #构建下载的语句，接下来的四句都是的啊
  temp<- getBinaryURL(url)
  #定义不同的文件名，使其不被覆盖
  note <- file(paste("1202",files2[i],sep = "."), open = "wb")
  writeBin(temp,note)
  close(note)
  Sys.sleep(2) #设置休息时间，缓冲一下
}

##XML包及其应用
##表格的下载
library(XML)
# http://www.bioguo.org/AnimalTFDB/BrowseAllTF.php?spe=Mus_musculus
url="http://www.bioguo.org/AnimalTFDB/BrowseAllTF.php?spe=Mus_musculus"
wp <- getURL(url) ##访问该网页
doc <- htmlParse(wp, asText = TRUE)
tables <- readHTMLTable(doc)  #因为输出结果为列表形式，故取其部分，保存为csv文件
write.csv(tables$table1,"table.csv")
head(read.csv("table.csv",header = T))

##抓取地震数据/中文数据Windows下有影响
# http://data.earthquake.cn/datashare/datashare_more_quickdata_new.jsp
url="http://data.earthquake.cn/datashare/datashare_more_quickdata_new.jsp"
wp=getURL(url)
doc <- htmlParse(wp, asText = TRUE)
tables <- readHTMLTable(doc)
tables <- readHTMLTable(doc,header = T)
tables

###抓取地震数据/英文数据
# http://219.143.71.11/wdc4seis@bj/earthquakes/csn_quakes_p001.jsp
url="http://219.143.71.11/wdc4seis@bj/earthquakes/csn_quakes_p001.jsp"
wp=getURL(url)
doc <- htmlParse(wp, asText = TRUE)
tables <- readHTMLTable(doc)
tables <- readHTMLTable(doc,header =F)
tables[2]

##XPATH  路径
##如何获取路径，即结点
url="http://www.w3school.com.cn/example/xmle/books.xml"
doc=xmlParse(url)
#解析,抓取信息，利用结点抓取
getNodeSet(doc,'/bookstore/book[1]')
getNodeSet(doc,'/bookstore/book[last()]')
getNodeSet(doc,'/bookstore/book[position()<3]')
##利用属性抓取,全文抓取，符合lang的
getNodeSet(doc,'//title[@lang]')
getNodeSet(doc,'//book/price')
#抓取多个并列的数据,用|抓取
getNodeSet(doc,'//title[@lang]|//book/price')

##实例一，抓取电影团购
url="http://t.dianping.com/list/guangzhou?q=%E7%94%B5%E5%BD%B1"
temp=getURL(url,followlocation=T)
temp
temp=getURL("http://t.dianping.com/guangzhou?q=%E7%94%B5%E5%BD%B1",encoding="UTF-8",followlocation=T)
temp
#设置报头，重新尝试读取
myheader <- c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
temp=getURL("http://t.dianping.com/guangzhou?q=%E7%94%B5%E5%BD%B1",httpheader=myheader,encoding="UTF-8")
temp
write.table(temp,"temp.txt")#输出后，查看读取情况

##提取路径
x=htmlParse(temp) ##解析
getNodeSet(x,"\n")
youhui=sapply(getNodeSet(x,"\n"),xmlvalue)


##正则表达式抓取，，，较rpath高效
#查看字符串的长度
a="abcd"
list=c("adbd","dd","abc")
nchar(a)
length(a)
#字符串的替换
chartr("a","b",a)
#字符串的连接
paste("data","a")
paste("data","a",sep = "")
paste(list,collapse = "-")
#切割
substring(a,1,4)
substring(a,1,3)
substr(a,1,3)
substr(list,c(1,2),3)
substring(list,c(1,2),3)
#比较
list[1]>list[2]
list
list[2]>list[1]
#交集，并集，补集
union(a,list) #并集
intersect(a,list) #交集
setdiff(list,a) #补集
setdiff(a,list)
#匹配
match(a,list)
match("adbd",list) #匹配字符串
pmatch("ad",list)

##正则表达式，应用对象为文本
#这是定义的一个正则表达式
pattern="^[A-Za-z0-9\\._%+-]+@[A-Za-z0-9\\.-]+\\.[A-Za-z]{2,4}$"
sunsi@163.com
pattern1="\\w+a\\w+\\.[A-Za-z]{2,4}"

#查询返回值
list=c("sunshine@163.com","hujin","dasf.xon")
grepl(pattern,list) #返回逻辑值
grep(pattern,list) #返回位置
#替换值
paste1=paste(list,collapse = ",")
paste1
sub(pattern1,"data",paste1)  #在paste1中，符合pattern的替换为data
gsub(pattern1,"data",paste1) #全局性的替换
#返回某别属性
regexpr(pattern,list)#返回是否匹配以及匹配的长度
regexec(pattern,list)#返回一个list,返回匹配的位置
gregexpr(pattern1,pattern1)


##实例
##正则表达式，抓取信息
library(RCurl)
url="http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/601988.phtml?year=2015&jidu=3"
temp=getURL(url)
temp
##首先将输出的网页简单的以\r\n切割
k=strsplit(temp,"\r\n")
k   #此处的k是列表形'式的
write.table(k,file = "k.txt")
k[2]
k[[1]][2]
k=k[[1]] #提取列表的第一项
##以<a target='_blank'切割网页
time=k[grep("<a target='_blank'",k)]
time

##抓取时间
##提取日期
time=k[grep("<a target='_blank'",k)+1]
time
##进一步substring提取数字
time=substring(time,4,13)
time
##函数替换，将“-”改为“.”

##抓取开盘价
fpriceadr=k[grep("<a target='_blank'",k)+3]
fpriceadr
##进一步提取数字,通过匹配定位
fprice=gregexpr(">\\d+",fpriceadr)
for (i in 1:length(fpriceadr))
  temp=fprice[[1]]
temp
fprice=substring(fpriceadr[1],temp+1,temp+attr(temp,"match.length")+3)
fprice

###定义url，批量下载


