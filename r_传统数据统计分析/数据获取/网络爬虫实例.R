##实例
##正则表达式，抓取信息
library(RCurl)
url="http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/601988.phtml?year=2015&jidu=3"
temp=getURL(url)
temp
##首先将输出的网页简单的以\r\n切割
k=strsplit(temp,"\r\n")
k   #此处的k是列表形式的
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




###腾讯财经下载

