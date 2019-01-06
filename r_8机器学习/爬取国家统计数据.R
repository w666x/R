setwd("E:/Rexercise1/数据挖掘实验数据")
###网址切割
url1="http://www.stats.gov.cn/was5/web/search?page=1&channelid=250710&was_custom_expr=like%28Average+Price+of+Food+in+50+Cities%29%2Fsen&perpage=10&outlinepage=26"
url2=strsplit(url1,"&")
url2
a=strsplit(url2[[1]][1],"=")
a
###批量下载
##构造批量下载的网址
web=0
for (i in 1:26)
  web[i]=paste(paste(a[[1]][1],i,sep = "="),url2[[1]][2],url2[[1]][3],url2[[1]][4],url2[[1]][5],sep = "&")
web


library(RCurl)
library(XML)
###提取所有的网址信息
#设置报头，重新尝试读取
statistic=function(web,I=10){
  myheader <- c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
#NULL1=0
for (i in 1:I)
{
  url1=web[i]
  temp2=getURL(url1,encoding="utf-8",header=myheader)
  ###爬取对应网页的数据连接
  k=strsplit(temp2,"\r\n")
  #write.table(k,file = "k.txt")
  k=k[[1]]
  webs=k[grep("var urlstr =",k)]
  webs=unique(webs)
  webs  ##查询网页的网址爬虫的结果
  webs1=substring(webs,22,93)
  webs1 ##从爬虫结果中提取出网址
  write.table(webs,file = "sum_webs.txt",append = T,col.names = F)
  write.table(webs2,file="webs.txt",append = T,col.names =paste("webs of page",i,sep = "-"))
  webs2=strsplit(webs,"'")
  webs2
  for (j in 1:10){
    url2=webs2[[j]][2]  #可以考虑通过这种方式读取网页信息，但是，数据不知如何甄别
    #url2=webs1[j]         #数据容易甄别，不符合要求的数据，对应的网址切割的根本不存在
    if (url.exists(url2)==F)  ##避免非价格数据进入数据集
      print(paste(i,j,sep = "-"))  ##标记出异常网址的地址
    else{
      ###提取数据
      temp=getURL(url2,followlocation=T)
      temp1=iconv(temp,"","UTF-8") #转码
      Encoding(temp1) #UTF-8
      doc <- htmlParse(temp1,asText=T,encoding="UTF-8") #选择UTF-8进行网页的解析
      #查看doc的内容时显示有乱码，但没关系，table的解析结果没有乱码
      tables <-readHTMLTable(doc,header=T)
      write.table(tables[[1]],"temp1.txt")
      ##保存数据并做处理
      #write.csv(tables,"temp1.csv")
      temp1=read.table("temp1.txt")
      if (ncol(temp1)>10)  ##避免非目标数据进入数据集
        print(paste(i,j,sep = "-"))
      else{
        #head(temp1)
        #tail(temp1)
        dim(temp1)
        temp1=temp1[2:28,1:6]
        temp1=t(temp1)
        head(temp1)
        a1=substring(webs[j],67,82)###提取数据的行名，列名
        #a11=strsplit(webs,"\'")[[2]][2]  ##读取网址
        a2=substring(a1,2,7)
        a3=substring(a1,11,16) #给数据列命名
        a=cbind(a1,a2,a3)
        #chartr("NULL.","time_",rownames(temp1))  ##从所得的原始数据中分隔出行名
        coln=strsplit(rownames(temp1),"NULL.")  ##对原始行名进行切割
        coln1=c(0)
        for (k in 1:6) ##提取原始数据行名
          coln1[k]=coln[[k]][2]
        rownames(temp1)=c(paste(a3,coln1,sep = "_"))
        temp1=temp1[c(4:6),]  ##仅仅保留数据，产品名称单位舍去
        if (ncol(temp1)!=27) #避免多余的数据项污染数据集
          print(paste(i,j,sep = "-"))
        else
          write.table(temp1,file = "mydata.txt",append = T,col.names =F)  #将所有数据都保存到mydata中
        temp1=0 ##归零，避免影响前面的判断
      }
      }
  }
}
}


###开始爬取数据
statistic(web,26)
#[1] "1-2"   输出项即为数据爬虫失败的网址
#[1] "17-8"
#[1] "25-4"
#[1] "25-8"
#[1] "25-9"
###查看爬取的数据集，并整理排序
NULL1
mydata1=read.table("mydata.txt",header = F)
head(mydata1)
dim(mydata1)
write.csv(mydata1,file = "mydata.csv")
###
head(rownames(mydata1))
head(mydata1$V1)
dim(unique(mydata1))
length(mydata1$V1)
length(unique(mydata1$V1))
##提取数据
mydata1=as.matrix(mydata1) 
##提取原始价格
mydata2=matrix(0,250,28) #保存原始价格在mydata2中
mydata3=matrix(0,250,28) #保留比上期变动的价格
mydata4=matrix(0,250,28) #保留变动比率的数值
for (i in 1:250){
  mydata2[i,]=mydata1[3*i-2,]
  mydata3[i,]=mydata1[3*i-1,]
  mydata4[i,]=mydata1[3*i,]
}
head(mydata4);head(mydata2);tail(mydata3)
###保存数据文件到txt文件中
mydata5=list(0,mydata2,mydata3,mydata4)
for (i in 2:4){
  name=c(1,"原始价格","原始价格变动","原始价格变动比率")
  write.table(mydata5[[i]],file =paste(name[i],"数据文件.txt",sep = ""),row.names = F)
}




###给文件赋予合适的行名和列名
##读取文件
for (i in 2:4){
  name=c(1,"原始价格","原始价格变动","原始价格变动比率")
  mydata5[[i]]=read.table(paste(name[i],"数据文件.txt",sep = ""))
}
head(mydata4);head(mydata2);tail(mydata3)
##更改上述数据的行名和列名
mydata2=mydata2[-1,]
c2=mydata2[,1];head(c2)
c21=substring(c2,1,6) #获取了个数据的行名
mydata2[,1]=c21 ##改变第一列的数据名称
mydata21=0  
####将数据按时间的顺序排序
mydata21=mydata2[order(mydata2[,1]),]  
head(mydata21)
namemydata21=mydata21[,1] ##将数据的第一行单独提取出来，并赋值为行名
mydata212=mydata21[,-1]
row.names(mydata212)=namemydata21
head(mydata212)
colname=paste(a,a1,sep = "_")
colname=colname[2:28]
colnames(mydata212)=colname
write.csv(mydata212,file = "各年份原始价格.csv")

###相应的，对价格的变动以及变动比率作排序以及行列的名的处理
mydata31=mydata3[order(c21),] #按提取的年份的先后排序
mydata312=mydata31[,-1]
row.names(mydata312)=namemydata21
colnames(mydata312)=colname
write.csv(mydata312,file = "各年份价格变动.csv")
mydata41=mydata4[order(c21),] #按提取的年份的先后排序
mydata412=mydata41[,-1]
dim(mydata412)
row.names(mydata412)=namemydata21
colnames(mydata412)=colname
write.csv(mydata412,file = "各年份价格变动率.csv")




###获取列名以及行名，为上面的操作作准备
url2="http://www.stats.gov.cn/english/PressRelease/201206/t20120604_26680.html"
temp=getURL(url2,followlocation=T)
temp1=iconv(temp,"","UTF-8") #转码
Encoding(temp1) #UTF-8
doc <- htmlParse(temp1,asText=T,encoding="UTF-8") #选择UTF-8进行网页的解析
#查看doc的内容时显示有乱码，但没关系，table的解析结果没有乱码
tables <-readHTMLTable(doc,header=T)
#tables[[1]]
class(tables[[1]]);class(tables)
write.table(tables[[1]],"temp1.txt")
head(tables)
a=as.character(tables[[1]]$Items  )##提取各产品的名字
a1=as.character(tables[[1]]$`Specification Level`) ##提出各产品的属性
a2=as.character(tables[[1]]$Units) ##提出各产品的计量单位
a3=colnames(tables[[1]]) ##提出原始变革的列名
suma=cbind(a,a1,a2)
write.table(suma,file = "原始表格的各种属性名.txt",row.names = F,na="NA")
a3
