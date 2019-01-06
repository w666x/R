###生成表格
caff.matrix=matrix(c(1,2,3,4,5,6,7,8,9),3)
caff.matrix
rownames(caff.matrix)=c("hello","hi","go")
colnames(caff.matrix)=c("x","y","z")
caff.matrix
names(dimnames(caff.matrix))=c("marital","consumption")
caff.matrix
as.table(caff.matrix)
as.data.frame(caff.matrix)
as.data.frame(as.table(caff.matrix))
###数据处理的高级技术
a=c(1:20)
DIS=subset(a,a>10 & a<15)
DIS
range(DIS)
table(DIS)
DIS=cut(a,breaks=c(seq(1,21,5)),right = F,include.lowest = F)
table(DIS)
##数据等距分组
q=quantile(a)
q
aQ=cut(a,q,include.lowest = F)
table(aQ)
##对之前生成的因子水平进行修改
levels(aQ)=c("1st","2nd","3rd","4th")
###处理因子
pain=c(0,3,2,2,1)
fpain=factor(pain,levels = 0:3,labels = c("none","mild","medium","severe"))
fpain
table(fpain)
text.pain=c("none","mild","medium","severe")
factor(text.pain)
###对因子水平的顺讯进行重排
ftpain=factor(text.pain)
ftpain2=factor(ftpain,levels =c("none","mild","medium","severe") )
ftpain2
ftpain
##因子合并,如此操作是为了避免失误时不污染原始数据
ftpain3=ftpain2
levels(ftpain3)=list(none="none",
                     intermediate=c("mild","medium"),
                     severe="severe")
ftpain3
table(ftpain3)
##因子合并2
ftpain4=ftpain2
levels(ftpain4)=c("none","none","medium","medium")
table(ftpain4)

###日期的使用
install.packages("ISwR")
###数据的提取，rawdata为文件夹，
stroke=read.csv(system.file("rawdata","stroke.csv",package = "ISwR"),na.strings = "")
write.table(stroke,file="stroke.txt")
write.csv(strok1,file = "stroke1.csv")
stroke1=read.csv(file = "stroke1.csv")
names(stroke1)
head(stroke)
names(stroke1)=tolower(names(stroke1))
head(stroke1)
strok1=edit(stroke)
head(strok1)
###数据转换，使得年份可以加减
attach(stroke)
stroke1=transform(stroke1,
                 died=as.Date(x.died., format="%d.%m.%Y"),
                 dstr=as.Date(x..dstr.., format="%d.%m.%Y"))
detach()
summary(stroke1$died)
summary(stroke1$dstr-stroke1$died)
summary(stroke1$died-stroke1$dstr)
###数据处理
stroke=transform(stroke1,
                 end=pmin(died,as.Date("1996-1-1"),na.rm = T),
                 dead=!is.na(died) & died< as.Date("1996-1-1"))
head(stroke)
###检查数据处理是否得当，即是否有错误，如下
stroke=transform(stroke,
                 obstime=as.numeric(end-dstr,units="days")/365)
head(stroke)

###多变量重编码，由于有时需对若干的变量作同样的数据转换
help("read.csv2")
###直接用lapply将多个数据列进行转换
###将时间变量处理，变换日期的格式
rawstroke=read.csv2(system.file("rawdata","stroke.csv",package = "ISwR"),
                    na.strings = ".")
head(rawstroke)
ix=c("DSTR","DIED")
rawstroke[ix]=lapply(rawstroke[ix], as.Date,format="%d.%m.%Y")
head(rawstroke)
###将二进制变量转换成“yes/no”
ix=6:9
rawstroke[ix]=lapply(rawstroke[ix], 
                     factor,levels=0:1,labels=c("No","Yes"))
head(rawstroke)
###条件计算
strokesub=ISwR::stroke[1:10,2:3]
strokesub
###需计算存活模型需要的研究时间和事件/检查指数，
strokesub=transform(strokesub,
                    event=!is.na(died))
strokesub
strokesub=transform(strokesub,
                    obstime=ifelse(event,died-dstr,as.Date("1996-1-1")-dstr))
strokesub

###合并和重构数据框
###追加数据框
juul=ISwR::juul
head(juul)
tail(juul)
juulgrl=subset(juul,sex==2,select=-c(testvol,sex))
juulboy=subset(juul,sex==1,select = -c(menarche,sex))
head(juulboy)
head(juulgrl)
juulgrl$sex=factor("F");juulgrl$testvol=NA
juulboy$sex=factor("M");juulboy$menarche=NA
juulall=rbind(juulboy,juulgrl)
head(juulall)
tail(juulall)
levels(juulall$sex)
###合并数据框
nickel=ISwR::nickel
ewrates=ISwR::ewrates
head(nickel)
tail(ewrates)
nickel=transform(nickel,
                 agr=trunc(agein/5)*5,
                 ygr=trunc((dob+agein-1)/5)*5+1)
mrg=merge(nickel,ewrates,
          by.x=c("agr","ygr"),by.y = c("age","year"))
mrg1=merge(nickel,ewrates)
head(mrg)

###重塑数据框
head(ISwR::alkfos)
alkfos=ISwR::alkfos
head(alkfos)
a2=alkfos
dim(a2)
###sub函数在字符串内作替换操作
names(a2)=sub("c","c.",names(a2))
names(a2)
a.long=reshape(a2,varying = 2:8,direction = "long")
head(a.long)
dim(a.long)
###确定序列排序的序列号
o=with(a.long,order(id,time))
head(a.long[o,],10)
###重塑数据框二
a.long2=na.omit(a.long)
dim(a.long2)
attr(a.long2,"reshaping")==NULL
a.wide=reshape(a.long2,direction = "wide",
               v.names = "c",idvar = "id",timevar = "time")
head(a.wide)

###数据分组及案例操作
head(a.long)
l=split(a.long$c,a.long$id)
l[1:3]
l2=lapply(1, function(x) x / x[1] )
a.long$c.adj=unsplit(l2,a.long$id)
subset(a.long,id==1)
###数据分隔重组的函数
a.long$c.adj=ave(a.long$c,a.long$id,FUN=function(x) x/x[1])
head(a.long)
l=split(a.long,a.long$id)
l2=lapply(1,transform, c.adj=c/ c[l])
a.long2=unsplit(l2,a.long$id)
head(a.long2)

###时间分隔
head(nickel)
subset(nickel,id==4)
###将调查时间强制规定在60到65岁之间
entry=pmax(nickel$agein,60)
exit=pmin(nickel$ageout,65)
entry[1:10]
exit[1:10]
min(entry)
max(entry)
###找出活过65岁以及未活过60岁的观测
valid=(entry<exit)
entyr=entry[valid]
exit=exit[valid]
length(entry)
length(exit)
dim(nickel)
###判断方式二
cens=(nickel$ageout[valid] > 65)
###判断完毕后，对数据框数据进行切割
nickel60=nickel[valid,]
nickel60$icd[cens]=0 ###上述判断后，将icd设置为0
nickel60$agein=entry
nickel60$ageout=exit
nickel60$agr=60
nickel60$ygr=with(nickel60,trunc((dob+agein-1)/5)*5+1)
head(nickel60)
head(ISwR::nickel.expand)
