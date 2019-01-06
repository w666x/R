###方差分析
##单因素方差分析
#判断单因素的各个不同水平对结果是否有显著差异
X<-c(25.6, 22.2, 28.0, 29.8, 24.4, 30.0, 29.0, 27.5, 25.0, 27.7,
     23.0, 32.2, 28.8, 28.0, 31.5, 25.9, 20.6, 21.2, 22.0, 21.2)
A<-factor(rep(1:5, each=4)) #单因素的各个不同水平
miscellany<-data.frame(X, A)
miscellany
aov.mis<-aov(X~A, data=miscellany) #作方差分析
summary(aov.mis)
plot(miscellany$X~miscellany$A)



###均值的多重比较
#另外多重t检验也可以进行均值的多重比较，这是显而易见的嘛
##除了知道有某些均值彼此不同，还可以分析哪些均值是不同的
pairwise.t.test(X,A,p.adjust.method = "none")
pairwise.t.test(X,A,p.adjust.method = "holm")
pairwise.t.test(X,A,p.adjust.method = "bonferroni")


###同时置信区间法Tukey法
##对置信之差作出置信区间
sales<-data.frame(
  X=c(23, 19, 21, 13, 24, 25, 28, 27, 20, 18,
      19, 15, 22, 25, 26, 23, 24, 23, 26, 27),
  A=factor(rep(1:5, c(4, 4, 4, 4, 4)))
)
##首先作正常的单因素方差分析
summary(aov(X~A,sales))
##求均值差的同时置信区间
TukeyHSD(aov(X~A,sales))


###方差齐性检验(确定某因素的各水平下数据是否为等方差的)
bartlett.test(X~A,data = sales) #bartlett法作方差齐性检验
library(car)
levene.test(sales$X,sales$A) #Levene作方差齐性检验


###双因素方差分析
#两个因素是否对某一变量有显著性影响
juice<-data.frame(
  X = c(0.05, 0.46, 0.12, 0.16, 0.84, 1.30, 0.08, 0.38, 0.4,
        0.10, 0.92, 1.57, 0.11, 0.43, 0.05, 0.10, 0.94, 1.10,
        0.11, 0.44, 0.08, 0.03, 0.93, 1.15),
  A = gl(4, 6), #生成分组因子
  B = gl(6, 1, 24) #生成分组因子
)
class(juice$A)
juice.aov=aov(X~A+B,data = juice) #首先作双因素方差分析
summary(juice.aov)
###然后对因素分别作方差的齐性检验
bartlett.test(X~A,data = juice)
bartlett.test(X~B,data = juice)
###双因素方差分析二
rats<-data.frame(
  Time=c(0.31, 0.45, 0.46, 0.43, 0.82, 1.10, 0.88, 0.72, 0.43, 0.45,
         0.63, 0.76, 0.45, 0.71, 0.66, 0.62, 0.38, 0.29, 0.40, 0.23,
         0.92, 0.61, 0.49, 1.24, 0.44, 0.35, 0.31, 0.40, 0.56, 1.02,
         0.71, 0.38, 0.22, 0.21, 0.18, 0.23, 0.30, 0.37, 0.38, 0.29,
         0.23, 0.25, 0.24, 0.22, 0.30, 0.36, 0.31, 0.33),
  Toxicant=gl(3, 16, 48, labels = c("I", "II", "III")),
  Cure=gl(4, 4, 48, labels = c("A", "B", "C", "D"))
)
rats
op=par(mfrow=c(1,2)) #因为其都是因子，所以画出来的为箱形图
plot(Time~Toxicant+Cure,data = rats)
##interaction.plot作出交互效应图，检查因素之间的交互作用是否存在
with(rats,interaction.plot(Toxicant, Cure, Time, trace.label="Cure"))
with(rats,interaction.plot(Cure, Toxicant, Time, trace.label="Toxicant"))
##用双因素方差分析函数确认
rats.aov=aov(Time~Toxicant*Cure,data = rats)
summary(rats.aov)
##检验单个因子是否方差齐性
library(car)
levene.test(rats$Time,rats$Toxicant)
leveneTest(rats$Time,rats$Cure)
bartlett.test(Time~Toxicant,data = rats)
bartlett.test(Time~Cure,data = rats)


###协方差分析
##检验三种资料对猪的增肥效果是否影响
#此处应该还需要考虑猪的初始价格
feed<-rep(c("A","B","C"),each=8)
Weight_Initial <- c(15,13,11,12,12,16,14,17,17,16,
                    18,18,21,22,19,18,22,24,20,23,
                    25,27,30,32)
Weight_Increment <-c(85,83,65,76,80,91,84,90,97,90,
                     100,95,103,106,99,94,89,91,83,
                     95,100,102,105,110)
data.feed=data.frame(feed,Weight_Initial,Weight_Increment)
head(data.feed)
#作协方差分析
library(HH)
#若认为初始体重不同，但是增长速度相同
ancova(Weight_Increment~Weight_Initial+feed,data=data.feed)
#猪的初始体重和增长速度都不同
ancova(Weight_Increment~Weight_Initial*feed,
       data=data.feed)
