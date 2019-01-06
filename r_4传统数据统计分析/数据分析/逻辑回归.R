getwd()
setwd("E:/Rexercise1")
###表格化数据的逻辑回归
###no.yes为指定的生成因子的水平的名称
no.yes=c("no","yes")
somking=gl(2,1,8,no.yes)
obesity=gl(2,2,8,no.yes)
snoring=gl(2,4,8,no.yes)
n.tot=c(60,17,8,2,187,85,51,23)
n.hyp=c(5,2,1,0,35,13,15,8)
data.frame(somking,obesity,snoring,n.tot,n.hyp)
###生成数据框
expand.grid(somking=no.yes,obesity=no.yes,snoring=no.yes)
###数据处理,第二项为总的人数减去患病的人数
hyp.tbl=cbind(n.hyp,n.tot-n.hyp)
hyp.tbl
###逻辑回归模型,其中默认的即是logit作为连接函数
glm(hyp.tbl~somking+obesity+snoring,family = binomial("logit"))
glm(hyp.tbl~somking+obesity+snoring,binomial)
###逻辑回归模型二
prop.hyp=n.hyp/n.tot
prop.hyp
glm.hyp=glm(prop.hyp~somking+obesity+snoring,binomial,weights = n.tot)
summary(glm.hyp)
summary(glm(hyp.tbl~somking+obesity+snoring,binomial))
###输出参数的相关系系数
summary(glm(hyp.tbl~somking+obesity+snoring,binomial),corr=T)
###模型调整，去掉一项后再做逻辑回归
glm.hyp=glm(hyp.tbl~obesity+snoring,binomial)
summary(glm.hyp)

###偏差表分析(确定判别准则之后，就有相应的p值输出)
glm.hyp=glm(hyp.tbl~somking+obesity+snoring,binomial)
anova(glm.hyp,test = "Chisq")
###调整
glm.hyp=glm(hyp.tbl~snoring+obesity+somking,binomial)
anova(glm.hyp,test = "Chisq")
###调整
glm.hyp=glm(hyp.tbl~obesity+snoring,binomial)
anova(glm.hyp,test = "Chisq")

###分步求解，应用drop1函数
drop1(glm.hyp,test = "Chisq")

###与趋势检验之间的关联
caesar=ISwR::caesar.shoe
head(caesar)
dim(caesar)
shoe.score=1:6
shoe.score
summary(glm(t(caesar)~shoe.score,binomial))
anova(glm(t(caesar)~shoe.score,binomial))

###标准检验分析
caesar.yes=caesar["Yes",]
caesar.no=caesar["No",]
caesar.total=caesar.yes+caesar.no
prop.trend.test(caesar.yes,caesar.total)
prop.test(caesar.yes,caesar.total)

###似然剖面分析
###求出参数的置信区间
confint(glm.hyp)
###得到参数的标准置信区间
confint.default(glm.hyp)
###作出剖面图,显示参数的结果
library(MASS)
plot(profile(glm.hyp))

###让步比估计的表达
exp(cbind(OR=coef(glm.hyp),confint(glm.hyp)))

###原始数据的逻辑回归
###将描述分组信息的变量转化为因子
juul=ISwR::juul
dim(juul)
juul$menarche=factor(juul$menarche,labels = c("no","yes"))
juul$tanner=factor(juul$tanner)
juul.girl=subset(juul,age>8 & age<20 &
                   complete.cases(menarche))
attach(juul.girl)
length(juul.girl)
summary(glm(menarche~age,binomial))
summary(glm(menarche~age+tanner,binomial))
drop1(glm(menarche~age+tanner,binomial),test = "Chisq")

##预测
predict(glm.hyp)
predict(glm.hyp,type = "response")
###绘图
plot(age,fitted(glm(menarche~age,binomial)))
glm.menarche=glm(menarche~age,binomial)
age=seq(8,20,.1)
newages=data.frame(age=age)
predicted.probability=predict(glm.menarche,newages,type = "resp")
plot(predicted.probability~age,type="l")
rm(age)
###模型检查
fitted(glm.hyp)
prop.hyp
fitted(glm.hyp)*n.tot
data.frame(fit=fitted(glm.hyp)*n.tot,n.hyp,n.tot)

age.group=cut(age,c(8,10,12,13,14,15,16,18,20))
tb=table(age.group,menarche)
tb
###绘出观测数量占比与期望概率的图
rel.frep=prop.table(tb,1)[,2]
rel.frep
points(rel.frep~c(9,11,12.5,13.5,14.5,15.5,17,19),pch=5)
head(menarche)
tail(menarche)
length(menarche)
length(age.group)
length(age)
###处理二
age.gr=cut(age,c(8,12,13,14,20))
summary(glm(menarche~age+age.gr,binomial))
anova(glm(menarche~age+I(age^2)+I(age^3)+age.gr,binomial))
glm.menarche=glm(menarche~age+I(age^2)+I(age^3)+age.gr,binomial)
glm.menarche
predicted.probability=predict(glm.menarche,newages,type = "resp")
plot(predicted.probability~age,type="l")
points(rel.frep~c(9,11,12.5,13.5,14.5,15.5,17,19),pch=5)
detach()
