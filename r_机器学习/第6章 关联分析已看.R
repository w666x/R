##关联分析（支持度，置信度，提升度。控制关联强度）
##啤酒和尿布的故事
##从大量数据中发现项集之间的有趣关联或相互关系
#install.packages("arules")
library ( arules )  
#arules包：关联规则的数字化生成
data("Groceries")                                
summary(Groceries) 
inspect(Groceries[1:10]) #观测前十条交易                       

##通过支持度，置信度共同控制
#数据初探：生成规则0
rules0=apriori(Groceries,parameter=list(support=0.001,confidence=0.5))
rules0                                           
inspect(rules0[1:10]) #展示规则0中的前十条规则                       
#对生成的规则进行强度控制，支持度提高为0.005
rules1=apriori(Groceries,parameter=list(support=0.005,confidence=0.5))
rules1             
rules2=apriori(Groceries,parameter=list(support=0.005,confidence=0.6))
rules2 
rules3=apriori(Groceries,parameter=list(support=0.005,confidence=0.64))
rules3
inspect(rules3) #展示4条规则
 
##通过支持度控制    
#首先，将规则按支持度排列，然后取其前五行
rules.sorted_sup = sort ( rules0, by="support" )   
inspect ( rules.sorted_sup [1:5] )   
#通过置信度控制
#首先，将规则按置信度排列，然后取其前五行
rules.sorted_con = sort ( rules0, by="confidence" )   
inspect ( rules.sorted_con [1:5] )
#通过提升度控制
#首先，将规则按提升度排列，然后取其前五行
rules.sorted_lift = sort ( rules0, by="lift" )   
inspect ( rules.sorted_lift [1:5] ) 
#实际应用
#输出关联规则含有，mustard的项，且该项中仅包括两类
rules4=apriori(Groceries,parameter=list(maxlen=2,supp=0.001,conf=0.1),appearance=list(rhs="mustard",default="lhs"))
inspect ( rules4 )  
   
##改变输出结果的形式 
#输出频繁项集，且降序排列
itemsets_apr = apriori ( Groceries, parameter = list (supp=0.001,target = "frequent itemsets"),control=list(sort=-1)) 
itemsets_apr                                         
inspect(itemsets_apr[1:5])    
#输出最适合捆绑销售的五对商品
##降序排列，频繁项集大小为1-3，输出频繁项集
itemsets_ecl = eclat( Groceries, parameter = list ( minlen=1, maxlen=3,supp=0.001, target = "frequent itemsets"),control=list(sort=-1)) 
itemsets_ecl 
inspect(itemsets_ecl[1:5])

###关联规则的可视化
library ( arulesViz );library ( MASS );library ( scatterplot3d );library ( vcd );library ( grid )
library ( colorspace );library ( seriation );library ( cluster );library ( TSP );library ( gclus )
#生成关联规则五
rules5 = apriori ( Groceries, parameter = list ( support=0.002, confidence=0.5 ) )       
rules5    
#对关联规则5作图                                          
plot(rules5)                                      
plot(rules5,measure=c("support","lift"),shading=
       "confidence")
plot(rules5,method="grouped") 
plot(rules5,interactive=TRUE) #绘制互动散点图             
plot(rules5,shading="order", control=list(main = "Two‐key plot"))
plot(rules5[1:50], method="matrix", measure="lift")
plot(rules5[1:50], method="matrix3D", measure="lift")
plot(rules5[1:50], method="paracoord")




















