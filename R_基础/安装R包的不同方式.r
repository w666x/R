###安装R包的几种不同的方式
##githubinstall可以直接使用包名在github上安装
library(devtools)
#安装的时候，不仅需要包名，而且还需要仓库名
install_github("sinhrks4/ggfortify")
install.packages('githubinstall') #安装后可直接使用包名在github上安装
library(githubinstall)
githubinstall('AnomalyDetection')
githubinstall("mvstats")
gh_suggest("mvstats") #经给出包的地址
gh_suggest_username('hadly') #给出较详尽用户名
##列出某一个用户所开发的所有R包
yihuiverse=gh_list_packages(username = "yihui")
head(yihuiverse)
##安装他的所有R包
#repos=with(yihuiverse,paste(username,package_name,sep="/)) 
#通过关键词搜索R包
gh_search_packages("mvstats")
search=gh_search_packages("stats")
head(search)
##显示github上给定R函数的源代码
gh_show_source("mutate","dplyr")
library(dplyr) #第二种方式
gh_show_source(mutate)
gh_show_source(mvstats)
##更新R列表,以及更新所有R包
gh_update_package_list()
##查询R中已安装的软件包
dim(installed.packages())
installed.packages()[,1]
library() #也可以查询安装包的个数
