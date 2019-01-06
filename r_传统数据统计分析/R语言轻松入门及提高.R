###R语言轻松入门及提高
###数据保存
yourname=c(1:10)
getwd()
save(yourname,file = "yourname.rda")
yourname
rm(yourname)
ls()
load("yourname.rda")
yourname
getRversion()
R.Version()
cat('If you doubt whether if works,
    just try it out.')
install.packages("fortunes")
library("fortunes")
help("paste")
example("paste")
help.search("date")
?data
?Date
?Syntax
?"DateTimeClasses"
RSiteSearch("cluster analysis") #R查找
