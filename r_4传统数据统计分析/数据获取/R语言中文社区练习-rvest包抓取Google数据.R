##R语言中文社区练习-rvest包抓取Google数据
##展示的为该人的个人的学术数据
setwd("E:/Rexercise1")
##加载R包
library(rvest)
library(ggplot2)
##论文应用的次数
##使用selectorgadget的CSS浏览器找出“citid by”列
page=read_html("https://scholar.google.com/citations?
               user=sTR9SIQAAAAJ&hl=en&oi=ao")
##在HTML_nodes中指定CSS选择器，HTML_text用来提取文本
citations=page %>% html_nodes("#gsc_a_b,gsc_a_c") %>% 
  html_text() %>% as.numeric()
##查看应用次数，以及绘出引用次数的条形图
barplot(citations)

help("read_html")

# Literal xml/html is useful for small examples
read_xml("<foo><bar /></foo>")
read_html("<html><title>Hi<title></html>")
read_html("<html><title>Hi")

# From a local path
read_html(system.file("extdata", "r-project.html", 
                       package = "xml2"))

# From a url 即网络
cd <- read_xml("http://www.xmlfiles.com/examples/cd_catalog.xml")
me <- read_html("http://had.co.nz")
head(me)
me
