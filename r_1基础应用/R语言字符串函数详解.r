#R语言中文社区
setwd("E:/Rexercise1/R语言中文社区")
#R语言字符串函数详解
#stringr包
library(stringr)
#字符串的小大写转换
#invert_match返回非匹配模式的起始位置
#modifiers指定模式的类别
#连接字符串str_c
#str_conv指定字符串的编码
#计算字符串中的匹配模式的数目str_count
#str_detect 检测字符串中是否存在某种模式
#str_dup重复和连接字符串向量
#str_extract从字符串中提取匹配的模式
#str_length 字符串的长度
#str_locate 定位在字符串中匹配模式的位置
#str_match从字符串中提取匹配组
#str_order对字符向量进行排序
#str_pad在字符串的前后位置填充字符（如空格）
#str_place替换字符串中的匹配模式
#str_replace_na 将缺失值NA替换成'NA'
#str_split 根据一个分隔符将字符串进行分割
#str_sub按位置从字符向量中提取或者替换子字符串
#str_subset 提取匹配模式的字符串向量元素
#str_trim 删除字符串中的空格
#str_wrap,设置行宽度等段落设置
#word 从句子中提取单词
##基础包的字符串处理函数

#字符串的小大写转换
dog="The quick brown dog"
str_to_upper(dog) #将英文字母转换成大写
str_to_lower(dog) #将英文字母转换成小写
str_to_title(dog) #将英文字符串中的单词首字母转换成大写
#转化成不同的语种
str_to_upper("i","en") #将其转化成英语大写
str_to_upper("i","tr") #将其转化成Turkish

#invert_match返回非匹配模式的起始位置
numbers="1 and 2 and 4 and 456"
num_loc=str_locate_all(numbers,"[0-9]+")[[1]]
num_loc  #匹配数字，返回数字的起始位置 上述+号表示的是不仅仅是个位数
str_sub(numbers,num_loc[,"start"],num_loc[,"end"]) #提取数字
text_loc=invert_match(num_loc) #返回不匹配数字的起始位置
text_loc
str_sub(numbers,text_loc[,"start"],text_loc[,"end"])

#modifiers指定模式的类别
pattern="a.b"
strings=c("abb","a.b")
str_detect(strings,pattern)
str_detect(strings,fixed(pattern)) #按字节数比较
str_detect(strings,coll(pattern)) #比较字符串按排序规则
i=c("I","\u0130","i")
i
str_detect(i,regex('i',T))
str_detect(i,fixed('i',T))
str_detect(i,coll('i',T))
str_detect(i,coll("i",TRUE,locale = "tr"))
#word boundaries 单词边界，词组统计，分割
words=c("These are some words.")
str_count(words,boundary("word")) #统计语句中单词的个数
str_split(words," ")[[1]] #将语句分成若干个词组
str_split(words,boundary("word"))[[1]]
#使用正则表达式
str_extract_all("The Cat in the Hat","[a-z]+") #区分大小写
str_extract_all("The Cat in the Hat",regex("[a-z]+",T)) #忽略大小写
str_extract_all("a\nb\nc","^.")
str_extract_all("a\nb\nc", regex("^.", multiline = TRUE))
str_extract_all("a\nb\nc", "a.")
str_extract_all("a\nb\nc", regex("a.", dotall = TRUE))

#连接字符串
str_c("Letter:",letters[1:5])
str_c("Letter", letters[1:5], sep = ":") #sep可设置连接符
str_c(letters[1:5], " is for", "...") 
str_c(letters[-26], " comes before ", letters[-1])
str_c(letters)
str_c(letters, collapse = "") #collapse 将一个向量的所有元素连接成一个字符串，collapse设置元素间的连接符
str_c(letters, collapse = ", ")
#空值输出为空值
str_c(c("a",NA,"b"),"-d")
#空值仍当做字符串输出  
str_c(str_replace_na(c("a", NA, "b")), "-d")

#指定字符串的编码str_conv
x=rawToChar(as.raw(177)) #raw转化成character
x
str_conv(x,encoding = "ISO-8859-2") #按指定语法编码
str_conv(x,encoding = "ISO-8859-1")

#计算字符串中的匹配模式的数目str_count
fruit <- c("apple", "banana", "pear", "pineapple") 
str_count(fruit, "a") #计算向量fruit的每个元素含有a的数目
str_count(fruit, "p") 
str_count(fruit, "ap") 
str_count(fruit, "e") 
str_count(fruit, c("a", "b", "p", "p"))
str_count(c("a.", "...", ".a.a"), ".") #正则表达式中‘.’是指单个字符，不仅仅是字符‘.’
str_count(c("a.", "...", ".a.a"), fixed(".")) #fixed(".")指字符‘.’

#str_detect 检测字符串中是否存在某种模式
fruit <- c("apple", "banana", "pear", "pinapple") 
str_detect(fruit, "a") #fruit的元素是否包含a
str_detect(fruit, "pp")
str_detect(fruit, "^a") #fruit的元素是否以a开头
str_detect(fruit, "a$") #fruit的元素是否以a结尾
str_detect(fruit, "b") 
str_detect(fruit, "[eio]") #fruit的元素是否包含[aeiou]中的一个字符
#also vectorised over pattern
str_detect("aedfg",letters) #也可以作匹配用

#str_dup重复和连接字符串向量
fruit <- c("apple", "pear", "banana")
str_dup(fruit, 2) # 向量的每个元素重复2次，然后连接起来
str_dup(fruit, 1:3) 
str_c("ba", str_dup("na", 0:5)) #字符串重复后连接

#str_extract从字符串中提取匹配的模式
shopping_list <- c("apples 4x4", "bag of flour", "bag of sugar", "milk x2") 
#提取匹配模式的第一个字符串
str_extract(shopping_list, "\\d") # 提取数字
str_extract(shopping_list, "[a-z]+") #提取字母
str_extract(shopping_list, "[a-z]{1,4}") 
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")
# 提取所有匹配模式的字符串，结果返回一个列表
str_extract_all(shopping_list, "[a-z]+")
str_extract_all(shopping_list, "\\b[a-z]+\\b")
str_extract_all(shopping_list, "\\d")
# 提取所有匹配模式的字符串，结果返回一个矩阵，通过simplify = TRUE设置
str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE) 
str_extract_all(shopping_list, "\\d", simplify = TRUE)

#str_length 字符串的长度
str_length(letters) 
str_length(NA) 
str_length(factor("abc")) 
str_length(c("i", "like", "programming", NA))
# Two ways of representing a u with an umlaut 
#两种方式来输出u的变音
u1 <- "\u00fc" 
u2 <- stringi::stri_trans_nfd(u1) 
# The print the same: 
u1 
u2 
# But have a different length，但是长度不一样 
str_length(u1) 
str_length(u2) 
#即是他们有相同数量的字符串
str_count(u1) 
str_count(u2)

#str_locate 定位在字符串中匹配模式的位置
fruit <- c("apple", "banana", "pear", "pineapple") 
#返回匹配的第一个字符串的位置：
str_locate(fruit, "$") 
str_locate(fruit, "a") 
str_locate(fruit, "ap") 
str_locate(fruit, "e") 
str_locate(fruit, c("a", "b", "p", "p"))
#返回匹配的所有位置：
str_locate_all(fruit, "a") 
str_locate_all(fruit, "e") 
str_locate_all(fruit, c("a", "b", "p", "p"))
# 查找每个字符的位置 
str_locate_all(fruit, "")

#str_match从字符串中提取匹配组
strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569", "387 287 6718", "apple", "233.398.9187 ", "482 952 3315", "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000", "Home: 543.355.3679") 
strings #下面是定义电话号码的格式
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_extract(strings, phone) #返回匹配的最长字符串
str_match(strings, phone) #返回匹配的最长字符串，同时返回最长字符串中的子字符串
# Extract/match all 
str_extract_all(strings, phone) 
str_match_all(strings, phone)

#str_order对字符向量进行排序
str_order(letters,locale = "en") #返回顺序值
str_sort(letters, locale = "en")
str_order(letters, locale = "haw") #首先提取了元音字母
str_sort(letters, locale = "haw")

#str_pad在字符串的前后位置填充字符（如空格）
rbind( str_pad("hadley", 30, "left"), str_pad("hadley", 30, "right"), str_pad("hadley", 30, "both") )
# All arguments are vectorised except side 
str_pad(c("a", "abc", "abcdef"), 10) 
str_pad("a", c(5, 10, 20)) 
str_pad("a", 10, pad = c("-", "_", " "))
# Longer strings are returned unchanged 
str_pad("hadley", 3, pad = '-')
str_pad("hadley", width = 8, pad = '-')

#str_place替换字符串中的匹配模式
fruits <- c("one apple", "two pears", "three bananas");fruits 
str_replace(fruits, "[aeiou]", "-") #替换第一个匹配的字符
str_replace_all(fruits, "[aeiou]", "-")#替换所有匹配的字符
str_replace(fruits, "([aeiou])", "") 
str_replace(fruits, "([aeiou])", "\\1\\1") #复制后并替换
str_replace(fruits, "[aeiou]", c("1", "2", "3")) 
str_replace(fruits, c("a", "e", "i"), "-") #对应字符串的相关字符替换
fruits <- c("one apple", "two pears", "three bananas") 
str_replace(fruits, "[aeiou]", "-") 
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "([aeiou])", "") 
str_replace_all(fruits, "([aeiou])", "\\1\\1") 
str_replace_all(fruits, "[aeiou]", c("1", "2", "3")) 
str_replace_all(fruits, c("a", "e", "i"), "-")
# If you want to apply multiple patterns and replacements to the same # string, pass a named version to pattern. 
str_replace_all(str_c(fruits, collapse = "---"), c("one" = 1, "two" = 2, "three" = 3))

#str_replace_na 将缺失值替换成'NA'
str_replace_na(c(NA,"abd","def")) #将NA转化成“NA”

#str_split 根据一个分隔符将字符串进行分割
fruits <- c( "apples and oranges and pears and bananas", "pineapples and mangos and guavas" )
fruits
str_split(fruits, " and ")
#通过设置n，指定分割成n块
str_split(fruits, " and ", n = 3) #将字符串分割成3块
str_split(fruits, " and ", n = 2) #将字符串分割成2块
str_split(fruits, " and ", n = 5)
# Use fixed to return a character matrix  #较好可视性的结果输出
str_split_fixed(fruits, " and ", 3) 
str_split_fixed(fruits, " and ", 4)
str_split_fixed(fruits, " and ", 6)

#str_sub按位置从字符向量中提取或者替换子字符串
hw <- "Hadley Wickham"
str_sub(hw, 1, 6)
str_sub(hw, end = 6)
str_sub(hw, 8, 14) 
str_sub(hw, 8) 
str_sub(hw, c(1, 8), c(6, 14))
# Negative indices 
str_sub(hw, -1) 
str_sub(hw, -7)  #即提取-7到-1位置的子字符串
str_sub(hw, end = -7)
# Alternatively, you can pass in a two colum matrix, as in the 
# output from str_locate_all 
pos <- str_locate_all(hw, "[aeio]")[[1]] 
str_sub(hw, pos) 
str_sub(hw, pos[, 1], pos[, 2])
# Vectorisation 
str_sub(hw, seq_len(str_length(hw))) 
str_sub(hw, end = seq_len(str_length(hw)))
# 替换
x <- "BBCDEF" 
str_sub(x, 1, 1) <- "A"; x 
str_sub(x, -1, -1) <- "K"; x 
str_sub(x, -2, -2) <- "GHIJ"; x 
str_sub(x, 2, -2) <- ""; x

#str_subset 提取匹配模式的字符串向量元素
fruit <- c("apple", "banana", "pear", "pinapple") ;fruit
str_subset(fruit, "a") 
str_subset(fruit, "ap") 
str_subset(fruit, "^a") 
str_subset(fruit, "a$") 
str_subset(fruit, "b") 
str_subset(fruit, "[aeiou]")
# Missings are silently dropped 
str_subset(c("a", NA, "b"), ".")

#str_trim 删除字符串中的空格
print(" String with trailing and leading white space\t")
str_trim(" String with trailing and leading white space\t")
print("\n\nString with trailing and leading white space\n\n")
str_trim("\n\nString with trailing and leading white space\n\n")

#str_wrap,设置行宽度等段落设置
thanks_path <- file.path(R.home("doc"), "THANKS") 
thanks <- str_c(readLines(thanks_path), collapse = "\n") 
thanks <- word(thanks, 1, 3, fixed("\n\n")) 
cat(str_wrap(thanks), "\n") 
cat(str_wrap(thanks, width = 70), "\n") 
cat(str_wrap(thanks, width = 60, indent = 6), "\n")  #每行宽度为60，缩进6
cat(str_wrap(thanks, width = 80, indent = 6, exdent = 2), "\n")

#word 从句子中提取单词
#word(string, start = 1L, end = start, sep = fixed(""))
sentences <- c("Jane saw a cat", "Jane sat down");sentences
word(sentences, 1) #提取第一个单词
word(sentences, 2) #提取第二个单词
word(sentences, -1) #提取句子的最后一个单词
word(sentences, 2, -1) #提取第二个单词到最后一个单词
# Also vectorised over start and end 
word(sentences[1], 1:3, -1) 
word(sentences[1], 1, 1:4)
# 指定分隔符 
str <- 'abc.def..123.4568.999' 
word(str, 1, sep = fixed('..')) 
word(str, 2, sep = fixed('..'))


##基础包的字符串处理函数
#paste 字符串连接
#strsplit 字符串分割
x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
x;strsplit(x, "e")
unlist(strsplit("a.b.c", ".")) #此处挺奇怪的啊，为什么都成了点？？？
unlist(strsplit("a.b.c", "[.]"))#使用‘.’为分割符
#或者：
unlist(strsplit("a.b.c", ".", fixed = TRUE))
x<-'ascd123afrwf34535ddggh454fgf5e4'
unlist(strsplit(x, split = '[0-9]+', perl = TRUE))#以数字为分割符
unlist(strsplit(x, split = '[a-z]+', perl = TRUE))#以字母为分割符
#nchar 计算字符串的字符个数
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
nchar(x)
#substr字符串截取及替换
#对于单个字符串：
substr("abcdef", 2, 4)
substring("abcdef", 2, 4)
substring("abcdef", 1:6, 1:6)
substr(rep("abcdef", 4), 1:4, 4:5)
#对于字符串向量：
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)#对向量x每个元素截取子字符串
substring(x, 2, 4:6) #按顺序依次来，，，4,5,6
substring(x, 2) <- c("..", "+++")#以赋值进行替换，而且替换是往后面延续的
x
#字符串替换及大小变换
x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)#i:w,X:h,s:y，单个字符对应替换
chartr("a-cX", "D-Fw", x)
tolower(x)#转换成小写
toupper(x)#转换成大写
casefold(x, upper = FALSE)
casefold(x, upper = TRUE)
#字符匹配与替换
txt <- c("10arm03","Foot 12"," 678-lefroo.345", "__.bafoobar90..");txt
grep(pattern = 'foo', x = txt, value = FALSE)#区分大小写，结果返回匹配的元素索引
grep(pattern = 'foo', x = txt, value = TRUE)#区分大小写，结果返回匹配的元素值
grep(pattern = 'foo', x = txt, ignore.case = TRUE)#忽略大小写，结果返回匹配的元素索引
grep(pattern = 'foo', x = txt, ignore.case = TRUE, value = TRUE)#忽略大小写，结果返回匹配的元素值
grep(pattern = 'foo', x = txt, ignore.case = TRUE, value = TRUE, invert = TRUE)#忽略大小写，结果返回不匹配的元素值
grep(pattern = '^[0-9]+', x= txt, perl = TRUE)#返回以数字开头的元素索引
grep(pattern = '[0-9]+$', x= txt, perl = TRUE, value = TRUE)#返回以数字结尾的元素
grep(pattern = '\\d$', x= txt, perl = TRUE, value = TRUE)#返回以数字结尾的元素
#返回是都匹配的逻辑向量，YES或者NO
txt <- c("10arm03","Foot 12"," 678-lefroo.345", "__.bafoobar90..")
grepl(pattern = 'foo', x = txt) #里面是否有foo出现
grepl(pattern = '\\d$', x = txt, perl = TRUE) #是否以数字结尾
#替换匹配的元素的第一个字符串
txt <- c("10arm03","Foot 12 foot"," 678-lefroo.3a456", "__.bafoobar90foobar..")
sub(pattern = 'foo',replacement = '99', x = txt)#将元素中的第一个foo替换成99
sub(pattern = '\\d+$', replacement = '+++', x = txt, perl = TRUE)#将结尾的数字替换成+++
#替换匹配的所有元素
txt <- c("10arm03","Foot 12 foot"," 678-lefroo.345", "__.bafoobar90foobar..")
gsub(pattern = 'foo',replacement = '99', x = txt)#将所有的foo替换成99
gsub(pattern = '\\d+', replacement = '+++', x = txt, perl = TRUE)#将所有数字替换成+++
#返回每一个元素匹配的第一个位置及字符数目
txt <- c("10arm03","Foot 12 foot"," 678-lefroo.345", "__.bafoobar90foobar..")
regexpr(pattern = 'foo', text = txt)
regexpr(pattern = '\\d+', text = txt)
#返回每一个元素匹配的所有位置及相应的字符数目
txt <- c("10arm03","Foot 12 foot"," 678-lefroo.345", "__.bafoobar90foobar..")
gregexpr(pattern = 'foo', text = txt)
gregexpr(pattern = '\\d+', text = txt)
#返回元素匹配的第一个位置及字符数目
txt <- c(NA,"Foot 12 foot"," 678-lefroo.345", "__.bafoobar90foobar..")
regexec(pattern = 'foo', text = txt)
regexec(pattern = '\\d+', text = txt)
