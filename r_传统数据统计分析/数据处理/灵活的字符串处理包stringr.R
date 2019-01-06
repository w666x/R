##灵活的字符串处理包stringr

library(stringr)
#word()，从句子中提取词组
#word(string, start = 1L, end = start, sep = fixed(" "))
#例子
string = 'I like using R'
word(string,1,-1) #取出所有的句子
#"I like using R"
word(string, 1) #提取第一个单词
# "I"
word(string, -1) #提取最后一个单词
# "R"
word(string, 1, 1:4) #提取最后一个单词
#[1] "I" "I like"  "I like using" "I like using R"


#str_wrap()，将段落划分为华丽的格式，可设置每行的宽度等。
#str_wrap(string, width = 80, indent = 0, exdent = 0)
#例子，此处数据集为我选择打开的指定文件
#此处的训练文件为test.txt
string <- str_c(readLines(con = file.choose(), encoding = 'UTF-8'),collapse = '\n')
str_wrap(string) #默认以80个字节作为行宽
cat(str_wrap(string), sep = '\n') #以换行符连接每个固定长度的句子
cat(str_wrap(string, indent = 4)) #段落第一行空4个字符

#str_trim()，剔除字符串多余的首末空格
#str_trim(string, side = c("both", "left", "right"))
string <- '   Why is me? I have worded hardly!    '
str_trim(string, side = 'left')
str_trim(string, side = 'right')
str_trim(string, side = 'both')

#str_to_upper()，str_to_lower()，str_to_title()，字符串转换
string <- 'Simple, consistent wrappers for common string '
str_to_upper(string) #字符串统统转换成大写
str_to_lower(string)#字符串统统转换成小写
str_to_title(string)#字符串统统转换成标题形式

#str_subset()，根据正则表达式匹配字符串中的值
#str_subset(string, pattern)
#该函数与word()函数的区别在于前者提取字符串的子串，
#后者提取的是单词,而且str_sub也可以其替换的作用。
string <- "My name is LiuShunxiang, I'm 28."
str_sub(string, 1, 1)
#提取首个字符，此即为str_subset()
word(string, 1, 1)
#提取收个单词
str_sub(string, 1, 4)
#提取前四个字符
word(string, 1, 4)
#提取前四个word
str_sub(string, -1)
#提取最后一个字符
word(string, -1)
#提取最后一个单词
str_sub(string, -3,-2) <- 26
#将倒数第二个，倒数第三个字符改为26
string

#str_split()，字符串分割函数
#str_split(string, pattern, n = Inf)
#str_split_fixed(string, pattern, n)
#str_split与str_split_fixed的区别在于前者返回列表格式，后者返回矩阵格式
string <- 'myxyznamexyzisxyzliuxyzshunxyzxiang!'
#以xyz为分隔符字符串，遇到就分割
str_split(string, 'xyz')
#指定返回的分割的数量
str_split(string, 'xyz', n = 5) #最后一组就不会被分割
#返回矩阵格式
str_split_fixed(string, 'xyz', 6)

#str_order()，对字符向量排序
#str_order(x, decreasing = FALSE, na_last = TRUE, locale = "", ...)
#str_sort(x, decreasing = FALSE, na_last = TRUE, locale = "", ...)
str_order(letters, locale = "12")
str_sort(letters, locale = "en")

#str_replace()，字符串替换函数
#str_replace(string, pattern, replacement)
#str_replace_all(string, pattern, replacement)
#str_replace_na(string, replacement = "NA")
string <-'1989.07.17'
str_replace(string, '\\.', '-')
str_replace_all(string, '\\.', '-')
str_replace_na(string,replacement = 'NA')

#str_pad()，字符填充函数
#str_pad(string, width, side = c("left", "right", "both"), pad = " ")
string <- 'LiuShunxiang'
str_pad(string,10) #指定的长度少于string长度时，将只返回原string
str_pad(string,20)
str_pad(string,20,side = 'both',pad = '*')

#str_match(),str_match_all()，提取匹配的字符串
#str_match(string, pattern)
#str_match_all(string, pattern)
#str_extract(),str_extract_all，提取匹配的字符串
#，功能与str_match(),str_match_all()函数类似
string <- c('139-1234-5678','133,1267,4589','134 6543 7890','178 2345 1111 or 133 7890 1234')
string
str_match(string,'[1][3-9]{2}[- ,][0-9]{4}[- ,][0-9]{4}')
str_match_all(string,'[1][3-9]{2}[- ,][0-9]{4}[- ,][0-9]{4}')

#str_locate(),str_locate_all()，字符定位函数，返回匹配对象的首末位置
#str_locate(string, pattern)
#str_locate_all(string, pattern)
string <- c('liushunxiang1989','zhangsan1234')
str_locate(string,'s')
str_locate(string,'n')
str_locate_all(string,'n')

#str_length()，字符长度函数，该函数类似于nchar()函数，
#但前者将NA返回为NA，而nchar则返回2
#str_length(string)
string <- c('LiuShunxiang','1989-07-17',NA)
str_length(string)
nchar(string)

#str_c()，将多个字符串连接为单个字符串
#str_c(..., sep = "", collapse = NULL)
str_c(c(1989,07,17), sep = '-')  #使用sep
str_c(c(1989,07,17), collapse = '-')  #使用collapse
str_c('x',c(1:10),':')

#str_dup()，重复字符串
#str_dup(string, times)
fruit <- c("apple", "pear", "banana")
str_dup(fruit, 2)
str_dup(fruit, 1:3)
str_c("ba", str_dup("na", 0:5))

#str_detect()，检测函数，用于检测字符串中是否存在某种匹配模式
#str_detect(string, pattern)
string <- c('LiuShunxiang','Zhangsan','Philips1990')
str_detect(string,'L')
str_detect(string,'\\d')
str_detect(string,'[a-zA-Z0-9]')

#str_count()，计数能够匹配上的字符个数
#str_count(string, pattern = "")
string <- c('LiuShunxiang','Zhangsan','Philips1990')
str_count(string,'i')
str_count(string,'\\d')
str_count(string)

