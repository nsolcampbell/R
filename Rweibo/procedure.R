install.packages("Rweibo", repos = "http://R-Forge.R-project.org")
require(Rweibo)
#注册registerApp(app_name = "nsolcampbell", "2235573555", "652dbf2c9c0f1c301921ad9b80d120f0") #注册R中的app
roauth <- createOAuth(app_name = "nsolcampbell", access_name = "nsol") #在这里转到微博认证
res <- web.search.content("中央财经大学", page = 50, sleepmean = 10, sleepsd = 1)$Weibo

#将得到数据写入txt文件：write.table(res,"C:/Users/nixujun/Desktop/z.txt",row.name=F)
#读取微博搜索数据：res<-read.table("C:\\Users\\nixujun\\Desktop\\z.txt",header=F)

require(Rwordseg)
insertWords("中央财经大学")
n = length(res[, 1])
res = res[res!=" "]
words = unlist(lapply(X = res, FUN = segmentCN))
word = lapply(X = words, FUN = strsplit, " ")
v = table(unlist(word))
v = sort(v, deceasing = T)
v[1:100]
head(v)
d = data.frame(word = names(v), freq = v)


write.table(d,"C:/Users/nixujun/Desktop/wordseg.txt",row.name=F)

require(wordcloud)
d = read.table("C:/Users/nixujun/Desktop/wordseg.txt",header=T)
dd = tail(d, 150)
op = par(bg = "lightyellow")
# grayLevels = gray((dd$freq)/(max(dd$freq) + 140))
# wordcloud(dd$word, dd$freq, colors = grayLevels)
rainbowLevels = rainbow((dd$freq)/(max(dd$freq) - 10))
wordcloud(dd$word, dd$freq, col = rainbow(length(d$freq)))
par(op)




