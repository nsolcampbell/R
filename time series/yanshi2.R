setwd("D:\\dropbox\\Dropbox\\Apps\\R\\run")
res<-read.table("z.txt",header=F)

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
