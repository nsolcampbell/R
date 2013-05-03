
require(Rwordseg)
insertWords("泰囧")
n = length(res[, 1])
res = res[res!=" "]
words = unlist(lapply(X = res, FUN = segmentCN))
word = lapply(X = words, FUN = strsplit, " ")
v = table(unlist(word))
v = sort(v, deceasing = T)
v[1:100]
head(v)
d = data.frame(word = names(v), freq = v)

