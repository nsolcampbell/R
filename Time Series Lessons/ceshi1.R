
require(Rweibo)
res <- web.search.content("泰囧", page = 50, sleepmean = 10,
           sleepsd = 1)$Weibo

