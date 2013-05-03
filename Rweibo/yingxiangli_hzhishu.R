###load the package###
library(Rweibo)
#registerApp(app_name = "sinademo2", "xx", "xx")
#roauth <- createOAuth(app_name = "sinademo3", access_name = "cloudly")

registerApp(app_name = "nsolcampbell", "2235573555", "652dbf2c9c0f1c301921ad9b80d120f0") #注册R中的app
roauth <- createOAuth(app_name = "nsolcampbell", access_name = "nsol") #在这里转到微博认证
#res <- web.search.content("中央财经大学", page = 50, sleepmean = 10, sleepsd = 1)$Weibo
 
 
 
 
###get my h-index first###
 
get_followers <- function(uid_weibo, follow_count,...){
   follower_list <- data.frame()
   for (i in 1:ceiling(follow_count/200)){
       print(paste("reading page", i, "of user", uid_weibo))
       query <- paste0("friendships.followers(roauth, uid= ",uid_weibo,",count=200,
                          cursor= (",i,"-1)*200)[1]")
       #cannot pass the argument directly... don't know why
       follower_page <- eval(parse(text=query))
       follower_page2 <- sapply(follower_page[[1]], function(x) {
                               as.data.frame(x)[, c("id", "followers_count", "name")]})
       follower_page2 <- as.data.frame(t(follower_page2))
       follower_page2$uid_weibo <- uid_weibo
       follower_list <- rbind(follower_list, follower_page2)
}
### generate h-index ###
follower_list$id <- sapply(follower_list$id, unlist)
follower_list$name <- sapply(follower_list$name, unlist)
#follower_list <- follower_list[!duplicated(follower_list$id), ]
follower_list$followers_count <- sapply(follower_list$followers_count, unlist)
follower_list$rank <- nrow(follower_list)+1-rank(follower_list$followers_count, ties.method = "min")
h_index <- nrow(follower_list[follower_list$rank<=follower_list$followers_count, ])
print(paste("user", uid_weibo, "has an h-index of", h_index ))
return(list(follower_list, h_index))
}
 
tt <- get_followers(uid_weibo=1406511850, follow_count=1100)
### replace with your weibo id and follower count to get your h-index
tt[[2]] #h-index here
my_follower_count <- tt[[1]] #my follower list for later use
save(my_follower_count, file = "my_follower_count.rdata")
 
###my h-index:287###
### for those who's follower count > 5000, use friendships/followers ###
### compare with those how have 500-2000 followers ###
load("my_follower_count.rdata")
my_follower_count_5000 <- subset(my_follower_count, followers_count<=2000 & followers_count>=500)
names(my_follower_count_5000)
summary(my_follower_count_5000)
library(plyr)
my_follower_count_5000 <- arrange(my_follower_count_5000, desc(followers_count) )
my_follower_count_5000$pages <- with(my_follower_count_5000, ceiling(followers_count/200))
my_follower_count_5000$pages_cumsum <- cumsum(my_follower_count_5000$pages)
my_follower_count_5000$cycle <- ceiling(my_follower_count_5000$pages_cumsum/150)
#can only submit 150 API request per hour
for (i in 1:max(my_follower_count_5000$cycle)){
my_follower_hindex <- apply(my_follower_count_5000[my_follower_count_5000$cycle == i, 1:2], 1,
                                 function(x) {get_followers(x[1], x[2])[[2]]})
print(my_follower_hindex)
#wait until next hour due to API limit: an hour cycle
}