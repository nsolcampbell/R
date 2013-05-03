library(rJava);
library(RJDBC);
drv<-JDBC(“com.mysql.jdbc.Driver”,”D:\\Program Files\\MySQL\\Connector J 5.1.23\\mysql-connector-java-5.1.23-bin.jar”, “`”)

##”/home/fiona/Downloads/mysql-connector-java-5.1.14/mysql-connector-java-5.1.14-bin.jar”这是本人mysql-connector-java-5.1.14-bin.jar的存放位置，根据自己存放的位置做相应修改即可。

#dbConnect() 、dbSendQuery()、 dbGetQuery()、 dbDisconnect()这几个函数分别是连接数据库，给数据库发送命令，查询数据库，关闭数据库连接。

#用法如下：

#1.conn<-dbConnect(drv,”jdbc:mysql://${127.0.0.1}:3306/${database}?characterEncoding=utf-8″,”${root}“,”${password}“);

#关于需要用户自身输入的信息，都用${}标识出来。

#2.datatmp<-dbGetQuery(conn,paste(“select  *  from `MONITOR-NWMPING` WHERE HOST=’”,${pop_view_list[1,1]},”‘order by MONITOR “,sep=”))

#这里值得一提的是：在查询时，经常有些查询条件需要交互，这里paste()就起到作用了，将需要的查询条件先连贴成SQL句，再将命令传给mysql。

#3.dbSendQuery(con,”LOAD DATA LOCAL INFILE ‘/home/fiona/monitor_1.0/pingnowdata.txt’ INTO TABLE `PING_NOW_RAW`  FIELDS TERMINATED BY ‘,’ ENCLOSED BY ‘\”‘”);

#dbSendQuery()函数，本人用得最多的是在导入文件到数据库以及删表，建表。

#4.dbDisconnect(conn)即可关闭连接。