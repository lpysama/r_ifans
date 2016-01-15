#贴吧粉丝人数
#微博粉丝人数 -
#部落粉丝人数
#粉丝比例 微博
#地理位置比例 微博
#各站粉丝数 模拟
#粉丝网粉丝数
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "test",username="root", password="root",host="10.0.5.19",port=3306)
#贴吧粉丝人数
ss1 =sprintf("select  tieba,gzsl from tieba_hyd  where  sdate <= '%s' AND sdate>='%s' ",sdate-1,sdate-7) 

tieba.fan=dbGetQuery(conn,ss1)
#微博指数 
ss2=sprintf("select  tieba,gzsl from tieba_hyd  where  sdate <= '%s' AND sdate>='%s' ",sdate-1,sdate-7) 


#写地理位置 weibo数据





#写入年龄微博数据

#微博指数与百度指数

#部落部落与匹配
namelist= dbGetQuery(conn, "SELECT name,tieba FROM star_zh_copy")
tieba_bl.name=dbGetQuery(conn,'select qq,tname from qq_bl_name')

fan_num=merge(namelist,tieba_bl.name,by.x="name",by.y="tname")

#写入两个人数
fan_num=merge(fan_num,b)


#部落粉丝人数
ss1 =sprintf("select  tieba,gzsl from tieba_hyd  where  sdate <= '%s' AND sdate>='%s' ",sdate-1,sdate-7) 


#
dbDisconnect(conn)