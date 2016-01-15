#贴吧粉丝人数
#微博粉丝人数 -
#部落粉丝人数
#粉丝比例 微博
#地理位置比例 微博
#各站粉丝数 模拟
#粉丝网粉丝数

#mydata[!complete.cases(mydata),]
library(RMySQL)
library(plyr)
library(reshape2)
sdte="2016-01-15"
conn <- dbConnect(MySQL(), dbname = "test",username="root", password="root",host="10.0.5.19",port=3306)
#贴吧粉丝人数
ss1 =sprintf("select  tieba,gzsl from tieba_hyd  where  sdate <= '%s' AND sdate>='%s' ",sdate-1,sdate-7) 

tieba.fan=dbGetQuery(conn,ss1)
#微博指数+百度指数

ss2=sprintf("SELECT * FROM wb_qs_zs WHERE  sdate <= '%s' AND sdate>='%s'",sdate-1,sdate-7) 
wb_zs=dbGetQuery(conn,ss2)
wb_zs=ddply(wb_zs,.(name=wb_zs$name),colwise(mean))[,-c(2:5)]
##微博地域
ss3=sprintf("SELECT * FROM wb_dy_zs WHERE sdate <= '%s' AND sdate>='%s'",sdate-1,sdate-7) 
wb_zs_dy=dbGetQuery(conn,ss3)
wb_zs_dy[is.na(wb_zs_dy$sl),]$sl=0
wb_zs_dy$bl=as.numeric(unlist(strsplit(wb_zs_dy$bl,"%")))/100
wb_zs_dy=ddply(wb_zs_dy,.(name=wb_zs_dy$name,dy=wb_zs_dy$dy),colwise(mean))[,-c(3:5)]
xxxx=melt(wb_zs_dy,id.vars = c("name","dy"))
wb_zs_dy=dcast(xxxx,name~dy+variable)

##微博性别
ss4=sprintf("SELECT * FROM wb_sx_sex WHERE sdate <= '%s' AND sdate>='%s'",sdate-1,sdate-7) 
wb_zs_sex=dbGetQuery(conn,ss4)
wb_zs_sex[is.na(wb_zs_sex$bl_m),]$bl_m=0
wb_zs_sex[is.na(wb_zs_sex$bl_w),]$bl_w=0
wb_zs_sex$bl_m=as.numeric(wb_zs_sex$bl_m)
wb_zs_sex$bl_w=as.numeric(wb_zs_sex$bl_w)
wb_zs_sex=wb_zs_sex[,-c(1,2,5)]
wb_zs_sex=ddply(wb_zs_sex,.(name=wb_zs_sex$name),colwise(mean))


##微博年龄
ss5=sprintf("SELECT * FROM wb_sx_sex WHERE sdate=where  sdate <= '%s' AND sdate>='%s'",sdate-1,sdate-7) 
wb_zs_age=dbGetQuery(conn,ss5)

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