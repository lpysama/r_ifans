library(RMySQL)
library(xlsx)
library(plyr)

yesterday=Sys.Date()-1
file.name="C:\\Users\\Administrator\\Desktop\\app报告表头.xlsx"
#55数数据库
conn1 <- dbConnect(MySQL(), dbname = "app_ifensi_com", username="ifensi_com", password="fans_v2",host="10.0.6.55",port=3306)
dbSendQuery(conn1,'SET NAMES gbk')
s2= sprintf("SELECT from_unixtime(createtime,'%%Y-%%m-%%d')  createtime,count(memberid) rs from fs_member where from_unixtime(createtime,'%%Y-%%m-%%d') ='%s'  GROUP BY from_unixtime(createtime,'%%Y-%%m-%%d')", yesterday)
reg_user=dbGetQuery(conn1,s2)

dbDisconnect(conn1)
#13数据库
conn <- dbConnect(MySQL(), dbname = "test", username="ifensi_com", password="fans_v2",host="10.0.5.13",port=3306)
dbSendQuery(conn,'SET NAMES gbk')
ss = sprintf("SELECT * FROM umeng_app_zts where date=\"%s\" and (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')",as.Date(yesterday))
datax= dbGetQuery(conn,ss)
#排名数据
ss2=sprintf("SELECT from_unixtime(date/1000,'%%Y-%%m-%%d') AS 'DATE',`name` ,min(pm) from iso_rank where from_unixtime(date/1000,'%%Y-%%m-%%d')='%s' GROUP BY from_unixtime(date/1000,'%%Y-%%m-%%d'),`name` ",as.Date(yesterday))
data_rank=dbGetQuery(conn,ss2)

datax$name=''
datax[which(datax$app_id==iOS.id),]$name='iOS'
datax[which(datax$app_id==Andriod.id),]$name='Andriod'
##留存率 时间特殊
ss3.1=sprintf("SELECT * FROM umeng_app_lcl where date>'%s' and (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')",yesterday-15)
data_lcl=dbGetQuery(conn,ss3.1)
ss3.2= sprintf("SELECT xz_yh,app_id,date FROM umeng_app_zts where date>\"%s\" and (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')",as.Date(yesterday-15))
ls=dbGetQuery(conn,ss3.2)
ls$name=''
ls[which(ls$app_id==iOS.id),]$name='iOS'
ls[which(ls$app_id==Andriod.id),]$name='Andriod'
#停留时长
ss4=sprintf("SELECT * FROM umeng_app_yhrsy where date=\"%s\" and  (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')",as.Date(yesterday))
datax4=dbGetQuery(conn,ss4)
use_time=datax4$yh_s[1:8]+datax4$yh_s[9:16]
use_time=data.frame(t(use_time))
colnames(use_time)=datax4$key_name[1:8]

#平均使用时间


#平均启动次数



dbDisconnect(conn)
#
v1=data.frame(date=strftime(yesterday,"%Y年%m月%d日"))
v1$hy=datax$hy_yh[2]+datax$hy_yh[1]

v1$andriod_hy=datax$hy_yh[2]
v1$ios_hy=datax$hy_yh[1]
v1$new_user=sum(datax$xz_yh)
v1$reg_user=reg_user$rs
v1$new_user_Andriod=datax[which(datax$name=="Andriod"),]$xz_yh
v1$new_user_iOS=datax[which(datax$name=="iOS"),]$xz_yh
v1$cumu_user=sum(datax$lj_yh)
v1$ios_sell=data_rank[which(data_rank$name=="娱乐(畅销)"),]$`min(pm)`
v1$ios_download=data_rank[which(data_rank$name=="娱乐(免费)"),]$`min(pm)`
v1$cumu_user_Andriod=datax[which(datax$name=="Andriod"),]$lj_yh
v1$cumu_user_iOS=datax[which(datax$name=="iOS"),]$lj_yh
lcl=data.frame(lcl_total_2=NA,lcl_total_7=NA,lcl_total_14=NA,lcl_an_2=NA,lcl_an_7=NA,lcl_an_14=NA,lcl_ios_2=NA,lcl_ios_7=NA,lcl_ios_14=NA)
v1=cbind(v1,lcl)
v1=cbind(v1,use_time)
xx=loadWorkbook(file.name)
xx
addDataFrame(v1, getSheets(xx)[[1]], startRow=length(getRows(getSheets(xx)[[1]]))+1, startColumn=0)
#v2 v3


#留存率写入


#保存起来
file.remove(file.name) #删除老文件
saveWorkbook(xx, file.name) 