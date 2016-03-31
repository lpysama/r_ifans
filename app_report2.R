library(RMySQL)
library(xlsx)
library(plyr)
library(reshape2)
library(lubridate)


yesterday=Sys.Date()-1
file.name="C:\\Users\\Administrator\\Desktop\\app报告3.xlsx"
file.name2="C:\\Users\\Administrator\\Desktop\\app报告3.xlsx"
iOS.id='984000de8c5c89df24f7d155'
Andriod.id='8ea000eac15c89dfacd8d155'
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
ss2.2=sprintf("SELECT app,rank from app_ranking where date='%s'",yesterday)
data_rank2=dbGetQuery(conn,ss2.2)

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
ss5=sprintf("SELECT * FROM umeng_app_qd where date=\"%s\" and  app_id='8ea000eac15c89dfacd8d155'",as.Date(yesterday))
datax6=dbGetQuery(conn,ss5)
qd=datax6[c(5,6,8,9)]
row.names(qd)=qd$name
qd=qd[c("tencent","baidu","xiaomi","pp","anzhi","360","wandoujia","huawei","91","mumayi","lianxiang","ifensi","oppo","meizu","nduo","jifeng","wangyi"),]



dbDisconnect(conn)
#
v1=data.frame(date=strftime(yesterday,"%Y年%m月%d日"))
v1$hy=datax$hy_yh[2]+datax$hy_yh[1]
v1$ios_hy=datax$hy_yh[1]
v1$andriod_hy=datax$hy_yh[2]

v1$new_user=sum(datax$xz_yh)
v1$reg_user=reg_user$rs
v1$new_user_iOS=datax[which(datax$name=="iOS"),]$xz_yh
v1$new_user_Andriod=datax[which(datax$name=="Andriod"),]$xz_yh


v1$ios_sell=data_rank[which(data_rank$name=="娱乐(畅销)"),]$`min(pm)`
v1$ios_download=data_rank[which(data_rank$name=="娱乐(免费)"),]$`min(pm)`
#v1$ios_sell=data_rank2[which(data_rank2$app=="粉丝网畅销"),]$rank
#v1$ios_download=data_rank2[which(data_rank2$app=="粉丝焦点"),]$rank

v1$cumu_user=sum(datax$lj_yh)
v1$cumu_reg_user=0
v1$cumu_user_iOS=datax[which(datax$name=="iOS"),]$lj_yh
v1$cumu_user_Andriod=datax[which(datax$name=="Andriod"),]$lj_yh

lcl=data.frame(lcl_total_2=NA,lcl_total_7=NA,lcl_total_14=NA,lcl_ios_2=NA,lcl_ios_7=NA,lcl_ios_14=NA,lcl_an_2=NA,lcl_an_7=NA,lcl_an_14=NA)

v1=cbind(v1,lcl)
v1$user_visit_time=datax[which(datax$name=='Andriod'),]$pjr_sy_sc
v1=cbind(v1,use_time)

v1$user_visit_fre=datax[which(datax$name=='Andriod'),]$pjr_qd_cs

xx=loadWorkbook(file.name)
xx2=getRows(getSheets(xx)[[1]])

old_data1=read.xlsx(file.name,sheetIndex = 1,startRow = 2,encoding = "UTF-8")
old_data1=old_data1[-1,]
colnames(old_data1)=colnames(v1)


v1=rbind(v1,old_data1)
v1[1,]$cumu_reg_user=v1[2,]$cumu_reg_user+v1[1,]$reg_user
#v2
v2=as.vector(t(as.matrix(qd[2:4])))[1:51]

v2=data.frame(date=yesterday,t(v2))
old_data2=read.xlsx(file.name,sheetIndex = 2,startRow = 2,encoding = "UTF-8")
old_data2=old_data2[-1,]
colnames(old_data2)=colnames(v2)
v2=rbind(v2,old_data2)
#v3


#


#留存率写入

data_lcl_ios=data_lcl[data_lcl$app_id==iOS.id,]
v1[2:15,]$lcl_ios_2=rev(data_lcl_ios$lc_1[1:14])/100
v1[8:15,]$lcl_ios_7=rev(data_lcl_ios$lc_7[1:8])/100
v1[15,]$lcl_ios_14=rev(data_lcl_ios$lc_8[1])/100
data_lcl_andriod=data_lcl[data_lcl$app_id==Andriod.id,]
v1[2:15,]$lcl_an_2=rev(data_lcl_andriod$lc_1[1:14])/100
v1[8:15,]$lcl_an_7=rev(data_lcl_andriod$lc_7[1:8])/100
v1[15,]$lcl_an_14=rev(data_lcl_andriod$lc_8[1])/100


v1[,17]=as.numeric(v1[,17])
v1[,18]=as.numeric(v1[,18])
v1[,19]=as.numeric(v1[,19])
v1[,20]=as.numeric(v1[,20])
v1[,21]=as.numeric(v1[,21])
v1[,22]=as.numeric(v1[,22])
v1$lcl_total_2=round((v1$lcl_ios_2*v1$new_user_iOS+v1$new_user_Andriod*v1$lcl_an_2)/v1$new_user,digit=2)
v1$lcl_total_7=round((v1$lcl_ios_7*v1$new_user_iOS+v1$new_user_Andriod*v1$lcl_an_7)/v1$new_user,digit=2)
v1$lcl_total_14=round((v1$lcl_ios_14*v1$new_user_iOS+v1$new_user_Andriod*v1$lcl_an_14)/v1$new_user,digit=2)

#保存起来、
xx_time=format(strptime(round(seconds_to_period(mean(as.duration(hms(v1$user_visit_time))))),"%MM %SS"),"%H:%M:%S")

#v1.2=rbind(round(c(NA,colMeans(v1[2:22],na.rm = T),mean(as.difftime(v1$user_visit_time,format = "%H:%M:%S")),colMeans(v1[24:32],na.rm = T),na.rm=T),2),v1)
v1.2=rbind(round(c(NA,colMeans(v1[2:23],na.rm = T),0,colMeans(v1[25:33],na.rm = T),na.rm=T),2),v1)
v1.2$date=as.character(v1.2$date)
v1.2[1,]$user_visit_time=xx_time
v1.2[1,1]="均值"
v1.2[1,11:14]=NA
v2$date=as.character(v2$date)
v2.2=rbind(round(c(NA,colMeans(v2[2:52],na.rm = T)),2),v2)
v2.2[1,1]="均值"

addDataFrame(v1.2, getSheets(xx)[[1]], startRow=3, startColumn=1,col.names = F,row.names = F)
addDataFrame(v2.2, getSheets(xx)[[2]], startRow=3, startColumn=1,col.names = F,row.names = F)

 #删除老文件




#写入访问板块分析
conn <- dbConnect(MySQL(), dbname = "test", username="ifensi_com", password="fans_v2",host="10.0.5.13",port=3306)
dbSendQuery(conn,'SET NAMES gbk')
ssx1 = sprintf("SELECT * FROM app_access_map")
data_mapping= dbGetQuery(conn,ssx1)
ssx2 = sprintf("SELECT * FROM umeng_app_fwlj where date=\"%s\" ",as.Date(yesterday))
data_route=dbGetQuery(conn,ssx2)
dbDisconnect(conn)


datauk=merge(data_route,data_mapping,by.y = "page_name",by.x = "p_id",all.x = T)

datauk=datauk[which(datauk$lable!=""),]
datauk=datauk[which(datauk$lable!=""),]
datauk$time=as.numeric(unlist(strsplit(datauk$v_times," ",fixed = T))[(1:nrow(datauk))*2-1])

(cc=ddply(datauk,.(lable),summarise,visits2=sum(visits),time2=sum(time*visits)/visits2))

cc$visits2_percent=cc$visits2/sum(cc$visits2)
cc_lable=ordered(cc$lable,c("直播","粉团","新闻","活动","排行榜","启动页","个人中心","其他"))
cc$lable=cc_lable
cc2=cc[order(cc$lable),c(2,4,3)]
row.names(cc2)=cc[order(cc$lable),1]

old_data3=read.xlsx(file.name,sheetIndex = 3,startRow = 2,encoding = "UTF-8")
#old_data3=old_data3[-1,]


colnames(cc2)=c("点击次数"	,"占比",	"每次点击停留时间（秒）")
cc2=round(cc2,2)
write.csv(t(cc2),"C:\\Users\\Administrator\\Desktop\\cc.csv",fileEncoding="GBK")
ls3=data.frame(as.character(yesterday),t(as.vector(t(as.matrix(cc2)))))
names(ls3)=names(old_data3)
v3=rbind(ls3,old_data3)

addDataFrame(v3, getSheets(xx)[[3]], startRow=3,startColumn=1,col.names = F,row.names = F)
file.remove(file.name)
saveWorkbook(xx, file.name2) 
saveWorkbook(xx, paste0("C:\\Users\\Administrator\\Desktop\\","粉丝网app报告",as.character(yesterday),".xlsx") )

