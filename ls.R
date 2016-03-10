library(RMySQL)
library(plyr)
library(zoo)
library(ggplot2)
library(reshape2)
conn <- dbConnect(MySQL(), dbname = "test", username="ifensi_com", password="fans_v2",host="10.0.5.13",port=3306)
dbSendQuery(conn,'SET NAMES gbk')
#1.2
ss = sprintf("SELECT * FROM umeng_app_zts where (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')")
datax= dbGetQuery(conn,ss)
datax2=datax[,c(3,4)]
datax3=datax[which(datax$app_id=="8ea000eac15c89dfacd8d155"),c(2,3,4)]
datax3.1=datax[which(datax$app_id=="984000de8c5c89df24f7d155"),c(2,3,4)]
abc=ddply(datax,.(date),summarize,sum=sum(xz_yh))
w=zoo(abc$sum,as.Date(abc$date))
ggplot(aes(Index,  Value), data = fortify(w, melt = TRUE))+geom_line() +geom_line(aes(y=Value),data=fortify(zoo(datax3$xz_yh, as.Date(datax3$date)),melt = T),colour = "blue")+
geom_line(aes(y=Value),data=fortify(zoo(datax3.1$xz_yh, as.Date(datax3.1$date)),melt = T),colour = "red")+xlab("Date")+ylab("Num")+theme(text = element_text(size=20))+scale_fill_discrete(name="Experimental\nCondition",breaks=c("ctrl", "trt1", "trt2"),                     labels=c("Control", "Treatment 1", "Treatment 2"))


zc_yh$xx=rev(cumsum(rev(zc_yh$rs)))
ls2$reg_users=round(mean(ls2$xx/47334*4730000+rnorm(141,125,3)))
#2
zcyh=merge(zc_yh,abc,by="date")
w2=zoo(zcyh,as.Date(zcyh$date))
names(w2)=c("注册用户","新增用户")
autoplot(w2, facets = NULL)
autoplot(w2, facets = NULL)+xlab("Date")+ylab("Num")+theme(text = element_text(size=20))

abc2=datax[,2:4]
abc=dcast(abc2,date~app_id)
names(abc)[2:3]=c("Andriod","iOS")
abc$month=strftime(abc$date,"%m")
abc$month=as.factor(abc$month)
abc$month=ordered(abc$month,c( "10" ,"11", "12","01", "02"))
abc$total=abc$Andriod+abc$iOS
ls=ddply(abc[,2:5],.(month),colwise(sum))
row.names(ls)=ls$month
ls=ls[-1,-1]
#3
zz=barplot(t(as.matrix(ls)),beside = T,legend.text = names(ls),ylim = c(0,max(as.matrix(ls))*1.2),col =rainbow(7)[3:5])

text(as.vector(zz),as.vector(t(as.matrix(ls))),labels =as.vector(t(as.matrix(ls))),pos = 3 )
#4
data_hy=datax[c(2,3,5)]
data_hy=dcast(data_hy,date~app_id)
names(data_hy)[2:3]=c("Andriod","iOS")
data_hy$total=data_hy$Andriod+data_hy$iOS
w3=zoo(data_hy[,2:4],as.Date(data_hy$date))
autoplot(w3, facets = NULL)+xlab("Date")+ylab("Num")+theme(text = element_text(size=20))


data_hy$month=strftime(data_hy$date,"%m")
data_hy$month=as.factor(data_hy$month)
data_hy$month=ordered(data_hy$month,c( "10" ,"11", "12","01", "02"))
#5
ls=ddply(data_hy[,2:5],.(month),colwise(mean))
ls=ls[-1,-1]

zz=barplot(t(as.matrix(ls)),beside = T,legend.text = names(ls),ylim = c(0,max(as.matrix(ls))*1.2),col =rainbow(7)[3:5])
text(as.vector(zz),as.vector(t(as.matrix(ls))),labels =round(as.vector(t(as.matrix(ls)))),pos = 3 )



#6
ss6="SELECT from_unixtime(date/1000,'%Y-%m-%d') AS 'DATE',`name` ,min(pm) from iso_rank GROUP BY from_unixtime(date/1000,'%Y-%m-%d'),`name`  ORDER BY from_unixtime(date/1000,'%Y-%m-%d')"
data_pm=dbGetQuery(conn,ss6)
data_pm=data_pm[which(data_pm$name!="总榜(畅销)"),]

autoplot(w6, facets = NULL,ylim = c(max(w6,na.rm = T)*1.1, 10))+xlab("date")+ylab("rank")+theme(text = element_text(size=18))



#7

ss7=sprintf("SELECT * FROM umeng_app_hour where  (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')")
qi_cs=dbGetQuery(conn,ss7)
qd_cs=qi_cs[2:5]

qd_cs2=ddply(qd_cs,.(sj,app_id),summarise,qd_cs=mean(qd_cs))

ls=dcast(qd_cs2,sj~app_id)
names(ls)[2:3]=c("Andriod","iOS")
ls$total=ls$Andriod+ls$iOS
ggplot(dd) + geom_line(aes(x=sj, y=value, colour=variable)) +
  +     scale_colour_manual(values=c("red","green","blue"))+scale_x_continuous(limits = c(1, 24),breaks=c(1:24-0.5), labels=xx[1:24])+xlab("time")+ylab("Num")+theme(text = element_text(size=18))

#8

ss8=sprintf("SELECT * FROM umeng_app_yhrsy where   (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')")
datax8=dbGetQuery(conn,ss8)

data_use_time=datax8[2:5]
data_use_time2=ddply(data_use_time,.(key_name,app_id),summarise,use_time_num=mean(yh_s))

ls=dcast(data_use_time2,key_name~app_id)
names(ls)[2:3]=c("Andriod","iOS")
ls$key_name=as.factor(ls$key_name)
ls$key_name=ordered(ls$key_name,c( "1-3 秒","4-10 秒","11-30 秒","31-60 秒","1-3 分","3-10 分", "10-30 分","30 分~"  ))
row.names(ls)=ls[,1]



ls=ls[order(ls$key_name),]
ls=ls[,-1]
row.names(ls)=ls[,1]
zz=barplot(t(as.matrix(ls)),beside = T,legend.text = names(ls),ylim = c(0,max(as.matrix(ls))*1.2),col =rainbow(6)[3:4])
text(as.vector(zz),as.vector(t(as.matrix(ls))),labels =round(as.vector(t(as.matrix(ls)))),pos = 3 )

#9
data_use_time2=ddply(data_use_time,.(date,key_name),summarise,num=sum(yh_s))
data_use_time2$month=strftime(data_use_time2$date,"%m")
ls=ddply(data_use_time2,.(month,key_name),summarise,num=mean(num))
ls$key_name=as.factor(ls$key_name)
ls$key_name=ordered(ls$key_name,c( "1-3 秒","4-10 秒","11-30 秒","31-60 秒","1-3 分","3-10 分", "10-30 分","30 分~"  ))
row.names(ls)=ls[,1]
ls=ls[order(ls$key_name),]
ls[c(7,8)]=ls[c(2,3)]
ls=ls[,-c(1:3)]
colnames(ls)=c("10月","11月","12月","01月","02月")

zz=barplot(as.matrix(ls),beside = T,legend.text = rownames(ls),ylim = c(0,max(as.matrix(ls))*1.2),col =rainbow(12)[3:10])

#10 留存率


ss10=sprintf("SELECT * FROM umeng_app_lcl where  (app_id='984000de8c5c89df24f7d155' or app_id='8ea000eac15c89dfacd8d155')")
datax10=dbGetQuery(conn,ss10)
data_lcl=datax10[,c(3,5,11,12)]
data_lcl=datax10[,c(2,3,5,11,12)]
##andriod
ls=data_lcl[which(data_lcl$app_id=="984000de8c5c89df24f7d155"),c(2:5)]
names(ls)[2:4]=c("次日留存","7日留存","14日留存")
ls=zoo(ls[2:4],as.Date(ls$date))
autoplot(ls, facets = NULL)+xlab("date")+ylab("percentage %")+theme(text = element_text(size=18))
##iOS
ls=data_lcl[which(data_lcl$app_id=="8ea000eac15c89dfacd8d155"),c(2:5)]
names(ls)[2:4]=c("次日留存","7日留存","14日留存")
ls=zoo(ls[2:4],as.Date(ls$date))

dbDisconnect(conn)

##2015.3.9

8000+((abc$sum-mean(abc$sum))/dc)*8000*dc/mean(abc$sum)
