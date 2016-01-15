library(RMySQL)
library(xlsx)
sdate=Sys.Date()-1
sdate=as.Date("2016-01-14")
conn <- dbConnect(MySQL(), dbname = "test",username="root", password="root",host="10.0.5.19",port=3306)
ss =sprintf("select *  from star_rank_tieba where  date <= \"%s\" AND date>=\"%s\" ",sdate-1,sdate-7)
#部落与真实姓名对比
namel=dbGetQuery(conn,'select qq,tname from qq_bl_name')
star.list= dbGetQuery(conn,ss)
#粉丝规模
ss2 =sprintf("select  tieba,gzsl from tieba_hyd  where  sdate <= '%s' AND sdate>='%s' ",sdate-1,sdate-7) 
fan_scale=dbGetQuery(conn,ss2)

namelist= dbGetQuery(conn, "SELECT * FROM star_zh_copy")
#百科鲜花
ssfl=sprintf("select  name,score from baike_rank  where  date <= '%s' AND date>='%s' ",sdate-1,sdate-7) 
fl=dbGetQuery(conn,ssfl)
dbDisconnect(conn)
min.max.nor=function(x){x_min<-min(x)
x_max<-max(x)
x=(x-x_min)/(x_max-x_min)
return (x)}
starPoint<-function(x){
  y=vector()
  x[is.na(x)]=0
  x[is.nan(x)]=0
  y[order(x,decreasing =T)[1:30]]=min.max.nor(x[order(x,decreasing =T)[1:30]])*10+90
  y[order(x,decreasing =T)[31:100]]=min.max.nor(x[order(x,decreasing =T)[31:100]])*10+80
  y[order(x,decreasing =T)[101:200]]=min.max.nor(x[order(x,decreasing =T)[101:200]])*10+70
  y[order(x,decreasing =T)[201:400]]=min.max.nor(x[order(x,decreasing =T)[201:400]])*10+60
  y[order(x,decreasing =T)[400:length(x)]]=min.max.nor(x[order(x,decreasing =T)[400:length(x)]])*60
  y=round(y,digits = 2)
  return(y)
}

#影响力 微博指数+百度指数
star.list2=data.frame(weibo_zs=tapply(star.list$weibo_zs,as.factor(star.list$name),FUN = mean,na.rm=TRUE),
                      baidu_zs=tapply(star.list$baidu_zs,as.factor(star.list$name),FUN = mean,na.rm=TRUE),
                      tiezi=tapply(star.list$bh_tiezi,as.factor(star.list$name),FUN = mean,na.rm=TRUE))
star.list2$name=tolower(rownames(star.list2))
star.list2$weibo=starPoint(star.list2$weibo_zs)
star.list2$baidu=starPoint(star.list2$baidu_zs)
star.list2$影响力=round((star.list2$weibo+star.list2$baidu)/2,digits = 2)
#粉丝活跃 部落帖子+贴吧帖子(0.3+0.7)

#部落数据
conn <- dbConnect(MySQL(), dbname = "qq",username="root", password="root",host="10.0.5.19",port=3306)
ss_bl =sprintf("select *  from qq_bl where  sdate >= \"%s\" ",sdate-7)
bl= dbGetQuery(conn,ss_bl)
dbDisconnect(conn)


bl2=data.frame(charm=tapply(bl$star_charm_count,as.factor(bl$name),FUN = mean,na.rm=TRUE),
                              fans=tapply(bl$fans,as.factor(bl$name),FUN = mean,na.rm=TRUE),
                              tiezi=tapply(bl$bh_pls,as.factor(bl$name),FUN = mean,na.rm=TRUE))
bl2$name=rownames(bl2)
namel[which(namel$tname==""),]$tname=namel[which(namel$tname==""),]$qq

            ##部落帖子
bl2$name=tolower(bl2$name)
namel$tname=tolower(namel$tname)
namel$qq=tolower(namel$qq)
star.list2$bl_tiezi=0
star.list2$charm=0
star.list2$fans=0
bl2=merge(bl2,namel,by.x="name",by.y = "qq" )
for(n in 1:nrow(star.list2)) star.list2[n,]$bl_tiezi=max(bl2[which(bl2$tname==star.list2[n,]$name),]$tiezi)
for(n  in 1:nrow(star.list2)) star.list2[n,]$charm=max(bl2[which(bl2$tname==star.list2[n,]$name),]$charm)
for(n  in 1:nrow(star.list2)) star.list2[n,]$fans=max(bl2[which(bl2$tname==star.list2[n,]$name),]$fans)

star.list2[is.infinite(star.list2$charm),]$charm=0
star.list2[is.infinite(star.list2$bl_tiezi),]$bl_tiezi=0
star.list2[is.infinite(star.list2$fans),]$fans=0
star.list2$tiezi2=starPoint(star.list2$tiezi)
star.list2$bl_tiezi2=starPoint(star.list2$bl_tiezi)
star.list2$活跃度=round(star.list2$tiezi2*0.65+star.list2$bl_tiezi2*0.35,2)




#粉丝壕值  部落热度+鲜花榜
star.list2$baike=0
fl$name=tolower(fl$name)
for(n  in 1:nrow(star.list2)) star.list2[n,]$baike=max(fl[which(fl$name==star.list2[n,]$name),]$score)

star.list2[is.infinite(star.list2$baike),]$baike=0
star.list2$baike_fl=starPoint(star.list2$baike)
star.list2$charm_p=starPoint(star.list2$charm)

star.list2$壕值=round(star.list2$baike_fl*0.5+star.list2$charm_p*0.5,2)


#粉丝规模  部落人数+贴吧人数
star.list2$bl_renshu=starPoint(star.list2$fans)
fan2=data.frame(num=tapply(fan_scale$gzsl,as.factor(fan_scale$tieba),FUN = mean,na.rm=TRUE))
fan2$tieba=rownames(fan2)
fan2$tieba=tolower(fan2$tieba)
namelist$tieba=tolower(namelist$tieba)
namelist$name=tolower(namelist$name)
fan3<- merge(namelist,fan2,by="tieba")
fan3=fan3[c('name','num')]
star.list2$num=0
for(n in 1:nrow(star.list2)) star.list2[n,]$num=max(fan3[which(fan3$name==star.list2[n,]$name),]$num)

star.list2$fan_bl=starPoint(star.list2$fans)
star.list2$fan_t=starPoint(star.list2$num)
star.list2$粉丝规模=round((star.list2$fan_t+star.list2$fan_bl)/2,2)


filename=paste0("C:\\Users\\Administrator\\Desktop\\xx\\",sdate,".xlsx")


star.list3=star.list2[c('name','影响力','粉丝规模','活跃度','壕值')]
star.list3$四维平均=round((star.list3$粉丝规模+star.list3$影响力+star.list3$活跃度+star.list3$壕值)/4,2)
dataxx=star.list3[order(star.list3$四维平均,decreasing = T),][1:500,]
write.xlsx(dataxx,filename,sheetName="四维平均",append=T,row.names = F)
dataxx=star.list3[order(star.list3$影响力,decreasing = T),][1:500,]
write.xlsx(dataxx,filename,sheetName="影响力",append=T,row.names = F)
dataxx=star.list3[order(star.list3$粉丝规模,decreasing = T),][1:500,]
write.xlsx(dataxx,filename,sheetName="粉丝规模",append=T,row.names = F)
dataxx=star.list3[order(star.list3$活跃度,decreasing = T),][1:500,]
write.xlsx(dataxx,filename,sheetName="活跃度",append=T,row.names = F)
dataxx=star.list3[order(star.list3$壕值,decreasing = T),][1:500,]
write.xlsx(dataxx,filename,sheetName="壕值",append=T,row.names = F)