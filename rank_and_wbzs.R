library(RMySQL)
date1=(Sys.Date())-1

conn <- dbConnect(MySQL(), dbname = "test", username="root", password="root",host="10.0.5.19",port=3306)
ss = sprintf("SELECT * FROM tieba_hyd where sdate=\"%s\"",as.Date(date1))
datax= dbGetQuery(conn,ss)
ss2 = sprintf("SELECT * FROM  tieba_zhisu where sdate=\"%s\"",as.Date(date1))
tieba_zhishu= dbGetQuery(conn, ss2)

tieba_zhishu_today=tieba_zhishu[which(tieba_zhishu$sdate==as.Date(date1)),]
data2= dbGetQuery(conn, "SELECT * FROM star_zh_copy")
names(datax)=c("uuid","tieba","date","类别","榜单关注数","榜单签到数","type","签到数量","主题数量","关注数量","帖子数量","签到变化量","主题变化量","关注变化量","帖子变化量","update","月粉丝变化量","月帖子变化数","月主题变化数")
datax=datax[,-c(1,4,5,6,9,10,11,12,13,16,17,18,19)]
total<- merge(datax,data2,by="tieba")
total<- merge(total,tieba_zhishu_today,by="tieba")
total2=total[-which((total$zaishi=="否")|(total$gf=="是")),]
total2=total2[,c(1:6,8,28)]
total2[which(is.na(total2$zs)==T),]$zs=0

##total2=total2[-c(655),]



nmax=function(x,n){
  x=as.numeric(x)
  x=x[order(x, decreasing=TRUE)]
  return(x[1:n])
  
}

min.max.nor=function(x){x_min<-min(x)
x_max<-max(x)
x=(x-x_min)/(x_max-x_min)
return (x)}

x=min.max.nor(total2[,c(4,5,6,8)])*100
star_var=c(var(min.max.nor(nmax(total2[,4],100))),var(min.max.nor(nmax(total2[,5],100))),var(min.max.nor(nmax(total2[,6],100))),var(min.max.nor(nmax(total2[,8],100))))
sum_var=sum(star_var)
point=(star_var[1]/sum_var)*x[,1]+(star_var[2]/sum_var)*x[,2]+(star_var[3]/sum_var)*x[,3]+(star_var[4]/sum_var)*x[,4]*0.6
#粉丝网指数
total2$point=point

total2$fensiIndex=round(total2$point*500)
#特殊
total2[which(total2$name.x=='尼坤'),]$point =1.4*total2[which(total2$name.x=='尼坤'),]$point

rank1=vector()
rank1[order(point,decreasing = T)]=1:length(total2[,1])
total2=data.frame(total2,rank1)
names(total2)[11]="rank"
total2$degree=0
total2[which(total2$rank<=30),]$degree="S"
total2[which((total2$rank>30)&(total2$rank<=100)),]$degree="A"
total2[which((total2$rank>100)&(total2$rank<=300)),]$degree="B"
total2[which((total2$rank>300)&(total2$rank<=500)),]$degree="C"
total2[which(total2$rank>500),]$degree="D"


##分类排名
rankx=vector()
rank2=vector()
rankx[order(total2[which(total2$type.x=="内地"),]$point,decreasing = T)]=1:length(which(total2$type.x=="内地"))
rank2[which(total2$type.x=="内地")]=rankx
rankx=vector()
rankx[order(total2[which(total2$type.x=="港台"),]$point,decreasing = T)]=1:length(which(total2$type.x=="港台"))
rank2[which(total2$type.x=="港台")]=rankx
rankx=vector()
rankx[order(total2[which(total2$type.x=="韩国"),]$point,decreasing = T)]=1:length(which(total2$type.x=="韩国"))
rank2[which(total2$type.x=="韩国")]=rankx
rankx=vector()
rankx[order(total2[which(total2$type.x=="日本"),]$point,decreasing = T)]=1:length(which(total2$type.x=="日本"))
rank2[which(total2$type.x=="日本")]=rankx
rankx=vector()
rankx[order(total2[which(total2$type.x=="欧美"),]$point,decreasing = T)]=1:length(which(total2$type.x=="欧美"))
rank2[which(total2$type.x=="欧美")]=rankx
total2$rankIncategroy=rank2
dbDisconnect(conn)

#写数据库
conn <- dbConnect(MySQL(), dbname = "test", username="root", password="root",host="10.0.5.19",port=3306)

blacklist=dbGetQuery(conn,"select * from blacklist")

for(x in 1:length(total2[,1]))
{
  query = sprintf("INSERT INTO star_rank_tieba(name,rank,degree,date,type,rankInCategory,fensiIndex,baidu_zs,bh_guanzhu,qdsl,bh_tiezi) values(\"%s\",%d,\"%s\",\"%s\",\"%s\",%d,%d,%d,%d,%d,%d)",total2[x,]$name.x,total2[x,]$rank,total2[x,]$degree,total2[x,]$date,total2[x,]$type,total2[x,]$rankIncategroy,total2[x,]$fensiIndex,
                  total2[x,]$zs,total2[x,]$关注变化量,total2[x,]$签到数量,total2[x,]$帖子变化量)
  dbSendQuery(conn,query)}
blacklist=dbGetQuery(conn,"select *from blacklist")
dbDisconnect(conn)





########百度指数加进去dia


conn <- dbConnect(MySQL(), dbname = "test", username="root", password="root",host="10.0.5.19",port=3306)
ss = sprintf("SELECT * FROM wb_qs_zs where sdate=\"%s\"",as.Date(date1-1))
data3= dbGetQuery(conn, ss)
data3$weibo_zs=data3$pc+data3$mobile
total2$name.x=tolower(total2$name.x)
total2$name.x
total3=merge(total2,data3[,c(3,8)],by.x  ="name.x",by.y = "name")

total3=total3[order(total3$rank,decreasing=F),]
for(x in 1:nrow(total3)){
  for(n in 1:nrow(blacklist)){
    
    if (total3[x,]$name.x==blacklist[n,1])
    {
      total3[x,]$weibo_zs=mean(c(total3[x-1,]$weibo_zs,total3[x-2,]$weibo_zs,total3[x+1,]$weibo_zs,total3[x+2,]$weibo_zs))
      print(total3[x,]$name.x)
      break
    }
  }
}

for(x in 1:length(total3[,1]))
{
  query = sprintf("UPDATE star_rank_tieba set weibo_zs= %d where name='%s'AND date ='%s'",round(total3[x,]$weibo_zs),total3[x,]$name.x, as.Date(date1))
  dbSendQuery(conn,query)}

dbDisconnect(conn)