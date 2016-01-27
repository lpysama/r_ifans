#SELECT sum(fensicoin),memberid FROM fs_fb_log WHERE memberid NOT in (SELECT memberid FROM fs_m_info) GROUP BY memberid ORDER BY sum(fensicoin) DESC
conn <- dbConnect(MySQL(), dbname = "app_ifensi_com", username="ifensi_com", password="fans_v2",host="10.0.6.55",port=3306)
datasheet= dbGetQuery(conn,"SELECT sum(fensicoin),memberid FROM fs_fb_log WHERE memberid NOT in (SELECT memberid FROM fs_m_info) GROUP BY memberid ORDER BY sum(fensicoin) DESC")

dbDisconnect(conn)

#粉丝报告选人
filed="C:\Users\Administrator\Desktop\xx\到底"
xx=vector()
num=11
for(n in 1:length(dir(filed,pattern = "\\.xlsx",full.names = T)))
{
  for(m in 1:5)
  {
    xxxx=read.xlsx2(dir(filed,pattern = "\\.xlsx",full.names = T)[n],sheetIndex = m,startRow=2, endRow=num, colIndex=1,header = F)
    xxxx=as.character(xxxx$X1)
    xx=append(xx,xxxx)
  }
}

write.xlsx(as.data.frame(table(xx)),"C:\\Users\\Administrator\\Desktop\\ddd2.xls")