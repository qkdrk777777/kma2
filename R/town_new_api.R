#' change town forcast api data of format.
#'
#' The function is to cange downloaded files format
#'
#'  @param category= POP,PTY, REH,SKY,T3H,UUU,VEC,VVV,WSD,WAV
#'  @param url = api url
#'  @return
#'  @example
 #  pack2(c('xml2','XML','plyr'))
 # key='XqCpVL8gGGZXQquN9iXsLtBPS6dwAgnxfWorz1IlKu77R72UPgvE7Lx1J1%2BITprn3%2BoZ0cU3IQ1JnRCpEJ%2BkZw%3D%3D'
 # url=paste0('http://newsky2.kma.go.kr/service/SecndSrtpdFrcstInfoService2/ForecastSpaceData?ServiceKey=',
 # key,paste0('&base_date=',gsub('-','',substr(Sys.time()-3600*c(23:0),1,10))[substr(Sys.time()-3600*c(23:0),12,13)%in%c('02','05','08','11','14','17','20','23')]
 # ,'&base_time=',paste0(substr(Sys.time()-3600*c(23:0),12,13)[substr(Sys.time()-3600*c(23:0),12,13)%in%c('02','05','08','11','14','17','20','23')],'00')),paste0('&nx=',unique(area[,1:2])[,1],'&ny=',unique(area[,1:2])[,2]))
 #  api_forecast(cat='T3H',url[1:2])
#'  @export
api_forecast=function(cat='T3H',url=url[1:2]){
data2=lapply(url,function(x){xmlToDataFrame(nodes=getNodeSet(xmlParse(x),'//response/body/items/item'),stringsAsFactors = F)})


data3=NULL
for(i in 1:length(data2)){
  data3=rbind(data3,data2[[i]])
}

data3=data3[data3$category%in%cat,]
data4=merge(area,data3,by.y=c('nx','ny'),by.x=colnames(area)[1:2],all.y=T)
data4$forecast=(as.numeric(data4[,13])-as.numeric(data4[,10]))/100
data4$hour=(as.numeric(data4[,10]))/100

data4=data4[,c(9,15,16,14,1,2,4:8)]
colnames(data4)[c(1,4)]=c('date','forecast_value')
return(data4)
}

