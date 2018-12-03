#' change town forcast data of format.
#'
#' The function is to cange downloaded files format
#'
#'  @param dir1 = directory of save files
#'  @param City_index is an index to the list of citydata2.For example, if the city_index=1 is download by Seoul data.
#'
#'  @return
#'  @examples dir1='D:/dir/data'
#'  town_new(dir1=dir1)
#'  @export
town_new=function(dir1){
  ori=getwd()
  pack2('stringr');setwd(dir1)
  dir=list.files()[is.na(str_extract(list.files(),'\\p{Hangul}'))&list.files()%in%setdiff(list.files(),list.files()[grep('\\.|new',list.files())])]
  for(i in dir){
    t=1;dele=NULL;
  while(length(dele)!=1){
dele=area[area[,6]%in%
substr(unlist(str_extract_all(gsub('.csv','',list.files(i)),'(\\p{Hangul}.*\\p{Hangul})+')),1,
regexpr('_',unlist(str_extract_all(gsub('.csv','',list.files(i)),'(\\p{Hangul}.*\\p{Hangul})+')))-1)[t],4]
    t=t+1}


  for(j in list.files(i)){
    del=read.csv(paste0(i,'/',j),stringsAsFactors = F)[,-1]
    del$mon<-NA
    ifelse(i==2008,mon<-11:12,ifelse(i%in%c(2012,2018),mon<-1:10,mon<-1:12))
    index=c(1,which(is.na(del[,2])))

    for(k in 1:length(mon)){
      del[index[k]:index[k+1],'mon']<-mon[k]}
    #remove na
    del=na.omit(del)
    #date format , date+time format
    del$date=as.Date(paste(paste0(i,'-',ifelse(nchar(del$mon)==1,paste0('0',del$mon),del$mon),'-',ifelse(nchar(gsub(' ','',del$format..day))==1,paste0('0',gsub(' ','',del$format..day)),gsub(' ','',del$format..day)))))
    del$dts=as.POSIXct(paste(paste0(i,'-',ifelse(nchar(del$mon)==1,paste0('0',del$mon),del$mon),'-',ifelse(nchar(gsub(' ','',del$format..day))==1,paste0('0',gsub(' ','',del$format..day)),gsub(' ','',del$format..day))),paste0(substr(ifelse(nchar(del$hour)==3,paste0('0',del$hour),del$hour),1,2),':00:00')))
    #hour format change
    del$hour=as.numeric(gsub('00','',gsub(' ','',del$hour)))
    #forecast value colnames change
    del$forecast_value<-del[,4]
    #data format transform

    del=del[,c(7,6,2,3,8)]
    dir.create(paste0(i,'(new)'))
    del2=unlist(str_extract_all(gsub('.csv','',j),'(\\p{Hangul}.*\\p{Hangul})+'))
    del3=area[(area[,6]==substr(del2,1,regexpr('_',del2)-1))&area[,4]==names(citydata)[names(citydata)%in%dele],-3]
    if(nrow(del3)!=1){
      del3=del3[del3[,4]==substr(del2,regexpr('_',del2)+1,nchar(del2)),]}
    if(nrow(del3)!=1){stop('area check')}

    write.csv(cbind(del,del3),file=paste0(paste0(i,'(new)'),'/',j))
  }
  }
  setwd(org)
}
