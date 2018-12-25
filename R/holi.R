#' holi data generate
#'
#'
#' @examples holi(start='2017-12-25',end='2018-09-30')
#'  @export
holi=function(start='2017-12-25',end='2018-09-30',port1=4565L,port2=4566L){
  dateData=data.frame(date=seq(as.Date(start),as.Date(end),1),week=weekdays(seq(as.Date(start),as.Date(end),1))
                      ,mon=format(seq(as.Date(start),as.Date(end),1),format='%m')
                      ,stringsAsFactors = F)

  dateData$Lunar<-NULL
  #######
  if(!require('RSelenium')){
    install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
    install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
    install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
  };library(RSelenium)

  pack2(c('rvest','httr','stringr','RCurl','XML','progress'))

    pJS <<- wdman::phantomjs(port = port1)
    eCaps <<- list(
      chromeOptions = list(
        prefs = list("profile.default_content_settings.popups" = port2,
                     "download.prompt_for_download" = FALSE)
      )
    )
    rD <<- rsDriver(extraCapabilities = eCaps)
    remDr <<- rD$client
  remDr$open()
    url='https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query=%EC%9D%8C%EB%A0%A5%EB%B3%80%ED%99%98&oquery=2000%EB%85%84%EB%8F%84+%EA%B3%B5%ED%9C%B4%EC%9D%BC&tqi=T2TSGspySDwssspb2kossssstuo-354401'
  remDr$navigate(url)
  message('Lunar data product')
  pb <- progress_bar$new(total = nrow(dateData))
  date<-remDr$findElement(using='css selector',value='input.input_txt_out')
  for(i in 1:nrow(dateData)){
    pb$tick()
    date$clearElement()
    date$sendKeysToElement(list(gsub('-','.',dateData$date[i]), key = 'enter'))
    date2=remDr$findElements(using='css selector', value='div.set_date img')
    date2[[1]]$clickElement()
    remDr$findElement(using='css selector',value='strong.t_point')
    source<-remDr$getPageSource()
    del=read_html(source[[1]])%>%html_nodes(css='strong.t_point')%>%html_text()
    del=str_extract_all(del[1],'[0-9]+')[[1]]
    ifelse(nchar(del[2])==1,del[2]<-paste0('0',del[2]),del[2]<-del[2])
    ifelse(nchar(del[3])==1,del[3]<-paste0('0',del[3]),del[3]<-del[3])
    dateData$Lunar[i]=paste(del,sep='-',collapse = '-')
  }
  try(silent = T,{
  remDr$close()
  pJS$stop()
  })
  #######
  holiday=T
  if(holiday==T){
  dateData$holiday<-0
  dateData[sort(c(which(substr(dateData$Lunar,6,10)%in%c('04-08'))
                  ,which(substr(dateData$date,6,10)%in%c('01-01','03-01','05-05','06-06','08-15','10-03','10-09','12-25'))
                  ,which(substr(dateData$Lunar,6,10)%in%c('01-01'))-1
                  ,which(substr(dateData$Lunar,6,10)%in%c('01-01'))
                  ,which(substr(dateData$Lunar,6,10)%in%c('01-01'))+1
                  ,which(substr(dateData$Lunar,6,10)%in%c('08-15'))-1
                  ,which(substr(dateData$Lunar,6,10)%in%c('08-15'))
                  ,which(substr(dateData$Lunar,6,10)%in%c('08-15'))+1)),'holiday']<-1

  dateData[dateData$week%in%sort(unique(dateData$week))[6:5],'holiday']<-1
  dateData$day<-dateData$week
  }
  data=dateData

   k=1
  kk=1
  data$week=NA

    tryCatch({
      while(sum(is.na(data$week))!=0){
      while(weekdays(data$date[k])!=sort(unique(data$day))[6])k=k+1
             data$week[kk:k]<-as.numeric(substr(data$date[k],9,10))%/%7+1
             k=k+1
             kk=k
      }
             },
             error=function(e){
    data[is.na(data$week),'week']<<-data[max(which(!is.na(data$week))),'week']+1
    })
  dateData=data
return(dateData[,c('date','Lunar','holiday','day','week')])

}
# setwd('D:/package/kma2')
# devtools::document()
