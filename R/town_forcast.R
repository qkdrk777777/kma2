#' town forecast data download
#'
#' The function is to store the neighborhood forecast data in a designated path.
#'
#'  @param year is must be a year larger than 2008.
#'
#'  @param start_month If start_month is year=2008, start_month has a character value between 10 and 12,
#'  otherwise satr_month can have a character value between 01 and 12.
#'  @param City_index is an index to the list of citydata2.For example, if the city_index=1 is download by Seoul data.
#'
#'  @return
#'  @examples citydata2[12]
#'  for( year in 2008:2018){
#'   ifelse(year==2008,start_monthpaste0(10),start_month<-'01')
#'   ifelse(year==2018,end_month<-10,end_month<-12)
#'   town_forcast(dir="D:/dir",year=year,city_index=12,start_month =start_month,end_month = end_month)}
#'  @export
town_forcast=function(dir,year,city_index,start_month,end_month,
                      id='qkdrk777777@naver.com',pw='whckdwp1!@',port1=4502L,port2=4503L,port3=4567L){
  if(!require('RSelenium')){
    install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
    install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
    install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
  };library(RSelenium)

  pack2(c('rvest','httr','stringr','RCurl','XML','progress'))

try(silent = T,{
  pJS <<- wdman::phantomjs(port = port1)
  eCaps <<- list(
    chromeOptions = list(
      prefs = list("profile.default_content_settings.popups" = port2,
                   "download.prompt_for_download" = FALSE,"download.default_directory" = dir)
    )
  )
  rD <<- rsDriver(extraCapabilities = eCaps)
  remDr <<- rD$client
})
  #############
#
#   remDr <<- remoteDriver(port=port3, browserName = 'chrome',extraCapabilities = eCaps)
#   remDr$open()#run the driver

  setwd(dir)
  dir.create('data')

  for(year in year){
    setwd(paste0(dir,'/data'))
    dir.create(paste0(year))
    setwd(paste0('./',year))

    try(silent = T,{
      url="https://data.kma.go.kr/data/rmt/rmtList.do?code=420&pgmNo=572"
      remDr$navigate(url)

      remDr2=remDr$findElement(using='xpath',value='//*[@id="loginBtn"]')
      suppressMessages(remDr2$sendKeysToElement(list(key='enter')))

      id_=remDr$findElement(using='css selector',value='input#loginId.input-medium')
      pw_=remDr$findElement(using='css selector',value='input#passwordNo.input-medium')
      id_$sendKeysToElement(list(id))
      pw_$sendKeysToElement(list(pw))

      login=remDr$findElement(using='xpath',value='//*[@id="loginbtn"]')
      login$sendKeysToElement(list(key='enter'))
    })
    Sys.sleep(2)
    #set preiod of data

    start=remDr$findElement(using='xpath',value='//*[@id="startDt"]')
    end=remDr$findElement(using='xpath',value='//*[@id="endDt"]')
    start$sendKeysToElement(list(paste0(year)))
    end$sendKeysToElement(list(paste0(year)))
    city_n=0
    #set area
    for(city in citydata[[city_index]]){
      area=remDr$findElement(using='css selector',value='input#btnStn.selectBtn1.btn.btn-primary.VAR3_BTN')
      area$sendKeysToElement(list(key='enter'))

      Sys.sleep(2)
      area_1=remDr$findElement(using='css selector',value=paste0('span#ztree_',citydata2[[city_index]],'_switch'))
      area_1$clickElement()
      city_n=city_n+1

      Sys.sleep(2)
      area_2=remDr$findElement(using='css selector',value=paste0('span#ztree_',city,'_check'))
      area_2$clickElement()

      Sys.sleep(2)
      close0=remDr$findElements(using='class name',value='btn-close')
      close0[[3]]$clickElement()

      #set month
      st.mon=remDr$findElement(using='xpath',value='//*[@id="startMt"]')
      ed.mon=remDr$findElement(using='xpath',value='//*[@id="endMt"]')

      st.mon$sendKeysToElement(list(paste0(start_month)))
      ed.mon$sendKeysToElement(list(paste0(end_month)))

      #search
      search=remDr$findElement(using='class name',value='addBtn')
      remDr$mouseMoveToLocation(webElement = search)
      search$clickElement()
      button=NULL
      try(silent = T,{suppressMessages(
        button<<-remDr$findElement(using='css selector',value='button.buttonOK'))
      })
      if(!is.null(button)){stop('Check the date range')
        rD[['server']]$stop()
        remDr$close()
      }
      t=0
      n=as.numeric(remDr$findElement(using='class name',value='SEARCH_LIST_COUNT')$getElementText()[[1]])
      area_list2=NULL;list=list.files(dir,pattern='csv')
      while(t<n){
        date=as.character(readHTMLTable(remDr$getPageSource()[[1]])[[2]][seq(1,nrow(readHTMLTable(remDr$getPageSource()[[1]])[[2]]),2),3])
        del=gregexpr(',',as.character(readHTMLTable(remDr$getPageSource()[[1]])[[2]][seq(1,nrow(readHTMLTable(remDr$getPageSource()[[1]])[[2]]),2),2]))
        area_list=substr(as.character(readHTMLTable(remDr$getPageSource()[[1]])[[2]][seq(1,nrow(readHTMLTable(remDr$getPageSource()[[1]])[[2]]),2),2]),
                         matrix(unlist(del),byrow=T,ncol=2)[,1]+2,matrix(unlist(del),byrow=T,ncol=2)[,2]-1)

        area_list2=c(area_list2,area_list)
        del2=NULL
        down=remDr$findElements(using='css selector',value='input.btn.btn-default.DATA_DOWN_BTN')

        for(i in 1:length(down)){

          if(sum(gsub('.csv','',list.files())%in%paste0(year,'/',date[i],area_list[i],'_',names(citydata[[city_index]])[city_n]))==0){

          message(paste0(year,'/',date[i],area_list[i],'_',names(citydata[[city_index]])[city_n]))
          down[[i]]$clickElement()
          Sys.sleep(3)
          error=NULL
          try(silent = T,{
            suppressMessages(error<-remDr$findElement(using='css selector',value='button'))
            # error=remDr$findElement(using='css selector',value='button')
            error$clickElement()

            if(length(error)==1){down=remDr$findElements(using='css selector',value='input.btn.btn-default.DATA_DOWN_BTN')
            down[[i]]$clickElement()}
          })

          t=t+1
          close=NULL

          try(silent=T,{suppressMessages(close<-remDr$findElement(using='xpath',value='//*[@name=\"reqstPurposeCd\"]') )
            close$clickElement()
            if(length(close)==1){
              close2=remDr$findElements(using='css selector',value='input.btn.btn-primary')
              close2[[4]]$mouseMoveToLocation(webElement=close2[[4]])
              close2[[4]]$sendKeysToElement(list(key='enter'))
              Sys.sleep(3)
            }
          })


          write.csv(read.csv(paste0(dir,'/',setdiff(list.files(dir,pattern='csv'),list))),file=paste0(dir,'/data/',year,'/',date[i],area_list[i],'_',names(citydata[[city_index]])[city_n],'.csv'))
          file.remove(paste0(dir,'/',setdiff(list.files(dir,pattern='csv'),list)))



          if(t%%10==0){
            Sys.sleep(4)
            page=remDr$findElement(using='class name',value='next_page')
            page$clickElement()
            Sys.sleep(5)}
}else t=t+1
        }
      }

    }
  }

try(silent = T,{
  pJS$stop()
  remDr$close()
  rD[['server']]$stop()

  })

}
# setwd('D:/package/kma2')
# devtools::document()

