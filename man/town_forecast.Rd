% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/town_forecast.R
\name{town_forecast}
\alias{town_forecast}
\title{town forecast data download}
\usage{
town_forecast(dir, year, city_index, start_month, end_month,
  id = "qkdrk777777@naver.com", pw = "whckdwp1!@", port1 = 4502L,
  port2 = 4503L, port3 = 4567L)
}
\description{
The function is to store the neighborhood forecast data in a designated path.
}
\details{
@param year is must be a year larger than 2008.

@param start_month If start_month is year=2008, start_month has a character value between 10 and 12,
 otherwise satr_month can have a character value between 01 and 12.
 @param City_index is an index to the list of citydata2.For example, if the city_index=1 is download by Seoul data.

@return
}
\examples{
 citydata2[12]
 for( year in 2008:2018){
  ifelse(year==2008,start_month<-paste0(10),start_month<-'01')
  ifelse(year==2018,end_month<-10,end_month<-12)
  town_forecast(dir="D:/dir",year=year,city_index=12,start_month =start_month,end_month = end_month)}
 @export
}
