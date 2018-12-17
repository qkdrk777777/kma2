#' grid to lonlat datta form change
#'
#'
#'  @param grid_data is grid_x, grid_y combine dataframe form
#'  @return
#' @examples
#'  @export
grid2lonlat=function(grid_data){

  if(dim(grid_data)[2]!=2)  stop('please check the dimensions.')
  data=grid_data[,1:2]
  if(is.numeric(data[,1])!=T){
    grid_x=as.numeric(data[,1])
    grid_y=as.numeric(data[,2])
  }else{
    grid_x=data[,1]
    grid_y=data[,2]}
  RE = 6371.00877
  GRID = 5.0
  SLAT1 = 30.0
  SLAT2 = 60.0
  OLON = 126.0
  OLAT = 38.0
  XO =  43
  YO =  136

  DEGRAD=pi/180
  RADDEG=180/pi

  re =  RE / GRID;
  slat1 = SLAT1 * DEGRAD;
  slat2 = SLAT2 * DEGRAD;
  olon = OLON * DEGRAD;
  olat = OLAT * DEGRAD;

  sn=tan(pi*0.25+slat2*0.5)/tan(pi*0.25+slat1*0.5)
  sn=log(cos(slat1)/cos(slat2))/log(sn)
  sf=tan(pi*0.25+slat1*0.5)
  sf=((sf^sn)*cos(slat1))/sn
  ro=tan(pi*0.25+olat*0.5)
  ro=re*sf/(ro^sn)

  xn=grid_x-XO
  yn=ro-grid_y+YO
  ra=sqrt(xn^2+yn^2)

  if(sn<0.0){ra=-ra}
  alat=(re*sf/ra)^(1/sn)
  alat=2*atan(alat)-pi*.5
  data=NULL
  data=data.frame('theta'=rep(NA,nrow(grid_data)),'alon'=rep(NA,nrow(grid_data)))
  data$theta=atan(xn/yn)
  data[abs(xn)<=0,'theta']=0
  data[abs(yn)<=0,'theta']=pi*.5
  data[abs(yn)<=0&xn<0,'theta']=-data[abs(yn)<=0&xn<0,'theta']

  data$alon=data$theta/sn+olon

  lon=data$alon*RADDEG
  lat=alat*RADDEG
  return(data.frame(lon,lat))

}

# setwd('D:/package/kma2')
# devtools::document()
