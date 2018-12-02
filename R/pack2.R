#' install.packages & library
#'
#' mom_gamma function will calculate mom estimate for the shape parameter "k" and the scale parameter "theta" by using given sample.
#'
#'  @param package is package names.
#'  @return package & library
#'  @examples #' pack2(package=c('sp','plyr'))
#'  @export
pack2=function(package){

new.packages <- package[!(package %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in package)
  library(i,character.only = T)
}
