#' @title myread
#'
#' @param csv .CSV file name
#'
#' @return imports a csv file into r
#' @export
#'
#' @examples myread("DDT.csv")
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
