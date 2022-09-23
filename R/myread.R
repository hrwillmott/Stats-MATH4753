#' @title myread
#'
#' @param csv .CSV file name
#' @param dird directory to csv files
#' @importFrom utils read.table
#'
#' @return imports a csv file into r
#' @export
#'
#' @examples
#' \dontrun{myread("DDT.csv")}
myread=function(csv,dird){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
