#' camp3D
#'
#' @param genticmap
#' @param len Number of gene fragment groups
#' @param num Gene fragment number
#'
#' @return Colinear comparison of maps
#' @export
#'
#' @examples
camp3D <- function(genticmap,len,num) {
  genetic = data.frame(geneticMap %>% mutate(differ = V2-V3))##计算出连接两图谱线段倾斜程度
  chr =  vector(mode = "list", length = len)
  for(i in 1:9){
    y = paste("Chr0", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  for(i in 10:len){
    y = paste("Chr", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  x1 = vector()
  y1= vector()
  z1= vector()
  source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
    N =nrow(chr[[num]])
    for (j in 1:N){
      x = c(0,0,chr[[num]][j,2],0)
      y  =c(500,chr[[num]][j,3],500,500)
      z  =c(chr[[num]][j,3],0,0,chr[[num]][j,3])
      x1 = c(x1,x)
      y1 = c(y1,y)
      z1 = c(z1,z)}
   return( scatterplot3d(x1,y1,z1,type = "l",pch =10,color = "aquamarine3",grid = FALSE,box = FALSE))
    return(addgrids3d(x1,y1,z1,grid = c("xy","xz","yz")))
}
