#' scatter3D
#'
#' @param genticmap
#' @param len Number of gene fragment groups
#' @param num Gene fragment number
#'
#' @return Scatter plot of maps
#' @export
#'
#' @examples
scatter3D <- function(genticmap,len,num) {
  genetic = data.frame(geneticMap %>% mutate(differ = V2-V3))##计算出连接两图谱线段倾斜程度
  chr =  vector(mode = "list", length = len)
  for(i in 1:9){
    y = paste("Chr0", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  for(i in 10:len){
    y = paste("Chr", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  par(mfrow = c(1,2))
 return(scatterplot3d(chr[[num]]$V2,chr[[num]]$V3,chr[[num]]$V3,pch = 16,
                highlight.3d = TRUE,
                type="h",
                main = "Comparison of multiple maps"))
  return(scatter3D(chr[[num]]$V2,chr[[num]]$V3,chr[[num]]$V3,
            col = ramp.col(col = c("cyan", "blue"),
                           n = length(chr[[num]]$V3)),pch =18,
            cex = 1,  phi = 1,
            ticktype = "detailed",bty = "b2",
            main = "Comparison of multiple maps"))
}
