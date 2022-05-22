#' scatter
#'
#' @param genticmap
#' @param len Number of gene fragment groups
#' @param num Gene fragment number
#'
#' @return Scatter plot of genetic map and physical map
#' @export
#'
#' @examples
scatter <- function(genticmap,len,num) {
  genetic = data.frame(geneticMap %>% mutate(differ = V2-V3))##计算出连接两图谱线段倾斜程度
  chr =  vector(mode = "list", length = len)
  for(i in 1:9){
    y = paste("Chr0", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  for(i in 10:len){
    y = paste("Chr", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}

  gs <- list(NULL)
  for (i in 1:len){
    gs[[i]] = ggplot(data = chr[[i]] , aes(x = V2, y = V3)) +
      geom_point(show.legend = F , col = "steelblue2" , alpha = 0.5)+
      geom_smooth(col = "steelblue2" , method = "lm" , se = FALSE)+
      labs(x = "Map1",y = "Map2", title = "the relationship between MAP1 and MP2")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5,size = 16),
            axis.title = element_text(size = 14))
  }

  return(gs[[num]])
}
