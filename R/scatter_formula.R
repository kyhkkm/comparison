#' scatter_formula
#'
#' @param genticmap
#' @param len Number of gene fragment groups
#' @param num Gene fragment number
#'
#' @return Scatter plot of genetic map and physical map ,Regression formula and p-value
#' @export
#'
#' @examples
scatter_formula <- function(genticmap,len,num) {
  genetic = data.frame(geneticMap %>% mutate(differ = V2-V3))##计算出连接两图谱线段倾斜程度
  chr =  vector(mode = "list", length = len)
  for(i in 1:9){
    y = paste("Chr0", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  for(i in 10:len){
    y = paste("Chr", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}

  gq<- list(NULL)
  for (i in 1:len){
    gq[[i]] = ggscatter(chr[[i]] , x = "V2", y = "V3",col = "steelblue2",
                        add = "reg.line", conf.int = TRUE,
                        add.params = list(fill = "black"))+
      stat_cor(method = "pearson",
               label.x = 3, label.y = 70)+
      stat_poly_eq( aes(label = ..eq.label..),
                    formula = y~x,parse = TRUE, geom = "text",label.x = 3,label.y = 100, hjust = 0)}
  ###遗传图谱与物理图谱的共线性比较散点图与回归公式和pearson系数

  return(gq[[num]])
}
