#' outlier
#'
#' @param genticmap
#' @param len Number of gene fragment groups
#' @param num Gene fragment number
#' @return colinear comparison of genetic and physical maps ,T test and Error mark
#' @export
#'
#' @examples
outlier <- function(genticmap,len,num) {
  genetic = data.frame(geneticMap %>% mutate(differ = V2-V3))##计算出连接两图谱线段倾斜程度
  data = geneticMap%>% gather(2:3, key = "location", value = "place")
  chr =  vector(mode = "list", length = len)
  for(i in 1:9){
    y = paste("Chr0", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}
  for(i in 10:len){
    y = paste("Chr", i, sep = "")
    chr[[i]] = filter(genetic,V1 == y)}

  ch = vector(mode = "list", length = len)
  for(i in 1:9){
    y = paste("Chr0", i, sep = "")
    ch[[i]] = filter(data,V1 == y)}
  for(i in 10:len){
    y = paste("Chr", i, sep = "")
    ch[[i]] = filter(data,V1 == y)}
  ##分离20组基因片段两图片的位置信息

  df=  vector(mode = "list", length = len)
  dff=  vector(mode = "list", length =len)
  newdate = vector(mode = "list", length = len)
  outlier_values = vector(mode = "list", length = len)
  outdata = vector(mode = "list", length = len)
  row_name = vector(mode = "list", length = len)
  chr1  = vector(mode = "list", length = len)
  t = vector(mode = "list", length = len)
  box_picture  =  list(NULL)
  cam_picture = list(NULL)

  for(j in 1:len){
    df[[j]] = select(chr[[j]],differ)
    ##选取某一段基因组中的df值
    N =nrow(df[[j]])-1
    ##提取某一段基因的differ行数
    for(i in 1:N){
      df[[j]][i,1] = df[[j]][i,1] - df[[j]][i+1,1]
      dff[[j]] = data.frame(df[[j]][-c(N+1),1])
    }
    ##创建某一段基因组的dff值片段
    rowdate = c(1:N)
    newdate[[j]] = data.frame(dff[[j]],rowdate)
    ##编号基因标记点
    outlier_values[[j]] = data.frame(boxplot.stats(newdate[[j]][,1] )$out)
    #### 检测出异常值
    n = nrow(outlier_values[[j]])
    ###统计异常值个数
    outdata[[j]] = as.data.frame(matrix(nrow=n,ncol=2))
    for(i in 1:n){
      outdata[[j]][i,] = newdate[[j]][which(newdate[[j]][,1]==outlier_values[[j]][i,1]),]
    }
    ##检测出异常值编号
    row_name[[j]] = data.frame(outdata[[j]]$V2)
    ##提取异常值编号
    box_picture[[j]]  = ggplot(data = dff[[j]],aes( y =  df..j....c.N...1...1.) ) + geom_boxplot(show.legend = F)+labs(y = "")
    ##箱线图观测dff异常值
    chr1[[j]] = as.data.frame(matrix(nrow=(N+1)*2,ncol=1))
    ##构建异常值标注列
    for(i in 1:n){
      m =as.numeric( row_name[[j]][i,1])
      chr1[[j]][m,1] = "F"
      chr1[[j]][m+N+1,1] = "F"
    }
    ##异常值标注列进行标注
    ch[[j]] =  data.frame(ch[[j]],chr1[[j]])
    ##原数据框中添加异常值标注列
    cam_picture[[j]] = ggplot(data = ch[[j]],aes(x = location, y = place, group = V4)) +
      geom_point(aes(col = V1.1),
                 show.legend = F)+
      geom_line(aes(col = V1.1),show.legend = F)

    t[[j]] = dff[[j]]$df..j....c.N...1...1.%>%t.test()
    #遗传图谱与物理图谱折线异常值t测验
  }
  return(box_picture[[num]])
  return(cam_picture[[num]])
  return(t[[num]])
}
