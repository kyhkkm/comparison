#' camp
#'
#' @param genticmap
#'
#' @return Colinear comparison of genetic and physical maps
#' @export
#'
#' @examples
camp <- function(genticmap) {
  genticmap%>%mutate(V1 = str_replace_all(V1,"Cmo_Chr","Chr"))%>%
    mutate(V4 = paste(V1,V3,sep = "_")) %>%
    mutate( V3 = V3/100000) %>% gather(2:3, key = "location", value = "place")%>%
    ggplot(aes(x = location, y = place, group = V4)) +
    geom_point(aes(col = V1),
               show.legend = F)+
    geom_line(aes(col = V1),
              show.legend = F)+
    facet_grid(.~V1,scales = "free")+
    theme(panel.spacing = unit(0.05,"lines"),
          strip.background = element_blank(),
          strip.placement = "bottom",
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}
