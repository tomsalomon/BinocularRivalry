library("ggplot2")
library("gridExtra")

ggcorplot = function(my_data){
  n_vars = ncol(my_data)
  new_col_names = paste0("x",1:n_vars)
  old_col_name = gsub("\n"," ",colnames(PDS_data))
  colnames(my_data) = new_col_names
  vars = expand.grid(1:n_vars,1:n_vars)
  fill_colors = c('blue4','royalblue','red4','#FF6666')
  for(i in 1:nrow(vars)){
    xi = vars[i,1]
    yi = vars[i,2]
    dat_tmp = my_data[,c(xi,yi)]
    x_lab = old_col_name[xi]
    y_lab = paste0('\n',old_col_name[yi])
    colnames(dat_tmp) = c("x1","x2")
    y_lim = c(min(dat_tmp$x2), max(dat_tmp$x2))*1.2
    cor_t = cor.test(dat_tmp$x1,dat_tmp$x2)
    p = cor_t$p.value
    dat_tmp$col = fill_colors[xi]
    if (p<.001) {sig = "***"} else if (p<.01) {sig = "**"} else if (p<.05) {sig = "*"} else if (p<.1) {sig = "#"} else {sig = ""}
    r_text = paste(round(cor_t$estimate,digits = 2),sig)
    #cor.test(dat_tmp)
    options(warn=-1)
    if (xi == yi) {
      assign(paste0("p", i), 
             qplot(data=dat_tmp, x=x1, geom = "histogram", bins = 10, fill=I(fill_colors[xi])) +
               labs(x="", y="\n") + theme_bw() +
               ggtitle(paste0(xi,". ",x_lab)) +
               theme(plot.title = element_text(face="bold")) +
               geom_vline(xintercept = 0, linetype = "dashed") +
               theme(legend.position = "none",axis.text.y = element_blank(), axis.ticks.y = element_blank())
      ) } else {
        curr_color = 1 
        if (xi<=2 & yi<=2) {curr_color = 'blue4'}
        if (xi>=3 & yi>=3) {curr_color = 'red4'}
        assign(paste0("p", i), 
               qplot(data=dat_tmp, x=x1,y=x2, alpha =.5, fill = curr_color) + 
                 theme_bw() +
                 geom_point(color = curr_color, alpha = 0.4)  +
                 geom_smooth(method = 'lm', color = curr_color, fill = curr_color, alpha = 0.4)  + 
                 labs(x=x_lab, y=y_lab,title = bquote(italic(r) == .(r_text) ) ) +
                 #xlab(x_lab) + ylab(y_lab) +
                 #geom_text(aes(x = min(dat_tmp$x1)*0.5 + max(dat_tmp$x1)*0.5, y=y_lim[2]*0.9), label = r_text,color = 2) + 
                 #ggtitle(r_text) + 
                 # ylim(y_lim) +
                 theme(legend.position = "none", axis.ticks.y = element_blank() , axis.text.y=element_blank()) 
        )}    
  }
  options(warn=0)
  mylist <- mget(paste0("p",  1:nrow(vars)))
  do.call("grid.arrange", c(mylist, nrow = n_vars))
}
