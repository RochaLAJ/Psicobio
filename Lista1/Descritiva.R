#require(dplyr)
#require(magrittr)
#require(ggplot2)
#require(readxl)
#require(hrbrthemes)
#require(stringr)


#Plot Helper
#hist_fill <- c('cyan', 'orange', 'red', 'grey90')
#hist_color <- c('darkblue', 'yellow', 'blue', 'black')
#hist_list <- list()
#bar_list1 <- list()
#bar_list2 <- list()


#Function
plot_descriptive <- function(matrix, plot_type, fill, color){
  if(plot_type=='histogram')
  {
    for (i in 1:ncol(matrix)) {
      histogram <- ggplot(matrix, aes_string(x=matrix[[i]])) +
        geom_histogram(binwidth=3, aes(y = ..density..), fill=fill[[i]], color=color[[i]], alpha=0.9) +
        geom_function(fun = dnorm, args = list(mean = mean(matrix[[i]]), sd = sd(matrix[[i]]))) +
        theme(plot.title = element_text(size=15)) +
        labs(y = 'Densidade') +           
        ggtitle(paste(colnames(matrix)[i])) +
        scale_x_continuous('Distribuição') +
        theme_ipsum()
      hist_list[[i]] = histogram
    }
    m1 <- marrangeGrob(hist_list, ncol = 2, nrow = 2)  
    ggsave(filename = 'HISTOGRAM.png', plot = m1, width = 10, height = 10, dpi=600)
  }
  if(plot_type=='barplot')
  {
    for (x in 1:ncol(matrix)){
      verify_subvar_quantity = length(table(matrix[x]))
      if(verify_subvar_quantity==3){
        bar1 <- ggplot(matrix) + 
          geom_bar(col='black', fill=c('red','blue','darkgreen'), aes_(x=matrix[[x]]), alpha=0.7)  +
          theme(plot.title = element_text(size=15)) +
          labs(y = 'Qtde. Absoluta', x = 'Grupos') + 
          ggtitle(paste0(colnames(categorical_vars)[x])) +
          theme_ipsum()
        bar_list1[[x]] = bar1
      }
      else if(verify_subvar_quantity==2){
        bar2 <- ggplot(matrix) + 
          geom_bar(col='black', fill=c('turquoise3'), aes_(x=matrix[[x]]), alpha=0.7) +
          theme(plot.title = element_text(size=15)) +
          labs(y = 'Qtde. Absoluta', x = 'Grupos') + 
          ggtitle(paste(colnames(categorical_vars)[x])) +
          theme_ipsum()
        bar_list2[[x]] = bar2
      }
    }
  }
  m3 <- ggarrange(plotlist=c(bar_list1[2:3], bar_list2), ncol = 3, nrow=1)  
  ggsave(filename = 'BARPLOT.png', plot = m3, width = 13, height = 8, dpi=600, bg="white") 
}

# Usage Example:
plot_descriptive(continous_vars, plot_type='histogram', fill=hist_fill, color=hist_color)
plot_descriptive(categorical_vars, plot_type='barplot')
