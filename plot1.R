plot1 <- function(profile1,test,profile2,train,variable){
  ggplot() +
    geom_line(data = profile1, aes(x = x, y = y, color = test), size = 1, linetype = "solid", alpha = 0.7) +
    geom_line(data = profile2, aes(x = x, y = y, color = train), size = 1, linetype = "solid", alpha = 0.7) +
    # Renk körleri için uygun renkler
    scale_color_manual(values = c( "#CC79A7",  "#009E73"),
                       labels = c(test,train)) +
    ggtitle("") +
    labs(y = "Ortalama Tahmin", x = variable, color = " ") +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times New Roman", face = "bold", size = 13),
      plot.subtitle = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.text = element_text(family = "Times New Roman", size = 10),
      legend.title = element_text(family = "Times New Roman", size = 8),
      legend.text = element_text(family = "Times New Roman", size = 10),
      legend.position = "top"
    )
}
