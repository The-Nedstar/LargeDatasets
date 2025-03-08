###########################################
# Functions for the Large Dataset assignment
# author: anonymous
###########################################

## creation of a histogram
histogram <- function(Data, Xaxis, Xtitle, Title, BW, Line){
  temp <- ggplot(Data, aes(x = Xaxis)) +
    geom_histogram(aes(fill = as.factor(floor(..x.. / BW) %% 2)),
                   binwidth = BW, boundary = 1, 
                   colour = "black", size = 0.4)+
    (if (Line == TRUE) {
      geom_vline(xintercept = 1, colour = "red", 
                 size = 1.3, linetype = "21")
    }) +
    scale_fill_manual(values = c("#707070", "#606060")) +
    theme_bw()+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 14),
      title = element_text(size = 14)
    ) +
    xlab(Xtitle) +
    ylab("Frequency") +
    ggtitle(Title)
  return(temp)
}

## create a scatterplot
scatterplot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Title, Line){
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis)) +
    geom_point(colour = "darkblue")+
    (if (Line == TRUE) {
      geom_smooth(method = "lm", se = FALSE, linetype = "solid", colour = "darkred")
    }) +
    theme_bw()+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 14),
      title = element_text(size = 14)
    ) +
    xlab(Xtitle) +
    ylab(Ytitle) +
    ggtitle(Title)
  return(temp)
}