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

### Create Diagnostic plots
diagnostic_plots <- function(File, Model) {
  svglite(here("Figures", File), width = 10,
          height = 8,
          scaling = 1.3)
  par(mfrow=c(2,2))
  DiaPlots <- (plot(Model,1) | plot(Model,2)) / (plot(Model,3) | plot(Model,5))
  dev.off()
}

## create a boxplot
boxplot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Title){
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis, fill = Xaxis)) +
    geom_boxplot() +
    geom_beeswarm(alpha = 0.6, size = 0.75, corral.width = 0.5) +
    scale_fill_manual(values = c("#E89494", "#A4DFE4", "#8082E4", "#F2D16F", "#EC89E4")) +
    theme_bw() +
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