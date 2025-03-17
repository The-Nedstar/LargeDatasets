###########################################
# Functions for the Large Dataset assignment
# author: anonymous
###########################################

### creation of a histogram
histogram <- function(Data, Xaxis, Xtitle, Title, BW, Line, File){
  ## defining the histogram
  temp <- ggplot(Data, aes(x = Xaxis)) +
    geom_histogram(aes(fill = as.factor(floor(..x.. / BW) %% 2)), # alternative colours
                   binwidth = BW, boundary = 1, # ensuring it is split at the value 1
                   colour = "black", size = 0.4) +
    (if (Line == TRUE) { # only carrying out if specified
      geom_vline(xintercept = 1, colour = "red", 
                 size = 1.3, linetype = "21")}) + # creates a vertical line at the value 1
    scale_fill_manual(values = c("#707070", "#555555")) + # specifying alternating colours
    theme_bw()+
    theme(
      legend.position = "none", # removing the legend
      axis.title = element_text(size = 14),
      title = element_text(size = 14)
    ) +
    xlab(Xtitle) +
    ylab("Frequency") +
    ggtitle(Title)
  ## saving the graph as a .png
  png(filename = here("Figures", File), width = 25,
      height = 16,
      units = "cm", res = 400)
  print(temp)
  dev.off()
}

### create a scatterplot
scatterplot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Title, Line, File){
  ## creating the scatterplot
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis)) +
    geom_point(colour = "darkblue")+
    (if (Line == TRUE) { # Only running if specified
      geom_smooth(method = "lm", se = TRUE, linetype = "solid", 
                  colour = "darkred")}) + # creating a line defined by a generalised linear model
    theme_bw()+
    theme(
      legend.position = "none", # removing the legend
      axis.title = element_text(size = 14),
      title = element_text(size = 14)
    ) +
    xlab(Xtitle) +
    ylab(Ytitle) +
    ggtitle(Title)
  ## saving as a .png
  png(filename = here("Figures", File), width = 25,
      height = 16,
      units = "cm", res = 400)
  print(temp)
  dev.off()
}

### Create Diagnostic plots
diagnostic_plots <- function(File, Model) {
  ## saving plot as an SVG
  png(filename = here("Figures", File), width = 25,
      height= 20,
      units = "cm", res = 400)
  ## creating a multi plot
  # defining multi plot layout
  par(mfrow=c(2,2))
  # combining each diagnostic plot into one multi plot
  DiaPlots <- (plot(Model,1) | plot(Model,2)) / (plot(Model,3) | plot(Model,5))
  dev.off()
}

### create a boxplot
boxplot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Title, File){
  ## defining the boxplot
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis, fill = Xaxis)) +
    geom_boxplot() +
    geom_beeswarm(alpha = 0.6, size = 0.75, corral.width = 0.5) +
    scale_fill_manual(values = c("#E89494", "#A4DFE4", "#8082E4",
                                 "#F2D16F", "#EC89E4")) + # defining colours
    theme_bw() +
    theme(
      legend.position = "none", # removing the legend
      axis.title = element_text(size = 14),
      title = element_text(size = 14)
    ) +
    xlab(Xtitle) +
    ylab(Ytitle) +
    ggtitle(Title)
  ## saving as a .png
  png(filename = here("Figures", File), width = 25,
      height = 16,
      units = "cm", res = 400)
  print(temp)
  dev.off()
}