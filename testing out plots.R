PopPerf <- 
  as_data_frame(ComSingle$lambda) %>% 
  mutate(category = factor(
    case_when(value < 1  ~ "Decrease",
              value > 1  ~ "Increase")
  ))



row.names(PopPerf) <- row.names(ComSingle)
PopPerf <- as.factor(setNames(PopPerf$category,row.names(PopPerf)))

IUCNstatus <- as.factor(setNames(ComSingle$IUCNstatus, row.names(ComSingle)))
TreeNoLabels <- PrunedTree
TreeNoLabels$tip.label <- rep("", length(PrunedTree$tip.label))

# Define the dot colors based on PopPerf (assumed to be a character vector with "Decrease" or "Increase")
dotColors <- setNames(c("#EA0000", "#1E88E5"), c("Decrease", "Increase"))
tipDotColors <- dotColors[PopPerf]  # This maps each tip to its corresponding color

# Optionally, adjust the margins to provide extra space on the right
par(mar = c(5, 5, 5, 12))  

svglite(here("Figures", "PerfTree.svg"), width = 8,
        height = 8,
        scaling = 1.3)
# Plot the phylogenetic tree without the default tip labels
plot(PrunedTree, show.tip.label = FALSE, no.margin = FALSE)

# 1. Add colored dots at the tips using tiplabels with pch=21 (a filled circle)
tiplabels(pch = 21, bg = tipDotColors, cex = 1, offset = 0.1)

# 2. Add the IUCN status text immediately to the right of the dots
tiplabels(text = as.character(IUCNstatus),
          offset = 0.2,          # Adjust offset as needed
          adj = c(0, 0.5),       # Centers text vertically relative to the dot
          frame = "none",
          cex = 0.6,
          col = "black")

# 3. Add the species names further to the right
tiplabels(text = PrunedTree$tip.label,
          offset = 0.3,          # Increase offset so that species names appear after the IUCN text
          adj = c(0, 0.5),
          frame = "none",
          cex = 0.6,
          col = "black")

dev.off()
