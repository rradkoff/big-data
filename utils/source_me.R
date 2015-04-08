# Creates the plotOpts data frame, and an output directory based on the current working
# directory.
plotOpts <- data.frame(Prefix = "output/", Width = 6, Height = 4, Units = "in",
                       Res = 150, PointSize = 12, stringsAsFactors = FALSE)

# Source the plot utilities
source('plot_utils.R')
