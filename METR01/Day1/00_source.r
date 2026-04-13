####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and functions used to 
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# packages required ---------------------------

#packages <- c()
#install.packages(setdiff(packages, rownames(installed.packages())))
#lapply(packages, library, character.only = TRUE)

# Main functions ---------------------------


# plot settings ---------------------------
pallete = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]

theme_new <- function(base_size = 15, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 15, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}
