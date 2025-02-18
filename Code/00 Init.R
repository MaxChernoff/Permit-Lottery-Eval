# renv::init()


library('tidyverse')
library('writexl')
library('readxl')
library('janitor')
library('tidylog')

# renv::snapshot()


theme_te <- function() theme_set(theme_bw() %+replace%
                                   theme(axis.line = element_line(colour = "black"),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.background = element_blank(),
                                         legend.background = element_blank(),
                                         legend.key = element_blank(),
                                         strip.background = element_blank(),
                                         plot.background = element_blank() ))

dirs <- list()

dirs$input <- file.path(getwd(), "Raw Data")
dirs$output <- file.path(getwd(), 'Output')
