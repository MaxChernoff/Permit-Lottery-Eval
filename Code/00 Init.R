# renv::init()


library('tidyverse')
library('writexl')
library('readxl')
library('janitor')
library('tidylog')

# renv::snapshot()


dirs <- list()

dirs$input <- file.path(getwd(), "Raw Data")
dirs$output <- file.path(getwd(), 'Output')
