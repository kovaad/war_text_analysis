##############################
##   Quantitext analysis   ##
##   Kovács Ádám József   ##
###########################


# preparatory steps -------------------------------------------------------

#clear environment
rm(list = ls())

#load packages
devtools::install_github("poltextlab/HunMineR")

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr,tidyverse, quanteda, quanteda.textstats, ggrepel,text2vec, topicmodels,ggfortify, kableExtra,ggwordcloud, lubridate, tidytext )

#check out custom theme
source("theme_adam.R")




