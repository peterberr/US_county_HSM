# get the FA summaries by type, and population, for 2020-2060 by county

rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(stringr)

load('HSM_results/County_FloorArea.RData')
