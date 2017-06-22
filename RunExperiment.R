setwd("/Users/christophermealey/Desktop/R scripts")
library(rmetasim)
library(foreach)
library(psych)
source("RDlandscape.R")
source("diversity.R")
source("landscape.sample.R")
source("Ne_sample.R")
source("DiversityStats.R")
source("Compile diversity files.R")
source("Sweepstakes.R")
source("RunReplicate.R")

times(50) %do% run.replicate()

s=run.replicate()
#C:\\Users\\dnr.chris.mealey\\Desktop\\Results\\He\\Default