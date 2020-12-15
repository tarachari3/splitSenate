#Create dataframe used in analysis from voteview.com data

library(usedist)
library(phangorn)
library(readr)
library(tidyverse)
library(tidyr)
library(hash)
#install.packages("STAT")
library("STAT")
library(graphics) 
library(grDevices)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("viridis")  # Install
library("viridis") 
library(stringi)
library(pheatmap)   
library(gplots)


#https://voteview.com/data for all data
setwd("~/Desktop/senateSplitsTree")
source('distFuncs.R')
unzip(zipfile = "./data/voteData.zip", exdir = "./data")

Sall_members <- as.tibble(read_csv("./data/Sall_members.csv") )

Sall_votes <- as.tibble(read_csv("./data/Sall_votes.csv"))  

#Get real names
Sall_votes[c("name")] <- 
  lapply(Sall_votes[c("icpsr")], function(col) Sall_members$bioname[match(col, Sall_members$icpsr)])

#Get party affiliation
Sall_votes[c("party_code")] <- 
  lapply(Sall_votes[c("icpsr")], function(col) Sall_members$party_code[match(col, Sall_members$icpsr)])

class(Sall_votes)
Sall_votes <- na.omit(Sall_votes)

write.table(Sall_votes , file = "./data/Sall_votes_withPartyAndNames.csv",sep=",",row.names = FALSE)


