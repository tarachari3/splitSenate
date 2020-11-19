library(usedist)
library(phangorn)
library(readr)
library(tidyverse)
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
source('distFuncs.R')
#https://voteview.com/data for all data

setwd("~/Desktop/senateSplitsTree")
Sall_votes = as_tibble(read_csv("./Sall_votes_names_parties.csv") )


# Make distance matrix and nexus output for 114-116th congresses
for (i in c(114,115,116)){
  cong = i
  Sall_votes_sub  <- Sall_votes %>% filter(congress == cong)
  
  fname <- paste("./dist_",as.character(cong),"th.nex",sep="")
  
  l1DistsAll <- makeDistMat(Sall_votes_sub,fname)
}



# Get split distances for 116th congress

splitMat_116 <- read_delim("./splitWeights_tab_116th.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

splitDists_116 <- pairSplitDists(splitMat_116)

# Plot Split distances
par(cex.main=0.7)
hv <- heatmap(as.matrix(splitDists_116),scale="none",
              col=viridis(max(splitDists_116),direction = -1),
              main = "Senator Pairwise Split Distances")


# Compare dist changes from input to split-distance output --> Very small changes

percDiff <- distDiff(l1DistsAll, splitDists_116 )


# Get distances of all Senators from 'center'
centerDists <- centerDist(splitMat_116) 

plotCenterDist(centerDists)



# Plot percent diffs between L1 and NNet LS distances (Split distances)
par(mar=c(7,6,4,1)+.1)
boxplot(percDiff,labels = rownames(percDiff),
        main="Percent Difference in Pairwise L1 Dist vs Split Distances",
        xaxt="none",
        ylab = 'Percent Change (from L1 to Split)',
        cex.lab = 0.8, cex.main = 0.8,cex.axis=0.8)
axis(1, at=c(1:length(rownames(percDiff))),labels=rownames(percDiff), las=3,cex.axis=0.5)

hv <- heatmap(percDiff,scale="none",
              col=viridis(max(percDiff)),
              main = "Perc Changes in L1 Distances")

plot(as.matrix(splitDists_116 ),as.matrix(l1DistsAll ))



# Distance between non-Republican members only --> Check if Dem Primary Group still exists
cong = 116
Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)

fname <- paste("./dist_dem_",as.character(cong),"th.nex",sep="")

l1DistsDem <- makeDistMat(Sall_votes_dem,fname)


# Runs Test for Dem Primary Candidates grouping
numCands = 5
numRestAllParties = dim(as.matrix(l1DistsAll))[1] - numCands
numRestDem = dim(as.matrix(l1DistsDem))[1] - numCands

pvalAll = calcRunsTest(numCands,numRestAllParties,3)
print(pvalAll)
pvalDem = calcRunsTest(numCands,numRestDem,3)
print(pvalDem)


# Distance between Republican members only 
cong = 116
Sall_votes_rep <- Sall_votes %>% filter(congress == cong)
Sall_votes_rep  <- Sall_votes_rep %>% filter(party_code == 200)

fname <- paste("./dist_rep_",as.character(cong),"th.nex",sep="")

l1DistsRep <- makeDistMat(Sall_votes_rep,fname)


