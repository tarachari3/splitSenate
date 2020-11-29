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


setwd("~/Desktop/senateSplitsTree")
source('distFuncs.R')
#https://voteview.com/data for all data


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
par(mar=c(8,4,4,1)+.1,cex.main=0.5,cex.axis=0.1)
pdf(file = "./splitDists_116.pdf",width=10,height=10)
hv <- pheatmap(as.matrix(splitDists_116),scale="none",
              col=viridis(max(splitDists_116),direction = -1),
              main = "Senator Pairwise Split Distances",
              fontsize_row = 4,fontsize_col = 5)

dev.off()


# Compare dist changes from input to split-distance output --> Very small changes

percDiff <- distDiff(l1DistsAll, splitDists_116 )


# Get distances of all Senators from 'center'
centerDists <- centerDist(splitMat_116) 

plotCenterDist(centerDists)



# Plot percent diffs between L1 and NNet LS distances (Split distances)
par(mar=c(7,6,4,1)+.1)
pdf(file = "./percDiff_l1_fromNNet.pdf",width=10,height=10)
boxplot(percDiff,labels = rownames(percDiff),
        main="Percent Difference in Pairwise L1 Dist vs Split Distances",
        xaxt="none",
        ylab = 'Percent Change (from L1 to Split)',
        cex.lab = 0.8, cex.main = 0.8,cex.axis=0.8)
axis(1, at=c(1:length(rownames(percDiff))),labels=rownames(percDiff), las=3,cex.axis=0.35)
dev.off()


corr <- cor(as.vector(splitDists_116),as.vector(l1DistsAll))
mylabel = bquote(italic(corr) == .(format(corr, digits = 3)))

pdf(file = "./l1vsplit_dists_corr.pdf",width=10,height=10)
plot(as.matrix(splitDists_116 ),as.matrix(l1DistsAll ),
     xlab='NeighborNet Distances',ylab='L1 Distances')
text(x = 500, y = 300, labels = mylabel)
dev.off()


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

# Ranking votes of Dem primary candidates by agreement with rest of party

cong = 116
Sall_votes_dem <- Sall_votes %>% filter(congress == cong)
Sall_votes_dem  <- Sall_votes_dem %>% filter(party_code != 200)


votesDem <- makeVoteMat(Sall_votes_dem)
rows <- rownames(votesDem)


votesDem_sub <- votesDem[c("SANDERS_B_Ind","WARREN_E_Dem",
                           "KLOBUCHAR_A_Dem","BOOKER_C_Dem","HARRIS_K_Dem"),]

sameInds <- sapply(votesDem_sub, function(x) length(unique(x)) == 1 ) 
# Keep rollcall numbers associated with votes
rollcalls <- 1:length(sameInds)
rollcalls <- rollcalls[sameInds]

sameVotes <- votesDem[,sameInds]
scores <- rep(0,length(sameVotes))
for (i in 1:length(sameVotes)){
  
  comp <- sameVotes["SANDERS_B_Ind",i]
  v <- sameVotes[,i]
  same <- v == comp
  scores[i] <- sum(same)/length(v)

}

sortedScores <- sort(scores, index.return=TRUE)
justScores <- sortedScores$x
newRoll <- rollcalls[sortedScores$ix]

toPlot <- data.frame(justScores, newRoll)
colnames(toPlot) <- c('Percent','Rollcall')
toPlot$color <- ifelse(toPlot$Percent <= 0.25, 'steelblue', 'grey')

ggplot(toPlot, aes(x=Rollcall, y=Percent,color=color)) + 
  geom_point(alpha = 0.6,color=toPlot$color) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=10)) +
  xlab('Vote Rollcall Number') + 
  ylab('Percent Agreement (Within Party Votes)')

ggsave("demVoteAgreementCandidates.pdf")
