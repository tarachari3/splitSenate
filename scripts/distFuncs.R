# ---------- Make L1 distance matrix and nexus file for SplitsTree ----------
makeVoteMat <- function(Sall_votes) {
  
  retParty <- function(code) {
    if (code == 100) {
      return("Dem")
    }
    else if (code == 200){
      return("Rep")
    }
    else{
      return("Ind")
    }
    
  }
  
  Sall_votes[c("party")] <- 
    lapply(Sall_votes[c("party_code")], function(col) map(col,retParty))
  
  
  # yes-->1, no --> 0, abstain --> 0.5
  map_vote <- c(1.0,1.0,1.0, 0.0,0.0,0.0, 0.5,0.5,0.5)
  
  #Get names in Nexus-legal format
  Sall_votes <- Sall_votes %>% mutate(cast_code = map_vote[as.integer(cast_code)])
  
  #Change names --> LASTNAME_FirstInitial_Party
  newName <- function(name){
    new = substr(name, start=1, stop=str_locate(name, ",")[1]+2)
    new = gsub(", ", "_",new)
    new = gsub(" ", "_",new)
    
  }
  
  Sall_votes[c("name")] <- 
    lapply(Sall_votes[c("name")], function(col) map(col,newName))
  
  Sall_votes[c("plotID")] = paste(Sall_votes$name, Sall_votes$party, sep="_")
  
  sub_votes <-  Sall_votes[c('plotID','cast_code','rollnumber')]
  
  votes_df <- pivot_wider(sub_votes, names_from = rollnumber, values_from = cast_code)
  
  votes_df <- na.omit(votes_df) #Remove members who were not present for full term
  
  
  #Make numeric df, without names column
  votes_df <- as.data.frame(votes_df)
  rownames(votes_df) <- votes_df$plotID
  votes_df <- subset(votes_df,select=-c(plotID))
  
  return(votes_df)
}
makeDistMat <- function(Sall_votes,outfile='dist.nex') {
  
  retParty <- function(code) {
    if (code == 100) {
      return("Dem")
    }
    else if (code == 200){
      return("Rep")
    }
    else{
      return("Ind")
    }
    
  }
  
  Sall_votes[c("party")] <- 
    lapply(Sall_votes[c("party_code")], function(col) map(col,retParty))
  
  
  # yes-->1, no --> 0, abstain --> 0.5
  map_vote <- c(1.0,1.0,1.0, 0.0,0.0,0.0, 0.5,0.5,0.5)
  
  #Get names in Nexus-legal format
  Sall_votes <- Sall_votes %>% mutate(cast_code = map_vote[as.integer(cast_code)])
  
  #Change names --> LASTNAME_FirstInitial_Party
  newName <- function(name){
    new = substr(name, start=1, stop=str_locate(name, ",")[1]+2)
    new = gsub("'", "",new)
    new = gsub("\\(", "",new)
    new = gsub(", ", "_",new)
    new = gsub(" ", "_",new)
    
  }
  
  Sall_votes[c("name")] <- 
    lapply(Sall_votes[c("name")], function(col) map(col,newName))
  
  Sall_votes[c("plotID")] = paste(Sall_votes$name, Sall_votes$party, sep="_")
  
  sub_votes <-  Sall_votes[c('plotID','cast_code','rollnumber')]
  
  votes_df <- pivot_wider(sub_votes, names_from = rollnumber, values_from = cast_code)
  
  votes_df <- na.omit(votes_df) #Remove members who were not present for full term
  
  
  #Make numeric df, without names column
  votes_df <- as.data.frame(votes_df)
  rownames(votes_df) <- votes_df$plotID
  votes_df <- subset(votes_df,select=-c(plotID))
  
  
  #Get pairwise (L1) distances
  d <- dist(votes_df,method="manhattan",upper = FALSE,diag = TRUE)
  
  #Write distance matrix as nexus file
  #fname <- paste("./dist_",as.character(cong),"th.nex",sep="")
  write.nexus.dist(d,file=outfile, append = FALSE, upper = FALSE,
                   diag = TRUE, digits = getOption("digits"))
  
  return(d)
  
}

# ---------- Convert circular split-system weights to pairwise distances ----------
pairSplitDists <- function(splitMat) {
  n <- dim(splitMat)[2]
  # Get split-binary matrix
  splitMat_sub <- splitMat[,3:n]
  splitMat_sub<- as.data.frame(splitMat_sub)
  mems <- dim(splitMat_sub)[2]
  # Get split weights
  w <- as.data.frame(splitMat[,2])
  
  # Sum of all splits with i != j at unique i,j pairs (regardless of order)
  posPairs <- combn(c(1:mems), 2)
  
  splitDists <- matrix(0,mems,mems)
  
  # Go through all pairs, and calculate split distance
  pairs <- dim(posPairs)[2]
  for (p in c(1:pairs)){
    i = posPairs[1,p]
    j = posPairs[2,p]
    # Get rows where i,j are on opp sides of split
    opp <- splitMat_sub[splitMat_sub[,i] != splitMat_sub[,j],]
    splits <- as.integer(rownames(opp))
    
    dist <- sum(w[splits,])
    
    splitDists[i,j] = dist
    splitDists[j,i] = dist

  }
  
  splitDists <- as.data.frame(splitDists)
  rownames(splitDists) <- colnames(splitMat_sub)
  colnames(splitDists) <- colnames(splitMat_sub)
  
  return(as.dist(splitDists, upper = FALSE,diag = TRUE))
}

# ---------- Difference between distance matrices (Split or L1) ----------
distDiff <- function(l1Dists, splitDists) {
  
  diff <- l1Dists - splitDists
  delta <- diff / l1Dists
  delta <- -1*delta*100
  return(as.matrix(delta))
  
}

# ---------- Find distance of senators to center of a congress ----------
centerDist <- function(splitMat) {

  n <- dim(splitMat)[2]
  # Get split-binary matrix
  splitMat_sub <- splitMat[,3:n]
  splitMat_sub<- as.data.frame(splitMat_sub)
  mems <- dim(splitMat_sub)[2]
  # Get split weights
  w <- as.data.frame(splitMat[,2])
  
  colNames <- colnames(splitMat_sub)
  memInd <- c(1:mems)
  
  centerDists <- matrix(0, 1,mems)
  
  for (m in memInd) {
    
    if (stri_detect_fixed(colNames[m], "Rep")) {
      filt <- !stri_detect_fixed(colNames, "Rep")
      others <- memInd[filt]
    }
    else{
      filt <- stri_detect_fixed(colNames, "Rep")
      others <- memInd[filt]
    }
  
    
    
    # Get rows where i,j are on opp sides of split
    # Rows where all members of other part of opposite to this member
    pos <- splitMat_sub[,m] != splitMat_sub[,others]
    collapse <- (rowSums(pos) == ncol(pos))
    
    opp <- splitMat_sub[collapse,]
    splits <- as.integer(rownames(opp))
    
    dist <- sum(w[splits,])
    
    centerDists[,m] <- dist
    
  }
  colnames(centerDists) <- colNames
  return(centerDists)
  
}

# ---------- Plot center dists ----------
plotCenterDist <- function(centerDists) {
  colNames <- colnames(centerDists)

  filt <- stri_detect_fixed(colNames, "Rep")
  repValues <- centerDists[,filt]
  demValues <- centerDists[,!filt]
  
  sortRep <- as.data.frame(sort(repValues,decreasing=FALSE))
  colnames(sortRep) <- "dists"
  sortDem <- as.data.frame(sort(demValues,decreasing=TRUE))
  colnames(sortDem) <- "dists"
  
  forPlot <- rbind(sortDem,sortRep)
  
  # Add colors
  blue <- rep("#0392cf",length(demValues))
  red <- rep("#ee4035",length(repValues))
  colors <- c(blue,red)
  
  pdf(file = "./center_dists.pdf",width=7,height=5)
  x<- barplot(forPlot$dists,names=rownames(forPlot),col = colors,
              space = 0.3, width = .01, border=NA,xaxt="none",cex.axis=0.5)
  axis(1, at=x,labels=rownames(forPlot),
       las=3,cex.axis=0.3)
  lines(x=rep((x[length(demValues)]+x[length(demValues)+1])/2,2),
        y=c(0,max(forPlot$dists)),
        lty = 2)
  dev.off()
  
}

# ---------- Runs test (Wald-Wolfowitz test) , probability of <= 3 runs vs random (H_0) ----------
calcRunsTest <- function(numInGrp,numOutGrp,numRuns,exactTest=TRUE) {
  if (exactTest) {
    #Exact test since P(runs <= 3) is small calculation
    #https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Analysis_of_Runs.pdf
    
    # P(runs <= 3) = sum(P(runs = i)) for i = 2 (min), 3
    tot <- numInGrp+numOutGrp
    p_rof2 = 2*choose(numInGrp-1,0)*choose(numOutGrp-1,0)/choose(tot,numInGrp)
    p_rof3 = (choose(numInGrp-1,0)*choose(numOutGrp-1,1) + choose(numInGrp-1,1)*choose(numOutGrp-1,0))/choose(tot,numInGrp)
    p_rlessEq3 = p_rof2 + p_rof3
    return(p_rlessEq3)
  }
  else {
   # Z-test
    tot <- numInGrp+numOutGrp
    mu <- (2*numInGrp*numOutGrp)/(tot) + 1
    sigma <- sqrt((mu-1)*(mu-2)/(tot-1))
    #c <- 0.5
    
    z <- (numRuns-mu)/(sigma) # + c for continuity correction

    # One-tailed, <=  3 runs (mu)
    return(pnorm(-abs(z)))
  }

}


