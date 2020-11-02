library(usedist)
library(phangorn)
library(readr)
library(tidyverse)
library(hash)

#https://voteview.com/data for all data

cong = 114
#Read in matrix (csv) of samples by features
Sall_members <- as.tibble(read_csv("~/Downloads/Sall_members.csv") )
Sall_members <- Sall_members %>% filter(congress == cong ) 

Sall_votes <- as.tibble(read_csv("~/Downloads/Sall_votes.csv"))  
Sall_votes  <- Sall_votes %>% filter(congress == cong)

#Get real names
Sall_votes[c("name")] <- 
  lapply(Sall_votes[c("icpsr")], function(col) Sall_members$bioname[match(col, Sall_members$icpsr)])

#Get party affiliation
Sall_votes[c("party_code")] <- 
  lapply(Sall_votes[c("icpsr")], function(col) Sall_members$party_code[match(col, Sall_members$icpsr)])

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

#Isakson replaced by Loeffler around roll call 400
# votes_df <- votes_df %>% filter(plotID != 'ISAKSON_J_Rep')
# votes_df <- votes_df %>% filter(plotID != 'LOEFFLER_K_Rep')
      

#Make numeric df, without names column
votes_df <- as.data.frame(votes_df)
rownames(votes_df) <- votes_df$plotID
votes_df <- subset(votes_df,select=-c(plotID))


#Get pairwise (Euclidean) distances
d <- dist(votes_df,method="manhattan",upper = FALSE,diag = TRUE)
  
#Write distance matrix as nexus file
fname <- paste("/Users/tarachari/Desktop/dist_",as.character(cong),"th.nex",sep="")
write.nexus.dist(d,file=fname, append = FALSE, upper = FALSE,
                 diag = TRUE, digits = getOption("digits"))


