setwd("~/Desktop/senateSplitsTree")
source('distFuncs.R')
#https://voteview.com/data for all data

Sall_votes = as_tibble(read_csv("./Sall_votes_names_parties.csv") )

people <- c("BOOKER, Cory Anthony","WARREN, Elizabeth",
            "KLOBUCHAR, Amy","SANDERS, Bernard","HARRIS, Kamala Devi")

Sall_votes_sub  <- Sall_votes %>% 
  filter(grepl(paste(people, collapse="|"), name))

firstCong <- min(Sall_votes_sub$congress)
lastCong <- max(Sall_votes_sub$congress)



# Make distance matrix and nexus output for 114-116th congresses
for (i in firstCong:lastCong){
  cong = i
  Sall_votes_sub_dist  <- Sall_votes %>% filter(congress == cong)
  
  fname <- paste("./dist_",as.character(cong),"th.nex",sep="")
  
  l1DistsAll <- makeDistMat(Sall_votes_sub_dist,fname)
}

# Put in loop
#cong <- 116
data <- setNames(data.frame(matrix(ncol = length(people), nrow = lastCong-firstCong+1)), people)

for (cong in firstCong:lastCong){
  fname <- paste("./splitWeights_tab_",as.character(cong),"th.txt",sep="")
  splitMat <- read_delim(fname, "\t", escape_double = FALSE, trim_ws = TRUE)
  
  centerDists <- centerDist(splitMat) 
  
  Sall_votes_sub_cands  <- Sall_votes_sub %>% filter(congress == cong)
  cands <- unique(Sall_votes_sub_cands$name)
  
  for (c in cands){
    c_sub <- gsub(",.*", "", c)
    pos <- grep(c_sub,colnames(centerDists))
    
    dist <- centerDists[pos]
    
    df_pos <- grep(c,colnames(data))
    data[cong-firstCong+1,df_pos] <- dist
    
  }
}


