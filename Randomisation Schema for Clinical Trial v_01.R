
###############
# Assignment 2.2


################
# What does it do?


# A functio to create a reproducible randomization clinical trial schema for:
# 1. Any Number of Sites (up to 26)
# 2. Any Number of Subjects
# 3. Any Randomisation Ratio
# 3. Any Block Size


##################
#The function
#################

MultSites <- function(Nr.Sites,Randomisation.Ratio=c(1,1),Nr.Subject.Each.Site,Block.Size ){
  #Initial check
  if(sum(c(Nr.Subject.Each.Site%%Block.Size))>0) stop("The number of subject for each site should be a multiple of Block size for that site.")
  if(sum(c(Block.Size%%sum(Randomisation.Ratio)))>0) stop("The randomisation ratio does not agree with Block size")
  
  #Create site names
  Site.Names <- paste(LETTERS[1:Nr.Sites])
  for(i in 1:Nr.Sites) Site.Names[i] <- paste(rep(Site.Names[i],3), collapse = "")
  
  #Create site matrices
    Site.Matrix.Names <- vector("list",Nr.Sites)
  for(i in 1:Nr.Sites){
    a <- rep(Site.Names[i],Nr.Subject.Each.Site[i])
    b <- formatC(seq(1:Nr.Subject.Each.Site[i]), width=2, flag=0)
    c <- paste0(a,b)
    Site.Matrix.Names[[i]] <- t(matrix(c,nrow=(Block.Size[i]),ncol=(Nr.Subject.Each.Site[i]/Block.Size[i])))
  }
  
  #Create the randomisation schema
  for(i in 1:Nr.Sites)
    for(j in 1:nrow(Site.Matrix.Names[[i]])){
      g <- c(rep("T",Randomisation.Ratio[1]*(Block.Size[i]/sum(Randomisation.Ratio))),rep("C",Randomisation.Ratio[2]*(Block.Size[i]/sum(Randomisation.Ratio))))
      set.seed(sum(Nr.Subject.Each.Site)+j)
      h <- sample(g)
      Site.Matrix.Names[[i]][j,] <- paste(Site.Matrix.Names[[i]][j,],h, sep = "")
    }
  
  # Return the output
  names(Site.Matrix.Names) <- paste("Randomisation Scheme for Site", paste(LETTERS[1:Nr.Sites]))
  Site.Matrix.Names
}




##################
#Initial arguments
##################

# The number of sites
Nr.Sites <- 5     

# A vector with the number of subjects for each site in order (A-Z)
Nr.Subject.Each.Site <- c(24,30,36,50,40)  

# A vector with the block size for each site in order. (A-Z). Nr of Subjects for each site
# should be a multiple of the block size
Block.Size <- c(8,6,4,10,8)  

# A vector of randomisation ratio c(Nr.Control:Nr.Treatment):
# Should match with the number of subjects for each site i.e. for a block of size 5 
# only ratios c(1,4),c(4,1),c(3,2),c(2,3) are acceptable)
Randomisation.Ratio <- c(1,1) 

MultSites(Nr.Sites,Randomisation.Ratio,Nr.Subject.Each.Site,Block.Size)








