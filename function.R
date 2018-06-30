#function

#region rank

regionRank <- function(v.1,v.2) {
  
  return(rank(c(v.1,v.2))[1])
  
}

regionRegion <- function(v.1,v.2) {
  
  v.output=''
  rank1 <- regionRank(v.1,v.2)
  
  if (v.1==v.2 | rank1==1) {
    v.output <- paste(v.1,v.2,'-')
  }
  else {
   v.output <- paste(v.2,v.1,'-') 
  }
  
  return(v.output)
}
