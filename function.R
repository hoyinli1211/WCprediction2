#function

#region rank

regionRank <- function(v.1,v.2) {
  
  return(rank(c(v.1,v.2))[1])
  
}

regionString <- function(v.1.region,v.2.region,v.1.score,v.2.score,ind) {
  
  v.region=''
  v.score=0
  rank1 <- regionRank(v.1,v.2)
  
  if (v.1==v.2 | rank1==1) {
    v.region <- paste(v.1,v.2,'-')
    v.score <- (v.1.score-v.2.score)
  }
  else {
   v.region <- paste(v.2,v.1,'-') 
   v.score <- (v.2.score-v.1.score)
  }
  
  if (ind==1) {
    return(v.output)
  }
  else {
    return(v.score)
  }
}

