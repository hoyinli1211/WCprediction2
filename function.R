#function

regionString <- function(v.1.region,v.2.region,v.1.score,v.2.score,v.ind) {
  
  v.region=''
  v.score=0
  rank1 <- as.numeric(rank(c(v.1.region,v.2.region))[1])
  v.1.region <- as.character(v.1.region)
  v.2.region <- as.character(v.2.region)
  v.1.score <- as.integer(v.1.score)
  v.2.score <- as.integer(v.2.score)
  v.ind <- as.integer(v.ind)
  
  if (v.1.region==v.2.region | rank1==1) {
    v.region <- paste(c(v.1.region,v.2.region),collapse='-')
    v.score <- (v.1.score-v.2.score)
  }
  else {
    v.region <- paste(c(v.2.region,v.1.region),collapse='-') 
    v.score <- (v.2.score-v.1.score)
  }
  
  if (v.ind==0) {
    v.output <- v.region
  }
  else if (v.ind==1) {
    v.output <- v.score
  }
  else {
    v.output <- ''
  }
  
  return(v.output)
}
