#function

regionString <- function(v.1.region,v.2.region,v.1.score,v.2.score,ind) {
  
  v.region=''
  v.score=0
  rank1 <- rank(c(v.1.region,v.2.region))[1]
  
  if (v.1.region==v.2.region | rank1==1) {
    v.region <- paste(c(v.1.region,v.2.region),collapse='-')
    v.score <- (v.1.score-v.2.score)
  }
  else {
    v.region <- paste(c(v.2.region,v.1.region),collapse='-') 
    v.score <- (v.2.score-v.1.score)
  }
  
  if (ind==0) {
    v.output <- v.region
  }
  else if (ind==1) {
    v.output <- v.score
  }
  else {
    v.output <- ''
  }
  
  return(v.output)
}
