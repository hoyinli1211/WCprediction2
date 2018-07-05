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


wcSummaryByYear <- function(v.year, v.team, v.var) {
  
  v.year <- as.character(v.year)
  v.team <- as.character(v.team)
  v.var <- as.character(v.var)
  
  v.result <- df.wc %>%
                filter(year==v.year, home_team==v.team | away_team==v.team) %>%
                arrange(desc(date)) %>%
                filter(row_number() <= 1) %>%
                mutate(score.for=ifelse(home_team==v.team,home_score,away_score),
                       score.against=ifelse(away_team==v.team,away_score,home_score),) %>%
                pull(v.var)
  return(v.result)
  
}

df.train.Extraction <- function (v.year) {
  
  v.year <- as.character(v.year)
  v.strat.date <- df.wc.timeframe %>%
                    filter(year==v.year) %>%
                    pull(start.date.train)
  v.end.date <- df.wc.timeframe %>%
                  filter(year==v.year) %>%
                  pull(end.date.train)
  
  v.team <- df.wc.team %>%
              filter(year==v.year) %>%
              pull(team)
  
  df.result <- df %>%
                filter(home_team %in% v.team | away_team %in% v.team, year==v.year)
  
  return(df.result)
}

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


wcSummaryByYear <- function(v.year, v.team, v.var) {
  
  v.year <- as.character(v.year)
  v.team <- as.character(v.team)
  v.var <- as.character(v.var)
  
  v.result <- df.wc %>%
                filter(year==v.year, home_team==v.team | away_team==v.team) %>%
                arrange(desc(date)) %>%
                filter(row_number() <= 1) %>%
                mutate(score.for=ifelse(home_team==v.team,home_score,away_score),
                       score.against=ifelse(away_team==v.team,away_score,home_score)) %>%
                pull(v.var)
  return(v.result)
  
}

df.train.Extraction <- function (v.year) {
  
  v.year <- as.character(v.year)
  v.strat.date <- df.wc.timeframe %>%
                    filter(year==v.year) %>%
                    pull(start.date.train)
  v.end.date <- df.wc.timeframe %>%
                  filter(year==v.year) %>%
                  pull(end.date.train)
  
  v.team <- df.wc.team %>%
              filter(year==v.year) %>%
              pull(team)
  
  df.result <- df %>%
                filter(home_team %in% v.team | away_team %in% v.team, year==v.year)
  
  return(df.result)
}

wcPerformance <- function(v.year, v.team, v.id, v.var) {
  
  v.result <- df.wc %>%
                filter(year==v.year, home_team==v.team | away_team==v.team, id < v.id) %>%
                mutate(score.for=ifelse(home_team==v.team,home_score,away_score),
                       score.against=ifelse(home_team==v.team,away_score,home_score)) %>%
                group_by(year) %>%
                summarise(attack=sum(score.for),
                          defense=sum(score.against)) %>%
                pull(v.var)
  
  return(v.result)
  
}
