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

df.fifaRanking.Extraction <- function (df, v.year) {
  
  v.year <- '1998'
  
  v.year <- as.character(v.year)
  v.url <- df.wc.timeframe %>%
            filter(year==v.year) %>%
            pull(url.ranking)
  v.team <- df.wc.team %>%
              filter(year==v.year) %>%
              pull(team)
  
  df.extraction <- read_html(v.url) %>%
                    html_nodes("table") %>%
                    .[1] %>%
                    html_table(fill=TRUE) %>%
                    .[[1]]
  df.extraction <- df.extraction[,c(2,3)]
  df.extraction$year <- rep(v.year,dim(df.extraction)[1])
  colnames(df.extraction) <- c('rank','team','year')
  
  df.result <- df %>%
                bind_rows(df.extraction)
  
  return(df.result)
}
