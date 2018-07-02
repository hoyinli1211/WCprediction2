################################
#data acquisition
################################

library(rvest)

#FIFA coca-cola ranking
  #source: https://www.fifa.com/fifa-world-ranking/

df.fifaRanking.Extraction <- function (v.year, v.team) {
  
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
  
  df.extraction <- df.extraction[c(3,2,1)] %>%
                    mutate(year=as.character(year),
                           team=as.character(team),
                           rank=as.integer(rank))
  str(df.extraction)
  return(df.extraction)
}

df.fifa.ranking <- data.frame(year=character(),
                              team=character(),
                              rank=integer())

for (i in 1:dim(df.wc.timeframe)[1]) {
  v.year <- df.wc.timeframe$year[i]
  df.fifa.ranking <- df.fifa.ranking %>%
                      bind_rows(df.fifaRanking.Extraction(v.year))
}

