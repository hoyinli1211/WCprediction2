
library(dplyr)
library(lubirate)
library(ggplot2)
library(gridExtra)

url <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/historical.csv"
df <- read.csv(url)

df.wc <- df %>% 
          filter(tournament=='FIFA World Cup') %>%
          mutate(date=as.Date(date),
                 year=format(date,'%Y'),
                 score.max=pmax(home_score,away_score),
                 score.min=pmin(home_score,away_score),
                 score=paste0(score.max,'-',score.min),
                 score.diff=abs(score.max-score.min)
                 ) %>%
          arrange(score.diff,score.max,score.min)

#data visualization- score distribution
plot1 <- ggplot(df.wc, aes(score.min,score.max)) + 
          geom_count() +
          scale_size_area(max_size=5) +
          scale_y_discrete(name='score.max',limits=seq(0,10,1)) +
          labs(title='Score distribution of FIFA World Cup \n 1930-2014')

plot2 <- ggplot(df.wc, aes(x=score.diff)) + 
          geom_bar(stat='count', width=1,position='dodge') +
          scale_x_discrete(limits=seq(0,10,1)) +
          labs(title='Score difference distribution of FIFA World Cup \n 1930-2014')

grid.arrange(plot1,plot2,nrow=2)
