
library(dplyr)
library(lubirate)
library(ggplot2)
library(gridExtra)


######################################
#Data import and manipulation
######################################
#historical data on match game 
url.match <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/historical.csv"
df <- read.csv(url.match,stringsAsFactors=FALSE)

df <- df %>%  
        mutate(date=as.Date(date),
               year=format(date,'%Y'),
               score.max=pmax(home_score,away_score),
               score.min=pmin(home_score,away_score),
               score=paste0(score.max,'-',score.min),
               score.diff=abs(score.max-score.min)
               ) %>%
        filter(date > '1994-07-17')
# url.cty <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction2/master/Script.Cty.R"
# source(url.cty)

df <- df %>%
        left_join(df.cty2[,c(1,9)], by=c('home_team'='cty'))

#stage of world cup
df.wc <- df %>% 
          filter(tournament=='FIFA World Cup') %>%
          group_by(year) %>% 
          mutate(id = row_number())

v.id <- c(1:64)
v.stage1 <- c(rep('Group stage 1',12),rep('Group stage 2',12),rep('Group stage 3',12),rep('Group stage 4',12),rep('Round of 16',8),rep('Quarter-finals',4),rep('Semi-finals',2),'Third place play off','Final')
v.stage2 <- c(rep('Group stage',48),rep('Knockout stage',16))
df.stage <- data.frame(id=v.id,stage1=v.stage1, stage2=v.stage2)

df.wc <- df.wc %>%
          left_join(df.stage,by='id')

#data visualization- score distribution
plot1 <- ggplot(df.wc, aes(score.min,score.max)) + 
          geom_count() +
          scale_size_area(max_size=5) +
          scale_y_discrete(name='score.max',limits=seq(0,max(df.wc$score.max)+1,1)) +
          labs(title='Score distribution of FIFA World Cup \n 1998-2014')

plot2 <- ggplot(df.wc, aes(x=score.diff)) + 
          geom_bar(stat='count', width=1,position='dodge') +
          scale_x_discrete(limits=seq(0,max(df.wc$score.max),1)) +
          labs(title='Score difference distribution of FIFA World Cup \n 1998-2014')

plot3 <- ggplot(df.wc, aes(x=score.diff,fill=stage2)) + 
          geom_bar(stat='count', width=1,position='dodge') +
          scale_x_discrete(limits=seq(0,max(df.wc$score.max),1)) +
          labs(title='Score difference distribution of FIFA World Cup \n by stage type 1998-2014')


grid.arrange(plot1,plot2, plot3,nrow=2,ncol=2)
