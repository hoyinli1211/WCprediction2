#library
library(dplyr)
library(lubirdate)
library(ggplot2)
library(gridExtra)

rm(list=ls())

#function import
url.function <- 'https://raw.githubusercontent.com/hoyinli1211/WCprediction2/master/function.R'
source(url.function)

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
               score.diff=(home_score-away_score),
               score.diff.abs=abs(score.max-score.min),
               home_team=ifelse(home_team == 'Korea Republic' | home_team== 'Korea DPR','Republic of Korea',
                                ifelse(home_team=='USA','United States of America',
                                       ifelse(home_team=='Republic of Korea',Korea,home_team))),
               away_team=ifelse(away_team == 'Korea Republic' | away_team== 'Korea DPR','Republic of Korea',
                                ifelse(away_team=='USA','United States of America',
                                       ifelse(away_team=='Republic of Korea',Korea,away_team)))               
        ) %>%
        filter(date > '1994-07-17')    # as this is last of of 1994 world cup

url.cty <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction2/master/Script.Cty.R"
source(url.cty)

#Region of each team
df <- df %>%
        left_join(df.cty2[,c(1,9)], by=c('home_team'='cty')) 
colnames(df)[16] <- 'home.region'
df <- df %>%
        left_join(df.cty2[,c(1,9)], by=c('away_team'='cty'))
colnames(df)[17] <- 'away.region'
df <- df %>%
        mutate(home.region=ifelse(home_team=='Scotland','Europe',home.region),
               away.region=ifelse(away_team=='Scotland','Europe',away.region),
        )

#stage of world cup
df.wc <- df %>% 
          filter(tournament=='FIFA World Cup') %>%
          group_by(year) %>% 
          mutate(id = row_number())  %>%
          rowwise() %>%
          mutate(region.str=regionString(home.region,away.region,home_score,away_score,0),
                 region.score.diff=regionString(home.region,away.region,home_score,away_score,1))

v.id <- c(1:64)
v.stage1 <- c(rep('Group stage 1',16),rep('Group stage 2',16),rep('Group stage 3',16),rep('Round of 16',8),rep('Quarter-finals',4),rep('Semi-finals',2),'Third place play off','Final')
v.stage2 <- c(rep('Group stage',48),rep('Knockout stage',16))
df.stage <- data.frame(id=v.id,stage1=v.stage1, stage2=v.stage2)

df.wc <- df.wc %>%
          left_join(df.stage,by='id')

df.wc <- data.frame(df.wc)

df.wc.timeframe <- df.wc %>%
                    group_by(year) %>%
                    summarise(start.date.game=min(date),
                              end.date.game=max(date)) %>%
                    mutate(start.date.train=lag(start.date.game)+1,
                           end.date.train=start.date.game-1)
df.wc.timeframe$start.date.train[1] <- as.Date('1994-07-18')

df.wc.team.1 <- df.wc %>%
                  filter(stage1=='Group stage 1') %>%
                  select(year, team=home_team)
df.wc.team.2 <- df.wc %>%
                  filter(stage1=='Group stage 1') %>%
                  select(year, team=away_team)
df.wc.team <- df.wc.team.1 %>%
                bind_rows(df.wc.team.2) %>%
                rowwise() %>%
                mutate(stage=wcSummaryByYear(year,team,'stage1'))

list.train <- list()
for (i in 1:dim(df.wc.timeframe)[1]) {
  v.year <- df.wc.timeframe$year[i]
  list.train[[i]] <- df.trainExtraction(v.year)
}
names(list.train) <- df.wc.timeframe$year

#data visualization- score distribution
plot1 <- ggplot(df.wc, aes(score.min,score.max)) + 
          geom_count() +
          scale_size_area(max_size=5) +
          scale_y_discrete(name='score.max',limits=seq(-max(df.wc$score.max),max(df.wc$score.max),1)) +
          labs(title='Score distribution of FIFA World Cup \n 1998-2014')

plot2 <- ggplot(df.wc, aes(x=score.diff)) + 
          geom_bar(stat='count', width=1,position='dodge') +
          scale_x_discrete(limits=seq(-max(df.wc$score.max),max(df.wc$score.max),1)) +
          labs(title='Score difference distribution of FIFA World Cup \n 1998-2014')

plot3 <- ggplot(df.wc, aes(x=score.diff,fill=stage2)) + 
          geom_bar(stat='count', width=1,position='dodge') +
          scale_x_discrete(limits=seq(-max(df.wc$score.max),max(df.wc$score.max),1)) +
          labs(title='Score difference distribution of FIFA World Cup \n by stage type 1998-2014')


plot4 <- ggplot(df.wc, aes(x=reorder(region.str,region.score.diff,mean), y=region.score.diff)) +
          geom_boxplot() +
          labs(title='Score difference distribution of FIFA World Cup \n by region 1998-2014') +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot1,plot2, plot3, plot4,nrow=2,ncol=2)
