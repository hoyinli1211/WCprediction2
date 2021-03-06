#library
library(dplyr)
library(lubridate)
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
         home_team=ifelse(home_team == 'Korea Republic' ,'Republic of Korea',
                          ifelse(home_team=='USA','United States of America',home_team)),
         away_team=ifelse(away_team == 'Korea Republic','Republic of Korea',
                          ifelse(away_team=='USA','United States of America',away_team))           
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
  mutate(home.region=ifelse(home_team=='Scotland','Europe',
                            ifelse(home_team=='Korea DPR', 'Asia', home.region)),
         away.region=ifelse(away_team=='Scotland','Europe',
                            ifelse(away_team=='Korea DPR', 'Asia', away.region))
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
df.wc.timeframe$url.ranking <- c('https://www.fifa.com/fifa-world-ranking/ranking-table/men/rank=50/index.html',
                                 'https://www.fifa.com/fifa-world-ranking/ranking-table/men/rank=97/index.html',
                                 'https://www.fifa.com/fifa-world-ranking/ranking-table/men/rank=144/index.html',
                                 'https://www.fifa.com/fifa-world-ranking/ranking-table/men/rank=191/index.html',
                                 'https://www.fifa.com/fifa-world-ranking/ranking-table/men/rank=239/index.html')


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
df.wc.team <- data.frame(df.wc.team)

#data.acquisition import
url.dataAcquisition <- 'https://raw.githubusercontent.com/hoyinli1211/WCprediction2/master/dataAcquisition.R'
source(url.dataAcquisition)

df.wc.team <- df.wc.team %>%
  left_join(df.fifa.ranking, by=c('year','team'))

df.wc <- df.wc %>%
  left_join(df.fifa.ranking, by=c('year'='year','home_team'='team')) %>%
  left_join(df.fifa.ranking, by=c('year'='year','away_team'='team'))
colnames(df.wc)[23:24] <- c('home.fifa.rank','away.fifa.rank')

list.train <- list()
for (i in 1:dim(df.wc.timeframe)[1]) {
  v.year <- df.wc.timeframe$year[i]
  list.train[[i]] <- df.train.Extraction(v.year)
}
names(list.train) <- df.wc.timeframe$year



########################################
#correlation of rank and score difference
########################################
df.hypothesis1 <- df.wc %>%
  mutate(rank.diff=ifelse(home.fifa.rank < away.fifa.rank, away.fifa.rank-home.fifa.rank,home.fifa.rank-away.fifa.rank),
         score.diff=ifelse(home.fifa.rank < away.fifa.rank, home_score-away_score, away_score-home_score)) %>%
  select(rank.diff, score.diff)

# pearson correlation test
corr <- cor.test(df.hypothesis1$rank.diff, df.hypothesis1$score.diff, method='pearson')
corr


########################################
#simple poisson regression on fifa rank between teams for score prediction
########################################
# poisson regression
m1.a <- glm(home_score ~ home.fifa.rank + away.fifa.rank, family= 'poisson', data=df.wc)
summary(m1.a)
m1.b <- glm(away_score ~ home.fifa.rank + away.fifa.rank, family= 'poisson', data=df.wc)
summary(m1.b)

sim.home.fifa.rank <- 7
sim.away.fifa.rank <- 14

lamda1 <- exp(coef(m1.a)[1] + coef(m1.a)[2]*sim.home.fifa.rank + coef(m1.a)[3]*sim.away.fifa.rank)
lamda2 <- exp(coef(m1.b)[1] + coef(m1.b)[2]*sim.away.fifa.rank + coef(m1.b)[3]*sim.home.fifa.rank)

v.simulation <- paste(rpois(10000,lamda1),rpois(10000,lamda2),sep="-")
df.simulation <- data.frame(result=v.simulation)
df.simulation.summary <- df.simulation %>%
  group_by(result) %>%
  summarise(n=n(),ratio=n/10000) %>%
  arrange(desc(ratio))
View(df.simulation.summary)


########################################
#simple poisson regression on fifa rank and wc performance in round of 16 and group
########################################

df.wc.quarter <- df.wc %>%
                  filter(stage1=='Quarter-finals') %>%
                  rowwise() %>%
                  mutate(home.attack=wcPerformance(year,home_team, id, 'attack'),
                         home.defense=wcPerformance(year,home_team, id, 'defense'),
                         away.attack=wcPerformance(year,away_team, id, 'attack'),
                         away.defense=wcPerformance(year,away_team, id, 'defense'))

# poisson regression
m2.a <- glm(home_score ~ home.fifa.rank + away.fifa.rank + home.attack + away.defense, family= 'poisson', data=df.wc.quarter)
summary(m2.a)
m2.b <- glm(away_score ~ home.fifa.rank + away.fifa.rank + home.defense + away.attack, family= 'poisson', data=df.wc.quarter)
summary(m2.b)

sim.home.fifa.rank <- 7
sim.away.fifa.rank <- 14
sim.home.attack <- 6
sim.home.defense <- 4
sim.away.attack <- 6
sim.away.defense <- 1

lamda1 <- exp(coef(m2.a)[1] + coef(m2.a)[2]*sim.home.fifa.rank + coef(m2.a)[3]*sim.away.fifa.rank + coef(m2.a)[4]*sim.home.attack + coef(m2.a)[5]*sim.away.defense)
lamda2 <- exp(coef(m2.b)[1] + coef(m2.b)[2]*sim.away.fifa.rank + coef(m2.b)[3]*sim.home.fifa.rank + coef(m2.b)[4]*sim.away.attack + coef(m2.b)[5]*sim.home.defense)

v.simulation <- paste(rpois(10000,lamda1),rpois(10000,lamda2),sep="-")
df.simulation <- data.frame(result=v.simulation)
df.simulation.summary <- df.simulation %>%
  group_by(result) %>%
  summarise(n=n(),ratio=n/10000) %>%
  arrange(desc(ratio))
View(df.simulation.summary)

########################################
#data visualization- score distribution
########################################

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

plot5 <- ggplot(df.hypothesis1, aes(x=rank.diff, y=score.diff)) +
  geom_point() +
  labs(title='Score difference aginst FIFA score rank difference \n on FIFA World Cup gamrd (1998-2014)') +
  scale_y_continuous(breaks=seq(-5,8,1)) +
  scale_x_continuous(breaks=seq(0,100,10))

grid.arrange(plot1,plot2, nrow=2,ncol=1)
grid.arrange(plot3,plot4, nrow=2,ncol=1)
grid.arrange(plot5, nrow=2,ncol=1)
