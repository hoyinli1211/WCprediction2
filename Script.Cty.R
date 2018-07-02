library(stringr)
url.cty <- 'https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv'
df.cty <- read.csv(url.cty,stringsAsFactors=FALSE)
#str(df.cty)
df.cty2 <- df.cty[,c('official_name_en','CLDR.display.name','ISO3166.1.Alpha.2','ISO3166.1.Alpha.3','ISO3166.1.numeric','Continent','Dial','Region.Code','Region.Name')]
colnames(df.cty2) <- c('cty','city','cty.cd2','cty.cd3','cty.cdInt','continent','dial','region.cd','region')
df.cty2 <- df.cty2 %>%
  mutate(cty.cdInt = str_pad(df.cty2$cty.cdInt,3,pad="0"),
         cty=ifelse(cty=='United Kingdom of Great Britain and Northern Ireland','England',
                    ifelse(cty=='Russian Federation','Russia',
                           ifelse(cty %in% c('Iran (Islamic Republic of)','Iran IR'),'Iran',
                                  ifelse(cty=='Czechia','Czech Republic',
                                         ifelse(cty=="Côte d'Ivoire",'Ivory Coast',
                                                ifelse(cty=='Bosnia and Herzegovina','Bosnia-Herzegovina',
                                                       ifelse(cty='China PR','China',cty)
                                                )
                                         )
                                  )
                           )
                     )
               ),
         region=as.character(region)
  )
