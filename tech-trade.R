libraries <- c("pdftools", "tidyr",
               "ggplot2", "dplyr", "lubridate",
               "parallel", "doParallel", "tidytext",
               "tidyverse",
               "stringi", "Rmpfr",
               "data.table")
new.packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){install.packages(new.packages)} 
lapply(libraries, require, character.only = TRUE)

votes <- data.table::fread("~/Desktop/Courses/Concentration/gov50/UNVotes.csv")

votes <- votes[-which(votes$year<2004),]
#If vote yes, 1, otherwise (not member, abstain, not present), 0
votes$vote_y <- ifelse(votes$vote == 1, 1, 
                       ifelse(votes$vote == 2 | votes$vote == 8 | votes$vote == 9, 0,
                              -1)) #Can figure out anther way to do the voting, this is a crude one

votes <- votes[-which(votes$unres ==""),]
votes <- votes[-which(votes$Country =="YUG"),]
#votes <- votes[-grep("Israel*|Palest*|Palist*", votes$descr),] #To Remove Israel Votes
votes <- votes[-which(votes$me == 1),] #To Remove Israel Votes a different method
votes <- votes[-which(votes$nu == 1),] #To Remove nuclear weapons a different method

total_df <- data.frame(
  ccode = NA,
  us_dist = NA,
  cn_dist = NA,
  rus_dist = NA,
  year = NA
)

years <- 2005:2019
i <- 10
for(i in 1:length(years)){
  votes2 <- votes %>% filter(year==years[i]) %>%
    dplyr::select(resid, ccode, vote_y) %>%
    spread(resid, vote_y)
  
  if(sum(rowSums(is.na(votes2)))>0){votes2 <- votes2[-which(rowSums(is.na(votes2))>0),]}
  
  votes2$ccode <- countrycode::countrycode(votes2$ccode, origin = "cown", destination = "iso3c")
  
  ###Two dimension version
  d <- dist(votes2[,2:ncol(votes2)]) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dimensions
  mds <- fit$points %>%
    as_tibble()
  mds$ccode <- votes2$ccode
  colnames(mds)[1:2] <- c("Dim.1", "Dim.2")
  
  country_df <- data.frame(
    ccode = votes2$ccode,
    us_dist = ((mds$Dim.1[which(mds$ccode == "USA")] - mds$Dim.1)^2 + (mds$Dim.2[which(mds$ccode == "USA")] - mds$Dim.2)^2)^.5,
    cn_dist = ((mds$Dim.1[which(mds$ccode == "CHN")] - mds$Dim.1)^2 + (mds$Dim.2[which(mds$ccode == "CHN")] - mds$Dim.2)^2)^.5,
    rus_dist = ((mds$Dim.1[which(mds$ccode == "RUS")] - mds$Dim.1)^2 + (mds$Dim.2[which(mds$ccode == "RUS")] - mds$Dim.2)^2)^.5,
    year = years[i]
  )
  
  total_df <- bind_rows(total_df, country_df) #First time only
  
}
total_df <- total_df[-which(is.na(total_df$ccode)==T),]

#This is the one that makes Egypt over time
total_df %>% filter(ccode == "EGY") %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y = us_dist, colour = "US Distance")) + 
  geom_line(aes(y = cn_dist, colour = "CN Distance")) + 
  geom_line(aes(y = rus_dist, colour = "RU Distance"))

total_df %>% filter(ccode == "CAN") %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y = us_dist, colour = "US Distance")) + 
  geom_line(aes(y = cn_dist, colour = "CN Distance")) + 
  geom_line(aes(y = rus_dist, colour = "RU Distance"))

total_df %>% filter(ccode == "ISR") %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y = us_dist, colour = "US Distance")) + 
  geom_line(aes(y = cn_dist, colour = "CN Distance")) + 
  geom_line(aes(y = rus_dist, colour = "RU Distance"))

#run this summary
total_df_summary <- total_df %>%
  mutate(pre2010 = ifelse(year <2010, 1, 0)) %>%
  group_by(ccode, pre2010) %>%
  summarize(us_dist  = mean(us_dist),
            cn_dist = mean(cn_dist),
            rus_dist = mean(rus_dist))

#This is the one that makes Egypt for pre and post 2010 period
total_df_summary %>% filter(ccode == "EGY") %>%
  ggplot(aes(x = pre2010)) + 
  geom_line(aes(y = us_dist, colour = "US Distance")) + 
  geom_line(aes(y = cn_dist, colour = "CN Distance")) + 
  geom_line(aes(y = rus_dist, colour = "RU Distance"))

#China influence and change 
china_influence <- c("THA", "TZA", "ZWE", 
                     "VEN", "MYS", "UGA", 
                     "ZMB", "IRN", "EGY",
                     "MNG", "MMR", "NER",
                     "NGA", "MOZ", "MAR",
                     "COL", "SAU", "ECU",
                     "BLR", "KAZ", "KGZ",
                     "UZB", "TUR", "TJK",
                     "SGP", "USA", "KOR",
                     "LKA", "ZAF", "GBR",
                     "SDN", "SLE", "TTO",
                     "PAK", "PHL", "CMR",
                     "KHM", "ARG", "ETH",
                     "GHA", "IDN", "CIV", 
                     "KEN", "VNM", "LAO",
                     "LBY", "IRL", "JOR",
                     "JAM", "LSO", "LBR",
                     "MWI", "MLI", "NOR", 
                     "MUS", "CUB", "MEX", 
                     "IND", "DZA", "AZE",
                     "UKR", "MDA", "TKM",
                     "ARE", "URY", "SSD",
                     "SUR", "ESP", "SYC",
                     "SRB", "RWA", "ROU",
                     "SEN", "POL", "PER",
                     "NLD", "BWA", "BRA",
                     "BOL", "BRB", "BGD",
                     "BHS", "AUS", "ARM",
                     "ATG", "CAF", "TCD", 
                     "COG", "RCH", "DMA",
                     "ERI", "FRA", "GRD",
                     "GMB", "GIN", "ITA",
                     "HUN", "IRQ")

#Russia influence and change
russia_influence <- c("COM", "LTU", "MDV",
                      "NPL", "PSE", "YEM",
                      "NIC", "MDA", "TKM",
                      "TUR", "TJK", "SGP",
                      "UKR", "AZE", "BLR",
                      "KAZ", "KGZ", "UZB",
                      "THA", "ECU", "SAU",
                      "COL", "DZA", "IND",
                      "MEX", "CUB")

#USA influence and change
usa_influence <- c("JPN", "GBR", "NOR",
                   "THA", "CHN", "AUS",
                   "NZL", "DEU", "FRA", 
                   "KOR", "MEX","CAN", 
                   "HKG", "DEU", "NLD",
                   "SGP", "TWN", "IRL",
                   "VNM", "CHE", "BEL",
                   "HRV", "ARE", "IND", 
                   "NOR", "HUN", "JOR", 
                   "EGY", "RUS", "MNG",
                   "PHL", "NZL", "FJI",
                   "MHL", "IDN", "MMR", 
                   "BGD", "PAK", "TJK",
                   "AFG", "IRN", "IRQ",
                   "UZB", "KAZ", "KWT",
                   "BHR", "QAT", "OMN",
                   "COL", "BRA", "PER",
                   "RCH", "PAN", "ARG", 
                   "ISR", "CRI", "ECU",
                   "DOM", "ITA", "GCA", 
                   "DZA", "PRY", "TUR", 
                   "JAM", "URY", "SLV", 
                   "DNK", "ZAF", "POL", 
                   "BHS", "ESP", "SAU", 
                   "TTO", "CYM", "FIN", 
                   "MAR", "SWE", "VEN",
                   "NIC", "BDS", "ABW",
                   "KHM", "ATG", "BGR",
                   "TUN", "TGO", "LBY",
                   "EST", "BLZ", "KEN", 
                   "LKA", "SVK", "SVN", 
                   "SME", "TCA", "TZA", 
                   "BRU", "CUB", "CIV",
                   "YEM", "BEN", "CYP",
                   "DMA", "GNQ", "CMR",
                   "SEN", "MOZ", "KNA",
                   "NAM", "SRB", "DJI", 
                   "MLT", "COG", "GAB",
                   "GIN", "BLR", "NPL", 
                   "GRD", "LBR", "SOM",
                   "BWA", "UGA", "MUS", 
                   "VCT", "ZMB", "ZWE",
                   "PNG", "NER", "MRT",
                   "ALB", "TCD", "WAL", 
                   "MCO", "SDN", "MDG",
                   "BFA", "BIH", "ARM",
                   "WSM", "WAG", "MDV",
                   "MKD", "SMR", "MWI", 
                   "KGZ", "MDA", "TON",
                   "SWZ", "PLW", "RWA", 
                   "GRL", "MNE", "SYC", 
                   "LIE", "LAO", "RCA", 
                   "ERI", "SYR", "CPV", 
                   "TLS", "VUT", "AND", 
                   "BTN", "STP", "BDI", 
                   "KIR", "COM", "GNB",
                   "NRU", "LSO")



total_df_change <- total_df_summary %>%
  group_by(ccode) %>%
  summarize(us_dist = us_dist[1] - us_dist[2],
            cn_dist = cn_dist[1] - cn_dist[2],
            rus_dist= rus_dist[1] - rus_dist[2])

#China

total_df_change$china_influence <- ifelse(total_df_change$ccode %in% china_influence, 1, 0)

countries <- wbstats::wbcountries()

total_df_change <- total_df_change %>% left_join(countries, by = c("ccode" = "iso3c"))

#this is the nice one 
total_df_change %>%
  ggplot(aes(x = cn_dist, y =  fct_reorder(ccode, cn_dist),
             color = factor(china_influence))) +
  geom_point() + 
  theme_light() +
  labs(title = "Influence from China in UN Votes post 2010",
       subtitle = "Measuring change in distance from the China in aggregate UN vote trends
       from 2005-2009 vs. 2010-2019 following the start of Chinese tech trade in 2010",
       x = "Change in Distance",
       y = " ",
       caption = "Source: Voeten et al. UNGA Voting Data") +
  scale_color_manual(name = " ",
                     labels = c("Not in Chinese Technosphere", "In Chinese Technosphere"),
                     values = c("rosybrown2", "red3")) +
  theme(axis.text.y=element_blank(),
        text = element_text(family = "Palatino")) 

#Regression analysis
summary(lm(cn_dist ~ china_influence, data = total_df_change))

summary(lm(cn_dist ~ china_influence, data = total_df_change))

#Russia

total_df_change$russia_influence <- ifelse(total_df_change$ccode %in% russia_influence, 1, 0)

countries <- wbstats::wbcountries()

total_df_change <- total_df_change %>% left_join(countries, by = c("ccode" = "iso3c"))

#this is the nice one 
total_df_change %>%
  ggplot(aes(x = rus_dist, y =  fct_reorder(ccode, rus_dist),
             color = factor(russia_influence))) +
  geom_point() + 
  theme_light() +
  labs(title = "Influence from Russia in UN Votes post 2010",
       subtitle = "Measuring change in distance from the Russia in aggregate UN vote trends
       from 2005-2009 vs. 2010-2019 following the start of Chinese tech trade in 2010",
       x = "Change in Distance",
       y = " ",
       caption = "Source: Voeten et al. UNGA Voting Data") +
  scale_color_manual(name = " ",
                     labels = c("Not in Russian Technosphere", "In Russian Technosphere"),
                     values = c("rosybrown2", "red3")) +
  theme(axis.text.y=element_blank(),
        text = element_text(family = "Palatino")) 


#Regression analysis
summary(lm(rus_dist ~ russia_influence, data = total_df_change))

summary(lm(rus_dist ~ russia_influence, data = total_df_change))

#USA

total_df_change$usa_influence <- ifelse(total_df_change$ccode %in% usa_influence, 1, 0)

countries <- wbstats::wbcountries()

total_df_change <- total_df_change %>% left_join(countries, by = c("ccode" = "iso3c"))

#this is the nice one 
total_df_change %>%
  ggplot(aes(x = us_dist, y =  fct_reorder(ccode, us_dist),
             color = factor(usa_influence),
             binwidth = 0.05)) +
  geom_point() + 
  theme_light() +
  labs(title = "Influence from the U.S. in UN Votes post 2010",
       subtitle = "Measuring change in distance from the U.S. in aggregate UN vote trends
       from 2005-2009 vs. 2010-2019 following the start of Chinese tech trade in 2010",
       x = "Change in Distance",
       y = " ",
       caption = "Source: Voeten et al. UNGA Voting Data") +
  scale_color_manual(name = " ",
                     labels = c("Not in U.S. Technosphere", "In U.S. Technosphere"),
                     values = c("rosybrown2", "red3")) +
  theme(axis.text.y=element_blank(),
        text = element_text(family = "Palatino")) 


#Regression analysis
summary(lm(us_dist ~ usa_influence, data = total_df_change))

summary(lm(us_dist ~ usa_influence, data = total_df_change))


#Country_name technology_type year
#Zimbabwe 5_chinese_tech 2015

ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
                  label = votes2$ccode,
                  size = 1,
                  repel = TRUE)




##one dimension version
d <- dist(votes2[,2:ncol(votes2)]) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=1) # k is the number of dimensions
mds <- fit$points %>%
  as_tibble()
mds$ccode <- votes2$ccode
colnames(mds)[1] <- c("Dim.1")

mds %>%  ggplot(aes(x = Dim.1, y =  fct_reorder(ccode, Dim.1))) + geom_point()

country_df <- data.frame(
  ccode = votes2$ccode,
  us_dist = ((mds$Dim.1[which(mds$ccode == "USA")] - mds$Dim.1)^2)^.5,
  cn_dist = ((mds$Dim.1[which(mds$ccode == "CHN")] - mds$Dim.1)^2)^.5,
  rus_dist = ((mds$Dim.1[which(mds$ccode == "RUS")] - mds$Dim.1)^2)^.5
)
