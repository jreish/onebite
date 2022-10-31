###########################################################Libraries
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(forcats)
library(tidyverse)
library(tidyr)
library(stargazer)


###########################################################Reading in Data

scores <- read_sheet("Link to my google sheet (condifential)")

###########################################################Graph 1
#####Histogram of all scores

scores %>% 
  ggplot() + geom_histogram(aes(x=score), bins = 100) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) + 
  geom_vline(data=scores, xintercept = mean(scores$score), color="Red", linetype=4) +
  xlab("Score") +
  ylab("Count") +
  theme_bw() 

ggsave("graphs/histogram.jpg")

mean(scores$score)



###########################################################Graph 2
#####Rookie Scores over years, unadjusted

scores_date_rookies <- scores %>% 
  mutate(date=mdy(date...5),
         year=(year(date))) %>% 
  filter(score==1 | score==2 | score==3 | score==4 | 
           score==5 | score==6 | score==7 | score==8 | score==9 | score==10) 

scores_date_rookies %>% 
  count(year) %>% 
  ggplot()+ geom_col(aes(x=year, y=n)) +
  scale_x_continuous(breaks = seq(2013, 2022, by = 1)) + 
  xlab("Year") +
  ylab("Rookie Score Count") +
  theme_bw() 

ggsave("graphs/rookies_unadjusted.jpg")



###########################################################Graph 3
#####Rookie Scores over years, adjusted

scores %>% 
  mutate(date=mdy(date...5),
         year=(year(date))) %>% 
  filter(score==1 | score==2 | score==3 | score==4 | 
           score==5 | score==6 | score==7 | score==8 | score==9 | score==10) %>% 
  count(year) %>% 
  left_join(scores %>% 
              mutate(date=mdy(date...5),
                     year=(year(date))) %>% 
              count(year), by = "year") %>% 
  rename(rookie_reviews=n.x, totalreviews=n.y) %>% 
  mutate(percent_rookie=rookie_reviews/totalreviews) %>% 
  ggplot() + geom_col(aes(x=year, y=percent_rookie)) +
  scale_x_continuous(breaks = seq(2013, 2022, by = 1)) + 
  xlab("Year") +
  ylab("Percentage of Rookie Scores") +
  theme_bw() 

ggsave("graphs/rookies.jpg")








###########################################################Graph 4
#####Scores over time
scores %>% 
  mutate(date=mdy(date...5)) %>% 
  ggplot() +
  geom_point(aes(x=date, y=score), alpha=.7) +    
  xlab("Date") +
  ylab("Score") + 
  theme_bw() 


ggsave("graphs/dotplot.jpg")




  
###########################################################Making regression data
#####Average Pre and Post scores

lm_data <- scores %>% 
  mutate(mdy_date=mdy(date...5),
         post=ifelse(mdy_date>"2020-03-20", 1, 0)) %>% 
  select(score, mdy_date, post, city, state) 

lm_data %>% 
  group_by(post) %>% 
  summarize(mean(score))



###########################################################Naive Regression
#####First Difference Output

stargazer(lm(score ~ post, data=lm_data), type="text")




###########################################################Narrow Window Regression
#####First Difference Output
lm_data %>%
  filter(mdy_date>"2019-12-18"&mdy_date<"2020-8-28") %>% 
  group_by(post) %>% 
  summarize(mean(score))
