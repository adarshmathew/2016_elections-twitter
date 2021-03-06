library(tidyverse)
library(lubridate)
library(rtweet)

election_day.ids <- read_delim(paste(getwd(),"/data/dataverse-files/election-day.txt", sep = ""), 
                               delim = "\n", col_names = F) %>%
  rename(tweet.id = X1) %>%
  mutate(tweet.id = as.character(tweet.id))


tweet_sampler.details <- function(status_id.list, n_samples){
  set.seed(2016)
  
  tweets_samples.out <- status_id.list %>%
    sample_n(size = n_samples) %>%
    group_by(tweet.id) %>%
    do(xx = lookup_statuses(.$tweet.id))
  
  return(tweets_samples.out)
}

scraped.ids <- bind_rows(election_day.tweets_1 %>%
                           select(tweet.id),
                         election_day.tweets_2 %>%
                           select(tweet.id),
                         election_day.tweets_3.raw %>%
                           select(tweet.id), 
                         election_day.tweets_4.raw %>%
                           select(tweet.id))

# election_day.tweets_1 <- tweet_sampler.details(election_day.ids, 100000)
# election_day.tweets_2 <- tweet_sampler.details(election_day.ids %>% anti_join(election_day.tweets_1 %>% 
#                                                                                 select(tweet.id)),
#                                                100000)
# election_day.tweets_3.raw <- tweet_sampler.details(election_day.ids %>% 
#                                                  anti_join(election_day.tweets_1 %>%
#                                                              select(tweet.id)) %>%
#                                                  anti_join(election_day.tweets_2 %>%
#                                                              select(tweet.id)),
#                                                100000)
# 
# election_day.tweets_3 <- election_day.tweets_3.raw %>% 
#   unnest() %>%
#   mutate(tweet.id = as.character(tweet.id)) %>%
#   write_rds(paste(getwd(),"/data/election_day-tweets_3.rds", sep = ""))

election_day.tweets_4.raw <- tweet_sampler.details(election_day.ids %>%
                                                     anti_join(scraped.ids),
                                                   250000)

election_day.tweets_4 <- election_day.tweets_4.raw %>% 
  unnest() %>%
  mutate(tweet.id = as.character(tweet.id)) %>%
  write_rds(paste(getwd(),"/data/election_day-tweets_4.rds", sep = ""))

 
# write_rds(election_day.tweets_1, paste(getwd(),"/data/election_day-tweets_1.rds", sep = ""))
# write_rds(election_day.tweets_2, paste(getwd(),"/data/election_day-tweets_2.rds", sep = ""))

election_day.tweets_1 <- read_rds(paste(getwd(),"/data/election_day-tweets_1.rds", sep = ""))
election_day.tweets_2 <- read_rds(paste(getwd(),"/data/election_day-tweets_2.rds", sep = "")) %>% mutate(tweet.id = as.character(tweet.id))

election_day.tweets_comb <- bind_rows(election_day.tweets_1, 
                                      election_day.tweets_2 %>% 
                                        mutate(tweet.id = as.character(tweet.id)))

election_day.tweets_quoted <- election_day.tweets_comb %>% 
  filter(is.na(quoted_status_id)==F)

election_day.tweets_mentioned <- election_day.tweets_comb %>% 
  filter(is.na(mentions_user_id)==F)

election_day.tweets_replied <- election_day.tweets_comb %>% 
  filter(is.na(reply_to_status_id)==F)