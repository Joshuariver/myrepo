# Visualising Tour De France Data In R
# https://www.r-bloggers.com/visualising-tour-de-france-data-in-r/

rm(list=ls())
setwd("~/R/R Blogger")

library(remotes)
# install_github("alastairrushworth/tdf")
library(tdf)
library(tidyverse)

# visualise contents tdf::editions
glimpse(editions)


# visualise the total distance in the editions data since 1903

library(ggplot2)
editions %>%
  ggplot(aes(x = start_date, y = distance, 
             color = edition)) +
  geom_point() + 
  xlab('Race start date') + 
  ylab('Distance in kilometres') + 
  ggtitle('Tour de France total distance covered over time') + 
  theme(legend.position = "none")


# Tour de France winner average speed

library(ggrepel)
editions %>%
  ggplot(aes(x = start_date, y = distance / time_overall, 
             color = edition)) +
  geom_point(na.rm = TRUE) + 
  geom_label_repel(data = editions %>% sample_n(20), 
                   aes(label = winner_name), size = 2.3,  
                   nudge_y = -9, na.rm = TRUE,
                   segment.alpha = 0.2) + 
  xlab('Edition start date') + 
  ylab('Average speed km/h') + 
  ggtitle('Tour de France winners average speed') + 
  theme(legend.position = "none")


# Top 5 average speeds of Tour de France winners
editions %>%
  mutate(speed = distance / time_overall) %>%
  select(start_date, winner_name, speed) %>%
  arrange(desc(speed)) %>%
  print(n = 2)


# Tour de france winner Body mass index
editions %>%
  ggplot(aes(x = start_date, y = weight / height^2, 
             color = edition)) +
  geom_point(na.rm = TRUE) + 
  geom_label_repel(data = editions %>% sample_n(20), 
                   aes(label = winner_name), size = 1.8,  
                   nudge_y = 4, na.rm = TRUE,
                   segment.alpha = 0.2) + 
  xlab('Edition start date') + 
  ylab('Body mass index') + 
  ggtitle('Tour de France winners body mass index') + 
  theme(legend.position = "none")


# Stage Result
editions$stage_results$`2019`$`stage-21`



