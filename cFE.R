# Connecting Free Economic DBs
# https://www.r-bloggers.com/access-the-free-economic-database-dbnomics-with-r-2/

rm(list=ls())
setwd("~/R/R Blogger")
library(rdbnomics)
library(magrittr)
library(dplyr)
library(ggplot2)

df <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN') %>%
  filter(!is.na(value))

ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

df <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN')) %>%
  filter(!is.na(value))

ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

df <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'Eurostat/une_rt_q/Q.SA.TOTAL.PC_ACT.T.EA19')) %>%
  filter(!is.na(value))

ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics() + 
  theme(legend.text = element_text(size=7))

