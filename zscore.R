# How to compute the z-score with R
# https://www.r-bloggers.com/how-to-compute-the-z-score-with-r/
# 번역: Jaesun HA (joshuariver@gmail.com)

rm(list=ls())
getwd()

# What is Z-score (Z 값이란 무엇인가?)

# In short, the z-score is a measure that shows how much away (below or above) of the mean is a 
# specific value (individual) in a given dataset. In the example below, I am going to measure the 
# z value of body mass index (BMI) in a dataset from NHANES.

# Z값이란 주어진 데이터에서 각 관측치들이 얼마나 평균에서 떨어져 있는지를 나타낸 숫자이다.
# 아래 연습에서는 NHANES 데이터셋에서의 BMI(신체체적율)에 대한 Z 값을 다루어 보겠다.


# Loading packages and creating the dataset:
# 패키지를 로딩하고 데이터셋을 만들기
  
library(tidyverse)
library(RNHANES)
dat = nhanes_load_data("DEMO_E", "2007-2008") %>%
  select(SEQN, RIAGENDR) %>%
  left_join(nhanes_load_data("BMX_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, RIAGENDR, BMXBMI) %>% 
  filter(RIAGENDR == "1", !is.na(BMXBMI)) %>% 
  transmute(SEQN, Gender = RIAGENDR, BMI = BMXBMI)
dat
str(dat)

# Mean of BMI
# BMI 의 평균

mean(dat$BMI)

# Standard deviation of BMI:
# BMI 의 표준편차

sd(dat$BMI)  

# R 에서 Z 값 구하기기

dat %>% 
  mutate(zscore = (BMI - mean(BMI))/sd(BMI))

