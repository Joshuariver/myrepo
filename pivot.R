# https://www.r-bloggers.com/pivoting-tidily/

rm(list=ls())
setwd("~/R/R Blogger")


library('curl')    # download files
library('readxl')  # read from Excel sheets
library(writexl)   # write into excel sheets
library('tidyr')   # data processing
library('dplyr')   # mo data processing
library('forcats') # mo mo data processing
library('ggplot2') # plotting
theme_set(theme_bw())

## Load Data
tmp <- tempfile()
curl_download("https://github.com/gavinsimpson/plant-phys/raw/master/f18ph.xls", tmp)
plant <- read_excel(tmp, sheet = 1)
# write_xlsx(plant, path = "f18ph.xlsx", col_names = TRUE)
plant

pivot_longer(plant, -(1:3), names_to = "variable")

pivot_longer(plant, -(1:3), names_sep = ":", names_to = c("variable","day"))


plant <- pivot_longer(plant, -(1:3), names_sep = ":", names_to = c("variable","day"),
                      names_ptypes = list(day = integer()))
plant


plant <- pivot_wider(plant, names_from = variable, values_from = value)
plant



plant <- mutate(plant,
                id = paste0(cultivar, "_", treatment, "_", plantid),
                treatment = fct_relevel(treatment, 'control'))
plant


ggplot(plant, aes(x = day, y = height, group = id, colour = treatment)) +
  geom_point() +
  geom_line() +
  labs(y = 'Height (mm)', x = 'Day', colour = 'Treatment')








