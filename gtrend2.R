# #FunDataFriday – gTrendsR
# https://www.r-bloggers.com/fundatafriday-gtrendsr/


rm(list=ls())
setwd("~/R/R Blogger")

library(gtrendsR)

trends <- gtrends(c("Nerds", "Smarties"), geo ="CA")

plot(trends)

# With three more lines, you can make the graph interactive



library(plotly)

p <-plot(trends)

ggplotly(p)
         