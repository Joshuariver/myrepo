# Analyzing the bachelor franchise ratings with gtrendsR!
# https://www.r-bloggers.com/analyzing-the-bachelor-franchise-ratings-with-gtrendsr/

rm(list=ls())

#specify the packages of interest
packages = c("gtrendsR","tidyverse","gifski", "gganimate", "ggimage", "lubridate", "usethis")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# specify the color of interest

pink <- "#FF8DC6"
blue <- "#56C1FF"
yellow <- "#FAE232"


# google trend data 끌어오기

bachTrends <- gtrends(c("Bachelor in Paradise", "The Bachelor", "The Bachelorette"), geo ="US")
plot(bachTrends)


bachTrendsInterest <- bachTrends$interest_over_time
trends <- bachTrendsInterest %>% 
  filter(year(date)>2016) %>% 
  mutate(date = ymd(date),
         hits = as.numeric(hits))

# Create the same plot with ggplot2

#Frequency plot by keyword
p <- ggplot() + 
  geom_line(data=trends, aes(x=date, y=hits, group=keyword, color = keyword)) + 
  scale_color_manual(values=c( yellow, blue, pink)) +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(title = "The Bachelor Franchise Popularity ",
       subtitle = "Using data to find the most dramatic season ever!",
       caption = "Source: @littlemissdata", 
       x = "Date", y = "Hits") 
p



# Create an animation

t <- p + 
  transition_reveal(as.numeric(date)) 
gif <- animate(t, end_pause = 25, width = 800, height = 400, fps = 8)
gif
anim_save("Bachelor trends", gif)



# Bring in meta data about bachelor franchise shows

## Add lollipops
x <-read.csv("/Users/lgellis/Desktop/Files/Cloud/littlemissdata/gtrends/bachelorListing.csv", stringsAsFactors = FALSE)

# Turn the dates into proper dates.  
#Ratings are only tracked on sundays so get the closest Sunday for ratings
x <-x %>% 
  mutate(startDate = ymd(as.Date(startDate, "%m/%d/%y")),
         endDate = ymd(as.Date(endDate, "%m/%d/%y")),
         ratingStartDate = floor_date(startDate, "weeks"), 
         ratingEndDate = floor_date(endDate, "weeks"))
x
#Ratings are typically highest at the beginning
x<-left_join(x, trends, by = c("topic"= "keyword", "ratingEndDate"="date"))


# Get the images for each of the seasons

x <-x %>% 
  mutate(Image = case_when(season == "Nick Viall" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/Nick.png",
                           season == "Arie Luyendyk Jr" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/Arie.png", 
                           season == "Colton Underwood" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/Colton.png", 
                           season == "Rachel Lindsay" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/Rachel.png", 
                           season == "Becca Kufrin" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/Becca.png", 
                           season == "Hannah Brown" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/Hannah.png", 
                           topic == "Bachelor in Paradise" ~ "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/gtrendsR/images/BIP.png"))


# Create the combined chart

p <- ggplot() + 
  geom_line(data=trends, aes(x=date, y=hits, group=keyword, color = keyword), size=1) + 
  scale_color_manual(values=c(yellow, blue, pink)) +
  geom_segment(data=x, aes(x=ratingEndDate, 
                           xend=ratingEndDate, 
                           y=0, 
                           yend=hits, 
                           color=topic), size=1) +
  geom_image(data=x, aes(x=ratingEndDate, y=hits, image=Image), size=0.105) +
  theme_classic() +
  labs(title = "The Bachelor Franchise Popularity ",
       subtitle = "Using data to find the most dramatic season ever!",
       caption = "Source: @littlemissdata", 
       x = "Date", y = "Hits") +
  theme(legend.position="none",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size=10, face = "italic"),
        plot.caption = element_text(size = 8, face = "italic") )

p

