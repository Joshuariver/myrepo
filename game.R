# ‘There is a game I play’ – Analyzing Metacritic scores for video games
# https://www.r-bloggers.com/there-is-a-game-i-play-analyzing-metacritic-scores-for-video-games/

rm(list = ls())

library(data.table)
library(kableExtra)
games <- as.data.frame(fread("metacritic_games.csv"))

scroll_box(kable_styling(kable(head(games))),
           width = "100%", height = "500px")

games.agg <- aggregate(cbind(metascore, user_score) ~ game, FUN = mean, data = games)
nrow(games)

nrow(games.agg)

library(ggplot2)
ggplot(games.agg, aes(x = metascore, y = user_score)) +
  geom_point(alpha = .3, pch = 15) +
  geom_density_2d() +
  geom_segment(data = data.frame(),
               inherit.aes = F,
               aes(x = 0, y = 0, xend = 100, yend = 100),
               lty = "dashed", col = "red") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Metascore", y = "User score", fill = "Count")


library(ggrepel)
lmod <- lm(user_score ~ metascore, data = games.agg)

games.agg$res.user.score <- residuals(lmod)

lowest.res <- games.agg$game[order(games.agg$res.user.score)][1:10]
highest.res <- games.agg$game[order(games.agg$res.user.score,
                                    decreasing = T)][1:5]

games.agg$Label <- ifelse(games.agg$game %in% c(lowest.res, highest.res),
                          games.agg$game, NA)

ggplot(games.agg, aes(x = metascore, y = user_score)) +
  geom_point(pch = 15, aes(color = is.na(Label), alpha = is.na(Label))) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) +
  scale_alpha_manual(values = c("TRUE" = .3, "FALSE" = 1)) +
  geom_smooth(method = "lm", formula = "y ~ x", se =  F) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_text_repel(aes(label = Label), force = 3, size = 3) +
  guides(col = F, alpha = F) +
  labs(x = "Metascore", y = "User score", fill = "Count")


# Comparing the busiest developers

dev.tab <- sort(table(games$developer), decreasing = T)
games.dev <- games[games$developer %in% names(dev.tab[1:10]),]


games.dev.agg <- aggregate(cbind(metascore, user_score) ~ game + developer,
                           FUN = mean, data = games.dev)

games.dev.agg$developer <-
  factor(games.dev.agg$developer,
         levels = names(sort(tapply(games.dev.agg$metascore,
                                    games.dev.agg$developer,
                                    mean), decreasing = T)))

games.dev.agg <- games.dev.agg[order(games.dev.agg$metascore),]
games.dev.agg[1:7, "Label"] <- games.dev.agg[1:7, "game"]


library(ggbeeswarm)
ggplot(games.dev.agg, aes(x = developer, y = metascore)) +
  geom_boxplot(outlier.shape = NA) +
  geom_beeswarm(alpha = .5, pch = 15, col = "blue") +
  geom_point(stat = "summary", size = 4, col = "red", alpha = .5) +
  geom_text_repel(aes(label = Label), size = 3) +
  labs(x = "", y = "Metascore") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 12))

games.dev.agg$developer <-
  factor(games.dev.agg$developer,
         levels = names(sort(tapply(games.dev.agg$user_score,
                                    games.dev.agg$developer,
                                    mean), decreasing = T)))

games.dev.agg <- games.dev.agg[order(games.dev.agg$user_score),]
games.dev.agg$Label <- NA
games.dev.agg[1:7, "Label"] <- games.dev.agg[1:7, "game"]
ggplot(games.dev.agg, aes(x = developer, y = user_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_beeswarm(alpha = .5, pch = 15, col = "blue") +
  geom_point(stat = "summary", size = 4, col = "red", alpha = .5) +
  geom_text_repel(aes(label = Label), size = 3) +
  labs(x = "", y = "User score") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 12))

# Ratings through time

library(lubridate)
games$release <- as.Date(mdy(games$release_date))

is.outlier <- function (x) {
  x < quantile(x, .25) - 1.5 * IQR(x) |
    x > quantile(x, .75) + 1.5 * IQR(x)
}

games$is.out.meta <- is.outlier(games$metascore)
games$is.out.user <- is.outlier(games$user_score)

meta.mod <- lm(metascore ~ release, data = games, subset = !is.out.meta)
user.mod <- lm(user_score ~ release, data = games, subset = !is.out.meta)
summary(meta.mod)

summary(user.mod)

ggplot(games[!games$is.out.meta,], aes(x = release, y = metascore)) +
  geom_point(alpha = .3, pch = 15) +
  geom_smooth(se = F, method = "lm") +
  labs(subtitle = paste0("beta = ", signif(meta.mod$coefficients["release"], 3),
                         ", Outliers removed (Boxplot with r = 1.5)"),
       x = "Release date", y = "Metascore", title = "Release date vs. Metascore")


ggplot(games[!games$is.out.user,], aes(x = release, y = user_score)) +
  geom_point(alpha = .3, pch = 15) +
  geom_smooth(se = F, method = "lm") +
  labs(subtitle = paste0("beta = ", signif(user.mod$coefficients["release"], 3),
                         ", Outliers removed (Boxplot with r = 1.5)"),
       x = "Release date", y = "Metascore", title = "Release date vs. User score")


# ESRB ratings by genre

library(car)
sub.games <- unique(games[games$rating %in% c("E", "E10+", "M", "T") &
                            games$genre %in% c("Action", "Action Adventure",
                                               "Role-Playing", "Adventure",
                                               "Strategy", "Sports",
                                               "Simulation", "Racing",
                                               "Puzzle", "Shooter"),
                          c("rating", "genre", "game")])

sub.games$rating <- recode(sub.games$rating, "'E'='Everyone'; 'E10+'='Everyone 10+'; 'T'='Teen'; 'M'='Mature'")

sub.games.tab <- table(sub.games$rating, sub.games$genre)
scroll_box(kable_styling(kable(sub.games.tab)), width = "100%")


library(ca)
ca.fit <- ca(sub.games.tab) # do the ca
ca.sum <- summary(ca.fit)   # needed later for dimension percentages
ca.plot.obj <- plot(ca.fit)

make.ca.plot.df <- function (ca.plot.obj,
                             row.lab = "Rows",
                             col.lab = "Columns") {
  df <- data.frame(Label = c(rownames(ca.plot.obj$rows),
                             rownames(ca.plot.obj$cols)),
                   Dim1 = c(ca.plot.obj$rows[,1], ca.plot.obj$cols[,1]),
                   Dim2 = c(ca.plot.obj$rows[,2], ca.plot.obj$cols[,2]),
                   Variable = c(rep(row.lab, nrow(ca.plot.obj$rows)),
                                rep(col.lab, nrow(ca.plot.obj$cols))))
  rownames(df) <- 1:nrow(df)
  df
}

# Create plotting data.frame for ggplot
ca.plot.df <- make.ca.plot.df(ca.plot.obj,
                              row.lab = "Rating",
                              col.lab = "Genre")
# Extract percentage of variance explained for dims 
dim.var.percs <- ca.sum$scree[,"values2"]

library(ggrepel)
ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                       col = Variable, shape = Variable,
                       label = Label)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point(size = 4) +
  scale_x_continuous(limits = range(ca.plot.df$Dim1) + c(diff(range(ca.plot.df$Dim1)) * -0.2,
                                                         diff(range(ca.plot.df$Dim1)) * 0.2)) +
  scale_y_continuous(limits = range(ca.plot.df$Dim2) + c(diff(range(ca.plot.df$Dim2)) * -0.2,
                                                         diff(range(ca.plot.df$Dim2)) * 0.2)) +
  geom_label_repel(show.legend = F, segment.alpha = .5) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  labs(x = paste0("Dimension 1 (", signif(dim.var.percs[1], 3), "%)"),
       y = paste0("Dimension 2 (", signif(dim.var.percs[2], 3), "%)"),
       col = "", shape = "") +
  theme_minimal()
