# Benchmarking the portfolio
# https://www.r-bloggers.com/benchmarking-the-portfolio/


# Load package
library(tidyquant)
library(broom)

# Load data for portfolios
symbols <- c("SPY", "SHY", "GLD")
symbols_low <- tolower(symbols)

prices <- getSymbols(symbols, src = "yahoo",
                     from = "1990-01-01",
                     auto.assign = TRUE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols_low)

prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
ret <- ROC(prices_monthly)["2005/2019"]

# Load benchmark data
bench_sym <- c("VTI", "VXUS", "BND", "BNDX")
bench <- getSymbols(bench_sym, src = "yahoo",
                    from = "1990-01-01",
                    auto.assign = TRUE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(tolower(bench_sym))
bench <- to.monthly(bench, indexAt = "last", OHLC = FALSE)
bench_ret <- ROC(bench)["2014/2019"]

# Create different weights and portflios
# Equal weigthed
wt1 <- rep(1/(ncol(ret)), ncol(ret))
port1 <- Return.portfolio(ret, wt1) %>% 
  `colnames<-`("ret")

# Risk portfolio
wt2 <- c(0.9, 0.1, 0)
port2 <- Return.portfolio(ret, weights = wt2) %>% 
  `colnames<-`("ret")

# Naive portfolio
wtn <- c(0.5, 0.5, 0)
portn <- Return.portfolio(ret, wtn)

# Data frame of portfolios
port_comp <- data.frame(date = index(port1), equal = as.numeric(port1),
                        risky = as.numeric(port2),
                        naive = as.numeric(portn)) 

# Benchmark portfolio
wtb <- c(0.24, 0.21, 0.22, 0.33)
portb <- Return.portfolio(bench_ret, wtb, rebalance_on = "quarters") %>% 
  `colnames<-`("bench")

# Graph of portfolios vs. benchmark
port_comp %>% 
  filter(date >= "2014-01-01") %>% 
  mutate(bench = portb) %>% 
  gather(key,value, -date) %>% 
  group_by(key) %>% 
  mutate(value = cumprod(value+1)) %>% 
  ggplot(aes(date, value*100, color = key)) +
  geom_line() +
  scale_color_manual("", labels = c("Bench", "Equal", "Naive", "Risky"),
                     values = c("purple", "blue", "black", "red")) +
  labs(x = "",
       y = "Index",
       title = "The three portfolios with a benchmark",
       caption = "Source: Yahoo, OSM estimates") +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0))

# summary
port_comp %>% 
  filter(date >= "2014-01-01") %>% 
  mutate(bench = as.numeric(portb)) %>% 
  rename("Equal" = equal,
         "Naive" = naive,
         "Risky" = risky,
         "Bench" = bench) %>% 
  gather(Asset, value, -date) %>% 
  group_by(Asset) %>% 
  summarise(`Mean (%)` = round(mean(value, na.rm = TRUE),3)*1200,
            `Volatility (%)` = round(sd(value, na.rm = TRUE)*sqrt(12),3)*100,
            `Sharpe` = round(mean(value, na.rm = TRUE)/sd(value, na.rm=TRUE)*sqrt(12),2),
            `Cumulative (%)` = round(prod(1+value, na.rm = TRUE),3)*100) %>% 
  knitr::kable(caption = "Annualized performance metrics") 

# Portfolio
mean_ret <- apply(ret[,c("spy", "shy", "gld")],2,mean)
cov_port <- cov(ret[,c("spy", "shy", "gld")])

port_exam <- data.frame(ports = colnames(port_comp)[-1],
                        ret = as.numeric(apply(port_comp[,-1],2, mean)),
                        vol = as.numeric(apply(port_comp[,-1], 2, sd)))

bench_exam <- data.frame(ports = "bench",
                         ret = mean(bench_ret),
                         vol = sd(bench_ret))

bench_spy <- data.frame(ports = "sp",
                        ret = mean(ret$spy),
                        vol = sd(ret$spy))

bench_spy_14 <- data.frame(ports = "sp",
                           ret = mean(ret$spy["2014/2019"]),
                           vol = sd(ret$spy["2014/2019"]))

mean_ret_14 <- apply(ret[,c("spy", "shy", "gld")]["2014/2019"],2,mean)

cov_port_14 <- cov(ret[,c("spy", "shy", "gld")]["2014/2019"])

port_exam_14 <- port_comp %>% 
  filter(date >= "2014-01-01") %>% 
  select(-date) %>% 
  gather(ports, value) %>%  
  group_by(ports) %>% 
  summarise_all(list(ret = mean, vol = sd)) %>% 
  data.frame()


### Random weighting
# wts for full period
wts <- matrix(nrow = 1000, ncol = 3)
set.seed(123)
for(i in 1:1000){
  a <- runif(1,0,1)
  b <- c()
  for(j in 1:2){
    b[j] <- runif(1,0,1-sum(a,b))
  }
  if(sum(a,b) < 1){
    inc <- (1-sum(a,b))/3
    vec <- c(a+inc, b+inc)
  }else{
    vec <- c(a,b)
  }
  wts[i,] <- sample(vec,replace = FALSE)
}

# wts for 2014
wts1 <- matrix(nrow = 1000, ncol = 3)
set.seed(123)
for(i in 1:1000){
  a <- runif(1,0,1)
  b <- c()
  for(j in 1:2){
    if(j == 2){
      b[j] <- 1 - sum(a,b)
    }
    else {
      b[j] <- runif(1,0,1-sum(a,b))
    }
    vec <- c(a,b)
  }
  wts1[i,] <- sample(vec,replace = FALSE)
}

# Calculate random portfolios
# Weighting: wts
port <- matrix(nrow = 1000, ncol = 2)
for(i in 1:1000){
  port[i,1] <- as.numeric(sum(wts[i,] * mean_ret))
  port[i,2] <- as.numeric(sqrt(t(wts[i,] %*% cov_port %*% wts[i,])))
}

colnames(port) <- c("returns", "risk")
port <- as.data.frame(port)
port <- port %>% 
  mutate(sharpe = returns/risk)

# Calculate random portfolios since 2014
# Weighting: wts1
port_14 <- matrix(nrow = 1000, ncol = 2)
for(i in 1:1000){
  port_14[i,1] <- as.numeric(sum(wts1[i,] * mean_ret_14))
  port_14[i,2] <- as.numeric(sqrt(t(wts1[i,] %*% cov_port_14 %*% wts1[i,])))
}

colnames(port_14) <- c("returns", "risk")
port_14 <- as.data.frame(port_14)
port_14 <- port_14 %>% 
  mutate(sharpe = returns/risk)

# Grraph with Sharpe ratio
port %>% 
  ggplot(aes(risk*sqrt(12)*100, returns*1200, color = sharpe)) +
  geom_point(size = 1.2, alpha = 0.4) +
  geom_point(data = port_exam, aes(port_exam[1,3]*sqrt(12)*100,
                                   port_exam[1,2]*1200),
             color = "red", size = 6) +
  geom_point(data = port_exam, aes(port_exam[2,3]*sqrt(12)*100,
                                   port_exam[2,2]*1200),
             color = "purple", size = 7) +
  geom_point(data = port_exam, aes(port_exam[3,3]*sqrt(12)*100,
                                   port_exam[3,2]*1200),
             color = "black", size = 5) +
  scale_x_continuous(limits = c(0,14)) +
  labs(x = "Risk (%)",
       y = "Return (%)",
       title = "Simulated portfolios",
       color = "Sharpe ratio") +
  scale_color_gradient(low = "red", high = "green") +
  theme(legend.position = c(0.075,.8), 
        legend.key.size = unit(.5, "cm"),
        legend.background = element_rect(fill = NA))

# Graph since 2014
port_14 %>% 
  ggplot(aes(risk*sqrt(12)*100, returns*1200, color = sharpe)) +
  geom_point(size = 1.2, alpha = 0.4) +
  geom_point(data = port_exam_14, aes(port_exam_14[1,3]*sqrt(12)*100,
                                      port_exam_14[1,2]*1200),
             color = "blue", size = 6) +
  geom_point(data = port_exam_14, aes(port_exam_14[3,3]*sqrt(12)*100,
                                      port_exam_14[3, 2]*1200),
             color = "purple", size = 7) +
  geom_point(data = port_exam_14, aes(port_exam_14[2,3]*sqrt(12)*100,
                                      port_exam_14[2,2]*1200),
             color = "black", size = 5) +
  scale_x_continuous(limits = c(0,14)) +
  labs(x = "Risk (%)",
       y = "Return (%)",
       title = "Simulated portfolios since 2014",
       color = "Sharpe ratio") +
  scale_color_gradient(low = "red", high = "green") +
  theme(legend.position = c(0.075,0.8), 
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(.5, "cm"))

# Portfolios benchmarked vs Vanguard
port_14 %>%
  mutate(Bench = returns - bench_exam$ret) %>%
  # mutate(Bench = ifelse(Bench > 0, 1, 0)) %>% 
  ggplot(aes(risk*sqrt(12)*100, returns*1200, color = Bench)) +
  geom_point(size = 1.2, alpha = 0.4) +
  scale_color_gradient(low = "red", high = "green") +
  geom_point(data = port_exam_14, aes(port_exam_14[1,3]*sqrt(12)*100,
                                      port_exam_14[1,2]*1200),
             color = "blue", size = 6) +
  geom_point(data = port_exam_14, aes(port_exam_14[3,3]*sqrt(12)*100,
                                      port_exam_14[3,2]*1200),
             color = "purple", size = 7) +
  geom_point(data = port_exam_14, aes(port_exam_14[2,3]*sqrt(12)*100,
                                      port_exam_14[2,2]*1200),
             color = "black", size = 5) +  
  labs(x = "Risk (%)",
       y = "Return (%)",
       title = "Simulated portfolios since 2014") +
  theme(legend.position = c(0.06,0.8), 
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(.5, "cm"))

# Portfolios benchmarked vs Vanguard
port_14 %>%
  mutate(Bench = returns - bench_exam$ret) %>%
  mutate(Bench = ifelse(Bench > 0, 1, 0)) %>%
  ggplot(aes(risk*sqrt(12)*100, returns*1200, color = Bench)) +
  geom_point(size = 1.2, alpha = 0.4) +
  scale_color_gradient(low = "red", high = "green") +
  geom_point(data = port_exam_14, aes(port_exam_14[1,3]*sqrt(12)*100,
                                      port_exam_14[1,2]*1200),
             color = "blue", size = 6) +
  geom_point(data = port_exam_14, aes(port_exam_14[3,3]*sqrt(12)*100,
                                      port_exam_14[3,2]*1200),
             color = "purple", size = 7) +
  geom_point(data = port_exam_14, aes(port_exam_14[2,3]*sqrt(12)*100,
                                      port_exam_14[2,2]*1200),
             color = "black", size = 5) +  
  labs(x = "Risk (%)",
       y = "Return (%)",
       title = "Simulated portfolios") +
  theme(legend.position = c(0.05,0.8), 
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(.5, "cm"))

# Count how many portfolios are negative
pos_b <- port_14 %>%
  mutate(Bench = returns - bench_exam$ret) %>%
  mutate(Bench = ifelse(Bench > 0, 1, 0)) %>%
  summarise(bench = round(mean(Bench),2)*100) %>%
  as.numeric()

port_list_14 <- list()
for(i in 1:1000){
  port_list_14[[i]] <- Return.portfolio(ret["2014/2019"], wts[i,]) %>%
    data.frame() %>%
    summarise(returns = mean(portfolio.returns),
              excess_ret = mean(portfolio.returns) - mean(portb$bench),
              track_err = sd(portfolio.returns - portb$bench),
              risk = sd(portfolio.returns))
}


port_info <- port_list_14 %>% bind_rows
rfr <- mean(ret$shy)

# Graph info
port_info %>% 
  mutate(info_ratio = excess_ret/track_err) %>% 
  ggplot(aes(risk*sqrt(12)*100, returns*1200, color = info_ratio)) +
  geom_point(size = 1.2, alpha = 0.4) +
  geom_point(data = port_exam_14, aes(port_exam_14[1,3]*sqrt(12)*100,
                                      port_exam_14[1,2]*1200),
             color = "blue", size = 6) +
  geom_point(data = port_exam_14, aes(port_exam_14[3,3]*sqrt(12)*100,
                                      port_exam_14[3,2]*1200),
             color = "purple", size = 7) +
  geom_point(data = port_exam_14, aes(port_exam_14[2,3]*sqrt(12)*100,
                                      port_exam_14[2,2]*1200),
             color = "black", size = 5) +  
  labs(x = "Risk (%)",
       y = "Return (%)",
       title = "Simulated portfolios") +
  theme(legend.position = c(0.075,0.8), 
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(.5, "cm")) +
  scale_color_gradient("Information ratio", low = "red", high = "green") 
