
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
suppressWarnings(library(optparse))
suppressMessages(suppressWarnings(library(futile.logger)))
library(xts)
library(timetk)
library(tidyquant)

source("george_mcculloch.R")

flog.appender(appender.file("naive.log"), name="naive.io")

# input parameters
idx.file <- "../data/2012-2018/sp500-returns.csv" # idx data file
sec.file <- "../data/2012-2018/sp500-const-returns.csv" # security data file
returnType <- 'daily' # use weekly returns instead of daily

train.period <- 250 # length of period over which to run the MCMC
test.period <- 20
top.n.all <- 5
top.n.sector <- 1

# load the data
flog.info("Loading data from files")
idx.returns <- read_csv(idx.file, col_names = TRUE)
sec.returns <- read_csv(sec.file, col_names = TRUE)

# clean the data
idx.xts <- idx.returns %>% dplyr::select(datadate, close) %>% timetk::tk_xts()
colnames(idx.xts) <- "IDX"

sec.xts <- sec.returns %>% 
  select(datadate, tic, close) %>%
  spread(key = tic, value = close) %>%
  timetk::tk_xts()

# single run
# grab the training set
comb.data <- merge.xts(idx.xts, sec.xts, join="inner")
comb.ret <- CalculateReturns(comb.data)[-1,] # exclude first date as it will be NA

train.start <- index(comb.data[1])
train.end <- index(comb.data[train.period])
test.start <- index(comb.data[train.period+1])
test.end <- index(comb.data[train.period+test.period+1])

idx.train <- comb.ret[1:train.period,1]
sec.train <- comb.ret[1:train.period,-1]
sec.train <- sec.train[,apply(sec.train, 2, function(x) !any(is.na(x)))]

# grab the test set
idx.test <- periodReturn(idx.xts, period=returnType)[(train.period+2):(train.period+test.period+1)]
sec.test <- CalculateReturns(sec.xts[,!is.na(sec.xts[1,])])[(train.period+2):(train.period+test.period+1)]

# run MCMC on training data
post_params <- list(v0 = 9.925*(10^(-6)), v1 = .0062034, nu = 25, lambda = .007^2, calcType = "fast")

crsp.results <- run_MCMC(coredata(sec.train[,1:50])*100,coredata(idx.train)*100,  
                         n_burn = 1000, n_samples = 10000, post_param = post_params)

# get top stocks to include
# calculate their weights via lm
sortidx <- sort(colMeans(crsp.results$Samples), decreasing =TRUE, index.return=TRUE)$ix
gm.sel <- sec.train[,which(colMeans(crsp.results$Samples) >= 0.8)] # make this a parameter
gm.model <- unlist(lm(coredata(idx.train*100) ~ coredata(gm.sel*100)-1)$coefficients)
gm.tickers <- colnames(sec.train)[which(colMeans(crsp.results$Samples) >= 0.8)]

# want to only choose stocks that exist for the entire period....
# get the top x largest stocks by mkt cap at the end of the train period/beginning of the 
# test period
sec.train.start <- sec.returns %>%
  filter(datadate == train.end)

top.stocks <- sec.train.start %>%
  group_by(datadate) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank <= top.n.all) %>%
  ungroup() %>%
  dplyr::select(tic)

# get the top stock by gsector/ggroup/gsector/gsubind at the beginning
top.stocks.gsector <- sec.train.start %>%
  group_by(datadate, gsector) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank <= top.n.sector) %>%
  ungroup() %>%
  dplyr::select(tic)

top.stocks.ggroup <- sec.train.start %>%
  group_by(datadate, ggroup) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank <= top.n.sector) %>%
  ungroup() %>%
  dplyr::select(tic)

top.stocks.gind <- sec.train.start %>%
  group_by(datadate, gind) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank <= top.n.sector) %>%
  ungroup() %>%
  dplyr::select(tic)

top.stocks.gsubind <- sec.train.start %>%
  group_by(datadate, gsubind) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank <= top.n.sector) %>%
  ungroup() %>%
  dplyr::select(tic)


# compare the performance of thse portfolios
Rb <- idx.test
gm.ret <- Return.portfolio(sec.test[,gm.tickers], weights = gm.model)
top.n.ret <- Return.portfolio(sec.test[,unlist(top.stocks, use.names = FALSE)])
top.gsector.ret <- Return.portfolio(sec.test[,unlist(top.stocks.gsector, use.names = FALSE)]) 
top.ggroup.ret <- Return.portfolio(sec.test[,unlist(top.stocks.ggroup, use.names = FALSE)]) 
top.gind.ret <- Return.portfolio(sec.test[,unlist(top.stocks.gind, use.names = FALSE)]) 
top.gsubind.ret <- Return.portfolio(sec.test[,unlist(top.stocks.gsubind, use.names = FALSE)]) 

TrackingError(top.gind.ret, Rb)


  
