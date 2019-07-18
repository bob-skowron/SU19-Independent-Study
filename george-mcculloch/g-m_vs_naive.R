
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
convertWeekly <- TRUE # use weekly returns instead of daily

train.period <- 250 # length of period over which to run the MCMC
test.period <- 20
top.n <- 5

# load the data
flog.info("Loading data from files")
idx.returns <- read_csv(idx.file, col_names = TRUE)
sec.returns <- read_csv(sec.file, col_names = TRUE)

# clean the data
idx.xts <- idx.returns %>% dplyr::select(datadate, close) %>% timetk::tk_xts()

sec.xts <- sec.returns %>% 
  select(datadate, tic, close) %>%
  spread(key = tic, value = close) %>%
  timetk::tk_xts()

# single run
# grab the training set
idx.train <- periodReturn(idx.xts, period='daily')[2:train.period+1]
sec.train <- CalculateReturns(sec.xts[,!is.na(sec.xts[1,])])[2:train.period+1,]

# run MCMC on training data

# get top stocks to include
# calculate their weights via lm


# want to only choose stocks that exist for the entire period....
# get the top x largest stocks by mkt cap at the beginning
top.stocks <- sec.returns %>%
  group_by(datadate) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank <= top.n)

# get the top stock by gsector/ggroup/gsector/gsubind at the beginning
top.stocks.gsector <- sec.returns %>%
  group_by(datadate, gsector) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank == 1)

top.stocks.ggroup <- sec.returns %>%
  group_by(datadate, ggroup) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank == 1)

top.stocks.gind <- sec.returns %>%
  group_by(datadate, gind) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank == 1)

top.stocks.gsubind <- sec.returns %>%
  group_by(datadate, gsubind) %>%
  arrange(desc(mktcap)) %>%
  mutate(
    mktcap.rank = rank(desc(mktcap))
  ) %>%
  filter(mktcap.rank == 1)


# compare the performance of thse portfolios





  
