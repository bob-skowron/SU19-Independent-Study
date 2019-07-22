
packages <- c("readr","dplyr", "tidyr", "lubridate", "optparse", "futile.logger", 
               "xts", "timetk", "PerformanceAnalytics", "quantmod")
invisible(lapply(packages, function(x) suppressMessages(require(x, character.only = TRUE,quietly=TRUE,warn.conflicts = FALSE))))

source("george_mcculloch.R")

option_list <- list(
  make_option(c("-if", "--idxfile"), action="store_true", type="character", help="Index File to be parsed"),
  make_option(c("-sf", "--secfile"), action="store_true", type="character", help="Security File to be parsed"),
  make_option(c("-n", "--nstocks"), action="store_true", type="integer", default = 50, help="Number of stocks to include"),
  make_option(c("-b", "--nburn"), action="store_true", type="integer", default = 1000, help="Number of burn-in"),
  make_option(c("-s", "--nsamples"), action="store_true", type="integer", default = 10000, help="Number of samples"),
  make_option(c("-tr", "--train"), action="store_true", type="integer", default = 250, help="Number of days over which to train"),
  make_option(c("-ts", "--test"), action="store_true", type="integer", default = 250, help="Number of days over which to test"),
  make_option(c("-tna", "--topnall"), action="store_true", type="integer", default = 10, help="Number of top mkt cap stocks to use in naive test"),
  make_option(c("-tns", "--topnsec"), action="store_true", type="integer", default = 1, help="Number of top mkt cap stocks grouped by sector to use in naive test"),
  
  make_option(c("-o", "--outfile"), action="store_true", type="character", help="Output file")
)
args <- parse_args(OptionParser(option_list=option_list))


# get the top stock by gsector/ggroup/gsector/gsubind at the beginning
top.stocks <- function(securities, n, ...){
  return(
    securities %>%
      group_by_(...) %>%
      arrange(desc(mktcap)) %>%
      mutate(
        mktcap.rank = rank(desc(mktcap))
      ) %>%
      filter(mktcap.rank <= n) %>%
      ungroup() %>%
      dplyr::select(tic)
  )
}

# statistics to calculate for the portfolios vis-a-vis the bencmark
portfolio.stats <- function(port.returns, bmark.returns){
  return(
    list(
      Alpha = ActiveReturn(port.returns, bmark.returns, scale = 252),
      TE = TrackingError(port.returns, bmark.returns, scale = 252),
      IR = InformationRatio(port.returns, bmark.returns, scale = 252),
      corr = as.numeric(cor(port.returns, bmark.returns))
    )
  )
}

flog.appender(appender.file(paste0(getwd(), "/naive.log")), name="naive.io")

# input parameters
idx.file <- args$idxfile #../data/2012-2018/sp500-returns.csv" # idx data file
sec.file <- args$secfile # "../data/2012-2018/sp500-const-returns.csv" # security data file
returnType <- 'daily' # use weekly returns instead of daily

train.period <- args$train #250 # length of period over which to run the MCMC
test.period <- args$test #20
top.n.all <- args$topnall #10
top.n.sector <- args$topnsec #1

# load the data
flog.info("Loading data from files")
idx.returns <- read_csv(idx.file, col_names = TRUE)
sec.returns <- read_csv(sec.file, col_names = TRUE)

# clean the data
flog.info("Setting up the data")
idx.xts <- idx.returns %>% dplyr::select(datadate, close) %>% timetk::tk_xts()
colnames(idx.xts) <- "IDX"

sec.xts <- sec.returns %>% 
  select(datadate, tic, close) %>%
  spread(key = tic, value = close) %>%
  timetk::tk_xts()

# single run
# grab the training set
flog.info("Splitting data into training and test sets")
comb.data <- merge.xts(idx.xts, sec.xts, join="inner")
comb.ret <- CalculateReturns(comb.data)[-1,] # exclude first date as it will be NA

train.seq <- seq.int(from = 1, to = dim(comb.ret)[1], by = test.period)

all.series.ret <- xts()

for(t in train.seq){
  flog.info("Starting sequnce %s", t)
  train.start <- index(comb.data[t])
  train.end <- index(comb.data[(t-1)+train.period])
  test.start <- index(comb.data[t+train.period])
  test.end <- index(comb.data[t+train.period+test.period])
  
  idx.train <- comb.ret[t:(t+train.period-1),1]
  sec.train <- comb.ret[t:(t+train.period-1),-1]
  sec.train <- sec.train[,apply(sec.train, 2, function(x) !any(is.na(x)))]
  
  # grab the test set
  idx.test <- periodReturn(idx.xts, period=returnType)[(t+train.period):(t+train.period+test.period)]
  sec.test <- CalculateReturns(sec.xts[,!is.na(sec.xts[1,])])[(t+train.period):(t+train.period+test.period)]
  
  # run MCMC on training data
  flog.info("Running the MCMC on the training data")
  post_params <- list(v0 = 9.925*(10^(-6)), v1 = .0062034, nu = 25, lambda = .007^2, calcType = "fast")
  
  inc.stocks <- sample(seq(1,ncol(sec.train)), size = args$nstocks, replace = FALSE)
  crsp.results <- run_MCMC(coredata(sec.train[,inc.stocks])*100,coredata(idx.train)*100,  
                           n_burn = args$nburn, n_samples = args$nsample, post_param = post_params)
  
  # get top stocks to include
  # calculate their weights via lm
  flog.info("Identify the model based on the MCMC results")
  sortidx <- sort(colMeans(crsp.results$Samples), decreasing =TRUE, index.return=TRUE)$ix
  gm.sel <- sec.train[,which(colMeans(crsp.results$Samples) >= 0.8)] # make this a parameter
  gm.model <- unlist(lm(coredata(idx.train*100) ~ coredata(gm.sel*100)-1)$coefficients)
  gm.tickers <- colnames(sec.train)[which(colMeans(crsp.results$Samples) >= 0.8)]
  
  # want to only choose stocks that exist for the entire period....
  # get the top x largest stocks by mkt cap at the end of the train period/beginning of the 
  # test period
  flog.info("Compute the naive portfolios (top-n by sectors)")
  sec.test.start <- sec.returns %>%
    filter(datadate == train.end) %>%
    filter(tic %in% colnames(sec.train))
  
  groups <- c("datadate", "gsector", "ggroup", "gind", "gsubind")
  top.ns <- c(top.n.all, top.n.sector, top.n.sector, top.n.sector, top.n.sector)
  top.stocks.sel <- mapply(function(x, y) top.stocks(sec.test.start, y, "datadate", x), groups, top.ns)
  
  # compare the performance of thse portfolios
  flog.info("Compare the performance of GM vs. naive")
  gm.ret <- Return.portfolio(sec.test[,gm.tickers], weights = gm.model)
  top.stocks.ret <- lapply(top.stocks.sel, function(x) Return.portfolio(sec.test[,x]))
  
  all.port.ret <- append(list(GM = gm.ret), top.stocks.ret)
  merged.ret <- do.call(merge, all.port.ret)           # merge all objects in list
  colnames(merged.ret) <- c("GM", "Top", "Sector", "Group", "Ind", "SubInd")
  
  all.series.ret <- append(all.series.ret, merged.ret)
  
  print(lapply(all.port.ret, function(x) portfolio.stats(x, idx.test)))
}


