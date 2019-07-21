suppressWarnings(library(optparse))
suppressMessages(suppressWarnings(library(futile.logger)))
suppressMessages(suppressWarnings(library(dplyr)))

source("george_mcculloch.R")

option_list <- list(
  make_option(c("-f", "--file"), action="store_true", type="character", help="File to be parsed"),
  make_option(c("-n", "--nstocks"), action="store_true", type="integer", help="Number of stocks to include", default = -1),
  make_option(c("-b", "--nburn"), action="store_true", type="integer", help="Number of burn-in"),
  make_option(c("-s", "--nsamples"), action="store_true", type="integer", help="Number of samples"),
  make_option(c("-o", "--outfile"), action="store_true", type="character", help="Output file")
)
args <- parse_args(OptionParser(option_list=option_list))


flog.appender(appender.file("batchrun.log"), name="batch.io")

post_params <- list(v0 = 9.925*(10^(-6)), v1 = .0062034, nu = 25, lambda = .007^2, calcType = "fast")

# load the sample data from CRSP. Should be close to part 6
flog.info("Processing %s; NStocks: %s; NSamples: %s", args$outfile, args$nstocks, args$nsample, name = "batch.io")
crsp.data <- read.csv(args$file, header = TRUE)

flog.info("Loaded %s", args$file, name = "batch.io")

if(args$nstocks == -1){
  samplesize <- ncol(crsp.data) - 1
}else{
  samplesize <- args$nstocks
}

Y <- crsp.data[,1]
inc.stocks <- sample(seq(2,ncol(crsp.data)), size = samplesize, replace = FALSE)
X <- as.matrix(crsp.data[,2:201])
secs <- colnames(crsp.data)
colnames(X) <- NULL

# run the MCMC
flog.info("Starting MCMC", name = "batch.io")
ptm <- proc.time()
crsp.results <- run_MCMC(X*100, Y*100, n_burn = args$nburn, n_samples = args$nsample, post_param = post_params)
#crsp.results <- run_MCMC(X*100, Y*100, n_burn = 1000, n_samples = 10000, post_param = post_params)
flog.info("Completed MCMC in %s seconds; %s percent acceptance", (proc.time() - ptm)[3], crsp.results$AcceptanceRatio, name = "batch.io")

#colMeans(crsp.results$Samples)
sortidx <- sort(colMeans(crsp.results$Samples), decreasing =TRUE, index.return=TRUE)$ix
orderedsecs <- secs[sortidx+1]

# run regressions against data to get betas
# compare R^2s to "choose" how many assets to select
flog.info("Computing R-squareds", name = "batch.io")
rsq.results <- NULL
for(i in seq.int(1, samplesize)){
  rsq.results <- bind_rows(rsq.results, c(Idx = i, AdjR2 = summary(lm(Y ~ X[,sortidx[1:i]]))$adj.r.squared))
  #print(summary(lm(Y ~ X[,sortidx[1:i]]-1)))
}

out.folder <- paste0(args$outfile, "-", args$nsample, "-", samplesize)
dir.create(out.folder, showWarnings = FALSE)

pdf(paste0(out.folder, "/colmeans-plot.pdf"))
plot(sort(colMeans(crsp.results$Samples), decreasing=TRUE), ylab = "Inclusion Rate")
suppressMessages(dev.off())

pdf(paste0(out.folder, "/rsq-plot.pdf"))
plot(rsq.results$AdjR2, ylab = "Adjusted R Squared")
suppressMessages(dev.off())

save(crsp.results, file = paste0(out.folder, "/results.Rdata"))
save(orderedsecs, file = paste0(out.folder, "/ordered-secs.Rdata"))
save(rsq.results, file = paste0(out.folder, "/rsquareds.Rdata"))
flog.info("Saved output file", name = "batch.io")
