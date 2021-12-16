library(tweedie)
library(magrittr)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(tictoc)
library(ggplot2)
library(doParallel)



# The basic function we will use: 
simTweedieTest <-
  function(N) {
    t.test(rtweedie(
      N,
      mu = 10000,
      phi = 100,
      power = 1.9
    ),
    mu = 10000)$p.value
  }


# Replicating simTweedieTest M times: 
MTweedieTests <-
  function(N, M, sig) {
    sum(replicate(M, simTweedieTest(N)) < sig) / M
  }


# A parallel version of MTweedieTests:
MParTweedieTests <-
  function(N, M, sig, maxcores) {
    Cores <- min(parallel::detectCores(), maxcores)
    
    cl <- makeCluster(Cores)
    registerDoParallel(cl)
    
    m_per_core <- round(M / Cores)
    m_total <- m_per_core * Cores
    
    rejects_per_core <-
      foreach(
        i = 1:Cores,
        .combine = 'rbind',
        .packages = c('magrittr', 'dplyr', 'tweedie'),
        .export = c('simTweedieTest')
      ) %dopar%
      sum(replicate(m_per_core, simTweedieTest(N)) < sig)
    
    stopCluster(cl)
    
    sum(rejects_per_core) / m_total
  }


# A result data frame as before:
df <-
  expand.grid(
    N = c(10, 100, 1000, 5000, 10000),
    M = 1000,
    share_reject = NA
  )


# Start the experiments. First single CPU everywhere: 
tic("Single CPU on all calculations")
for (i in 1:nrow(df)) {
  df$share_reject[i] <-
    MTweedieTests(N = df$N[i],
                  M = df$M[i],
                  sig = .05)
}
toc(log = TRUE)


# Next we use doParallel for the loop over the result df: 
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)
cl <- makeCluster(Cores)
registerDoParallel(cl)

# Take the time as before:
tic(paste0("Parallel loop, ", Cores, " cores"))
res <-
  foreach(
    i = 1:nrow(df),
    .combine = 'rbind',
    .packages = c('magrittr', 'dplyr', 'tweedie')
  ) %dopar%
  tibble(
    N = df$N[i],
    M = df$M[i],
    share_reject =
      MTweedieTests(N = df$N[i],
                    M = df$M[i],
                    sig = .05)
  )

# Now that we're done, we close off the clusters
stopCluster(cl)

toc(log = TRUE)


# Finally a regular loop over the result df, 
# but parallel version of the simulation function: 
tic("Parallel repeat")
for (i in 1:nrow(df)) {
  df$share_reject[i] <-
    MParTweedieTests(
      N = df$N[i],
      M = df$M[i],
      sig = .05,
      maxcores = 8
    )
}
toc(log = TRUE)


# You can see below that parallelizing the mMTweedieTests-function
# speeds things up much more than parallelizing the final loop.
#
# In this case, the replicate(M,simTweedieTest(N)) is just very
# demanding, and particularly when M is large. Therefore,
# parallelizing this part leads to speed gains.
#
# Parallelizing the final loop doesn't do very much. We only
# have five different calls to mMTweedieTests, so having
# more than five cores doesn't help at all. And some of
# the calls (e.g. mMTweedieTests when M=10) are very fast.
# This means that the core assigned to M=10 will finish
# early, and the rest will wait for the M=5000 to finish.
#
# If we instead wanted to call mMTweedieTests many times
# with a low M, the results might be reversed!

tic.log() %>%
  unlist %>%
  tibble(logvals = .) %>%
  separate(logvals,
           sep = ":",
           into = c("Function type", "log")) %>%
  mutate(log = str_trim(log)) %>%
  separate(log,
           sep = " ",
           into = c("Seconds"),
           extra = "drop")
