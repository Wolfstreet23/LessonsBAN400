# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(tidyverse)
library(tictoc)
library(purrr)
library(furrr)
library(doParallel)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 

toc(log = TRUE)



# Assignment 3 with parallel computing:----

tic()
Sys.sleep(1)
toc()

# Tictoc can also store to logs, so we can make comparisons across
# experiments. However, the log is stored in an awkward format, so
# let's make a function for printing out results in a data frame:
printTicTocLog <-
  function() {
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
  }

#Running a test:

tic.clearlog()

tic("Test")
Sys.sleep(1)
toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()


#Timing the original function in assignment 3:

df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

tic.clearlog()
tic("Regular loop")

for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 

toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()


#The regular loop in assignment 3 took 34.25 secs to complete.


# The function detectCores finds the number of cores
# available on the machine. We update the "Cores"-value
# to the minimum of the chosen cores and the available cores.
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores:
cl <- makeCluster(Cores)

# Next we register the cluster..
registerDoParallel(cl)

# Take the time as before:
tic(paste0("Parallel loop, ", Cores, " cores"))

df <- 
  foreach(
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 
  ) %dopar%
  
  for(i in 1:nrow(df)){ 
    df$share_reject[i] <-  
      MTweedieTests( 
        N=df$N[i], 
        M=df$M[i], 
        sig=.05) 
  } 

# Now that we're done, we close off the clusters
stopCluster(cl)

toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()


#As our function demonstrates the difference between the normal loop and the parallel loop,
#and it is night and day. 32.36 sec vs 0.01 sec.


#Parallel computing with furrr:-----

plan(multisession, workers = Cores) #Sets up 8 clusters, which equates to the 8 CPU cores on my pc

tic(paste0("furrr, ", Cores, "cores"))

df <- 
    expand.grid( 
      N = c(10,100,1000,5000, 10000), 
      M = 1000, 
      share_reject = NA) 
  
  for(i in 1:nrow(df)){ 
    df$share_reject[i] <-  
      MTweedieTests( 
        N=df$N[i], 
        M=df$M[i], 
        sig=.05) 
  } 

toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()

#Furr is also very fast using only 0.01 seconds to conduct the loop



  df <- 
    expand.grid( 
      N = c(10,100,1000,5000, 10000), 
      M = 1000, 
      share_reject = NA) 
  
  for(i in 1:nrow(df)){ 
    df$share_reject[i] <-  
      MTweedieTests( 
        N=df$N[i], 
        M=df$M[i], 
        sig=.05) 
    
    toc(log = TRUE)
    
  } 



#Rewriting the function in assignment one and checking the speed which is nor only 0.01 sec with 8 cores
  
maxcores <- 8

plan(multisession, workers = Cores) #Sets up 8 clusters, which equates to the 8 CPU cores on my pc

tic(paste0("furrr, ", Cores, "cores"))

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 

toc(log = TRUE)



#The solution to the tasks from Canvas:-----




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







#Example from the parallell html on canvas:-----


load("parallel_data.Rdata")

sales_price_mnok <- 1
car_cost_mnok <- .95

df %>%
  head() %>% 
  knitr::kable()

calcProfits <-
  function(df,
           sales_price_mnok,
           car_cost_mnok,
           lead_days) {
    df %>%
      mutate(
        sales_price_BTC = sales_price_mnok / NOKBTC,
        mNOK_val_sales =
          lead(NOKBTC, lead_days, order_by = date)
        * sales_price_BTC,
        profit_mnok = mNOK_val_sales - car_cost_mnok
      )
  }


initial_equity <- 10

test_neg_equity <-
  function(df, startdate, lead_days) {
    tmpdf <-
      df %>%
      filter(date >= startdate) %>%
      calcProfits(sales_price_mnok, car_cost_mnok, lead_days) %>%
      filter(complete.cases(.))
    
    if (nrow(tmpdf) > 0) {
      tmpdf %>%
        mutate(cumulative_profits_mnok = cumsum(profit_mnok)) %>%
        summarise(negative_equity =
                    1 * (min(
                      cumulative_profits_mnok + initial_equity
                    ) < 0)) %>%
        pull %>%
        return
    } else{
      return(NA_real_)
    }
  }

# Before we run the function, lets use some functionality
# for storing how long time it takes to complete the calculation.
# Let's use the tictoc-library:



# We can use tictoc to time a function..:
tic()
Sys.sleep(1)
toc()

# Tictoc can also store to logs, so we can make comparisons across
# experiments. However, the log is stored in an awkward format, so
# let's make a function for printing out results in a data frame:
printTicTocLog <-
  function() {
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
  }

tic.clearlog()

tic("Test")
Sys.sleep(1)
toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()

df_res <-
  expand.grid(date = df$date,
              lead_days = c(1, 5, 10, 30, 60)) %>%
  mutate(neg_eq = NA) %>%
  as_tibble()



tic.clearlog()
tic("Regular loop")


printTicTocLog() %>%
  knitr::kable()

for(i in 1:nrow(df_res)) {
  df_res$neg_eq[i] <-
    test_neg_equity(df,
                    startdate = df_res$date[i],
                    lead_days = df_res$lead_days[i])
}


# The function detectCores finds the number of cores
# available on the machine. We update the "Cores"-value
# to the minimum of the chosen cores and the available cores.
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores:
cl <- makeCluster(Cores)

# Next we register the cluster..
registerDoParallel(cl)

# Take the time as before:
tic(paste0("Parallel loop, ", Cores, " cores"))
res <-
  foreach(
    i = 1:nrow(df_res),
    .combine = 'rbind',
    .packages = c('magrittr', 'dplyr')
  ) %dopar%
  tibble(
    date = df_res$date[i],
    lead_days = df_res$lead_days[i],
    neg_eq =
      test_neg_equity(
        df,
        startdate = df_res$date[i],
        lead_days = df_res$lead_days[i]
      )
  )

# Now that we're done, we close off the clusters
stopCluster(cl)

toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()



tic("purrr")
df_res$neg_eq <-
  df %>%
  map2_dbl(as.list(df_res$date),
           as.list(df_res$lead_days),
           test_neg_equity,
           df = .)

toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()


plan(multisession, workers = Cores)

tic(paste0("furrr, ", Cores, " cores"))

df_res$neg_eq <-
  df %>%
  future_map2_dbl(as.list(df_res$date),
                  as.list(df_res$lead_days),
                  test_neg_equity,
                  df = .)

toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()

maxcores <- 12

for (Cores in 1:maxcores) {
  plan(multisession, workers = Cores)
  
  tic(paste0("furrr, ", Cores, " cores"))
  
  df_res$neg_eq <-
    df %>%
    future_map2_dbl(as.list(df_res$date),
                    as.list(df_res$lead_days),
                    test_neg_equity,
                    df = .)
  
  toc(log = TRUE)
}

printTicTocLog() %>%
  tail(maxcores) %>%
  separate(
    `Function type`,
    sep = " ",
    into = c("Function type", "nCores"),
    extra = "drop"
  ) %>%
  mutate(
    Seconds = as.numeric(Seconds),
    nCores = as.numeric(nCores),
    lowered_compute_time = Seconds / lag(Seconds, order_by = nCores) - 1,
    theoretical_max = lag(nCores) / nCores - 1
  ) %>%
  ggplot(aes(x = nCores)) +
  geom_line(aes(y = lowered_compute_time, col = "Realized performance gain")) +
  geom_line(aes(y = theoretical_max, col = "Theoretical performance gain")) +
  theme_classic() +
  xlab("Number of cores") +
  ylab("Lowered compute time by additional core") +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')



