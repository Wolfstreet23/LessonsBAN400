install.packages("tidyverse")
library(tidyverse)
library(readxl)


df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output
#> [1] -0.24576245 -0.28730721 -0.05669771  0.14426335


#Calculating the mean of all the individual columns in the df "mtcars"

view(mtcars)
head(mtcars)
data(mtcars)

output <- vector("double", ncol(mtcars))  # 1. output
for (i in seq_along(mtcars)) {            # 2. sequence
  output[[i]] <- mean(mtcars[[i]])      # 3. body
}
output


#Determining the type of each column in the "nycflights13::flights" df.

install.packages("nycflights13")
library(nycflights13)

view(nycflights13::flights)

output <- vector("double", ncol(nycflights13::flights))  # 1. output
for (i in seq_along(nycflights13::flights)) {            # 2. sequence
  output[[i]] <- typeof(nycflights13::flights[[i]])      # 3. body
}
output


#Computing the number of unique values in each column of the "iris" df.


view(iris)

output <- vector("double", ncol(iris))  # 1. output
for (i in seq_along(iris)) { 
  output[[i]] <- length(unique(iris[[i]])) # 3. body
}
output


#Eliminate the for loop in each of the following examples by taking advantage of an 
#existing function that works with vectors:

out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out

#Solution:
paste(letters, collapse = "")

x <- mtcars
map_dbl(.x, mean)

map(mtcars, mean)

map_dfr(mtcars, mean)
