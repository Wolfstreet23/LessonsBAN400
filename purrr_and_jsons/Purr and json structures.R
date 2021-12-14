#Some tasks from the teachings----

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





#Using purr on traffic data from Vegvesenet -----


library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)

# Today we are going to work with some more advanced topics in 
# terms of data handling and processing. We will play with an API
# from Vegvesenet. Vegvesenet has an API we can query for data
# on traffic volumes at many sensor stations in Norway. 
# 
# The API uses graphQL for requests. This is a relatively
# new language we might see more of in the future?
# 
# First, define a function where we can submit queries to an API. 
GQL <- function(query,
                ...,
                .token = NULL,
                .variables = NULL,
                .operationName = NULL,
                .url = url) {
  pbody <-
    list(query = query,
         variables = .variables,
         operationName = .operationName)
  if (is.null(.token)) {
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <-
      POST(
        .url,
        body = pbody,
        encode = "json",
        add_headers(Authorization = auth_header),
        ...
      )
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(res$errors)) {
    warning(toJSON(res$errors))
  }
  res$data
}


# The URL we will use is stored below: 
url <- "https://www.vegvesen.no/trafikkdata/api/"


# First, let's figure out which sensor stations that are operable. 
# The query below extracts all the stations, with a date for 
# when the statiosn was in operation as well as a long/latitude. 
qry <-
  '
{
    trafficRegistrationPoints {
        id
        name
        latestData {
            volumeByDay
        }
        location {
            coordinates {
                latLon {
                    lat
                    lon
                }
            }
        }
    }
}
'

# Allright - let's try submitting the query: 
stations <-GQL(qry) 


# We now have the a long list in memory - 6mb! - with just 
# a little information on each station. We can note that this 
# is a list, not a dataframe. For our purposes, it would be better if
# the list was instead a data frame, with one row pr. sensor station. 


# First, we can note that the list only has one entry..   
length(stations)

# However, the list contains another list, called trafficRegistrationPoints. 
# This list has almost 3000 entries. We can select this sublist using 
# either $ or [[1]]. Note that when we subset a list, using [[i]] selects
# the contents of the item [[i]]. 
length(stations$trafficRegistrationPoints)

length(stations[[1]])

# Let's look at the first entry of this long list. We can see there is
# a station ID, station name, date time of latest recording from the station
# and coordinates. This looks like something that could fit well within 
# a data frame, with columns id, name, latestdata, lat, and lon. The 
# question is how!
stations[[1]][[1]]

# We could perhaps hope that we can force this list into a data frame. For
# this we will use as_tibble: 
stations[[1]][[1]] %>% 
  as_tibble()


# We now want to apply this as_tibble transformation to each of the stations, 
# and combine them in a single data frame. We could do this with lapply, 
# and then bind toghether the rows: 
lapply(stations[[1]], . %>% as_tibble) %>% 
  bind_rows

# Using the map_df-function we traverse all the entries in the stations list, 
# and transform these lists to data frames. 

# There is still some work left to do with the date time and location 
# columns. As you can see below, they are still in a list format. 
stations[[1]] %>% 
  map_df(~as_tibble(.))

# We can try to pull out the insides of the contents of the latestData-
# column. It is formatted as a list, but actually only contains one 
# date time entry. 
stations[[1]] %>% 
  map_df(~as_tibble(.)) %>% 
  head(1) %>% 
  select(latestData) %>% 
  pull


# There are two complications to this one.. 
# 1:  Similarly to the previous task, we want to apply a transformation
#     to all entries of a list.. 
# 2:  However at least one of the entries does not contain the list item
#     "latestData". 
#     
# As you can see from the documentation, the map-functions are 
# very flexible, and we can e.g. use them to extract named items
# from a list. Below, we are asking map_chr to return the first item
# of each sub list in latestData. However, this will fail if it 
# meets an entry that does not have aything stored under latestdata!

stations[[1]] %>% 
  map_df(~as_tibble(.)) %>% 
  mutate(latestData = map_chr(latestData, 1))


# One way of solving this one is to write a custom "unlisting"-function.
# The function below unlists the elements of latestData - if there are 
# any elements there. If it the content is null, the function just
# returns an empty character string. 

unlist_safe <- 
  function(x){
    x <- unlist(x)
    if(is.null(x)){
      return("")
    }else{
      return(x)
    }
  }

# This should now work: 
stations[[1]] %>% 
  map_df(~as_tibble(.)) %>% 
  mutate(latestData = map_chr(latestData, unlist_safe))

# Alternatively, we can use the defaults in map_chr. It will 
# now have a safe fallback value it can use if it doesn't 
# find the element we are looking for in latestData. 
stations[[1]] %>% 
  map_df(~as_tibble(.)) %>% 
  mutate(latestData = map_chr(latestData, 1, .default = ""))


# Next, let's format the date format. Date formats can be tricky, but is
# an obstacle you just have to learn to work with. We can reformat the 
# latestData column into a date by simply using as.Date - however - 
# we now have lost information on the time of day. Let's see if we 
# can retain all the information in the column. 
stations[[1]] %>% 
  map_df(~as_tibble(.)) %>% 
  mutate(latestData = map_chr(latestData, 1, .default="")) %>% 
  mutate(latestData = as.Date(latestData))

# There are several functions we can use to transform the string into
# a date time variable. as_datetime in lubridate works in this case. 
# Note that the interpretation of dates may be dependent on the time zone
# settings on your laptop. Here, we are explicitly stating that we want the
# a Europe/Berlin timezone on the variable: 
stations[[1]] %>%
  map_df( ~ as_tibble(.)) %>%
  mutate(latestData = map_chr(latestData, 1, .default = "")) %>%
  mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>%
  mutate(hh = hour(latestData)) %>%
  arrange(desc(hh))


#Let's take on the final location variable. Complete the operation by unpacking the location column into two columns: 
#lat and lon. You may use the functions you have already seen, or see of you can find mode specialized functions. 
#Note: This a nested list i.e. the contents of a cell in "location" is a list with one entry. 
#This list contains two other lists..
#Submit your homework with a complete script that reads in data from Vegvesenet, 
#and transforms it to a data frame without any list columns.
#The script should return a dataframe similar to the one below (only the first few entries shown).

stations[[1]] %>%
  map_df( ~ as_tibble(.)) %>%
  mutate(latestData = map_chr(latestData, 1, .default = "")) %>%
  mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>%
  mutate(hh = hour(latestData)) %>%
  arrange(desc(hh)) %>% 
  mutate(lat = map_chr(location$coordinates$latLon$lat, 1, .default = "")) %>%  
  mutate(lon = map_chr(location$coordinates$latLon$lon, 1, .default = ""))

#Can not get this to get the longtitude and latitude for each station

?map_chr

stations[[1]] %>% 
  map_df(~as_tibble(.)) %>% 
  head(3) %>% 
  select(location) %>% 
  pull



