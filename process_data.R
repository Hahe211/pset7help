library(tidyverse)
library(fs)

# automatically download the file
download.file(url = "https://github.com/TheUpshot/2018-live-poll-results/archive/master.zip",
              destfile = "master.zip",
              quiet = TRUE,
              # making sure the download works whether you have Mac or PC
              mode = "wb")
# unzip the downloaded zip file
unzip("master.zip")

# delete the zip file
file_delete("master.zip")

# read in the data
file_names <- dir_ls("2018-live-poll-results-master/data")
polls <- map_dfr(file_names, read_csv, .id = "source")

results <- read_csv("mt_2_results.csv")

write_rds(polls, "forecast.rds")

