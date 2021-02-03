library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/tabulate")

dir()

library(tidyverse)
# make a list to fill
df_list <- list()
# the county abbreviations
cnty_abv <- c("alb","bld","chs","dgl","gvl","mes","sac","son")
# loop to get data
for (i  in 1:8) {
  cname <- cnty_abv[i]
  # get data
  df <- read.csv(paste0("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/tabulate/",cname, ".csv"), stringsAsFactors = F)
  df <- mutate(df, county_ID = paste0(cname))
  # get parcels sizes and county ID
  df_list[[i]] <- df %>% select(OBJECTID, AREA, PERCENTAGE, county_ID)
  #df_all <- bind_rows(df_all, df)
}


# combine all of the county data
alldata <- bind_rows(df_list)

#### Count of CE's ####

ce_nced_overlay_count <- data.frame("County ID"=character(), "Total CE Overlays"=numeric(), "Average Percent Overlay"=logical())

nrow(alldata)

count(alldata, County.ID=="alb")

test <- alldata %>%
count(county_ID)

test %>% ggplot(aes(county_ID, n)) + geom_bar(stat = "identity")


