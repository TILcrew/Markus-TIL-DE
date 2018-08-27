# setting up my working directory
setwd("C:/Users/Markus/Documents/Markus/Weekly Challenge Alteryx/Week1")
dir()

library(plyr)

# import both .csv files
data    <- read.csv2("data.csv", header = T, sep = ",")
lookup  <- read.csv2("lookup.csv", header = T, sep = ",") 

################### create new range field to join our data ################### 
# extract the start and the end of our range
for (i in 1:nrow(lookup)) {
  lookup$start[i]   <- as.numeric(substr(as.character(lookup$Range[i]),start = 1,stop = 4))
  lookup$end[i]     <- as.numeric(substr(as.character(lookup$Range[i]),start = 6,stop = 9))
}

lookup$end[1]-lookup$start[1]+1

# build new lookup table
dummy <- lookup[rep(seq_len(nrow(lookup)), each=20),]
# add this f#+#"$%# last line
dummy <-rbind(dummy,dummy[100,])
# get rid of this stupid row names
row.names(dummy)<-c()
# split it and create correct join indicator
dummyL   <- split(dummy,f=dummy$Range)
for (i in 1:length(dummyL)) {
  for (j in 1:(dummyL[[i]]$end[1]-dummyL[[i]]$start[1]+1)){
    dummyL[[i]]$Postal.Area[j] <- seq(from=dummyL[[i]]$start[1],
                                   to=dummyL[[i]]$end[1])[j]
  }
}
lookup <-do.call("rbind.fill",dummyL)
row.names(lookup)<-c()

################### join the data ################### 
# join
join  <-merge(x = data,
              y = lookup,
              by = "Postal.Area")
# select columns
join    <-join[,c("Region","Sales.Rep","Responder")]


###################  summarize it ################### 
join$ID         <- paste(join$Region,join$Sales.Rep,join$Responder,sep = "-")
count_result    <- count(join, "ID")
result          <- subset(join,!duplicated(join$ID))
result          <- merge(x = result,
                         y = count_result,
                         by = "ID",
                         all.x = T)



