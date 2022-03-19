library(jsonlite)
library(dplyr)

#https://stackoverflow.com/questions/35016713/how-do-i-import-a-jsonl-file-in-r-and-how-do-i-transform-it-in-csv/62311093#62311093
lines <- readLines("data/spanish_test.jsonl")
lines <- lapply(lines, fromJSON)
lines <- lapply(lines, unlist)
data <- bind_rows(lines)
write.csv(data,file="data/articles.csv",row.names=FALSE)
