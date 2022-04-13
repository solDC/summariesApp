library(dplyr)
aux <- read.csv("data/articles.csv")
aux <- aux %>% select(id, title, summary)
write.csv(aux,file="data/summaries2.csv",row.names=FALSE)
