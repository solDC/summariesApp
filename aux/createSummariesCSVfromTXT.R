library(readr)
summaries <- as.data.frame(read_lines("../data/resumenesTEST.txt"))
colnames(summaries)<-c("summary")
write.csv(summaries,file="../data/summaries.csv",row.names=FALSE)
summariesNew <- read_csv("../data/summaries.csv",show_col_types = FALSE)
summariesNew
