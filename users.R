library(sodium)
user_base <- data.frame(
  username_id = c("admin","user1","user2"),
  passod = sapply(c("adminpass","pass1", "pass2"), sodium::password_store),
  permission = c("admin", "expert","expert")
)

#https://stackoverflow.com/questions/51386027/how-to-export-tibble-to-csv 
write.csv(user_base,file="data/users.csv",row.names=FALSE)
