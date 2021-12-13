library(readr)
library(tidyverse)

# make a function to fill the NA
fill_0_1 <- function(i){
  if (is.na(i)) return(0)
  else return(1)
}


get_the_first_word <- function(i){
  (strsplit(i, ";", fixed= T)[[1]][1])
}

clean_data <- function(){
  # get the csv data and save them as dataframes
  df1 <- read.csv(file = "steam.csv", header = T)
  df2 <- rbind(df1[20:25,],df1 %>% filter(appid == "4920"))
  
  ##-----------------get the first word in steamspy-----------
  # get_the_first_word <- function(i){
  #   strsplit(i, ";", fixed= T)[[1]][1]
  # }
  # 
  # df2$match1 <- df2$steamspy_tags %>% lapply(get_the_first_word)
  
  ##------------------- platform --------------------------
  # separate the flatforms
  df1<-df1 %>% mutate(Windows=str_match(df1$platforms,regex('(windows)'))[,1],
                      Mac=str_match(df1$platforms,regex('(mac)'))[,1],
                      Linux=str_match(df1$platforms,regex('(linux)'))[,1])
  
  
  # change the NA into 0
  df1$Linux <- df1$Linux %>% lapply(fill_0_1) %>% as.numeric()
  df1$Windows <- df1$Windows %>% lapply(fill_0_1) %>% as.numeric()
  df1$Mac <- df1$Mac %>% lapply(fill_0_1) %>% as.numeric()
  
  # add platform1 to record how many platforms the game support
  df1 <- df1 %>% mutate(platform1 = df1$Linux + df1$Windows + df1$Mac)
  
  
  ##------------------- rating ratio --------------------------
  
  # make one rating ratio column, defining it as (positive ratings/negative ratings)
  df1$rating_ratio <- df1$positive_ratings/df1$negative_ratings
  
  ##------------------- owners --------------------------
  # rank the owners
  df1 <-df1 %>% separate(col=owners,
                         into = c("owner1", "owner2"),
                         sep = "-",
                         fill='warn')
  df1 <- df1 %>% mutate_at(vars(contains("owner1")),as.numeric)
  df1 <- df1 %>% mutate(owner_rank=dense_rank(owner1))
  
  
  ## delete the rows where the data do not match the variables
  # write.csv(df1, "data1.csv")
  
  ##-----------------get the first word in steamspy-----------
  
  
  df1$steamspy_tags1 <- df1$steamspy_tags %>% lapply(get_the_first_word)
  df1 <- df1 %>% mutate_at(vars(contains("steamspy_tags1")),unlist)
  
  ##-----------------get the first publisher-----------
  
  df1$publisher1 <- df1$publisher %>% lapply(get_the_first_word)
  df1 <- df1 %>% mutate_at(vars(contains("publisher1")),unlist)
  
  ##-----------------get the multiplayer string in categories----
  
  df1<-df1 %>% mutate(multiplayer=str_match(df1$categories,regex('(Multi-player)'))[,1])
  df1$multiplayer <- lapply(df1$multiplayer, fill_0_1) %>% as.numeric()
  
  
  # df1 %>% group_by(steamspy_tags1) %>% summarise(count = n())
  
  # delete the useless columns
  df1 <- df1 %>% select(-c("appid","english","developer","publisher","platforms","required_age",
                 "categories","genres","steamspy_tags","owner2"))

}
df1 <- clean_data()


# 
# df22 <- data.frame()
# 
# cate <- df1$categories
# df_split_col <- data.frame(do.call("rbind", strsplit(as.character(df1$categories), ";", fixed = TRUE)))
# 
# for (i in colnames(df_split_col)){
#   df11 <- as.data.frame(unique(df_split_col[,i]))
#   df22 <- rbind(df22,df11)
#   df22 <- unique(df22)
# }
# colnames(df22) <- "categories"
# 
# # for (i in 1:nrow(df22)){
# #   df22[i,1] <- paste0("cate_",df22[i,1])
# # }
#   
# lst1 <- c(pull(df22, categories))
# 
# df2 <-df2 %>% separate(col=categories,
#                        into = df22$categories,
#                        sep = ";",
#                        fill='warn')
# 
# df2 %>% select(starts_with("cate"))
