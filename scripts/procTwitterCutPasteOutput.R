
library(tidyverse)
library(stringr)

z <- bind_rows(
  readLines("data/twitterFollowingInventory171029") %>%
    { ifelse(grepl("^\\s*$",.),"|",.) } %>%str_c(collapse=",") %>%
    { gsub("\\|,\\|","|",.) } %>%
    str_split(pattern="\\|,") %>%unlist %>%
    str_split(",",n=4) %>%
    map(function(x){ str_c(sub(".*(@.*)$","\\1",x[1])," , ",x[3]) })%>%
    unlist%>%{tibble(Type="Following",Account=.)}
  ,readLines("data/twitterFollowerInventory171029") %>%
    { ifelse(grepl("^\\s*$",.),"|",.) } %>%str_c(collapse=",") %>%
    { gsub("\\|,\\|","|",.) } %>%
    str_split(pattern="\\|,") %>%unlist %>%
    str_split(",",n=4) %>%
    map(function(x){ str_c(sub(".*(@.*)$","\\1",x[1])," , ",x[3]) })%>%
    unlist%>%{tibble(Type="Follower",Account=.)}
  )  %>% 
  group_by(Account)%>%spread(Type,Type) %>%
  arrange(Follower,Following)

write_delim(z,"data/twitterInventory171029.txt")
