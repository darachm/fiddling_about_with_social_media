
library(tidyverse)
library(stringr)

z <- readLines("data/facebookGroupInventory171029")%>%
  {ifelse(.=="","|",.)}%>%str_c(collapse=",")%>%
  str_split(pattern="\\|,")%>%unlist%>%str_split(",",n=2)%>%
  map(function(x){x[1]})%>%unlist%>%unique
z <- z[z!=""]
write_delim(data.frame(z),"data/facebookGroupInventory171029.txt")
