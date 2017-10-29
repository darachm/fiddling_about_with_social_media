
library(tidyverse)
library(stringr)

left_join(
  read_csv("../data/YeastAlumniTrackingSheet.csv") 
  ,read_csv("../data/SocialMediaTrackingSheet.csv") %>% 
    mutate(IDString=sub("[_0-9]*$","",IDString))
  ,by="IDString") %>% 
  select(IDString,Year,CourseRole,Facebook,Twitter,Email)%>%
  arrange(-Year,CourseRole)%>%
  filter(CourseRole!="Lecturer")%>%
  write_csv("../data/toplist.csv")


