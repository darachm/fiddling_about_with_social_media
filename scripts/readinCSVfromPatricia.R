
library(tidyverse)
library(stringr)

indatar <- read_csv("data/c-yeas-fordarach.csv") %>% 
  distinct %>% 
  mutate(Year=as.numeric(str_c(ifelse(Year>69,19,20),Year))) %>%
  mutate(Position=c(
    `-`=NA
   ,`1`="LabHead",`2`="Professor",`3`="MD",`4`="StaffScientist"
   ,`5`="Postdoc",`6`="GradStudent",`7`="Other",`8`="Press"
   )[Position])%>%
  mutate(CourseRole=c(
    A="TeachingAssistant"
    ,C="Co-Instructor"
    ,I="Instructor"
    ,L="Lecturer"
    ,S="Student"
    )[`Role at Course`]) %>% select(-`Role at Course`) %>%
  mutate(g_deg=c(
    `AM,PhD`="AM,PhD"
    ,`D.Phil`="PhD"
    ,`M.D.`="MD"
    ,`M.Sc.`="MS"
    ,`MA`="MA"
    ,`Mag.`="Mag."
    ,`MAppSc`="MAS"
    ,`MD`="MD"
    ,`MD-PhD`="MD,PhD"
    ,`ME`="ME"
    ,`MS`="MS"
    ,`MS PhD`="MS,PhD"
    ,`MS,PhD`="MS,PhD"
    ,`Msc`="MS"
    ,`MSc`="MS"
    ,`n/a`=NA
    ,`PhD`="PhD"
    ,`Ph.D`="PhD"
    ,`Ph.D.`="PhD"
    ,`phd`="PhD"
    ,`phD`="PhD"
    ,`PhD`="PhD"
    ,`PHD`="PhD"
    ,`PhD ip`="PhD"
    ,`PhD/MS`="MS,PhD"
    ,`Pos-do`="PhD" # I assume this is a post-doctoral researcher
    )[g_deg]) %>%
#Need to re-code the undergraduate degrees, maybe
  mutate_at(.vars=c("ug_from","ug_to","gs_from","gs_to")
    ,function(x){ # We exclude anything that doesn't
      x <- ifelse(grepl("^\\d\\d$",x)
        ,str_c(ifelse((as.numeric(x)>50)&(as.numeric(x)<100),19,20),x)
        ,x)
      x <- ifelse(grepl("\\d\\d\\d\\d",x),as.numeric(x),NA)
      return(x)
    })%>%
  arrange(-Year)

# Below, we try to track unique people. We assume no name changes,
# and no gender changes. This list should be sorted and inspected
# for putative possible violations of the above assumptions.
procdatar <- indatar %>% 
  unite(IDString,`First Name`,`M.I.`,`Last Name`,`Gender`,remove=F)%>%
  group_by(IDString)%>%nest%>%arrange(IDString)

save(procdatar,file="data/nestedTibble.RData")

procdatar%>%unnest %>% write_csv("data/unnestedData.csv")

