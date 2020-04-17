library(tidyverse)
library(lubridate)
library(htmltab)

cases = htmltab("https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich",6) %>% 
  rename("Datum"=1,B=2,K=3,N=4,O=5,Sb=6,St=7,T=8,V =9, W=10,
         confirmed=11,deaths=13,recovered=14) %>%
  select(1,11,13,14) %>%
  mutate(date = paste0(substr(Datum,1,5),".20"),
         date = dmy(date),
         deaths=as.numeric(deaths),
         confirmed=gsub("[.]","",confirmed),
         confirmed=gsub("[^0-9.-]","",confirmed),
         confirmed=as.numeric(confirmed),
         recovered=as.numeric(recovered),
         ) %>% select(-Datum)

tests = htmltab("https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich",10) %>% 
  rename("Datum"=1,tests=2) %>%
  na.omit() %>% 
  select(1,2) %>%
  mutate(date = paste0(substr(Datum,1,5),".20"),
         date = dmy(date),
         tests=gsub("[.]","",tests),
         tests=gsub("[^0-9.-]","",tests),
         tests=gsub("36327","5327",tests),
         tests.new=as.numeric(tests))
