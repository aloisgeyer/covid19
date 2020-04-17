# descriptive.at.R 
# descriptive statistics for COVID19 using only Austrian data
# Source of data: https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich

library(tidyverse)
library(lubridate)
library(htmltab)
library(ggplot2)  

cases = htmltab("https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich",6) %>% 
  select("Datum"=1,confirmed=11,deaths=13,recovered=14) %>%
  mutate(date = paste0(substr(Datum,1,5),".20"),
         date = dmy(date),
         deaths=as.numeric(deaths),
         confirmed=gsub("[.]","",confirmed),
         confirmed=gsub("[^0-9.-]","",confirmed),
         confirmed=as.numeric(confirmed),
         recovered=as.numeric(recovered),
         ) %>% select(-Datum)

tests = htmltab("https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich",10) %>% 
  select("Datum"=1,tests=2) %>%
  na.omit() %>% 
  mutate(date = paste0(substr(Datum,1,5),".20"),
         date = dmy(date),
         tests=gsub("[.]","",tests),
         tests=gsub("[^0-9.-]","",tests),
         tests=gsub("36327","4836",tests), # 4836 results from a linear interpolation between 3519 (04/01) and 6153 (04/03)
         tests.new=as.numeric(tests))

# adjust test reports prior to 04/02 by distributing the exceptional value 36327 from 2020/04/02 (36327-4836)=31491 proportionately 
n=sum(tests[,"date"]<="2020-04-02")
h=tests[1:n,"tests.new"]/(36327-4836) # =31491
tests[1:n,"tests.new"]=tests[1:n,"tests.new"]+((36327-4836)*h/sum(h))
  
tests = tests %>% 
  mutate(tests=cumsum(tests.new),
         tests.weekly=tests-dplyr::lag(tests,n=7),
         tests.pct.weekly=tests.weekly/tests
  ) %>% select(-Datum)

all=merge(cases,tests,by="date")

all=all%>%arrange(date)%>%
  mutate(deaths.new=deaths-dplyr::lag(deaths,n=1),
         deaths.rate=deaths.new/dplyr::lag(deaths,n=1),
         deaths.pct=deaths.new/deaths,
         deaths.weekly=deaths-dplyr::lag(deaths,n=7,default=0),
         deaths.pct.weekly=deaths.weekly/deaths,
         confirmed.new=confirmed-dplyr::lag(confirmed,n=1),
         confirmed.rate=confirmed.new/dplyr::lag(confirmed,n=1),
         confirmed.pct=confirmed.new/confirmed,
         confirmed.weekly=confirmed-dplyr::lag(confirmed,n=7,default=0),
         confirmed.pct.weekly=confirmed.weekly/confirmed
         )

day_breaks = seq(as.Date("2020-03-01"), today(), "1 week")
day_labels = format(day_breaks, "%d/%m")

all=all%>%filter(date>"2020-02-29")

plt=ggplot(all) + geom_point(aes(x=date, y=confirmed.new*10, colour = "confirmed (daily new)" )) + 
  geom_point(aes(x=date, y=tests.new, colour = "tests (daily new)")) + 
  scale_x_date(breaks = day_breaks, labels = day_labels) + 
  xlab("date") +
  scale_y_continuous(trans="pseudo_log", 
                     sec.axis = sec_axis(~./10, name = "confirmed")) + 
  scale_colour_manual(values = c("red", "blue")) + 
  labs(colour = "",
       y="tests") +
  theme(legend.position = c(0.5, 0.25))
plt
ggsave(paste0("confirmed+test.new.at.",today(),".pdf"),units="cm",width=20,height=12)

plt=ggplot(all) + geom_point(aes(x=date, y=confirmed.pct.weekly, colour = "confirmed (weekly new as % of current total)" )) + 
  geom_point(aes(x=date, y=tests.pct.weekly, colour = "tests (weekly new as % of current total)")) + 
  scale_x_date(breaks = day_breaks, labels = day_labels) + 
  xlab("date") +
  scale_colour_manual(values = c("red", "blue")) + 
  labs(colour = "",y="") +
  theme(legend.position = c(0.3, 0.2))
plt
ggsave(paste0("confirmed+test.pct.weekly.at.",today(),".pdf"),units="cm",width=20,height=12)
