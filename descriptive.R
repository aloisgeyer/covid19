# https://joachim-gassen.github.io/2020/03/meet-tidycovid19-yet-another-covid-19-related-r-package/

# country.sel is the list auf countries for which comparisons are made and charts are produced; last entry is used
country.sel=c("Italy","Austria","Spain","Sweden","Germany","United Kingdom")
country.sel=c("Italy","Austria","Spain","France","Belgium","Greece")
country.sel=c("Italy","Austria","Spain","Sweden","Netherlands","United Kingdom")
# country used as a reference (for shifting time axis)
country.ref="Italy"

#remotes::install_github("joachim-gassen/tidycovid19") # requires "remotes" to be installed
suppressPackageStartupMessages({
  library(tidycovid19)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)  
  library(ggrepel)
  library(utils)
  library(httr)
  library(scales)
  library(lubridate)
})

all <- download_merged_data(cached = T)

all=all%>%arrange(country,date)%>%group_by(country)%>%
  mutate(pop=population/1000000,
         confirmed.pm=confirmed/pop,
         deaths.pm=deaths/pop,
         deaths.new=deaths-dplyr::lag(deaths,n=1),
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

#obtain date of first death
dfd=all%>%arrange(country,date)%>%group_by(country)%>%
  mutate(diff=deaths-dplyr::lag(deaths,n=1))%>%
  filter(diff>0)%>%filter(row_number()==1)%>%
  summarize(date.first.death=date)%>%data.frame()

#obtain date of first death.pm > 1 #
ddpm=all%>%arrange(country,date)%>%group_by(country)%>%
  filter(deaths.pm>1) %>% filter(row_number() == 1) %>%
  summarize(date.deaths.pm = date) %>% data.frame()

#obtain date of first confirmed case pm > 1
dcpm=all%>%arrange(country,date)%>%group_by(country)%>%
  filter(confirmed.pm>1)%>%filter(row_number()==1)%>%
  summarize(date.confirmed.pm=date)%>%data.frame()

all=merge(all,dfd,by="country")
all=merge(all,ddpm,by="country")
all=merge(all,dcpm,by="country")

all=all%>%arrange(country,date)%>%group_by(country)

most.recent=all%>%group_by(country)%>%arrange(date)%>%filter(row_number()==n())%>%data.frame()
most.recent=most.recent%>%filter(country%in%country.sel)
write.table(format(as.matrix(most.recent[,c("country","date","confirmed","deaths","confirmed.pm","deaths.pm","deaths.pct","deaths.pct.weekly","deaths.rate","date.first.death")])),
            file="most.recent.csv",sep=" ",row.names = F)

sel=all%>%filter(country%in%country.sel)%>%arrange(country,date)%>%group_by(country)

# sel=sel%>%filter(date>"2020-02-29")%>%data.frame()

sel %>% filter(country=="Austria") %>% select(confirmed.new) %>% tail()

day_breaks = seq(as.Date("2020-03-01"), today(), "1 week")
day_labels = format(day_breaks, "%d/%m")
# 
# plt=ggplot(sel%>%filter(date>"2020-02-29"), aes(x=date, y=deaths.pm,col=country,shape=country)) + geom_point() +
#   scale_x_date(breaks = day_breaks, labels = day_labels) + 
#   scale_y_continuous(trans="pseudo_log") +
#   ylab("deaths (per million population)")
# plt
# 
# plt=ggplot(sel%>%filter(date>"2020-02-29"), aes(x=date, y=deaths.pct.weekly,col=country,shape=country)) + geom_point() + geom_smooth(se=F,span=2) + 
#   scale_x_date(breaks = day_breaks, labels = day_labels) + 
#   ylab("death rate (weekly new over total)")
# plt
# 
# plt=ggplot(sel%>%filter(date>"2020-02-29"), aes(x=date, y=confirmed.pm,col=country,shape=country)) + geom_point() +
#   scale_x_date(breaks = day_breaks, labels = day_labels) + 
#   scale_y_continuous(trans="log10") +
#   ylab("confirmed (per million population)")
# plt

# shift time axis according to one of the following criteria (last criteria is used)
crit="deaths.rate"
crit="deaths.pm"
crit="confirmed.rate"
crit="date.confirmed.pm"
crit="date.deaths.pm"
crit="date.first.death"

ref=sel%>%filter(country==country.ref&date>get(crit))%>%data.frame() # data for reference country
sel.mod=ref # to be stacked below

for (j in 1:length(country.sel)){
  
  if (country.sel[j]!=country.ref){
    cs=country.sel[j]
    sel.c=subset(sel,country==cs)
    
    if(crit=="date.first.death" | crit=="date.confirmed.pm" | crit=="date.deaths.pm"){
      h=sel%>%filter(country==cs&date>=get(crit))%>%data.frame()
      h[,"date"]=ref[1:nrow(h),"date"] # shift data by overwriting with the reference country's dates
      h[,"country"]=paste0(cs,".shifted.",nrow(ref)-nrow(h))
      
    }else{
      
      # compute cross-correlations and determine lag with max correlation
      cc=ccf(sel.c[,crit],ref[,crit],lag.max=14,plot=F,type="correlation")
      cc=cc$acf[1:14]
      c.max.index=which(cc==max(cc))

      h=sel%>%filter(country==cs)%>%
        mutate(deaths.pm=dplyr::lead(deaths.pm,n=c.max.index),
               deaths.rate=dplyr::lead(deaths.rate,n=c.max.index),
               confirmed.pm=dplyr::lead(confirmed.pm,n=c.max.index),
               confirmed.pct.weekly=dplyr::lead(confirmed.pct.weekly,n=c.max.index))
      h[h[,"country"]==cs,"country"]=paste0(cs,".shifted.",c.max.index)
    }
    
    sel.mod=rbind(sel.mod,h)
    
  }
}

plt=ggplot(sel.mod, aes(x=date, y=deaths.pm,col=country,shape=country)) + geom_point() +
  scale_x_date(breaks = day_breaks, labels = day_labels) + 
  scale_y_continuous(trans="pseudo_log") + 
  ylab("deaths (per million population)") + 
  xlab(paste0("shift based on ",crit)) +
  theme(legend.position = c(0.75, 0.3))
plt
ggsave(paste0("deaths.pm.",paste0(country.sel,collapse="."),".",today(),".pdf"),units="cm",width=20,height=12)

plt=ggplot(sel.mod, aes(x=date, y=deaths.pct.weekly,col=country,shape=country)) + geom_point() + geom_smooth(se=F,span=2) + 
  scale_x_date(breaks = day_breaks, labels = day_labels) + 
  ylab("death rate (weekly new over total)") + 
  xlab(paste0("shift based on ",crit)) +
  theme(legend.position = c(0.2, 0.25))
plt
ggsave(paste0("deaths.pct.weekly.",paste0(country.sel,collapse="."),".",today(),".pdf"),units="cm",width=20,height=12)

plt=ggplot(sel.mod, aes(x=date, y=confirmed.pm,col=country,shape=country)) + geom_point() + 
  scale_x_date(breaks = day_breaks, labels = day_labels) + 
  scale_y_continuous(trans="log10") + 
  ylab("confirmed (per million population)") + 
  xlab(paste0("shift based on ",crit)) +
  theme(legend.position = c(0.75, 0.3))
plt
ggsave(paste0("confirmed.pm.",paste0(country.sel,collapse="."),".",today(),".pdf"),units="cm",width=20,height=12)

plt=ggplot(sel.mod, aes(x=date, y=confirmed.pct.weekly,col=country,shape=country)) + geom_point() + 
  scale_x_date(breaks = day_breaks, labels = day_labels) + 
  ylab("confirmed rate (weekly new over total)") + 
  xlab(paste0("shift based on ",crit)) +
  theme(legend.position = c(0.8, 0.75))
plt
ggsave(paste0("confirmed.pct.weekly.",paste0(country.sel,collapse="."),".",today(),".pdf"),units="cm",width=20,height=12)
