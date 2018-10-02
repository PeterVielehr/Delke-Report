library(tidyverse)

officers.byzone <- codeddata %>% 
  filter(!is.na(Officer.Employee.number)) %>%
  filter(!is.na(zone)) %>%
  group_by(Officer.Employee.number, zone) %>%
  dplyr::summarise(n=n(),
                   blackstops=sum(black, na.rm=T),
                   whitestops=sum(white, na.rm=T),
                   probablecause=sum(probablecause, na.rm=T),
                   consentsearch=sum(consentsearch, na.rm=T),
                   totalsearch=sum(search, na.rm = T),
                   allserchevidence=sum(evidence[search==1], na.rm=T),
                   totalsearch.black=sum(search[black==1], na.rm = T),
                   allserchevidence.black=sum(evidence[search==1 & black==1], na.rm=T),
                   totalsearch.white=sum(search[white==1], na.rm = T),
                   allserchevidence.white=sum(evidence[search==1 & white==1], na.rm=T),
                   total.crime.reduction=sum(crime.reduction, na.rm = T),
                   search.crime.reduction=sum(crime.reduction[search==1], na.rm = T),
                   evidence.crime.reduction=sum(crime.reduction[search==1 & evidence==1], na.rm = T),
                   search.noncrime.reduction=sum(search[crime.reduction==0], na.rm = T),
                   evidence.noncrime.reduction=sum(evidence[search==1 & crime.reduction==0], na.rm = T)) %>%
  mutate(total.noncrime.reduction=n-total.crime.reduction,
         probcause.rate=round((probablecause/n)*100, digits = 1),
         consent.rate=round((consentsearch/n)*100, digits = 1),
         searchpct=round((totalsearch/n)*100, digits = 1),
         allsearch.hitrate=round((allserchevidence/totalsearch)*100, digits = 1),
         blackrate=round((blackstops/n)*100, digits = 1),
         whiterate=round((whitestops/n)*100, digits = 1),
         blacksearchrate=round((totalsearch.black/blackstops)*100, digits = 1),
         whitesearchrate=round((totalsearch.white/whitestops)*100, digits = 1),
         searchrate.crime.reduction=round((search.crime.reduction/total.crime.reduction)*100, digits = 1),
         hitrate.crime.reduction=round((evidence.crime.reduction/search.crime.reduction)*100, digits = 1),
         delke=ifelse(Officer.Employee.number==focalofficer, T, F)) 


z<-officers.byzone$zone[officers.byzone$Officer.Employee.number==focalofficer & officers.byzone$n>=10]

delke.zones<-officers.byzone %>%
  filter(n>=10) %>%
  filter(zone %in% z) %>%
  plyr::ddply(c("zone"), transform, blackrate.std = scale(blackrate)) %>%
  plyr::ddply(c("zone"), transform, searchpct.std = scale(searchpct))%>%
  plyr::ddply(c("zone"), transform, blackrate.mean = mean(blackrate))%>%
  plyr::ddply(c("zone"), transform, blackrate.sd = sd(blackrate))



