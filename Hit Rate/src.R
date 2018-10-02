
officers.gt100stops.gt5search<-officers.summary %>%
  filter(n>=100) %>%
  filter(totalsearch.black>5)%>%
  filter(!is.na(Officer.Employee.number)) %>%
  mutate(delke=ifelse(Officer.Employee.number==focalofficer, T, F)) 

hitrate<-officers.gt100stops.gt5search %>%
  select(delke, allsearch.hitrate, black.hitrate) %>%
  rename(`All Drivers`=allsearch.hitrate,
         `Black Drivers`=black.hitrate) %>%
  gather(hittype, value, `All Drivers`, `Black Drivers`)

hitrate.delke<-officers.gt100stops.gt5search %>%
  mutate(delkesearchhitrate=allsearch.hitrate[delke==T],
         delkeblacksearchhitrate=black.hitrate[delke==T]) %>%
  gather(key= delkerate, delkevalue, delkesearchhitrate, delkeblacksearchhitrate)

hitrate$delkevalue<-hitrate.delke$delkevalue
