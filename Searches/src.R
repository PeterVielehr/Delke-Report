
officers.gt100stops<-officers.summary %>%
  filter(n>=100) %>%
  filter(!is.na(Officer.Employee.number)) %>%
  mutate(delke=ifelse(Officer.Employee.number==focalofficer, T, F),
         pecentile.blacksearch=round(percent_rank(blacksearchrate)*100, digits = 1),
         pecentile.whitesearch=round(percent_rank(whitesearchrate)*100, digits = 1),
         pecentile.allsearch=round(percent_rank(searchpct)*100, digits = 1)) 
