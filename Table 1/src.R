

z<-officers.byzone$zone[officers.byzone$Officer.Employee.number==focalofficer & officers.byzone$n>=10]

delke.zones.allofficer.summary<-officers.byzone %>%
  filter(n>=10) %>%
  filter(zone %in% z) %>%
  filter(!is.na(zone)) %>%
  group_by(zone) %>%
  summarise(mean.stops=round(mean(n, na.rm=T), digits=1),
            sd.stops=round(sd(n, na.rm=T), digits=1),
            median.stops=round(median(n, na.rm=T), digits=1),
            mean.blackstops=round(mean(blackstops, na.rm=T), digits=1),
            mean.whiterate=round(mean(whiterate, na.rm=T), digits=1),
            sd.blackstops=round(sd(blackstops, na.rm=T), digits=1),
            mean.blackrate=round(mean(blackrate, na.rm=T), digits=1),
            sd.blackrate=round(sd(blackrate, na.rm=T), digits=1),
            median.blackstops=round(median(blackstops, na.rm=T), digits=1))

delke.summary<-delke.zones %>%
  filter(Officer.Employee.number==focalofficer)

