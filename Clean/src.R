library(tidyverse)


codeddata<-mnpd17.18 %>%
  mutate(hispanic=ifelse(Suspect.Ethnicity=="H" | Suspect.Ethnicity=="Y", 1, 
                         ifelse(is.na(Suspect.Ethnicity), NA, 0)),
         white=ifelse(hispanic==1, 0, 
                      ifelse(Race=="W", 1, 
                             ifelse(is.na(Race), NA, 0))),
         black=ifelse(hispanic==1, 0, 
                      ifelse(Race=="B", 1, 
                             ifelse(is.na(Race), NA, 0) )),
         otherrace=ifelse(white==0 & black==0 & hispanic==0, 1, 0),
         race=ifelse(white==1, "white", ifelse(black==1, "black", ifelse(hispanic==1, "hispanic", ifelse(otherrace==1, "other race", NA)))),
         female=ifelse(is.na(Sex), NA, ifelse(Sex=="F", 1, ifelse(Sex=="U", NA, 0))),
         search=ifelse(SEARCHOCCUR=="Y", 1, 0),
         patdown=ifelse(Pat.Down.Search=="Y", 1, 0),
         #search justificaiton
         probablecause=ifelse(Search.Probable.Cause=="Y", 1, 0),
         consentsearch=ifelse(probablecause==1, 0, ifelse(Search.Consent=="Y", 1, 0)),
         warrantsearch=ifelse(Search.Warrant=="Y", 1, 0),
         plainview=ifelse(Search.Plain.View=="Y", 1, 0),
         inventorysearch=ifelse(Search.Inventory=="Y", 1, 0),
         arrestsearch=ifelse(Search.Arrest=="Y", 1, 0),
        
         searchtype=ifelse(consentsearch==1, "Consent Search",
                           ifelse(probablecause==1, "Probable Cause",
                                  ifelse(search==1, "Other Search", NA))),
         
         #evidence in searches
         evidence=ifelse(is.na(EVIDENCESEIZED), NA, ifelse(EVIDENCESEIZED=="Y", 1, 0)),
         drugs=ifelse(is.na(Drugs.Seized), NA, ifelse(Drugs.Seized=="Y", 1, 0)),
         weapons=ifelse(is.na(Weapons.Seized), NA, ifelse(Weapons.Seized=="Y", 1, 0)),
         otherseized=ifelse(is.na(Other.Seized), NA, ifelse(Other.Seized=="Y", 1, 0)),


         zone= ifelse(Zone<100, NA, 
                      ifelse(is.na(Zone), NA, Zone)),
         month = format(Stop.Date.Time, "%m"), year = format(Stop.Date.Time, "%Y"),
         month.yr=as.Date(paste(year, month, "1", sep = "/")),
         delke=ifelse(Officer.Employee.number==focalofficer, T, F),
         crime.reduction=ifelse(Crime.Reduction.Initiative=="Y", 1, 
                                ifelse(is.na(Crime.Reduction.Initiative), NA, 0))) 

