library(rgdal)
library(sp)
library(RColorBrewer)
library(tmap)

towns <- readOGR(dsn=paste0(path, "Data/mnpd_zone/"), layer="Eight_Zone")
towns.nash<-towns
towns.nash$delkezones<-towns.nash$ZONE %in% z
towns.nash$delkezones[FALSE]<-NA
towns.nash$zonenames<-ifelse(towns.nash$delkezones==T, as.character(towns.nash$ZONE), NA)
