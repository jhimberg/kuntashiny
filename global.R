library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)

library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)
library(RColorBrewer)
library(animation)
library(gganimate)
library(ggiraph)

source("initGeoDemografia.R")

data.directory <- "Data/"

load(paste(data.directory, "geografia.RData", sep=""))
load(paste(data.directory, "demografia.RData", sep=""))
load(paste(data.directory, "kunnat.kartogrammi.RData", sep=""))

geo$kunta$kartogrammi<-transmute(kunnat.kartogrammi,
                                 id,
                                 long,
                                 lat,
                                 order,
                                 hole,
                                 piece,
                                 group,
                                 kunta,
                                 kuntanimi=iconv(kuntanimi,to="UTF-8"))
                                 
rm(kunnat.kartogrammi)

kartta <- function(df, aluejako="pono.3", title.label=NA, geo_=geo, color.map="OrRd", color.limits=c(NA,NA)) {
  # yhdistää kartan ja datan; plottaa ensimmäisen muuttujan jonka nimi ei ole "alue" 
  geodata <- list()
  geodata[["pono.5"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=pono) 
  geodata[["pono.3"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,3))
  geodata[["pono.2"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,2))
  geodata[["pono.1"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,1))
  geodata[["kuntanimi"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntanimi)
  geodata[["kunta"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntano)
  geodata[["kartogrammi.kuntanimi"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kuntanimi)
  geodata[["kartogrammi.kunta"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kunta)
  geodata <- left_join(geodata[[aluejako]](geo_), df, by="alue")
  
  attr=names(select(df, -alue))[1] 
  if(is.na(title.label)) title.label <- attr
  
  p <- ggplot(data=arrange(geodata, order), aes(x=long, y=lat))+ 
    geom_polygon_interactive(aes_string(fill = attr, group="group", tooltip = "alue"), 
                             colour=NA)+
    scale_fill_gradientn(colours= brewer.pal(9,color.map), 
                         values = NULL, 
                         space = "Lab", 
                         na.value = "grey50", 
                         guide = "colourbar", 
                         limits=color.limits)+
    theme_void()+theme(legend.title=element_blank())+ggtitle(title.label)
  
  if(!grepl("^pono",aluejako)) 
    p<-p+coord_equal(ratio=1) 
  else 
    p<-p+coord_equal(ratio=2.1) 
  return(p)
}

kartta.animaatio <- function(df, aluejako="pono.3", geo_=geo, color.map="OrRd", title.label=NA) {
  # yhdistää kartan ja datan; plottaa ensimmäisen muuttujan, ei muita
  
  geodata <- list()
  geodata[["pono.5"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=pono) 
  geodata[["pono.3"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,3))
  geodata[["pono.2"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,2))
  geodata[["pono.1"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono,1,1))
  geodata[["kuntanimi"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntanimi)
  geodata[["kunta"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntano)
  geodata[["kartogrammi.kuntanimi"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kuntanimi)
  geodata <- left_join(geodata[[aluejako]](geo_), df, by="alue")
  attr=names(select(df, -alue, -aika))[1] 
  if(is.na(title.label)) title.label <- attr
  
  p <-ggplot(data=arrange(geodata,order), aes(x=long, y=lat, frame=aika))+ 
    geom_polygon_interactive(aes_string(fill=attr, group="group", tooltip="alue"), colour=NA)+
    scale_fill_gradientn(colours= brewer.pal(9,color.map), values = NULL, space = "Lab", na.value = "grey50", 
                         guide = "colourbar") + theme_void() +
    theme(legend.title=element_blank()) + ggtitle(title.label) 
  return(p)
}

kuntadata <- mutate(demografia$kunta$tunnusluku,
                    kuntanimi=iconv(kuntanimi, to="UTF-8"), 
                    kuntanimi=map.vanhat.kuntanimet(kuntanimi)) %>%
  filter(kuntanimi!="KOKO MAA") 

koko.maa <- filter(demografia$kunta$tunnusluku, kuntanimi=="KOKO MAA") 

kunta.stat.vars <- names(select(demografia$kunta$tunnusluku,-vuosi,-kuntanimi))
vuodet <- unique(demografia$kunta$tunnusluku$vuosi)
karttatyyppi=list(label=c("tavallinen","kartogrammi"), 
                  aluejako=c("kuntanimi","kartogrammi.kuntanimi"))



