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


data.directory <- "."

load(paste(data.directory,"/Data/geografiat.RData", sep=""))
load(paste(data.directory,"/Data/demografiat.RData", sep=""))
load(paste(data.directory,"/Data/kunnat.kartogrammi.RData", sep=""))

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

# Kunnat v. 2017
map.kunta <- function(v) 
  plyr::mapvalues(as.character(v),as.character(geo$kunta.vanhat2uudet$kuntano.old), 
            as.character(geo$kunta.vanhat2uudet$kunta)) %>% iconv(.,to="UTF-8")

map.kuntano <- function(v)
  plyr::mapvalues(as.character(v),as.character(geo$kunta.vanhat2uudet$kuntano.old), 
            as.character(geo$kunta.vanhat2uudet$kuntano))

#postinumero tai kunta on ahvenanmaalla
is.ahvenanmaa <-function(v)  ifelse(v %in% c("Sottunga","Föglö", "Kumlinge", "Lumparland", "Sund", 
                                             "Vårdö", "Hammarland", "Eckerö","Lemland", "Finström", 
                                             "Geta", "Kökar","Saltvik","Jomala","Brändö","Maarianhamina","Mariehamn") | 
                                      grepl("^22",v),T,F)


# korvataan low-arvoa pienemmät "repvaluella"

limitl<-function(x,low,repvalue){ 
  x[x<low & !is.na(x)]<-repvalue;
  return(x);
}

# korvataan high-arvoa suuremmat "repvaluella"
limith<-function(x,high, repvalue){ 
  x[x>high & !is.na(x)]<-repvalue;
  return(x);
}

# korvataan ylä- ja alarajalla
cut.lh <-function(x, limits) {
  limitl(x,limits[1],limits[1]) %>% 
    limith(.,limits[2],limits[2]) %>% return
}

# rajoita ylä- ja alaraja muuttujalla kvantiileihin 
cut.quantile<- function(df, vars, quantile.limits=c(0.025,0.975)) {
  for (i in vars) df[[i]] <- cut.lh(df[[i]], quantile(df[[i]],quantile.limits))
  return(df)}

kunnat.latlong<-read.csv(file=paste(data.directory,"/Data/kunnat_latlong.csv",sep=""),sep="\t")

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

# puuttuuko arvo (misvalues) vai ei 
notmissing<-function(z,misvalues=NA, nomis=NA) {
  misvalues<-cbind(misvalues,NA);
  m<-lapply(z[,!(names(z) %in% nomis)], function(x) {!(x %in% misvalues)}) 
  return(cbind(data.frame(m),z[nomis]))
}

s.tab<-function(rownames, s, sep=" ",N=1) {
  s<-str_split(s,sep)
  l<-sapply(s,length)
  return(data.frame(id=unlist(unname(mapply(function(a,b) rep(a,b), rownames, l))),
                    word=unname(unlist(s)),
                    N= unlist(mapply(function(a,b) rep(a,b), N, l))))
}

nvl <- function(a,b) {
  ifelse(is.na(a),b,a)
}


map.ikaluokka<-
  function(v) as.numeric(plyr::mapvalues(as.character(v), 
                                   c("0-4",  "5-9",  "10-14","15-19","20-24",
                                     "25-29","30-34","35-39","40-44","45-49",
                                     "50-54","55-59","60-64","65-69","70-74",
                                     "75-79","80-84","85-89","90-"),
                                   c(2,       7,      12,    17,      22,
                                     27,      32,     37,    42,      47,
                                     52,      57,     62,    67,      72,
                                     77,      82,     87,    91)))

map.vanhat.kuntanimet<-
  function(v) plyr::mapvalues(v,c("Maarianhamina - Mariehamn","Pedersören kunta","Koski Tl"), 
                        c("Maarianhamina",iconv("Pedersören",to="UTF-8"),"Koski"))


kuntadata <- mutate(demografia$kunta, kuntanimi=plyr::mapvalues(as.character(kuntanimi),"Koski Tl","Koski")) %>%
  filter(kuntanimi!="KOKO MAA")

kunta.stat.vars <- names(select(demografia$kunta,-vuosi,-kuntanimi))
vuodet <- unique(demografia$kunta$vuosi)
karttatyyppi=list(label=c("tavallinen","kartogrammi"), 
                  aluejako=c("kuntanimi","kartogrammi.kuntanimi"))



