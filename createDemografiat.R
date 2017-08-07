library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)

library(pxweb)
library(pxR)
library(RCurl)
library(curl)


library(rgdal)
library(gpclib)
library(maptools)
gpclibPermit()

library(gisfin)
library(gdata)

working.directory<-"."
setwd(working.directory)

# Haetan joitakin kunnittaisia ikätietoja
## 

get.geo <-function(data.name="tilastointialueet:kunta4500k_2017",name.ext=".shp") {
  data.file=paste(tempdir(),"/",str_split_fixed(data.name,pattern=":",n=2)[2],sep="")
  url.head <- "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName="
  url.tail <- "&outputFormat=SHAPE-ZIP"
  zip.file <- paste(tempdir(),"/","shape.zip",sep="")
  curl_download(paste(url.head,data.name, url.tail,sep=""), zip.file)
  unzip(zip.file,exdir=tempdir())  
  return(sp2df(readShapeSpatial(paste(tempdir(),"/",str_split_fixed(data.name,pattern=":",n=2)[2],name.ext,sep=""))))
}

geo<-list()

## nimet
## Duukkiksen redusoitu postinumerokattadata

kml.file="Data/Postinumerot 20150102.kml" 
layer.name <- rgdal::ogrListLayers(kml.file)
pnro.sp <- rgdal::readOGR(dsn = kml.file, layer = layer.name)
pnro.sp@data <- pnro.sp@data[1]
names(pnro.sp@data) <- "pnro"
pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)

geo$pono.duukkis <- sp2df(pnro.sp) %>% 
  rename(pono=pnro)

## Hateaan kuntarajakartat 4500k 2014-2017

geo$kunta <- list()
name.ext <-list() 
name.ext[["2017"]]<-".shp"
for (y in c("2017")) 
  geo$kunta[[y]] <- rbind(geo$kunta,
                          get.geo(paste("tilastointialueet:kunta4500k_",y,sep=""), 
                                  name.ext=name.ext[[y]]) %>% 
                            mutate(nimi=iconv(nimi, from="latin1", to="UTF-8")) %>%
                            rename(kuntano=kunta,kunta=nimi) %>% 
                            select(id,long,lat,order,hole,piece,group,vuosi,kuntano,kuntanimi=kunta) %>%
                            mutate(kuntanimi=map.vanhat.kuntanimet(kuntanimi)))

# Kunta

kunnat<-read.csv(file="Data/kunnat2017.csv",fileEncoding="MAC",sep=";")

# Kuntien koodausta
geo$kunta.vanhat2uudet<-merge(select(kunnat,kunta,kunta.old,kuntano.old),
                              select(kunnat,kunta=kunta.old,kuntano=kuntano.old),
                              by="kunta") %>% 
  select(kunta.old,kunta,kuntano.old,kuntano) %>% 
  mutate(kuntano.old=str_pad(kuntano.old, 3, side="left", pad="0"),
         kuntano=str_pad(kuntano, 3, side="left", pad="0"))

#### Postinumeroaluedatat ja tarkka postinumeroaluekartta

demografia<-list()

v.2017<-read.px(textConnection(getURL("https://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/Kuntien_avainluvut/2017/kuntien_avainluvut_2017_aikasarja.px"))) %>%
  as.data.frame %>% filter(!grepl("region|maakunta|seutukunta|Kainuu|Ålands landsbyg|Ålands skärgård|Uusimaa|Varsinais-Suomi|Satakunta|Kanta-Häme|Pirkanmaa|Päijät-Häme|Kymenlaakso|Etelä-Karjala|Etelä-Savo|Pohjois-Savo|Pohjois-Karjala|Keski-Suomi|Etelä-Pohjanmaa|Pohjanmaa|Keski-Pohjanmaa|Pohjois-Pohjanmaa|Lappi|Ahvenanmaa - Åland",
                                  Alue.2017,ignore.case=FALSE)) %>%
  mutate(Alue.2017=map.vanhat.kuntanimet(Alue.2017), 
         Tiedot=as.character(Tiedot),
         Vuosi=as.numeric(as.character((Vuosi)))) %>% 
  rename(Alue=Alue.2017) %>%
  filter(.,!(Tiedot %in% c("Väkiluvun muutos edellisestä vuodesta, %")))

demografia$kunta<-rbind(v.2017,
                        read.table(file="Data/kunta.2.csv",sep=";",header=TRUE)) %>% 
  dcast(Vuosi+Alue ~ Tiedot, fill=NA, value.var="value") %>% 
  rename(vuosi=Vuosi,kuntanimi=Alue)

rm(v.2017)

# kunnat väkiluku 2016-2017 (helmikuu)
demografia$kunnat$vakiluku<-
  rbind(read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vamuu/001_vamuu_tau_107.px"))) %>% as.data.frame %>% mutate(vuosi=2017) ,
        read.px(textConnection(getURL("http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vrm/vamuu/005_vamuu_tau_101.px"))) %>% as.data.frame %>% mutate(vuosi=2016)) %>%
  filter(Sukupuoli=="Sukupuolet yhteensä" & Alue!="KOKO MAA" & Kuukausi!="Yhteensä") %>%
  rename(kuntanimi=Alue,kuukausi=Kuukausi) %>%
  mutate(kuntanimi=map.vanhat.kuntanimet(as.character(kuntanimi)),
         kuukausi=mapvalues(kuukausi,c("Tammikuu","Helmikuu","Maaliskuu","Huhtikuu",
                                       "Toukokuu","Kesäkuu","Heinäkuu","Elokuu","Syyskuu",
                                       "Lokakuu","Marraskuu","Joulukuu"),seq(1,12))) %>%
  transmute(vuosi,kuukausi,kuntanimi,vakiluku=value) %>%
  mutate(kuukausi=paste0(vuosi,"-",str_pad(kuukausi,2,side="left","0")))

##
kuntavaalit.www<-list()
kuntavaalit<-list()

yrs<-c("2012","2017")
kuntavaalit.www[[yrs[1]]]<-"http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vaa/kvaa/2012_07/920_kvaa_2012_tau_161_fi.px"
kuntavaalit.www[[yrs[2]]]<-"http://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/StatFin/vaa/kvaa/2017_07/920_kvaa_2017_tau_161.px"

for (y in yrs) {
  kuntavaalit[[y]]<-read.px(textConnection(getURL(kuntavaalit.www[[y]]))) %>% as.data.frame %>% 
    filter(Puolueiden.kannatus=="Ääniä yhteensä" & Puolue!="Yhteensä" & Sukupuoli=="Kaikki ehdokkaat" & grepl("^[0-9][0-9][0-9]",Alue)) %>% mutate(kuntano=substr(Alue,1,3),kuntanimi=substr(Alue,5,100)) %>% 
    select(Puolue,value,kuntano,kuntanimi) %>% 
    dcast(kuntano+kuntanimi ~ Puolue,value.var="value")
  kuntavaalit[[y]]<-mutate(kuntavaalit[[y]],kuntano=map.kuntano(kuntano)) %>% select(-kuntanimi) %>% 
    group_by(kuntano) %>% 
    summarise_all(funs(sum)) 
  kuntavaalit[[y]]$N<-rowSums(select(kuntavaalit[[y]],-kuntano))
}

demografia$kunnat$vaalit<-kuntavaalit

## 

paavo<-rbind(get.geo("postialue:pno_tilasto_2017"),
             get.geo("postialue:pno_tilasto_2016"),
             get.geo("postialue:pno_tilasto_2015"))

paavo.vars <- read.csv(file="Data/paavo.koodit.txt", 
                       sep=";",
                       fileEncoding="MAC",
                       stringsAsFactors = FALSE)  

geo$pono.statfi.2017<-select(paavo,id,long,lat,order,hole,piece,group,nimi,euref_x,euref_y,vuosi,pono=posti_alue) %>%
  filter(vuosi==2017)

paavo.5<-group_by(paavo,posti_alue,vuosi) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(-id,-long,-lat,-order,-hole,-piece,-group,-namn) %>% 
  rename(pono=posti_alue, kuntano=kunta) %>%
  mutate(pono=as.character(pono), pono.level=5,
         kuntano=as.character(kuntano),
         nimi=iconv(nimi,from="latin1",to="UTF-8")) %>% 
  mutate_if(is.numeric,function(x) ifelse(x==-1,NA,x))

wmean<-function(x,y) return(weighted.mean(x,ifelse(is.na(y),0,y),na.rm=T))

paavo.aggr <- function(d,i,attr="pono", vars=paavo.vars)
  group_by(d, vuosi, pono=str_sub(pono,1,i)) %>% 
  select(pono,vuosi,one_of(filter(vars,aggr=="sum")$koodi)) %>% 
  summarise_all(sum, na.rm=TRUE) %>% 
  left_join(.,
            group_by(d, vuosi,pono=str_sub(pono,1,i)) %>% 
              summarise(he_kika=wmean(he_kika,he_vakiy),
                        hr_ktu=wmean(hr_ktu,hr_tuy),
                        hr_mtu=wmean(hr_mtu,hr_tuy),
                        te_takk=wmean(te_takk,te_taly),
                        te_as_valj=wmean(te_as_valj,te_taly),
                        tr_ktu=wmean(tr_ktu,tr_kuty),
                        tr_mtu=wmean(tr_mtu,tr_kuty),
                        ra_as_kpa=wmean(ra_as_kpa,ra_asunn),
                        euref_x=wmean(euref_x,pinta_ala),
                        euref_y=wmean(euref_y,pinta_ala),
                        kuntano=NA,
                        pono.level=i,
                        nimi=NA
              ),
            by=c("vuosi","pono")) %>% as.data.frame

### Lasketaan keskiarvot ja summat Paavo datalle pono3 ja pono3

demografia$paavo$data<-rbind(paavo.5,paavo.aggr(paavo.5,3),
                             paavo.aggr(paavo.5,2),
                             paavo.aggr(paavo.5,1))
demografia$paavo$vars<-paavo.vars

save(file="demografiat.RData", demografia)
save(file="geografiat.RData", geo)


