
working.directory <- "."
setwd(working.directory)

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
limitl <- function(x,low,repvalue){ 
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

# nvl
nvl <- function(a,b) {
  ifelse(is.na(a),b,a)
}

kunnat.latlong <- read.csv(file="Data/kunnat_latlong.csv",sep="\t")

map.vanhat.kuntanimet<-
  function(v) plyr::mapvalues(v,c("Maarianhamina - Mariehamn","Pedersören kunta","Koski Tl"), 
                              c("Maarianhamina",iconv("Pedersören",to="UTF-8"),"Koski"))
