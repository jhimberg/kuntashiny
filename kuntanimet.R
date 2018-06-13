
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


map.vanhat.kuntanimet<-
  function(v) plyr::mapvalues(v,c("Maarianhamina - Mariehamn","Pedersören kunta","Koski Tl"), 
                              c("Maarianhamina",iconv("Pedersören",to="UTF-8"),"Koski"))


# Kuntien nimien koordinaatit - ei tarvita tässä
kunnat.latlong <- read.csv(file=here::here(data.directory, "kunnat_latlong.csv"), sep="\t")
