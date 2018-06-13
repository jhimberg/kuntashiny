library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)

data.directory <- "Data"
source("kuntanimet.R")

geo <- readRDS(file=here::here(data.directory, "geografia.rds"))
demografia <- readRDS(file=here::here(data.directory, "demografia.rds"))

geo$kunta$kartogrammi <- transmute(readRDS(file="Data/kuntakartogrammi.rds"),
                                 id,
                                 long,
                                 lat,
                                 order,
                                 hole,
                                 piece,
                                 group,
                                 kunta,
                                 kuntanimi=iconv(kuntanimi, to="UTF-8"))

## Globaaleja  
kuntadata <- mutate(demografia$kunta$tunnusluku,
                    kuntanimi=iconv(kuntanimi, to="UTF-8"), 
                    kuntanimi=map.vanhat.kuntanimet(kuntanimi)) %>%
  filter(kuntanimi!="KOKO MAA") 

koko.maa <- filter(demografia$kunta$tunnusluku, kuntanimi=="KOKO MAA") 
kunta.stat.vars <- names(select(demografia$kunta$tunnusluku,-vuosi,-kuntanimi))
vuodet <- unique(demografia$kunta$tunnusluku$vuosi)
karttatyyppi=list(label=c("tavallinen","kartogrammi"), 
                  aluejako=c("kuntanimi","kartogrammi.kuntanimi"))


## Tuottaa kartan (ks. geografia.rds)
## df dataframe jossa oltava sarakkeet "alue" jossa sopiva karttajako (ks. alla) ja ainakin yksi muu sarake
## yhdistää kartta-aineiston ja datan; plottaa ensimmäisen df:n muuttujan jonka nimi ei ole "alue" 
## color.limits asettaa manuaalisest ylä- ja alarajan väriskaalalle. Oletus: automaattinen
## color.map: colorbrew:n väriasteikon koodi
kartta <- function(df, aluejako="pono.3", title.label=NA, geo_=geo, color.map="PuBu", color.limits=c(NA,NA)) {
  geodata <- list()
  geodata[["pono.5"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=pono) 
  geodata[["pono.3"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono, 1, 3))
  geodata[["pono.2"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono, 1, 2))
  geodata[["pono.1"]] <- function(geo_) mutate(geo_$pono.duukkis, alue=substr(pono, 1, 1))
  geodata[["kuntanimi"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntanimi)
  geodata[["kunta"]] <- function(geo_) mutate(geo_$kunta$"2017", alue=kuntano)
  geodata[["kartogrammi.kuntanimi"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kuntanimi)
  geodata[["kartogrammi.kunta"]] <- function(geo_) mutate(geo_$kunta$kartogrammi, alue=kunta)
  geodata <- left_join(geodata[[aluejako]](geo_), df, by="alue")
  
  attr <- names(select(df, -alue))[1] 
  if(is.na(title.label)) title.label <- attr
  
  p <- ggplot(data=arrange(geodata, order), aes(x=long, y=lat)) + 
    geom_polygon_interactive(aes_string(fill = attr, group="group", tooltip = "alue", 
                                        data_id="alue"), colour=NA) +
    theme_void() + 
    theme(legend.title=element_blank()) + 
    ggtitle(title.label)
  
  p <- p + scale_fill_gradientn(colours= brewer.pal(6, color.map), 
                                values = NULL, 
                                space = "Lab", 
                                na.value = "grey50", 
                                guide = "colourbar", 
                                limits = color.limits)
  
  if(!grepl("^pono",aluejako)) 
    p <- p + coord_equal(ratio=1) 
  else 
    p <- p + coord_equal(ratio=2.1) 
  return(p)
}





