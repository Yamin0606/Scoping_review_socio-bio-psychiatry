#https://cosx.org/2014/08/r-maps-for-china
library(ggplot2)
setwd("/Users/yaminzhang/Nutstore\ Files/文章筛选/documents\ for\ final\ review/analysis")
source("./code/functions.R")
#there were three maps on provincial distribution of studies 
##1 based on location of the institute that conducted the study  
##2 based on where the study (on human subjects) was conducted 
##3  based on where the study (random sampling) was conducted 
#input
char_chi <- rpls(xlsx::read.xlsx2("statistics-CHI_study _design.xlsx",sheetName = "charac"))
char_eng <- rpls(xlsx::read.xlsx2("statistics-study design.xlsx",sheetName = "charac"))
random_eng <- char_eng[char_eng$sm %in% c("cluster sampling","quota sampling","random sampling"),1]
random_chi <- char_chi[char_chi$sm %in% c("cluster sampling","quota sampling","random sampling"),1]
random <- c(random_chi,random_eng)

prov_eng <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "geog-chi")
prov_chi <- xlsx::read.xlsx2("statistics - CHI -all.xlsx",sheetName = "geog-chi")
prov <- rbind(prov_eng,setNames(prov_chi,names(prov_eng)))
prov <- prov[which(prov$value_area!="/"),]

prov2_chi <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "year_loca")
prov2_eng <- xlsx::read.xlsx2("statistics - CHI -all.xlsx",sheetName = "year_loca")
prov2 <- rbind(prov2_eng,setNames(prov2_chi,names(prov2_eng)))
######map 1
tt <- as.data.frame(table(prov2$province_chi))
names(tt) <- c("NAME","freq")
max(tt$freq)
x=rgdal::readOGR("china-province-border-data/bou2_4p.shp")
x$NAME <- iconv(x$NAME,from="GBK")
head(x@data)
x@data$id <- rownames(x@data)
x@data   <- plyr::join(x@data, tt, by="NAME")
x.df     <- fortify(x)
x.df     <- plyr::join(x.df,x@data, by="id")
ggplot(x.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=freq))+
  coord_fixed()+theme_classic()+
  scale_fill_gradient(high = "darkblue",low = "lightblue",limits = c (0,130))+
  labs(fill='number of articles')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())+
  labs(x="",y="") + theme(legend.position="top")

#map 2
tt <- as.data.frame(table(prov$province))
names(tt) <- c("NAME","freq")
max(tt$freq)
x=rgdal::readOGR("china-province-border-data/bou2_4p.shp")
x$NAME <- iconv(x$NAME,from="GBK")
head(x@data)
x@data$id <- rownames(x@data)
x@data   <- plyr::join(x@data, tt, by="NAME")
x.df     <- fortify(x)
x.df     <- plyr::join(x.df,x@data, by="id")
ggplot(x.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=freq))+
  coord_fixed()+theme_classic()+
  scale_fill_gradient(high = "darkgreen",low = "lightgreen",limits = c (0,65)) +
  labs(fill='number of articles')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())+
  labs(x="",y="") + theme(legend.position="top")

#map 3
prov_rand <- prov[prov$ID %in% intersect(prov$ID,random),]
tt <- as.data.frame(table(prov_rand$province))
names(tt) <- c("NAME","freq")
max(tt$freq)

x=rgdal::readOGR("china-province-border-data/bou2_4p.shp")
x$NAME <- iconv(x$NAME,from="GBK")
head(x@data)
x@data$id <- rownames(x@data)
x@data   <- plyr::join(x@data, tt, by="NAME")
x.df     <- fortify(x)
x.df     <- plyr::join(x.df,x@data, by="id")

ggplot(x.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=freq))+
  coord_fixed()+theme_classic()+
  scale_fill_gradient(high = "darkgreen",low = "lightgreen",limits = c (0,15)) +
  labs(fill='number of articles')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())+
  labs(x="",y="") + theme(legend.position="top")
