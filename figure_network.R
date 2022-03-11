#figure 6 and table s8
#Figure 6. Links bridging socioenvironmental factors, biomarkers and mental health outcomes 
#Table S8. Number of associations between mental health outcomes and biomarkers identified included studies

#https://kateto.net/networks-r-igraph
library(dplyr);library(tidyr);library(igraph)
setwd("/Users/yaminzhang/Nutstore\ Files/文章筛选/documents\ for\ final\ review/analysis")
#input
#English papers
da1 <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "freq-links")
da2 <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "men_out")
da <- merge(da1,da2[,c(1,5:7)],by="ID")
for (i in 2:ncol(da)){
  da[,i] <- ifelse(da[,i]=="",NA,da[,i])
}
d1 <- da[,1:5] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T)
d2 <- da[,c(1,6:9)] %>% gather(key = "biom", value="value_biom", -c(1),na.rm = T)
d3 <- da[,c(1,10:12)] %>% gather(key = "outcome", value="value_outcome", -c(1),na.rm = T)
freq=cbind(table(d1$ID),table(d2$ID),table(d3$ID))

df=data.frame()
for (i in unique(da$ID)) {
  num1=freq[i,1]
  num2=freq[i,2]
  num3=freq[i,3]
  sa0=d1[d1$ID==i,"value_sa"]
  biom0=d2[d2$ID==i,"value_biom"]
  outcome0=d3[d3$ID==i,"value_outcome"]
  c_sa <- c()
  c_biom <- c()
  for (j1 in 1:num1){
    c_sa <- c(c_sa,rep(sa0[j1],num2*num3))
    for (j2 in 1:num2) {
      c_biom <- c(c_biom,rep(biom0[j2],num3))
    }
  }
  df_n <- data.frame(ID=rep(i,times=num1*num2*num3),SA=c_sa,BIOM=c_biom,OUT=rep(outcome0,num1*num2))
  df <- rbind(df,df_n)
}
df_out_eng <- df
#Chinese papers
da1 <- xlsx::read.xlsx2("statistics - CHI -all.xlsx",sheetName = "freq-links")
da2 <- xlsx::read.xlsx2("statistics - CHI -all.xlsx",sheetName = "men_out")
da <- merge(da1[,1:8],da2,by="ID")
da <- rpls(da)

d1 <- da[,1:4] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T)
d2 <- da[,c(1,5:8)] %>% gather(key = "biom", value="value_biom", -c(1),na.rm = T)
d3 <- da[,c(1,9:11)] %>% gather(key = "outcome", value="value_outcome", -c(1),na.rm = T)
freq=cbind(table(d1$ID),table(d2$ID),table(d3$ID))

df=data.frame()
for (i in unique(da$ID)) {
  num1=freq[i,1]
  num2=freq[i,2]
  num3=freq[i,3]
  sa0=d1[d1$ID==i,"value_sa"]
  biom0=d2[d2$ID==i,"value_biom"]
  outcome0=d3[d3$ID==i,"value_outcome"]
  c_sa <- c()
  c_biom <- c()
  for (j1 in 1:num1){
    c_sa <- c(c_sa,rep(sa0[j1],num2*num3))
    for (j2 in 1:num2) {
      c_biom <- c(c_biom,rep(biom0[j2],num3))
    }
  }
  df_n <- data.frame(ID=rep(i,times=num1*num2*num3),SA=c_sa,BIOM=c_biom,OUT=rep(outcome0,num1*num2))
  df <- rbind(df,df_n)
}
df_out_chi <- df
###
df <- rbind(df_out_chi,df_out_eng)
df[df$BIOM=="o","BIOM"] <- "l" # merge others molecule with GxE, Epigenetics
#df[df$SA=="J","SA"] <- "K" 
df[df$SA=="M","SA"] <- "K" #merge left-behind and ses
df[df$OUT=="intelligence","OUT"]  <- "cognition_human" #to make sure the order is the same
####
nodes=rbind(as.data.frame(table(df$SA)),as.data.frame(table(df$BIOM)),as.data.frame(table(df$OUT)))
nodes$type=c(rep(1,dim(table(df$SA))),rep(2,dim(table(df$BIOM))),rep(3,dim(table(df$OUT))))

t1 <- df[,2:3];names(t1) <- c("from","to")
t2 <- df[,3:4];names(t2) <- c("from","to")
df2 <- rbind(t1,t2)
df2$type<- c(rep("link1",nrow(t1)),rep("link2",nrow(t2)))
nrow(unique(df2[,c("from", "to")]))
links <- df2 %>% count(from,to,type)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)

aa <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "sa")
bb <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "biom")
bb[bb$symbol=="m","biom_abbr"] <- "microbe"

colrs <- c("lightsteelblue2", "tomato", "gold")
V(net)$color <- colrs[V(net)$type]
V(net)$size <- 1+V(net)$Freq/8
V(net)$label.color <- "black"
names(V(net))

V(net)$label <- c(aa$sa_abbr,bb$biom_abbr[c(1:13)],
                  c("ant","ant-l","cog-l","cog","mdd","mdd-l","NeuD","NeuD-l","O_NeuB")) 
 
E(net)$width <- E(net)$weight/6
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

ori <- par("mar")
par(mar=c(0.5, 0.5, 0.5, 0.5))
plot(net,edge.color=edge.col,layout=layout_in_circle(net),edge.color=edge.col,
     vertex.label.degree=pi/5,vertex.label.dist=1)
legend("bottom", c("socioenvironmental factor","biomarker", "mental health outcome"),pt.bg=colrs, pch=21,
        pt.cex=2, cex=1, bty="n", ncol=3,inset = -0.02,xpd=T,text.width=0.6)
#text.width: adjust the distance between labels

#replace number with degree 
deg <- degree(net, mode="all")
plot(net, vertex.size=deg,layout=layout_in_circle(net),edge.color=edge.col,
     vertex.label.degree=pi/5,vertex.label.dist=1.5)

#table s8
table(t2$from,t2$to)
