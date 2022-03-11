#table 1 Distribution of studies about different biomarkers across socioenvironmental factors and mental health outcomes
#There are 13 biomarker categories & 3 socioenvironmental factors & 5 mental health outcome categories
setwd("/Users/yaminzhang/Nutstore\ Files/文章筛选/documents\ for\ final\ review/analysis")
source("./code/functions.R")
library(tidyr);library(ggplot2);library(dplyr)

###################biomarker 1 neurotransmitter ###################
neotrans_g <- xlsx::read.xlsx2("statistics-biomarkers-a-LQY.xlsx",sheetName="grouping")
table(neotrans_g$group)
neotrans_e <- xlsx::read.xlsx2("statistics-biomarkers-a-LQY.xlsx",sheetName="eng")
neotrans_e <- rpls(neotrans_e[,c(1,4:11)])
neotrans_e1 <- neotrans_e %>% gather(key = "neotrans_e", value="value_neotrans_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(neotrans_e1$value_neotrans_e),decreasing = T));t1
neotrans_c <- xlsx::read.xlsx2("statistics-biomarkers-a-LQY.xlsx",sheetName="chi")
neotrans_c <- rpls(neotrans_c[,c(1,5:14)])
neotrans_c1 <- neotrans_c %>% gather(key = "neotrans_c", value="value_neotrans_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(neotrans_c1$value_neotrans_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)

aa <- list()
for (x in unique(neotrans_g$group)){
  a0<- neotrans_g[neotrans_g$group==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
names(tt) <- c("Var1","English","Chinese")
tt$English <- as.numeric(tt$English);tt$Chinese <- as.numeric(tt$Chinese)
tt$all <- apply(tt[,2:3],1,sum)
x=tt[order(tt$all,decreasing = T),]
x$Var1 <- factor(x$Var1,levels = c("DA","5-HT","NEr","monoamines_E","glu","GABA","ACh","other"))
###by language###
#x1 <- x[,1:3] %>% gather(key = "group",value = "frequency",-c(1))
#x1$group <- factor(x1$group,levels = c("English","Chinese"))
#ggplot(x1, aes(x=Var1, y=frequency,fill=group)) + 
#  geom_bar(stat="identity",position = "dodge")+
#  theme_classic()+xlab("")+ylab("frequency") + labs(fill="")+
#  theme(axis.text.x = element_text(angle = 45,vjust=0.6))+
#  scale_fill_manual('language',values = c("cyan4","darkorange2"))+
# theme(legend.title = element_blank())
####
names(neotrans_c1)[3] <- "value_neotrans"
names(neotrans_e1)[3] <- "value_neotrans"
aa <- rbind(neotrans_c1[,c(1,3)],neotrans_e1[,c(1,3)])
aa1 <- merge(aa,neotrans_g[,c("Var1","group")],by.x="value_neotrans",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (nt in unique(df1$group)){
  tt1 <- df1 %>% filter(group==nt) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[nt]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==nt,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[nt]] <- table(t1$value_sa)
}
###################biomarker 2 neurotrophic factor###################
ntro_g <- xlsx::read.xlsx2("statistics-biomarkers-b-YYB.xlsx",sheetName="grouping")
nt <-  ntro_g[ntro_g$group=="NT",1]
other <- ntro_g[ntro_g$group=="other",1]
ntro_e <- xlsx::read.xlsx2("statistics-biomarkers-b-YYB.xlsx",sheetName="eng")
ntro_e <- rpls(ntro_e)
ntro_e1 <- ntro_e %>% gather(key = "ntro_e", value="value_ntro_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(ntro_e1$value_ntro_e),decreasing = T));t1
ntro_c <- xlsx::read.xlsx2("statistics-biomarkers-b-YYB.xlsx",sheetName="chi")
ntro_c <- rpls(ntro_c[,c(1,4:6)])
ntro_c1 <- ntro_c %>% gather(key = "ntro_c", value="value_ntro_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(ntro_c1$value_ntro_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
tt["NT",] <- c("NT",apply(tt[tt$Var1 %in% nt,2:3],2,sum));tt <- format(tt)
tt["other",] <- c("other",apply(tt[tt$Var1 %in% other,2:3],2,sum));tt <- format(tt)
tt <- tt[tt$Var1 %in% c("BDNF","NGF","NT","other"),]
tt$all <- apply(tt[,2:3],1,sum)
x=tt
x$Var1 <- factor(x$Var1,levels = c(x$Var1))
names(x)[2:3] <- c("English","Chinese")
###################
names(ntro_c1)[3] <- "value_ntro"
names(ntro_e1)[3] <- "value_ntro"
aa <- rbind(ntro_c1[,c(1,3)],ntro_e1[,c(1,3)])
aa1 <- merge(aa,ntro_g[,c("Var1","group")],by.x="value_ntro",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (ntr in unique(df1$group)){
  tt1 <- df1 %>% filter(group==ntr) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[ntr]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==ntr,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[ntr]] <- table(t1$value_sa)
}
###################biomarker 3 Endocrinological###################
horm_g <- xlsx::read.xlsx2("statistics-biomarkers-c-YYB.xlsx",sheetName="grouping")
horm_e <- xlsx::read.xlsx2("statistics-biomarkers-c-YYB.xlsx",sheetName="eng")
horm_e <- rpls(horm_e)
horm_e1 <- horm_e[,c(1,3:8)] %>% gather(key = "horm_e", value="value_horm_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(horm_e1$value_horm_e),decreasing = T));t1
horm_c <- xlsx::read.xlsx2("statistics-biomarkers-c-YYB.xlsx",sheetName="chi")
horm_c <- rpls(horm_c[,c(1,4:10)])
horm_c1 <- horm_c %>% gather(key = "horm_c", value="value_horm_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(horm_c1$value_horm_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
aa <- list()
for (x in unique(horm_g$group)){
  a0<- horm_g[horm_g$group==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
names(tt) <- c("Var1","English","Chinese")
####
names(horm_c1)[3] <- "value_horm"
names(horm_e1)[3] <- "value_horm"
aa <- rbind(horm_c1[,c(1,3)],horm_e1[,c(1,3)])
aa1 <- merge(aa,horm_g[,1:2],by.x="value_horm",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (hm in unique(df1$group)){
  tt1 <- df1 %>% filter(group==hm) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[hm]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==hm,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[hm]] <- table(t1$value_sa)
}
###################biomarker 4 Oxidative###################
oxid_g <- xlsx::read.xlsx2("statistics-biomarkers-d-YYB.xlsx",sheetName="grouping")
oxid_e <- xlsx::read.xlsx2("statistics-biomarkers-d-YYB.xlsx",sheetName="eng")
oxid_e <- rpls(oxid_e)
oxid_e1 <- oxid_e %>% gather(key = "oxid_e", value="value_oxid_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(oxid_e1$value_oxid_e),decreasing = T));t1
oxid_c <- xlsx::read.xlsx2("statistics-biomarkers-d-YYB.xlsx",sheetName="chi")
oxid_c <- rpls(oxid_c[,c(1,4:11)])
oxid_c1 <- oxid_c %>% gather(key = "oxid_c", value="value_oxid_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(oxid_c1$value_oxid_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
aa <- list()
for (x in unique(oxid_g$group)){
  a0<- oxid_g[oxid_g$group==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
names(tt) <- c("Var1","English","Chinese")
####
names(oxid_c1)[3] <- "value_oxid"
names(oxid_e1)[3] <- "value_oxid"
aa <- rbind(oxid_c1[,c(1,3)],oxid_e1[,c(1,3)])
aa1 <- merge(aa,oxid_g[,c("Var1","group")],by.x="value_oxid",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (oxi in unique(df1$group)){
  tt1 <- df1 %>% filter(group==oxi) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[oxi]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==oxi,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[oxi]] <- table(t1$value_sa)
}
###################biomarker 5 Immunological###################
immu_g <- xlsx::read.xlsx2("statistics-biomarkers-e-YYB-1208.xlsx",sheetName="grouping")
immu_e <- xlsx::read.xlsx2("statistics-biomarkers-e-YYB-1208.xlsx",sheetName="eng")
immu_e <- rpls(immu_e[,c(1,3:19)])
immu_e1 <- immu_e %>% gather(key = "immu_e", value="value_immu_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(immu_e1$value_immu_e),decreasing = T));t1
immu_c <- xlsx::read.xlsx2("statistics-biomarkers-e-YYB-1208.xlsx",sheetName="chi")
immu_c <- rpls(immu_c[,c(1,5:14)])
immu_c1 <- immu_c %>% gather(key = "immu_c", value="value_immu_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(immu_c1$value_immu_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
aa <- list()
for (x in unique(immu_g$group1)){
  a0<- immu_g[immu_g$group1==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
names(tt) <- c("Var1","English","Chinese")
####
names(immu_c1)[3] <- "value_immu"
names(immu_e1)[3] <- "value_immu"
aa <- rbind(immu_c1[,c(1,3)],immu_e1[,c(1,3)])
aa1 <- merge(aa,immu_g[,c("Var1","group1")],by.x="value_immu",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (immu in unique(df1$group)){
  tt1 <- df1 %>% filter(group1==immu) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[immu]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group1==immu,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[immu]] <- table(t1$value_sa)
}
###################biomarker 6 Metabolic###################
metab_g <- xlsx::read.xlsx2("statistics-biomarkers-f-WH.xlsx",sheetName="grouping")
metab_g <- metab_g %>% filter(protocol=="y") %>% select(Var1,group,group1)
metab_e <- xlsx::read.xlsx2("statistics-biomarkers-f-WH.xlsx",sheetName="eng")
metab_e <- rpls(metab_e[,c(1,4:11)])
metab_e1 <- metab_e %>% gather(key = "metab_e", value="value_metab_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(metab_e1$value_metab_e),decreasing = T));t1
metab_c <- xlsx::read.xlsx2("statistics-biomarkers-f-WH.xlsx",sheetName="chi")
metab_c <- rpls(metab_c[,c(1,6:13)])
metab_c1 <- metab_c %>% gather(key = "metab_c", value="value_metab_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(metab_c1$value_metab_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
aa <- list()
for (x in unique(metab_g$group)){
  a0<- metab_g[metab_g$group==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
tt$Freq.x <- as.numeric(tt$Freq.x);tt$Freq.y <- as.numeric(tt$Freq.y)
tt$all <- apply(tt[,2:3],1,sum)
x=tt[order(tt$all,decreasing = T),]
names(x)[1:3] <- c("Var1","English","Chinese")
####
names(metab_c1)[3] <- "value_metab"
names(metab_e1)[3] <- "value_metab"
aa <- rbind(metab_c1[,c(1,3)],metab_e1[,c(1,3)])
aa1 <- merge(aa,metab_g[,c("Var1","group")],by.x="value_metab",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (meta in unique(df1$group)){
  tt1 <- df1 %>% filter(group==meta) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[meta]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==meta,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[meta]] <- table(t1$value_sa)
}
###################biomarker 7 Nutrition###################
nutri_g <- xlsx::read.xlsx2("statistics-biomarkers-g-LQY.xlsx",sheetName="grouping")
nutri_e <- xlsx::read.xlsx2("statistics-biomarkers-g-LQY.xlsx",sheetName="eng")
nutri_e <- rpls(nutri_e[,c(1,5:9)])
nutri_e1 <- nutri_e %>% gather(key = "nutri_e", value="value_nutri_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(nutri_e1$value_nutri_e),decreasing = T));t1
nutri_c <- xlsx::read.xlsx2("statistics-biomarkers-g-LQY.xlsx",sheetName="chi")
nutri_c <- rpls(nutri_c[,c(1,5:8)])
nutri_c1 <- nutri_c %>% gather(key = "nutri_c", value="value_nutri_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(nutri_c1$value_nutri_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
aa <- list()
for (x in unique(nutri_g[,"group"])){
  a0<- nutri_g[nutri_g$group==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
names(tt) <- c("Var1","English","Chinese")
tt$English <- as.numeric(tt$English);tt$Chinese <- as.numeric(tt$Chinese)
tt$all <- apply(tt[,2:3],1,sum)
x=tt[order(tt$all,decreasing = T),]
####
names(nutri_c1)[3] <- "value_nutri"
names(nutri_e1)[3] <- "value_nutri"
aa <- rbind(nutri_c1[,c(1,3)],nutri_e1[,c(1,3)])
aa1 <- merge(aa,nutri_g[,c("Var1","group")],by.x="value_nutri",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
table(df1$group)
men_out <- read.table("mental_out_all.txt",header = T)
tt1 <- df1 %>% filter(group=="mineral") %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T); table(tt1$value_sa)
tt1 <- df1 %>% filter(group=="Vitamin") %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T); table(tt1$value_sa)
t1 <- men_out[men_out$ID %in% df1[df1$group=="mineral","ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);table(t1$value_sa)
t1 <- men_out[men_out$ID %in% df1[df1$group=="Vitamin","ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);table(t1$value_sa)
###################biomarker 8 Other molecules###################
Omol_g <- xlsx::read.xlsx2("statistics-biomarkers-l-YYB.xlsx",sheetName="grouping")
Omol_e <- xlsx::read.xlsx2("statistics-biomarkers-l-YYB.xlsx",sheetName="eng")
Omol_e <- rpls(Omol_e[,c(1,4:10)])
Omol_e1 <- Omol_e %>% gather(key = "Omol_e", value="value_Omol_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(Omol_e1$value_Omol_e),decreasing = T));t1
Omol_c <- xlsx::read.xlsx2("statistics-biomarkers-l-YYB.xlsx",sheetName="chi")
Omol_c <- rpls(Omol_c[,c(1,5:6)])
Omol_c1 <- Omol_c %>% gather(key = "Omol_c", value="value_Omol_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(Omol_c1$value_Omol_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
aa <- list()
for (x in unique(Omol_g$group)){
  a0<- Omol_g[Omol_g$group==x,1]
  aa0 <- c(x,apply(tt[tt$Var1 %in% a0 ,2:3],2,sum));
  tt <- format(tt)
  aa[[x]] <- aa0
}
tt <- as.data.frame(t(as.data.frame(aa)))
names(tt) <- c("Var1","English","Chinese")
tt$English <- as.numeric(tt$English);tt$Chinese <- as.numeric(tt$Chinese)
tt$all <- apply(tt[,2:3],1,sum)
tt$all <- as.numeric(tt$all)
x=tt[order(tt$all,decreasing = T),]
#####
names(Omol_c1)[3] <- "value_Omol"
names(Omol_e1)[3] <- "value_Omol"
aa <- rbind(Omol_c1[,c(1,3)],Omol_e1[,c(1,3)])
aa1 <- merge(aa,Omol_g[,c("Var1","group")],by.x="value_Omol",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
table(df1$group)
tt1 <- df1 %>%  filter(group %in% unique(df1$group)) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T)
table(tt1$value_sa)
men_out <- read.table("mental_out_all.txt",header = T)
t1 <- men_out[men_out$ID %in% df1[df1$group %in% unique(df1$group),"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T)
table(t1$value_sa)
###################biomarker 9 Other (G x E, epigenetics)###################
other_e <- xlsx::read.xlsx2("statistics-biomarkers-o-YYB-1207.xlsx",sheetName="eng")
other_e <- rpls(other_e[,c(1,7:8)])
other_e1 <- other_e %>% gather(key = "other_e", value="value_other_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(other_e1$value_other_e),decreasing = T));t1
other_c <- xlsx::read.xlsx2("statistics-biomarkers-o-YYB-1207.xlsx",sheetName="chi")
other_c <- rpls(other_c[,c(1,5:6)])
other_c1 <- other_c %>% gather(key = "other_c", value="value_other_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(other_c1$value_other_c),decreasing = T));t2
t2 <- as.data.frame(matrix(data=c("GxE",3),ncol=2));names(t2)<- names(t1)
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
tt[,2] <- as.numeric(tt[,2]);tt[,3] <- as.numeric(tt[,3])
tt$all <- apply(tt[,2:3],1,sum)
x=tt[order(tt$all,decreasing = T),]
x$Var1 <- factor(x$Var1,levels = c(x$Var1))
names(x)[2:3] <- c("English","Chinese")
#####
names(other_c1)[3] <- "value"
names(other_e1)[3] <- "value"
aa <- rbind(other_c1[,c(1,3)],other_e1[,c(1,3)])
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (oth in unique(df1$value)){
  tt1 <- df1 %>% filter(value==oth) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[oth]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$value==oth,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[oth]] <- table(t1$value_sa)
}
###################biomarker 10 Cells not in brain###################
bcell_g <- xlsx::read.xlsx2("statistics-biomarkers-i-YYB.xlsx",sheetName="grouping")
bcell_e <- xlsx::read.xlsx2("statistics-biomarkers-i-YYB.xlsx",sheetName="eng")
bcell_e <- rpls(bcell_e[,c(1,6,10,13,16,19)])
bcell_e1 <- bcell_e %>% gather(key = "bcell_e", value="value_bcell_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(bcell_e1$value_bcell_e),decreasing = T));t1
bcell_c <- xlsx::read.xlsx2("statistics-biomarkers-i-YYB.xlsx",sheetName="chi")
bcell_c <- rpls(bcell_c[,c(1,7,11)])
bcell_c1 <- bcell_c %>% gather(key = "bcell_c", value="value_bcell_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(bcell_c1$value_bcell_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
granulocytes <- c("neutrophils","eosinophils","basophils")
MPS <- c("monocyte","phagocytized_haemocyte","mononuclear_phagocyte")
tt["granulocytes",] <- c("granulocytes",apply(tt[tt$Var1 %in% granulocytes,2:3],2,sum));tt <- format(tt)
tt["MPS",] <- c("MPS",apply(tt[tt$Var1 %in% MPS,2:3],2,sum));tt <- format(tt)
tt <- tt[!tt$Var1 %in% c(granulocytes,MPS),]
tt$all <- apply(tt[,2:3],1,sum)
x=tt[order(tt$all,decreasing = T),]
x$Var1 <- factor(x$Var1,levels = c(x$Var1))
names(x)[2:3] <- c("English","Chinese")
########
names(bcell_c1)[3] <- "value_bcell"
names(bcell_e1)[3] <- "value_bcell"
aa <- rbind(bcell_c1[,c(1,3)],bcell_e1[,c(1,3)])
aa1 <- merge(aa,bcell_g[,c("Var1","group")],by.x="value_bcell",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (cnb in unique(df1$group)){
  tt1 <- df1 %>% filter(group==cnb) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[cnb]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==cnb,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[cnb]] <- table(t1$value_sa)
}
###################biomarker 11 Cells in brain###################
bcell_g <- xlsx::read.xlsx2("statistics-biomarkers-h-YYB.xlsx",sheetName="grouping")
bcell_e <- xlsx::read.xlsx2("statistics-biomarkers-h-YYB.xlsx",sheetName="eng")
bcell_e <- rpls(bcell_e[,c(1,9,12)])
bcell_e1 <- bcell_e %>% gather(key = "bcell_e", value="value_bcell_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(bcell_e1$value_bcell_e),decreasing = T));t1
bcell_c <- xlsx::read.xlsx2("statistics-biomarkers-h-YYB.xlsx",sheetName="chi")
bcell_c <- rpls(bcell_c[,c(1,7,10,13,16)])
bcell_c1 <- bcell_c %>% gather(key = "bcell_c", value="value_bcell_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(bcell_c1$value_bcell_c),decreasing = T));t2
tt <- merge(t1,t2, by="Var1",all = T)
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
names(tt) <- c("Var1","English","Chinese")
tt$English <- as.numeric(tt$English);tt$Chinese <- as.numeric(tt$Chinese)
tt$all <- apply(tt[,2:3],1,sum)
aa <- c(bcell_g[bcell_g$group=="neuron_all","Var1"],bcell_g[bcell_g$group=="glial_cell","Var1"],"neural stem cell")
x=tt[order(tt$all,decreasing = T),]
x$Var1 <- factor(x$Var1,levels = aa)
names(x)[2:3] <- c("English","Chinese")
########
names(bcell_c1)[3] <- "value_bcell"
names(bcell_e1)[3] <- "value_bcell"
aa <- rbind(bcell_c1[,c(1,3)],bcell_e1[,c(1,3)])
aa1 <- merge(aa,bcell_g[,c("Var1","group")],by.x="value_bcell",by.y="Var1",all.x = T)
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa1,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (cb in unique(df1$group)){
  tt1 <- df1 %>% filter(group==cb) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[cb]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$group==cb,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[cb]] <- table(t1$value_sa)
}
###################biomarker 12 and 13 neuroimaging & neurophysiology & microbe###################
nphy_e <- xlsx::read.xlsx2("statistics-biomarkers-K-LQY.xlsx",sheetName="eng")
nphy_e <- rpls(nphy_e[,c(1,5:7)])
nphy_e1 <- nphy_e %>% gather(key = "nphy_e", value="value_nphy_e", -c(1),na.rm = T)
t1 <- as.data.frame(sort(table(nphy_e1$value_nphy_e),decreasing = T));t1
nphy_c <- xlsx::read.xlsx2("statistics-biomarkers-K-LQY.xlsx",sheetName="chi")
nphy_c <- rpls(nphy_c[,c(1,5:6)])
nphy_c1 <- nphy_c %>% gather(key = "nphy_c", value="value_nphy_c", -c(1),na.rm = T)
t2 <- as.data.frame(sort(table(nphy_c1$value_nphy_c),decreasing = T));t2
###########
nimg_e <- xlsx::read.xlsx2("statistics-biomarkers-j-LQY.xlsx",sheetName="eng")
nimg_e <- rpls(nimg_e[,c(1,4,5)])
nimg_e1 <- nimg_e %>% gather(key = "nimg_e", value="value_nimg_e", -c(1),na.rm = T)
t3 <- as.data.frame(sort(table(nimg_e1$value_nimg_e),decreasing = T));t3
nimg_c <- xlsx::read.xlsx2("statistics-biomarkers-j-LQY.xlsx",sheetName="chi")
nimg_c <- rpls(nimg_c[,c(1,4:5)])
nimg_c1 <- nimg_c %>% gather(key = "nimg_c", value="value_nimg_c", -c(1),na.rm = T)
t4 <- as.data.frame(sort(table(nimg_c1$value_nimg_c),decreasing = T));t4
#####
gutmicro_e <- xlsx::read.xlsx2("statistics-biomarkers-m-YYB-1207.xlsx",sheetName="eng")
gutmicro_e <- rpls(gutmicro_e[,c(1,4)])
gutmicro_e1 <- gutmicro_e %>% gather(key = "gutmicro_e", value="value_gutmicro_e", -c(1),na.rm = T)
t5 <- as.data.frame(sort(table(gutmicro_e1$value_gutmicro_e),decreasing = T));t5
gutmicro_c <- xlsx::read.xlsx2("statistics-biomarkers-m-YYB-1207.xlsx",sheetName="chi")
gutmicro_c <- rpls(gutmicro_c[,c(1,5)])
gutmicro_c1 <- gutmicro_c %>% gather(key = "gutmicro_c", value="value_gutmicro_c", -c(1),na.rm = T)
t6 <- as.data.frame(sort(table(gutmicro_c1$value_gutmicro_c),decreasing = T));t6
tt1 <- merge(t1,t2, by="Var1",all = T)
tt2 <- merge(t3,t4, by="Var1",all = T)
tt <- rbind(tt1,tt2)
tt <- tt[!is.na(tt$Var1),]
tt[is.na(tt)] <- 0
tt$Var1 <- as.character(tt$Var1)
names(tt) <- c("Var1","English","Chinese")
tt <- rbind(tt,c("m_microbe",t5[1,1],t6[1,1]))
tt$English <- as.numeric(tt$English);tt$Chinese <- as.numeric(tt$Chinese)
tt$all <- apply(tt[,2:3],1,sum)
x=tt[order(tt$all,decreasing = T),]
x$Var1 <- factor(x$Var1,levels = c(x$Var1))
########
names(nphy_c1)[3] <- "value"
names(nphy_e1)[3] <- "value"
names(nimg_c1)[3] <- "value"
names(nimg_e1)[3] <- "value"
names(gutmicro_c1)[3] <- "value"
names(gutmicro_e1)[3] <- "value"
aa <- rbind(nphy_c1[,c(1,3)],nphy_e1[,c(1,3)],nimg_c1[,c(1,3)],nimg_e1[,c(1,3)],
            gutmicro_e1[,c(1,3)],gutmicro_c1[,c(1,3)])
df <- read.table("data_da-biom_multi_all.txt",header = T)
df1 <- merge(aa,df,by="ID")
men_out <- read.table("mental_out_all.txt",header = T)
res_freq_se <- list()
res_freq_mo <- list()
for (orgl in unique(df1$value)){
  tt1 <- df1 %>% filter(value==orgl) %>% select(ID,sa1,sa2,sa3) %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_se[[orgl]] <- table(tt1$value_sa)
  t1 <- men_out[men_out$ID %in% df1[df1$value==orgl,"ID"],] %>% gather(key = "sa", value="value_sa", -c(1),na.rm = T);
  res_freq_mo[[orgl]] <- table(t1$value_sa)
}