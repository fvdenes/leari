library(readr)
library(dplyr)

#surveys <- read.csv("C:/Users/voeroesd/Dropbox/A.leari/road_surveys_Aleari/road-surveys_Aleari_21.csv", na.strings = c("NA", ""))
#surveys <- read.csv2("~/Dropbox/A.leari-CHICO/road_surveys_Aleari/road-surveys_Aleari_21.csv", na.strings = c("NA", ""))
surveys <- read.csv("~/Dropbox/A.leari/road_surveys_Aleari/road-surveys_Aleari_21.csv", na.strings = c("NA", ""))


names(surveys)
surveys<-surveys[,-2]
names(surveys)[1]<-"trip"
levels(surveys$trip)<-c(1,2,4,3,5)


names(surveys)
names(surveys)[26]<-"habfrag_length"
names(surveys)[12]<-"elevation_start"
names(surveys)[31]<-"ranching"
names(surveys)[32]<-"agriculture"
str(surveys)

surveys$elevation_start<-as.integer(surveys$elevation_start)

# para as colunas ranching, agriculture, Opuntia, Zea, Prosopis e Agave, subsituir os NAs por 0
surveys$ranching<-as.character(surveys$ranching)
surveys$ranching[which(is.na(surveys$ranching))]<-0
surveys$ranching[which(!surveys$ranching==0)]<-1
surveys$ranching<-as.numeric(surveys$ranching)

surveys$agriculture<-as.character(surveys$agriculture)
surveys$agriculture[which(is.na(surveys$agriculture))]<-0
surveys$agriculture[which(!surveys$agriculture==0)]<-1
surveys$agriculture<-as.numeric(surveys$agriculture)

surveys$Opuntia<-as.character(surveys$Opuntia)
surveys$Opuntia[which(is.na(surveys$Opuntia))]<-0
surveys$Opuntia<-as.factor(surveys$Opuntia)

surveys$Zea<-as.character(surveys$Zea)
surveys$Zea[which(is.na(surveys$Zea))]<-0
surveys$Zea<-as.factor(surveys$Zea)

surveys$Prosopis<-as.character(surveys$Prosopis)
surveys$Prosopis[which(is.na(surveys$Prosopis))]<-0
surveys$Prosopis<-as.factor(surveys$Prosopis)

surveys$Agave<-as.character(surveys$Agave)
surveys$Agave[which(is.na(surveys$Agave))]<-0
surveys$Agave<-as.factor(surveys$Agave)

surveys$licuri_presence<-as.character(surveys$licuri_presence)
surveys$licuri_presence[which(is.na(surveys$licuri_presence))]<-0
surveys$licuri_presence[which(!surveys$licuri_presence==0)]<-1
surveys$licuri_presence<-as.numeric(surveys$licuri_presence)

surveys$range<-as.character(surveys$range)
surveys$occurrence<-as.character(surveys$occurrence)
# Arrumando colunas de data e hora para formato POSIX (mais adequado para dados temporais)
surveys$time_beg2<-strptime(paste(surveys$date,surveys$time_beg), format="%Y-%m-%d %H:%M:%S",tz="Brazil/East")
surveys$time_end2<-strptime(paste(surveys$date,surveys$time_end), format="%Y-%m-%d %H:%M:%S",tz="Brazil/East")
surveys$date2<-strptime(surveys$date,format="%Y-%m-%d" ,tz="Brazil/East")
head(surveys)

surveys$time_hab_change2<-strptime(paste(surveys$date,surveys$time_hab_change), format="%d/%m/%y %H:%M:%S",tz="Brazil/East")


#checking for duplicates habitat fragment lengths in each transect


surveys$fragID<-NA

# a function to identify different habitat fragments within transects (even those with equal length and habitat)
fx<-function(x){
  z<-x
  runs<-rle(z$habfrag_length)
  z2<-z[cumsum(runs$length),]
  z2$id<-seq(1,nrow(z2))
  z2$id
  
  runs2<-runs
  runs2$values<-z2$id
  
  z$id<-inverse.rle(runs2)
  return(z)
}

for(i in levels(surveys$transect_excel)){
  x<-surveys[which(surveys$transect_excel==i),c(8,26,27)]
  teste<-fx(x)
  surveys[which(surveys$transect_excel==i),"fragID"]<-teste$id
}


# variavel juntando ID do transecto, km do fragmento de habitat, e id do fragmento de hábitat dentro do transecto, para identificar cada fragmento percorrido - vai ser necessaria para agregar as observaaaes por fragmento de habitat
surveys$fragID<-paste(surveys$transect_excel,surveys$habfrag_length,surveys$fragID)
surveys[which(surveys$transect_excel=="T24"),c(8,26,27,60)]
# criando copia simplificada da planilha (excluindo colunas e linhas que nao vao ser usadas) para agregar por fragmento de habitat

surveys2<-surveys[which(surveys$transect_analysisR=="T"),c(1:6,8:10,62,26,12,13,60,58,59,27:33,40:42,43,44,45,47,34,35,36,37,38,39,56,57)]
str(surveys2)
names(surveys2)
names(surveys2)[14]<-"date_frag"
names(surveys2)[15]<-"time_start"
names(surveys2)[16]<-"time_end"

#criando dataframe agregando por fragmento de habitat
surveys3<-data.frame(fragID=unique(surveys2$fragID))

for(i in 1:nrow(surveys3)){
  surveys3$trip[i]<-  as.character(surveys2$trip[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$country[i]<-  as.character(surveys2$country[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$biome[i]<-  as.character(surveys2$biome[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$season[i]<-  as.character(surveys2$season[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$state[i]<-  as.character(surveys2$state[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$ecoregion[i]<-  as.character(surveys2$ecoregion[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$transect_excel[i]<-  as.character(surveys2$transect_excel[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$transect_analysisR[i]<-  as.character(surveys2$transect_analysisR[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$km_total[i]<- as.character(surveys2$km_total[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$habfrag_length[i]<-  as.character(surveys2$habfrag_length[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$elevation_start[i]<-  as.character(surveys2$elevation_start[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$elevation_end[i]<-  as.character(surveys2$elevation_end[match(surveys3$fragID[i],surveys2$fragID)])
  
  surveys3$time_start[i]<-  as.character(surveys2$time_start[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$time_end[i]<-  as.character(surveys2$time_end[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$date_frag[i]<-  as.character(surveys2$date_frag[match(surveys3$fragID[i],surveys2$fragID)])
  
  surveys3$habitat[i]<-  as.character(surveys2$habitat[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$vegetation.general[i]<-  as.character(surveys2$vegetation.general[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$vegetation.predominance[i]<-  as.character(surveys2$vegetation.predominance[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$anthropogenic_use[i]<-  as.character(surveys2$antropogenic_use[match(surveys3$fragID[i],surveys2$fragID)])

  surveys3$Opuntia[i]<-  as.character(surveys2$Opuntia[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$Zea[i]<-  as.character(surveys2$Zea[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$Prosopis[i]<-  as.character(surveys2$Prosopis[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$Agave[i]<-  as.character(surveys2$Agave[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$range[i]<- as.character(surveys2$range[match(surveys3$fragID[i],surveys2$fragID)])
  surveys3$occurrence[i]<- as.character(surveys2$occurrence[match(surveys3$fragID[i],surveys2$fragID)])
}

surveys3
str(surveys3)  

# agregando colunas de presença de ranching, agriculture, licuri
ranching_aggregate<-aggregate(surveys2$ranching,list(surveys2$fragID),FUN=max)
surveys3$ranching<-0
surveys3$ranching<-ranching_aggregate$x[order(ranching_aggregate$Group.1)[surveys3$fragID]]

agriculture_aggregate<-aggregate(surveys2$agriculture,list(surveys2$fragID),FUN=max)
surveys3$agriculture<-0
surveys3$agriculture<-agriculture_aggregate$x[order(agriculture_aggregate$Group.1)[surveys3$fragID]]

licuri_presence_aggregate<-aggregate(surveys2$licuri_presence,list(surveys2$fragID),FUN=max)
surveys3$licuri_presence<-0
surveys3$licuri_presence<-licuri_presence_aggregate$x[order(licuri_presence_aggregate$Group.1)[surveys3$fragID]]

#re-formatando variaveis temporais
surveys3$time_start<-as.POSIXct(surveys3$time_start,tz="Brazil/East")
surveys3$time_end<-as.POSIXct(surveys3$time_end,tz="Brazil/East")
surveys3$date_frag<-as.POSIXct(surveys3$date_frag,tz="Brazil/East")

#reformatando variaveis que sao fatores
surveys3$trip<-as.factor(surveys3$trip)
surveys3$country<-as.factor(surveys3$country)
surveys3$biome<-as.factor(surveys3$biome)
surveys3$season<-as.factor(surveys3$season)
surveys3$state<-as.factor(surveys3$state)
surveys3$ecoregion<-as.factor(surveys3$ecoregion)
surveys3$transect_excel<-as.factor(surveys3$transect_excel)
surveys3$transect_analysisR<-as.factor(surveys3$transect_analysisR)
surveys3$habitat<-as.factor(surveys3$habitat)
surveys3$vegetation.general<-as.factor(surveys3$vegetation.general)
surveys3$vegetation.predominance<-as.factor(surveys3$vegetation.predominance)
surveys3$licuri_presence<-as.factor(surveys3$licuri_presence)

surveys3$ranching<-as.factor(surveys3$ranching)
surveys3$agriculture<-as.factor(surveys3$agriculture)
surveys3$Opuntia<-as.factor(surveys3$Opuntia)
surveys3$Zea<-as.factor(surveys3$Zea)
surveys3$Prosopis<-as.factor(surveys3$Prosopis)
surveys3$Agave<-as.factor(surveys3$Agave)

surveys3$occurrence<-as.factor(surveys3$occurrence)
surveys3$range<-as.factor(surveys3$range)

# reformatando colunas numericas
surveys3$habfrag_length<-as.numeric(surveys3$habfrag_length)
surveys3$km_total<-as.numeric(surveys3$km_total)
surveys3$elevation_start<-as.numeric(surveys3$elevation_start)
surveys3$elevation_end<-as.numeric(surveys3$elevation_end)

# adicionanado colunas de fogo e village a partir de anthropogenic use
levels(surveys2$antropogenic_use)

surveys3$fire<-0
surveys3$fire[which(surveys3$anthropogenic_use=="fire")]<-"fire"
surveys3$fire<-as.factor(surveys3$fire)

surveys3$village<-0
surveys3$village[which(surveys3$anthropogenic_use=="village")]<-"village"
surveys3$village<-as.factor(surveys3$village)

# adicionando colunas de registro de gado em surveys3
levels(surveys2$overgr)

overgr_aggregate<-aggregate(surveys2$overgr_individuals,list(surveys2$fragID,surveys2$overgr),FUN=sum)

## cattle
cattle<- subset(overgr_aggregate,Group.2=="cattle")
surveys3$cattle<-0
surveys3$cattle[which(surveys3$fragID%in%cattle$Group.1)]<-cattle$x[match(surveys3$fragID[which(surveys3$fragID%in%cattle$Group.1)],cattle$Group.1)]

## dog
dog<- subset(overgr_aggregate,Group.2=="dog")
surveys3$dog<-0
surveys3$dog[which(surveys3$fragID%in%dog$Group.1)]<-dog$x[match(surveys3$fragID[which(surveys3$fragID%in%dog$Group.1)],dog$Group.1)]

## donkey
donkey<- subset(overgr_aggregate,Group.2=="donkey")
surveys3$donkey<-0
surveys3$donkey[which(surveys3$fragID%in%donkey$Group.1)]<-donkey$x[match(surveys3$fragID[which(surveys3$fragID%in%donkey$Group.1)],donkey$Group.1)]

## goat
goat<- subset(overgr_aggregate,Group.2=="goat")
surveys3$goat<-0
surveys3$goat[which(surveys3$fragID%in%goat$Group.1)]<-goat$x[match(surveys3$fragID[which(surveys3$fragID%in%goat$Group.1)],goat$Group.1)]

## horse
horse<- subset(overgr_aggregate,Group.2=="horse")
surveys3$horse<-0
surveys3$horse[which(surveys3$fragID%in%horse$Group.1)]<-horse$x[match(surveys3$fragID[which(surveys3$fragID%in%horse$Group.1)],horse$Group.1)]

## pig
pork<- subset(overgr_aggregate,Group.2=="pork")
surveys3$pig<-0
surveys3$pig[which(surveys3$fragID%in%pork$Group.1)]<-pork$x[match(surveys3$fragID[which(surveys3$fragID%in%pork$Group.1)],pork$Group.1)]

## poultry
poultry<- subset(overgr_aggregate,Group.2=="poultry")
surveys3$poultry<-0
surveys3$poultry[which(surveys3$fragID%in%poultry$Group.1)]<-poultry$x[match(surveys3$fragID[which(surveys3$fragID%in%poultry$Group.1)],poultry$Group.1)]

## sheep
sheep<- subset(overgr_aggregate,Group.2=="sheep")
surveys3$sheep<-0
surveys3$sheep[which(surveys3$fragID%in%sheep$Group.1)]<-sheep$x[match(surveys3$fragID[which(surveys3$fragID%in%sheep$Group.1)],sheep$Group.1)]

# adicionando coluna de registro de casas ### 
homes<-aggregate(surveys2$total_houses,list(surveys2$fragID),FUN=sum,na.rm=T)
surveys3$total.houses<-0
surveys3$total.houses[which(surveys3$fragID%in%homes$Group.1)]<-homes$x[match(surveys3$fragID[which(surveys3$fragID%in%homes$Group.1)],homes$Group.1)]


# Preparando registros de Psittas para agregar por fragmento de h?bitat
surveys2_parrots<-subset(surveys2,sppgroup==1)
surveys2_parrots$sppobs<-factor(surveys2_parrots$sppobs)

## separando registros auditivos "O" das contagens

surveys2_parrots$heard<-NA
surveys2_parrots$heard[which(surveys2_parrots$individuals=="O")]<-1

surveys2_parrots$ind2<-as.character(surveys2_parrots$individuals)
surveys2_parrots$ind2[which(surveys2_parrots$individuals=="O")]<-NA
surveys2_parrots$ind2<-as.integer(surveys2_parrots$ind2)

cbind(surveys2_parrots$ind2,surveys2_parrots$heard)

## criando coluna com ocorr?ncia (colapsando abundancias e combinando com registros auditivos). como surveys2_parrots s?o todos os registros de psittas, basta colocar tudo como 1
surveys2_parrots$occu <- 1

str(surveys2_parrots)
names(surveys2_parrots)
surveys2_parrots[,c(31,33,37:39)]

surveys2_parrots[,c("sppobs","individuals","heard","ind2","occu")]

# agregando contagens e ocorr?ncia de psitac?deos por fragmento de h?bitat
ind_aggregate<-aggregate(surveys2_parrots$ind2,list(surveys2_parrots$fragID,surveys2_parrots$sppobs),FUN=sum, na.rm=T)
str(ind_aggregate)

occu_aggregate<-aggregate(surveys2_parrots$occu,list(surveys2_parrots$fragID,surveys2_parrots$sppobs),FUN=mean, na.rm=T)

surv<-surveys3

for(i in 1:length(levels(ind_aggregate$Group.2))){
  surv$x<-0
  sp<- subset(ind_aggregate,Group.2==levels(ind_aggregate$Group.2)[i])
  surv$x[which(surveys3$fragID%in%sp$Group.1)]<-sp$x[match(surv$fragID[which(surv$fragID%in%sp$Group.1)],sp$Group.1)]
  names(surv)[ncol(surv)]<-paste("count",levels(ind_aggregate$Group.2)[i])
  
  surv$y<-0
  sp2<- subset(occu_aggregate,Group.2==levels(ind_aggregate$Group.2)[i])
  surv$y[which(surveys3$fragID%in%sp2$Group.1)]<-sp2$x[match(surv$fragID[which(surv$fragID%in%sp2$Group.1)],sp2$Group.1)]
  names(surv)[ncol(surv)]<-paste("occu",levels(ind_aggregate$Group.2)[i])
}


# adicionando coluna com riqueza de psittas
surv$parrot.richness<-rowSums(surv[,grep("occu",names(surv))[-1]])
str(surv)



# corrigindo ultimos erros, etc.
names(surv)
names(surv)[grep(" ",names(surv))]<-unlist(lapply(names(surv[,grep(" ",names(surv))]),FUN=gsub,pattern=" ",replacement="_"))
names(surv)[45]<-"count_Anodorhynchus_hyacinthinus"
names(surv)[46]<-"occu_Anodorhynchus_hyacinthinus"
names(surv)[63]<-"count_Eupsitula_sp"
names(surv)[64]<-"occu_Eupsitula_sp"
names(surv)[69]<-"count_Primolius_maracana"
names(surv)[70]<-"occu_Primolius_maracana"
# uma observac?o de cattle com valor = 3937. converter para NA pois ? provavelmente um erro
surv$cattle[which(surv$cattle==3937)]<-NA

#algumas observacoes tem habfrag_length=0 - remover!
surv<-surv[-which(surv$habfrag_length==0),]

rm(list=setdiff(ls(),"surv"))

#excluir registros em habitat urbano. Sao 11 apenas
subset(surv,habitat=="urban")
surv<-subset(surv,habitat!="urban")
surv$habitat<-factor(surv$habitat)
# inserir coluna de meses
surv$month<-format(surv$date_frag,"%m")

# selecinar apenas observaçoes da Caatinga
surv_caa<-subset(surv,ecoregion!="BEYOND")
surv_caa$ecoregion<-factor(surv_caa$ecoregion)

# exportar arquivo Rdata      

gc()
save.image("C:/Users/voeroesd/Dropbox/A.leari/road_surveys_Aleari/modelo-road-surveys.Rdata")
save.image(("~/Dropbox/A.leari-CHICO/road_surveys_Aleari/modelo-road-surveys.Rdata"))
save.image(("~/Dropbox/A.leari/road_surveys_Aleari/modelo-road-surveys.Rdata"))
#exportar planilha csv

write.csv(surv,"C:/Users/voeroesd/Dropbox/A.leari/road_surveys_Aleari/road-surveys_agregado_habitats.csv",row.names = F)
write.csv(surv,"~/Dropbox/A.leari-CHICO/road_surveys_Aleari/road-surveys_agregado_habitats.csv",row.names = F)
write.csv(surv,"~/Dropbox/A.leari/road_surveys_Aleari/road-surveys_agregado_habitats.csv",row.names = F)
write.csv(surv_caa,"~/Dropbox/A.leari/road_surveys_Aleari/road-surveys_agregado_habitats_caatinga.csv",row.names = F)
