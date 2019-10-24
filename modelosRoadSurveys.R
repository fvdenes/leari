# Analysis of parrot road survey data ####
library(pscl)
library(MASS)
library(bbmle)
library("ggplot2")
library(gridExtra)
library("nnet")
library(glm.predict)
library(stringr)

# Functions ####
# A function to generate predictions (with CIs) from zeroinfl models
predCIs<-function(object,newdata, CIprobs=c(0.025,0.975)){
  mf <- model.frame(delete.response(object$terms$full), 
                    newdata, na.action = na.pass, xlev = object$levels)
  X <- model.matrix(delete.response(object$terms$count), 
                    mf, contrasts = object$contrasts$count)
  Z <- model.matrix(delete.response(object$terms$zero), 
                    mf, contrasts = object$contrasts$zero)
  
  
  mu <- exp(X %*% coef(object,"count"))[,1]
  phi <- plogis(Z %*% coef(object,"zero"))[,1]
  rval <- (1 - phi) * mu
  library(MASS)
  frame<-mvrnorm(n=1000,mu=coef(object),Sigma=vcov(object))
  frame_mu<-exp(X %*% t(frame)[grepl("count", colnames(frame)),])
  frame_phi<-plogis(Z %*% t(frame)[grepl("zero", colnames(frame)),])
  frame_rval<- (1-frame_phi) * frame_mu
  
  frameCIs<-t(apply(cbind(rval,frame_rval),1,FUN=quantile,probs=CIprobs))
  
  frameCIs2<- data.frame(N=rval,lower=frameCIs[,1], upper=frameCIs[,2])
  frameCIs2$lower2<- frameCIs2$N-(frameCIs2$upper-frameCIs2$lower)/2
  frameCIs2$upper2<- frameCIs2$N+(frameCIs2$upper-frameCIs2$lower)/2
  
  out<- data.frame(N= frameCIs2$N,lower=frameCIs2$lower2, upper=frameCIs2$upper2)
  
  return(out)
}

# Load dataset ####
load("C:/Users/voeroesd/Dropbox/A.leari/road_surveys_Aleari/modelo-road-surveys.Rdata")
#load("~/Dropbox/A.leari-CHICO/road_surveys_Aleari/modelo-road-surveys.Rdata") # para carregar do computador da Erica
#load("~/Dropbox/A.leari/road_surveys_Aleari/modelo-road-surveys.Rdata") 

# Registros de psitacideos ####

## quais especies tem 15 registros ou mais?
tabela_dim<-as.data.frame(cbind(apply(surv_caa[,grep("count_",colnames(surv_caa))],2,FUN = function(x){length(which(x>0))}),
apply(surv_caa[,grep("occu_",colnames(surv_caa))],2,FUN = function(x){length(which(x>0))})))
names(tabela_dim)<- c("encounters(counted)","encounters(presence)")
tabela_dim

tabela_dim$total.individuals<-apply(surv_caa[,grep("count_",colnames(surv_caa))],2,FUN = sum)


rownames(tabela_dim)<-substring(rownames(tabela_dim),first=7)
tabela_dim
#write.csv(tabela_dim,"C:/Users/voeroesd/Dropbox/A.leari/road_surveys_Aleari/summary_parrot_records.csv",row.names = F)
#write.csv(tabela_dim,"~/Dropbox/A.leari/road_surveys_Aleari/summary_parrot_records.csv",row.names = F)

# save.image("C:/Users/voeroesd/Dropbox/A.leari/road_surveys_Aleari/modelo-road-surveys.Rdata")
# 
# ## explorando os dados
# table(surv$habitat)
# table(surv$habitat,surv$ranching)
# table(surv$habitat,surv$agriculture)
# table(surv$habitat,surv$cattle)
# boxplot(habfrag_length~habitat,data=subset(surv,cattle>0))
# table(surv$ranching,surv$cattle)
# table(surv$habitat,surv$goat)
# boxplot(goat~habitat,data=subset(surv,goat>0))
# table(surv$ranching,surv$goat)
# summary(glm(cattle~habitat,data=surv,family="poisson",offset=scale(habfrag_length)))
# 
# boxplot(total.houses~habitat,data=surv)
# table(surv$total.houses)
# table(surv$total.houses,surv$habitat)
# plot(table(surv$parrot.richness))
# table(surv$month)
# boxplot(goat~ranching,data=surv)
# boxplot(cattle~licuri_presence,data=surv)
# 
# table(surv$ecoregion)
# names(surv)
# 
# table(surv$licuri_presence)
#
# table(subset(surv,occu_Eupsittula_cactorum==1)$biome)
# table(subset(surv,occu_Eupsittula_cactorum==1)$ecoregion)
# table(subset(surv,occu_Eupsittula_cactorum==1)$habitat)
# table(subset(surv,occu_Eupsittula_cactorum==1)$vegetation.predominance)
# table(subset(surv,occu_Eupsittula_cactorum==1)$licuri_presence)
# table(subset(surv,occu_Eupsittula_cactorum==1)$agriculture)
# 
# table(subset(surv,occu_Thectocercus_acuticaudatus==1)$habitat)
# table(surv$ranching)

# ### variaveis independentes:
# 
# ## month

## ecoregion

## licuri_presence

## cattle
## goat

## habitat

## ranching
## agriculture
## fire
## village
# 
# # Assessing correlations among predictors####
# #Correspondence analysis for factor - factor correlations
# corresp(~ agriculture + ranching, data=surv_caa)
# corresp(~ agriculture + fire, data=surv_caa)
# corresp(~ agriculture + village, data=surv_caa)
# corresp(~ agriculture + licuri_presence, data=surv_caa)
# corresp(~ ranching + fire, data=surv_caa)
# corresp(~ ranching + village, data=surv_caa)
# corresp(~ ranching + licuri_presence, data=surv_caa)
# corresp(~ fire + village, data=surv_caa)
# corresp(~ fire + licuri_presence, data=surv_caa)
# corresp(~ village + licuri_presence, data=surv_caa)
# corresp(~ range + ecoregion, data=surv_caa)
# corresp(~ occurrence + ecoregion, data=surv_caa)
# #Correlation between continuous variables (cattle and goat)
# cor(surv_caa$cattle,surv_caa$goat, use="complete.obs", method="spearman")
# cor.test(surv_caa$cattle,surv_caa$goat,method="spearman")
# 
# corresp(~ cattle + agriculture, data=surv_caa)
# corresp(~ cattle + ranching, data=surv_caa)
# corresp(~ cattle + fire, data=surv_caa)
# corresp(~ cattle + village, data=surv_caa)
# corresp(~ cattle + licuri_presence, data=surv_caa)
# 
# corresp(~ goat + agriculture, data=surv_caa)
# corresp(~ goat + ranching, data=surv_caa)
# corresp(~ goat + fire, data=surv_caa)
# corresp(~ goat + village, data=surv_caa)
# corresp(~ goat + licuri_presence, data=surv_caa)

# #### E. cactorum: ####

# Modelo de ocorrencia ####

# m1<- glm(
#   occu_Eupsittula_cactorum~ #variavel resposta (dependente)
#   month+ecoregion+licuri_presence+cattle+goat+total.houses+#variaveis preditora (independente)
#     habitat,
#   offset=scale(habfrag_length), #
#   family="binomial",
#   data=surv_caa)
# 
# summary(m1)
# anova(m1, test="Chisq")
# # 
# # ## modelo 2 - com variaveis 'ranching'e 'agriculture'
# # m2<- glm(
# #   occu_Eupsittula_cactorum~ #variavel resposta (dependente)
# #     month+ecoregion+licuri_presence+cattle+goat+#variaveis preditora (independente)
# #     ranching+agriculture+fire+village,
# #   offset=scale(habfrag_length), #
# #   family="binomial",
# #   data=surv_caa)
# # 
# # summary(m2)
# # anova(m2, test="Chisq")
# # 
# # AICctab(m1,m2)
# 
# ## Modelos de abundancia ####
# 
# ma1<-zeroinfl(count_Eupsittula_cactorum~
#                 month+ecoregion+licuri_presence+cattle+goat+#variaveis preditora (independente)
#                 habitat|1
#               ,dist ="poisson",
#               offset = scale(surv_caa$habfrag_length),
#               data=surv_caa)
# summary(ma1)
# AIC(ma1)

Ecac1<-zeroinfl(count_Eupsittula_cactorum~
                ecoregion+cattle+goat+
                ranching+agriculture+fire+range|1
              ,dist ="poisson",
              offset = scale(surv_caa$habfrag_length),
              data=surv_caa)
summary(Ecac1)

Ecac2<-zeroinfl(count_Eupsittula_cactorum~
                  ecoregion+cattle+goat+
                  ranching+agriculture+fire+occurrence|1
                ,dist ="poisson",
                offset = scale(surv_caa$habfrag_length),
                data=surv_caa)
summary(Ecac2)

Ecac3<-zeroinfl(count_Eupsittula_cactorum~
                  month+ecoregion+cattle*ranching+goat*
                  ranching+agriculture+fire+occurrence|1
                ,dist ="poisson",
                offset = scale(surv_caa$habfrag_length),
                data=surv_caa)
summary(Ecac3)

AICtab(Ecac1,Ecac2,Ecac3)
confint(Ecac2)

# Predictions #
#Figuras com as predicoes de abundancia em diferentes configuracoes das variaveis independentes

# Preds.Fig - Cattle ####
pdf1.Ecac<-data.frame(
  ecoregion=rep("RASO",404),
  cattle=rep(seq(0,100),4),
  goat=rep(0,404),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404)
)

Ecac_cattle<-predCIs(object=Ecac2,newdata=pdf1.Ecac)
Ecac_cattle$cattle<-rep(seq(0,100),4)
Ecac_cattle$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
Ecac_cattle$occurrence<-factor(Ecac_cattle$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Ecac_cattle$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
Ecac_cattle$presence<-factor(Ecac_cattle$presence, levels = c("Present","Absent"))
Ecac_cattle

# p1.Ecac<-ggplot(data=Ecac_cattle)+
#   geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
#   geom_line(mapping = aes(x=cattle, y=N,color=occurrence,linetype=presence),size=1)+
#   labs(y="Mean relative abundance", x="Cattle abundance") +
#   scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
#   scale_linetype_discrete(name= "Lear's Macaw occurrence")+
#   guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))
# 
# p1.Ecac



# Preds.Fig - Goat ####
pdf2.Ecac<-data.frame(
  month=rep("04",404),
  ecoregion=rep("RASO",404),
  cattle=rep(0,404),
  goat=rep(seq(0,100),4),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404)
)

Ecac_goat<-predCIs(object=Ecac2,newdata=pdf2.Ecac)
Ecac_goat$goat<-rep(seq(0,100),4)
Ecac_goat$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
Ecac_goat$occurrence<-factor(Ecac_goat$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Ecac_goat$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
Ecac_goat$presence<-factor(Ecac_goat$presence, levels = c("Present","Absent"))
Ecac_goat

# p2.Ecac<-ggplot(data=Ecac_goat)+
#   geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
#   geom_line(mapping = aes(x=goat, y=N,color=occurrence,linetype=presence),size=1)+
#   labs(y="Mean relative abundance", x="goat abundance") +
#   scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
#   scale_linetype_discrete(name= "Lear's Macaw occurrence")+
#   guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))
#   
# p2.Ecac

# Preds.Fig. - Ranching ####
pdf3.Ecac<-data.frame(
  month=rep("04",8),
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  ranching=factor(rep(0:1,4)),
  agriculture=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Ecac_ranching<-predCIs(object=Ecac2,newdata=pdf3.Ecac)
Ecac_ranching$ranching<-c("No pasture","Pasture")
Ecac_ranching$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Ecac_ranching$occurrence<-factor(Ecac_ranching$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Ecac_ranching$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Ecac_ranching

# p3.Ecac<-ggplot(data=Ecac_ranching)+
#   geom_errorbar(mapping = aes(x=ranching, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=ranching,y=N, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="Ranching")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p3.Ecac

# Preds.Fig. - Agriculture ####
pdf4.Ecac<-data.frame(
  month=rep("04",8),
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0:1,4)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Ecac_agriculture<-predCIs(object=Ecac2,newdata=pdf4.Ecac)
Ecac_agriculture$agriculture<-c("No cultivation","Cultivation")
Ecac_agriculture$agriculture<-factor(Ecac_agriculture$agriculture, levels = c("No cultivation","Cultivation"))
Ecac_agriculture$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Ecac_agriculture$occurrence<-factor(Ecac_agriculture$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Ecac_agriculture$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Ecac_agriculture


# p4.Ecac<-ggplot(data=Ecac_agriculture)+
#   geom_errorbar(mapping = aes(x=agriculture, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=agriculture,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="agriculture")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p4.Ecac

# Preds.Fig. - Fire ####
pdf5.Ecac<-data.frame(
  month=rep("04",8),
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(c(0,"fire"),4)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Ecac_fire<-predCIs(object=Ecac2,newdata=pdf5.Ecac)
Ecac_fire$fire<-c("No fire","Fire")
Ecac_fire$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Ecac_fire$occurrence<-factor(Ecac_fire$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Ecac_fire$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Ecac_fire



# p5.Ecac<-ggplot(data=Ecac_fire)+
#   geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=fire,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="fire")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p5.Ecac

# Preds.Fig. - Ecoregion ####
pdf6.Ecac<-data.frame(
  month=rep("04",8),
  ecoregion=rep(c("RASO","BDO"),4),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Ecac_ecoregion<-predCIs(object=Ecac2,newdata=pdf6.Ecac)
Ecac_ecoregion$ecoregion<-c("RASO","BDO")
Ecac_ecoregion$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Ecac_ecoregion$occurrence<-factor(Ecac_ecoregion$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Ecac_ecoregion$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Ecac_ecoregion

Ecac_ecoregion2<-Ecac_ecoregion[1:7,]

# p6.Ecac<-ggplot(data=Ecac_ecoregion2)+
#   geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=ecoregion,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="ecoregion")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p6.Ecac
# 
# 
# #Grid E. cac####
# grid.arrange(p1.Ecac,p2.Ecac,p3.Ecac,p4.Ecac,p5.Ecac,p6.Ecac,nrow=3)

#### Forpus xanthopterygius ####

Fxan<-zeroinfl(count_Forpus_xanthopterygius~
                 ecoregion+cattle+goat+
                 ranching+agriculture+fire+range|1
               ,dist ="poisson",
               offset = scale(surv_caa$habfrag_length),
               data=surv_caa)
summary(Fxan)

Fxan2<-zeroinfl(count_Forpus_xanthopterygius~
                 ecoregion+cattle+goat+
                 ranching+agriculture+fire+occurrence|1
               ,dist ="poisson",
               offset = scale(surv_caa$habfrag_length),
               data=surv_caa)
summary(Fxan2)

AICtab(Fxan,Fxan2)

AICtab(Fxan,Fxan2)

# Preds.Fig - Cattle ####
pdf1.Fxan<-data.frame(
  ecoregion=rep("RASO",404),
  cattle=rep(seq(0,100),4),
  goat=rep(0,404),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404)
)

Fxan_cattle<-predCIs(object=Fxan2,newdata=pdf1.Fxan)
Fxan_cattle$cattle<-rep(seq(0,100),4)
Fxan_cattle$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
Fxan_cattle$occurrence<-factor(Fxan_cattle$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Fxan_cattle$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
Fxan_cattle$presence<-factor(Fxan_cattle$presence, levels = c("Present","Absent"))
Fxan_cattle

# p1.Fxan<-ggplot(data=Fxan_cattle)+
#   geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
#   geom_line(mapping = aes(x=cattle, y=N,color=occurrence,linetype=presence),size=1)+
#   labs(y="Mean relative abundance", x="Cattle abundance") +
#   scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
#   scale_linetype_discrete(name= "Lear's Macaw occurrence")+
#   guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))
# 
# p1.Fxan



# Preds.Fig - Goat ####
pdf2.Fxan<-data.frame(
  month=rep("04",404),
  ecoregion=rep("RASO",404),
  cattle=rep(0,404),
  goat=rep(seq(0,100),4),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404)
)

Fxan_goat<-predCIs(object=Fxan2,newdata=pdf2.Fxan)
Fxan_goat$goat<-rep(seq(0,100),4)
Fxan_goat$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
Fxan_goat$occurrence<-factor(Fxan_goat$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Fxan_goat$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
Fxan_goat$presence<-factor(Fxan_goat$presence, levels = c("Present","Absent"))
Fxan_goat

# p2.Fxan<-ggplot(data=Fxan_goat)+
#   geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
#   geom_line(mapping = aes(x=goat, y=N,color=occurrence,linetype=presence),size=1)+
#   labs(y="Mean relative abundance", x="goat abundance") +
#   scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
#   scale_linetype_discrete(name= "Lear's Macaw occurrence")+
#   guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))
# 
# p2.Fxan

# Preds.Fig. - Ranching ####
pdf3.Fxan<-data.frame(
  month=rep("04",8),
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  ranching=factor(rep(0:1,4)),
  agriculture=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Fxan_ranching<-predCIs(object=Fxan2,newdata=pdf3.Fxan)
Fxan_ranching$ranching<-c("No pasture","Pasture")
Fxan_ranching$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Fxan_ranching$occurrence<-factor(Fxan_ranching$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Fxan_ranching$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Fxan_ranching

# p3.Fxan<-ggplot(data=Fxan_ranching)+
#   geom_errorbar(mapping = aes(x=ranching, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=ranching,y=N, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="Ranching")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p3.Fxan

# Preds.Fig. - Agriculture ####
pdf4.Fxan<-data.frame(
  month=rep("04",8),
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0:1,4)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Fxan_agriculture<-predCIs(object=Fxan2,newdata=pdf4.Fxan)
Fxan_agriculture$agriculture<-c("No cultivation","Cultivation")
Fxan_agriculture$agriculture<-factor(Fxan_agriculture$agriculture, levels = c("No cultivation","Cultivation"))
Fxan_agriculture$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Fxan_agriculture$occurrence<-factor(Fxan_agriculture$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Fxan_agriculture$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Fxan_agriculture

# 
# p4.Fxan<-ggplot(data=Fxan_agriculture)+
#   geom_errorbar(mapping = aes(x=agriculture, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=agriculture,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="agriculture")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p4.Fxan

# Preds.Fig. - Fire ####
pdf5.Fxan<-data.frame(
  month=rep("04",8),
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(c(0,"fire"),4)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Fxan_fire<-predCIs(object=Fxan2,newdata=pdf5.Fxan)
Fxan_fire$fire<-c("No fire","Fire")
Fxan_fire$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Fxan_fire$occurrence<-factor(Fxan_fire$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Fxan_fire$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Fxan_fire



# p5.Fxan<-ggplot(data=Fxan_fire)+
#   geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=fire,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="fire")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p5.Fxan

# Preds.Fig. - Ecoregion ####
pdf6.Fxan<-data.frame(
  month=rep("04",8),
  ecoregion=rep(c("RASO","BDO"),4),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

Fxan_ecoregion<-predCIs(object=Fxan2,newdata=pdf6.Fxan)
Fxan_ecoregion$ecoregion<-c("RASO","BDO")
Fxan_ecoregion$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
Fxan_ecoregion$occurrence<-factor(Fxan_ecoregion$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
Fxan_ecoregion$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
Fxan_ecoregion

Fxan_ecoregion2<-Fxan_ecoregion[1:7,]

# p6.Fxan<-ggplot(data=Fxan_ecoregion2)+
#   geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=1)+
#   geom_point( mapping=aes(x=ecoregion,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=3)+
#   labs(y="Mean relative abundance", x="ecoregion")+
#   scale_color_discrete(name= "Lear's Macaw records")+
#   scale_shape_discrete(name= "Lear's Macaw occurrence")
# p6.Fxan
# 
# 
# #Grid F.xan####
# grid.arrange(p1.Fxan,p2.Fxan,p3.Fxan,p4.Fxan,p5.Fxan,p6.Fxan,nrow=3)


#Composite plots - E.cac and F.xan ####
#Composite cattle ####
p1.comp<-ggplot(data=Ecac_cattle)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=cattle, y=N,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(italic("N"["E. cactorum"])), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")
p1.comp

p2.comp<-ggplot(data=Fxan_cattle)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=cattle, y=N,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(italic("N"["F. xanthopterygius"])), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        legend.key.size=unit(0.30,"cm"))

p2.comp

#Composite goat ####
p3.comp<-ggplot(data=Ecac_goat)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=goat, y=N,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),
         line = guide_colorbar(order=2),
         fill = guide_legend(order = 1))+
  labs(y=expression(italic("N"["E. cactorum"])), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")

p3.comp

p4.comp<-ggplot(data=Fxan_goat)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=goat, y=N,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1,override.aes = list(colour = "white",fill="white")),
         linetype = guide_legend(order = 1,override.aes = list(colour = "white",fill="white")),
         fill = guide_legend(order = 1)
         )+
  labs(y=expression(italic("N"["F. xanthopterygius"])), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill="white"),
        legend.key.size=unit(0.30,"cm")
        )

p4.comp

#Composite Ranching ####
p5.comp<-ggplot(data=Ecac_ranching)+
  geom_errorbar(mapping = aes(x=ranching, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ranching,y=N, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("N"["E. cactorum"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")
p5.comp

p6.comp<-ggplot(data=Fxan_ranching)+
  geom_errorbar(mapping = aes(x=ranching, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ranching,y=N, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("N"["F. xanthopterygius"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        legend.key.size=unit(0.30,"cm"))

p6.comp

#Composite agriculture ####
p7.comp<-ggplot(data=Ecac_agriculture)+
  geom_errorbar(mapping = aes(x=agriculture, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=agriculture,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("N"["E. cactorum"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "none")
p7.comp

p8.comp<-ggplot(data=Fxan_agriculture)+
  geom_errorbar(mapping = aes(x=agriculture, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=agriculture,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(italic("N"["F. xanthopterygius"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white"),
        legend.key.size=unit(0.30,"cm"))

p8.comp

#Composite fire ####
p9.comp<-ggplot(data=Ecac_fire)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("N"["E. cactorum"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "none")
p9.comp

p10.comp<-ggplot(data=Fxan_fire)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(italic("N"["F. xanthopterygius"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white"),
        legend.key.size=unit(0.30,"cm"))
p10.comp

#Composite ecoregion ####
p11.comp<-ggplot(data=Ecac_ecoregion2)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("N"["E. cactorum"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "none")
p11.comp

p12.comp<-ggplot(data=Fxan_ecoregion2)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(italic("N"["F. xanthopterygius"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))
p12.comp

# Grid comp ####
grid_comp<-grid.arrange(p1.comp,p2.comp,p3.comp,p4.comp,p5.comp,p6.comp,p7.comp,p8.comp,p9.comp,p10.comp,p11.comp,p12.comp,nrow=6,widths=c(1,1.4))

ggsave(filename="C:/Users/voeroesd/Dropbox/Ecac_Fxan_plots.pdf", 
       plot = grid_comp, 
       width = 210, 
       height = 297, 
       units = "mm")

# ggsave(filename="~/Dropbox/Ecac_Fxan_plots.pdf", 
#        plot = grid_comp, 
#        width = 210, 
#        height = 297, 
#        units = "mm")


#### Species richness ####

rich<-zeroinfl(parrot.richness~
                 ecoregion+cattle+goat+
                 ranching+agriculture+fire+range|1
               ,dist ="poisson",
               offset = scale(surv_caa$habfrag_length),
               data=surv_caa)
summary(rich)

rich2<-zeroinfl(parrot.richness~
                  ecoregion+cattle+goat+
                  ranching+agriculture+fire+occurrence|1
                ,dist ="poisson",
                offset = scale(surv_caa$habfrag_length),
                data=surv_caa)
summary(rich2)

AICtab(rich,rich2)

summary(rich)
confint(rich)


# Preds.Fig - Cattle ####
pdf1.rich<-data.frame(
  ecoregion=rep("RASO",404),
  cattle=rep(seq(0,100),4),
  goat=rep(0,404),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404)
)



rich_cattle<-predCIs(object=rich2,newdata=pdf1.rich)
rich_cattle$cattle<-rep(seq(0,100),4)
rich_cattle$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
rich_cattle$occurrence<-factor(rich_cattle$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
rich_cattle$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
rich_cattle$presence<-factor(rich_cattle$presence, levels = c("Present","Absent"))
rich_cattle

p1.rich<-ggplot(data=rich_cattle)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=cattle, y=N,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(italic("R"["parrots"])), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")

p1.rich

# Preds.Fig - Goat ####
pdf2.rich<-data.frame(
  ecoregion=rep("RASO",404),
  cattle=rep(0,404),
  goat=rep(seq(0,100),4),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404)
)

rich_goat<-predCIs(object=rich2,newdata=pdf2.rich)
rich_goat$goat<-rep(seq(0,100),4)
rich_goat$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
rich_goat$occurrence<-factor(rich_goat$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
rich_goat$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
rich_goat$presence<-factor(rich_goat$presence, levels = c("Present","Absent"))
rich_goat

p2.rich<-ggplot(data=rich_goat)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=goat, y=N,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),
         line = guide_colorbar(order=2),
         fill = guide_legend(order = 1))+
  labs(y=expression(italic("R"["parrots"])), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        legend.key.size=unit(0.25,"cm"))
p2.rich


# Preds.Fig. - Ranching ####
pdf3.rich<-data.frame(
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  ranching=factor(rep(0:1,4)),
  agriculture=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

rich_ranching<-predCIs(object=rich2,newdata=pdf3.rich)
rich_ranching$ranching<-c("No pasture","Pasture")
rich_ranching$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
rich_ranching$occurrence<-factor(rich_ranching$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
rich_ranching$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
rich_ranching

p3.rich<-ggplot(data=rich_ranching)+
  geom_errorbar(mapping = aes(x=ranching, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ranching,y=N, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("R"["parrots"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")
p3.rich



# Preds.Fig. - Agriculture ####
pdf4.rich<-data.frame(
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0:1,4)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

rich_agriculture<-predCIs(object=rich2,newdata=pdf4.rich)
rich_agriculture$agriculture<-c("No cultivation","Cultivation")
rich_agriculture$agriculture<-factor(rich_agriculture$agriculture, levels = c("No cultivation","Cultivation"))
rich_agriculture$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
rich_agriculture$occurrence<-factor(rich_agriculture$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
rich_agriculture$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
rich_agriculture


p4.rich<-ggplot(data=rich_agriculture)+
  geom_errorbar(mapping = aes(x=agriculture, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=agriculture,y=N, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("R"["parrots"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        legend.key.size=unit(0.25,"cm"))
p4.rich

# Preds.Fig. - Fire ####
pdf5.rich<-data.frame(
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(c(0,"fire"),4)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

rich_fire<-predCIs(object=rich2,newdata=pdf5.rich)
rich_fire$fire<-c("No fire","Fire")
rich_fire$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
rich_fire$occurrence<-factor(rich_fire$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
rich_fire$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
rich_fire



p5.rich<-ggplot(data=rich_fire)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(italic("R"["parrots"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "none")
p5.rich



# Preds.Fig. - Ecoregion ####
pdf6.rich<-data.frame(
  ecoregion=rep(c("RASO","BDO"),4),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8)
)

rich_ecoregion<-predCIs(object=rich2,newdata=pdf6.rich)
rich_ecoregion$ecoregion<-c("RASO","BDO")
rich_ecoregion$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
rich_ecoregion$occurrence<-factor(rich_ecoregion$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
rich_ecoregion$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
rich_ecoregion

rich_ecoregion2<-rich_ecoregion[1:7,]

p6.rich<-ggplot(data=rich_ecoregion2)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=N, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(italic("R"["parrots"])))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))
p6.rich



#Grid Parrot Richness####
grid_richness<-grid.arrange(p1.rich,p2.rich,p3.rich,p4.rich,p5.rich,p6.rich,nrow=3,widths=c(1,1.4))

ggsave(filename="C:/Users/voeroesd/Dropbox/richness_plots.pdf", 
       plot = grid_richness, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/richness_plots.pdf", 
#        plot = grid_richness, 
#        width = 210, 
#        height = 140, 
#        units = "mm")



# Modelos para Licuri ####
surv_caa$offset<-scale(surv_caa$habfrag_length)[,1]

m_licuri<- glm(
  licuri_presence ~ #variavel resposta (dependente)
  ecoregion+
    cattle+goat+
    ranching+
    agriculture+
    fire+
    occurrence+
    offset(offset),
  family="binomial",
  data=surv_caa)

summary(m_licuri)
anova(m_licuri, test="Chisq")

# Preds.Fig - Cattle ####
pdf1.licuri<-data.frame(
  ecoregion=rep("RASO",404),
  cattle=rep(seq(0,100),4),
  goat=rep(0,404),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404),
  offset=rep(0,404)
)

str(pdf1.licuri)

licuri_cattle<-predict(object=m_licuri,newdata=pdf1.licuri,type="response",se.fit=T)
licuri_cattle<-data.frame(p=licuri_cattle$fit,lower=licuri_cattle$fit-1.96*licuri_cattle$se.fit,upper=licuri_cattle$fit+1.96*licuri_cattle$se.fit)
licuri_cattle$cattle<-rep(seq(0,100),4)
licuri_cattle$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
licuri_cattle$occurrence<-factor(licuri_cattle$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
licuri_cattle$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
licuri_cattle$presence<-factor(licuri_cattle$presence, levels = c("Present","Absent"))


p1.licuri<-ggplot(data=licuri_cattle)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=cattle, y=p,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(psi["Licuri palm"]), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=6),
        axis.title.y = element_text(size=6,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.position = "none")
p2.licuri

# Preds.Fig - Goat ####
pdf2.licuri<-data.frame(
  ecoregion=rep("RASO",404),
  goat=rep(seq(0,100),4),
  cattle=rep(0,404),
  ranching=factor(rep(0,404)),
  agriculture=factor(rep(0,404)),
  fire=factor(rep(0,404)),
  occurrence=factor(rep(seq(0,3),each=101)),
  habfrag_length=rep(0,404),
  offset=rep(0,404)
)

str(pdf2.licuri)

licuri_goat<-predict(object=m_licuri,newdata=pdf2.licuri,type="response",se.fit=T)
licuri_goat<-data.frame(p=licuri_goat$fit,lower=licuri_goat$fit-1.96*licuri_goat$se.fit,upper=licuri_goat$fit+1.96*licuri_goat$se.fit)
licuri_goat$goat<-rep(seq(0,100),4)
licuri_goat$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=101))
licuri_goat$occurrence<-factor(licuri_goat$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
licuri_goat$presence<-factor(rep(c("Absent","Present","Absent","Present"),each=101))
licuri_goat$presence<-factor(licuri_goat$presence, levels = c("Present","Absent"))


p2.licuri<-ggplot(data=licuri_cattle)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.4)+
  geom_line(mapping = aes(x=cattle, y=p,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(psi["Licuri palm"]), x="Goat abundance")+
  theme(axis.title.x = element_text(size=6),
        axis.title.y = element_text(size=6,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        legend.key.size=unit(0.25,"cm"))

p2.licuri



# Preds.Fig. - Ranching ####

pdf3.licuri<-data.frame(
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  ranching=factor(rep(0:1,4)),
  agriculture=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8),
  offset=rep(0,8)
)


licuri_ranching<-predict(object=m_licuri,newdata=pdf3.licuri,type="response",se.fit=T)
licuri_ranching<-data.frame(p=licuri_ranching$fit,lower=licuri_ranching$fit-1.96*licuri_ranching$se.fit,upper=licuri_ranching$fit+1.96*licuri_ranching$se.fit)
licuri_ranching$ranching<-c("No pasture","Pasture")
licuri_ranching$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
licuri_ranching$occurrence<-factor(licuri_ranching$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
licuri_ranching$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
licuri_ranching

p3.licuri<-ggplot(data=licuri_ranching)+
  geom_errorbar(mapping = aes(x=ranching, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ranching,y=p, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["Licuri palm"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.position = "none")
p3.licuri


# Preds.Fig. - Agriculture ####
pdf4.licuri<-data.frame(
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0:1,4)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8),
  offset=rep(0,8)
)


licuri_agriculture<-predict(object=m_licuri,newdata=pdf4.licuri,type="response",se.fit=T)
licuri_agriculture<-data.frame(p=licuri_agriculture$fit,lower=licuri_agriculture$fit-1.96*licuri_agriculture$se.fit,upper=licuri_agriculture$fit+1.96*licuri_agriculture$se.fit)
licuri_agriculture$agriculture<-c("No cultivation","Cultivation")
licuri_agriculture$agriculture<-factor(licuri_agriculture$agriculture, levels = c("No cultivation","Cultivation"))
licuri_agriculture$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
licuri_agriculture$occurrence<-factor(licuri_agriculture$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
licuri_agriculture$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
licuri_agriculture

p4.licuri<-ggplot(data=licuri_agriculture)+
  geom_errorbar(mapping = aes(x=agriculture, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=agriculture,y=p, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["Licuri palm"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        legend.key.size=unit(0.25,"cm"))
p4.licuri

# Preds.Fig. - Fire ####
pdf5.licuri<-data.frame(
  ecoregion=rep("RASO",8),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(c(0,"fire"),4)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8),
  offset=rep(0,8)
)


licuri_fire<-predict(object=m_licuri,newdata=pdf5.licuri,type="response",se.fit=T)
licuri_fire<-data.frame(p=licuri_fire$fit,lower=licuri_fire$fit-1.96*licuri_fire$se.fit,upper=licuri_fire$fit+1.96*licuri_fire$se.fit)
licuri_fire$fire<-c("No fire","Fire")
licuri_fire$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
licuri_fire$occurrence<-factor(licuri_fire$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
licuri_fire$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
licuri_fire

p5.licuri<-ggplot(data=licuri_fire)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=p, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["Licuri palm"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position = "none")
p5.licuri

# Preds.Fig. - Ecoregion ####
pdf6.licuri<-data.frame(
  ecoregion=rep(c("RASO","BDO"),4),
  cattle=rep(0,8),
  goat=rep(0,8),
  agriculture=factor(rep(0,8)),
  ranching=factor(rep(0,8)),
  fire=factor(rep(0,8)),
  occurrence=factor(rep(seq(0,3),each=2)),
  habfrag_length=rep(0,8),
  offset=rep(0,8)
)

licuri_ecoregion<-predict(object=m_licuri,newdata=pdf6.licuri,type="response",se.fit=T)
licuri_ecoregion<-data.frame(p=licuri_ecoregion$fit,lower=licuri_ecoregion$fit-1.96*licuri_ecoregion$se.fit,upper=licuri_ecoregion$fit+1.96*licuri_ecoregion$se.fit)
licuri_ecoregion$ecoregion<-c("RASO","BDO")
licuri_ecoregion$occurrence<-factor(rep(c("No record","Core","Historically occupied","Recently occupied"),each=2))
licuri_ecoregion$occurrence<-factor(licuri_ecoregion$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))
licuri_ecoregion$presence<-factor(c("Absent","Absent","Present","Present","Absent","Absent","Present","Present"))
licuri_ecoregion

licuri_ecoregion2<-licuri_ecoregion[1:7,]

p6.licuri<-ggplot(data=licuri_ecoregion2)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=p, color=occurrence, shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(psi["Licuri palm"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6),
        legend.text = element_text(color = "white",size=6),
        legend.title = element_text(color = "white",size=6),
        legend.key = element_rect(fill = "white",size=6),
        legend.key.size=unit(0.25,"cm"))
p6.licuri



# Grid Licuri ####
grid_licuri<-grid.arrange(p1.licuri,p2.licuri,p3.licuri,p4.licuri,p5.licuri,p6.licuri,nrow=3,widths=c(1,1.35))

ggsave(filename="C:/Users/voeroesd/Dropbox/licuri_plots.pdf", 
       plot = grid_licuri, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/licuri_plots.pdf", 
#        plot = grid_licuri, 
#        width = 210, 
#        height = 140, 
#        units = "mm")

# Habitat model ####
surv_caa$offset<-scale(surv_caa$habfrag_length)[,1]
offset2<-cbind(surv_caa$offset,surv_caa$offset,surv_caa$offset,surv_caa$offset)

hab1<-multinom(habitat~
                 cattle+
                 goat+
                 licuri_presence+
                 fire+
                 ecoregion+
                 occurrence+
                 offset(offset2), data = surv_caa)
summary(hab1)




hab2<-multinom(habitat~
                 cattle+
                 goat+
                 licuri_presence+
                 fire+
                 ecoregion+
                 occurrence, data = surv_caa)
summary(hab2)

# inclusion of the offset does not affect estimates

#Preds.Fig Cattle ####
hab.cattle<-predicts(model=hab2,values="0-100;0;0;0;F(2);F",set.seed=1)

colnames(hab.cattle)[10]<-"habitat"
hab.cattle$habitat<-as.factor(hab.cattle$habitat)
levels(hab.cattle$habitat)[levels(hab.cattle$habitat)=="conserved"] <- "Conserved"
levels(hab.cattle$habitat)[levels(hab.cattle$habitat)=="degraded"] <- "Degraded"
levels(hab.cattle$habitat)[levels(hab.cattle$habitat)=="degraded_mixed_agriculture"] <- "Degraded mixed with agriculture"
levels(hab.cattle$habitat)[levels(hab.cattle$habitat)=="rural_settlement"] <- "Rural settlement"

hab.cattle$occurrence<-as.factor(hab.cattle$occurrence)
levels(hab.cattle$occurrence)[levels(hab.cattle$occurrence)=="0"] <- "No record"
levels(hab.cattle$occurrence)[levels(hab.cattle$occurrence)=="1"] <- "Core"
levels(hab.cattle$occurrence)[levels(hab.cattle$occurrence)=="2"] <- "Historically occupied"
levels(hab.cattle$occurrence)[levels(hab.cattle$occurrence)=="3"] <- "Recently occupied"
hab.cattle$occurrence<-factor(hab.cattle$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))


hab.cattle$presence<-rep(c("Absent","Present","Absent","Present"),each=404)

hab.degraded<-subset(hab.cattle,habitat=="Degraded")
hab.degraded.agri<-subset(hab.cattle,habitat=="Degraded mixed with agriculture")
hab.rural<-subset(hab.cattle,habitat=="Rural settlement")
hab.conserved<-subset(hab.cattle,habitat=="Conserved")

# 
# hab.norecord<-subset(hab.cattle,occurrence=="No record")
# hab.core<-subset(hab.cattle,occurrence=="Core")
# hab.historic<-subset(hab.cattle,occurrence=="Historically occupied")
# hab.recent<-subset(hab.cattle,occurrence=="Recently occupied")

p1.hab.degraded<-ggplot(data=hab.degraded)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=cattle, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(psi["degraded habitat"]), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p1.hab.degraded

p1.hab.degraded.agri<-ggplot(data=hab.degraded.agri)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=cattle, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Habitat conservation status")+
  scale_color_discrete(name= "Habitat conservation status")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),fill = guide_legend(order = 1))+
  labs(y=expression(psi["degraded habitat with cultivation"]), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p1.hab.degraded.agri

p1.hab.rural<-ggplot(data=hab.rural)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=cattle, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Habitat conservation status")+
  scale_color_discrete(name= "Habitat conservation status")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),fill = guide_legend(order = 1))+
  labs(y=expression(psi["rural settlement"]), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p1.hab.rural

p1.hab.conserved<-ggplot(data=hab.conserved)+
  geom_ribbon(mapping = aes(x=cattle, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=cattle, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Habitat conservation status")+
  scale_color_discrete(name= "Habitat conservation status")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1,override.aes = list(colour = "white",fill="white")),
         linetype = guide_legend(order = 1,override.aes = list(colour = "white")),
         fill = guide_legend(order = 1))+
  labs(y=expression(psi["conserved habitat"]), x="Cattle abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)


p1.hab.conserved

grid_hab_cattle<-grid.arrange(p1.hab.degraded,p1.hab.degraded.agri,p1.hab.rural,p1.hab.conserved,nrow=2,widths=c(1,1.45))

ggsave(filename="C:/Users/voeroesd/Dropbox/habitat_cattle.pdf", 
       plot = grid_hab_cattle, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/habitat_cattle.pdf", 
#        plot = grid_hab_cattle, 
#        width = 210, 
#        height = 140, 
#        units = "mm")

# Preds.Fig Goat####
hab.goat<-predicts(model=hab2,values="0;0-100;0;0;F(2);F",set.seed=1)

colnames(hab.goat)[10]<-"habitat"
hab.goat$habitat<-as.factor(hab.goat$habitat)
levels(hab.goat$habitat)[levels(hab.goat$habitat)=="conserved"] <- "Conserved"
levels(hab.goat$habitat)[levels(hab.goat$habitat)=="degraded"] <- "Degraded"
levels(hab.goat$habitat)[levels(hab.goat$habitat)=="degraded_mixed_agriculture"] <- "Degraded mixed with agriculture"
levels(hab.goat$habitat)[levels(hab.goat$habitat)=="rural_settlement"] <- "Rural settlement"

hab.goat$occurrence<-as.factor(hab.goat$occurrence)
levels(hab.goat$occurrence)[levels(hab.goat$occurrence)=="0"] <- "No record"
levels(hab.goat$occurrence)[levels(hab.goat$occurrence)=="1"] <- "Core"
levels(hab.goat$occurrence)[levels(hab.goat$occurrence)=="2"] <- "Historically occupied"
levels(hab.goat$occurrence)[levels(hab.goat$occurrence)=="3"] <- "Recently occupied"
hab.goat$occurrence<-factor(hab.goat$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))


hab.goat$presence<-rep(c("Absent","Present","Absent","Present"),each=404)

hab.degraded.2<-subset(hab.goat,habitat=="Degraded")
hab.degraded.agri.2<-subset(hab.goat,habitat=="Degraded mixed with agriculture")
hab.rural.2<-subset(hab.goat,habitat=="Rural settlement")
hab.conserved.2<-subset(hab.goat,habitat=="Conserved")
# 
# hab2.norecord<-subset(hab.goat,occurrence=="No record")
# hab2.core<-subset(hab.goat,occurrence=="Core")
# hab2.historic<-subset(hab.goat,occurrence=="Historically occupied")
# hab2.recent<-subset(hab.goat,occurrence=="Recently occupied")

p2.hab.degraded<-ggplot(data=hab.degraded.2)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=goat, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Lear's Macaw records")+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),shape = guide_colorbar(order=2),fill = guide_legend(order = 1))+
  labs(y=expression(psi["degraded habitat"]), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p2.hab.degraded

p2.hab.degraded.agri<-ggplot(data=hab.degraded.agri.2)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=goat, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Habitat conservation status")+
  scale_color_discrete(name= "Habitat conservation status")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),fill = guide_legend(order = 1))+
  labs(y=expression(psi["degraded habitat with cultivation"]), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p2.hab.degraded.agri

p2.hab.rural<-ggplot(data=hab.rural.2)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=goat, y=mean,color=occurrence,linetype=str_wrap(presence)),size=0.5)+
  scale_fill_discrete(name= "Habitat conservation status")+
  scale_color_discrete(name= "Habitat conservation status")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1),fill = guide_legend(order = 1))+
  labs(y=expression(psi["rural settlement"]), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p2.hab.rural

p2.hab.conserved<-ggplot(data=hab.conserved.2)+
  geom_ribbon(mapping = aes(x=goat, ymin=lower, ymax=upper,fill=occurrence),alpha=0.3)+
  geom_line(mapping = aes(x=goat, y=mean,color=occurrence,linetype=presence),size=0.5)+
  scale_fill_discrete(name= "Habitat conservation status")+
  scale_color_discrete(name= "Habitat conservation status")+
  scale_linetype_discrete(name= "Lear's Macaw occurrence")+
  guides(color = guide_legend(order = 1,override.aes = list(colour = "white",fill="white")),
         linetype = guide_legend(order = 1,override.aes = list(colour = "white")),
         fill = guide_legend(order = 1))+
  labs(y=expression(psi["conserved habitat"]), x="Goat abundance")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)


p2.hab.conserved

grid_hab_goat<-grid.arrange(p2.hab.degraded,p2.hab.degraded.agri,p2.hab.rural,p2.hab.conserved,nrow=2,widths=c(1,1.45))

ggsave(filename="C:/Users/voeroesd/Dropbox/habitat_goat.pdf", 
       plot = grid_hab_goat, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/habitat_goat.pdf", 
#        plot = grid_hab_goat, 
#        width = 210, 
#        height = 140, 
#        units = "mm")

# Preds.Fig Licuri ####
hab.licuri<-predicts(model=hab2,values="0;0;F;0;F(2);F",set.seed=1)


colnames(hab.licuri)[10]<-"habitat"
hab.licuri$habitat<-as.factor(hab.licuri$habitat)
levels(hab.licuri$habitat)[levels(hab.licuri$habitat)=="conserved"] <- "Conserved"
levels(hab.licuri$habitat)[levels(hab.licuri$habitat)=="degraded"] <- "Degraded"
levels(hab.licuri$habitat)[levels(hab.licuri$habitat)=="degraded_mixed_agriculture"] <- "Degraded mixed with agriculture"
levels(hab.licuri$habitat)[levels(hab.licuri$habitat)=="rural_settlement"] <- "Rural settlement"

hab.licuri$occurrence<-as.factor(hab.licuri$occurrence)
levels(hab.licuri$occurrence)[levels(hab.licuri$occurrence)=="0"] <- "No record"
levels(hab.licuri$occurrence)[levels(hab.licuri$occurrence)=="1"] <- "Core"
levels(hab.licuri$occurrence)[levels(hab.licuri$occurrence)=="2"] <- "Historically occupied"
levels(hab.licuri$occurrence)[levels(hab.licuri$occurrence)=="3"] <- "Recently occupied"
hab.licuri$occurrence<-factor(hab.licuri$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))


hab.licuri$presence<-rep(c("Absent","Present","Absent","Present"),each=8)

hab.licuri$licuri_presence<-as.factor(hab.licuri$licuri_presence)
levels(hab.licuri$licuri_presence)[levels(hab.licuri$licuri_presence)=="0"] <- "Licuri palm absent"
levels(hab.licuri$licuri_presence)[levels(hab.licuri$licuri_presence)=="1"] <- "Licuri palm present"

hab.degraded.3<-subset(hab.licuri,habitat=="Degraded")
hab.degraded.agri.3<-subset(hab.licuri,habitat=="Degraded mixed with agriculture")
hab.rural.3<-subset(hab.licuri,habitat=="Rural settlement")
hab.conserved.3<-subset(hab.licuri,habitat=="Conserved")
# hab3.norecord<-subset(hab.licuri,occurrence=="No record")
# hab3.core<-subset(hab.licuri,occurrence=="Core")
# hab3.historic<-subset(hab.licuri,occurrence=="Historically occupied")
# hab3.recent<-subset(hab.licuri,occurrence=="Recently occupied")


p3.hab.degraded<-ggplot(data=hab.degraded.3)+
  geom_errorbar(mapping = aes(x=licuri_presence, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=licuri_presence,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["degraded habitat"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p3.hab.degraded

p3.hab.degraded.agri<-ggplot(data=hab.degraded.agri.3)+
  geom_errorbar(mapping = aes(x=licuri_presence, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=licuri_presence,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["degraded habitat with cultivation"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p3.hab.degraded.agri

p3.hab.rural<-ggplot(data=hab.rural.3)+
  geom_errorbar(mapping = aes(x=licuri_presence, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=licuri_presence,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["rural settlement"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p3.hab.rural

p3.hab.conserved<-ggplot(data=hab.conserved.3)+
  geom_errorbar(mapping = aes(x=licuri_presence, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=licuri_presence,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(psi["conserved habitat"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p3.hab.conserved

grid_hab_licuri<-grid.arrange(p3.hab.degraded,p3.hab.degraded.agri,p3.hab.rural,p3.hab.conserved,nrow=2,widths=c(1,1.45))

ggsave(filename="C:/Users/voeroesd/Dropbox/habitat_licuri.pdf", 
       plot = grid_hab_licuri, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/habitat_licuri.pdf", 
#        plot = grid_hab_licuri, 
#        width = 210, 
#        height = 140, 
#        units = "mm")


# Preds.Fig fire ####
hab.fire<-predicts(model=hab2,values="0;0;0;F;F(2);F",set.seed=1)


colnames(hab.fire)[10]<-"habitat"
hab.fire$habitat<-as.factor(hab.fire$habitat)
levels(hab.fire$habitat)[levels(hab.fire$habitat)=="conserved"] <- "Conserved"
levels(hab.fire$habitat)[levels(hab.fire$habitat)=="degraded"] <- "Degraded"
levels(hab.fire$habitat)[levels(hab.fire$habitat)=="degraded_mixed_agriculture"] <- "Degraded mixed with agriculture"
levels(hab.fire$habitat)[levels(hab.fire$habitat)=="rural_settlement"] <- "Rural settlement"

hab.fire$occurrence<-as.factor(hab.fire$occurrence)
levels(hab.fire$occurrence)[levels(hab.fire$occurrence)=="0"] <- "No record"
levels(hab.fire$occurrence)[levels(hab.fire$occurrence)=="1"] <- "Core"
levels(hab.fire$occurrence)[levels(hab.fire$occurrence)=="2"] <- "Historically occupied"
levels(hab.fire$occurrence)[levels(hab.fire$occurrence)=="3"] <- "Recently occupied"
hab.fire$occurrence<-factor(hab.fire$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))


hab.fire$presence<-rep(c("Absent","Present","Absent","Present"),each=8)

hab.fire$fire<-as.factor(hab.fire$fire)
levels(hab.fire$fire)[levels(hab.fire$fire)=="0"] <- "No fire"
levels(hab.fire$fire)[levels(hab.fire$fire)=="fire"] <- "Fire"

hab.degraded.4<-subset(hab.fire,habitat=="Degraded")
hab.degraded.agri.4<-subset(hab.fire,habitat=="Degraded mixed with agriculture")
hab.rural.4<-subset(hab.fire,habitat=="Rural settlement")
hab.conserved.4<-subset(hab.fire,habitat=="Conserved")

# hab4.norecord<-subset(hab.fire,occurrence=="No record")
# hab4.core<-subset(hab.fire,occurrence=="Core")
# hab4.historic<-subset(hab.fire,occurrence=="Historically occupied")
# hab4.recent<-subset(hab.fire,occurrence=="Recently occupied")


p4.hab.degraded<-ggplot(data=hab.degraded.4)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["degraded habitat"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p4.hab.degraded

p4.hab.degraded.agri<-ggplot(data=hab.degraded.agri.4)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["degraded habitat with cultivation"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p4.hab.degraded.agri

p4.hab.rural<-ggplot(data=hab.rural.4)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["rural settlement"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p4.hab.rural

p4.hab.conserved<-ggplot(data=hab.conserved.4)+
  geom_errorbar(mapping = aes(x=fire, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=fire,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(psi["conserved habitat"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p4.hab.conserved

grid_hab_fire<-grid.arrange(p4.hab.degraded,p4.hab.degraded.agri,p4.hab.rural,p4.hab.conserved,nrow=2,widths=c(1,1.45))

ggsave(filename="C:/Users/voeroesd/Dropbox/habitat_fire.pdf", 
       plot = grid_hab_fire, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/habitat_fire.pdf", 
#        plot = grid_hab_fire, 
#        width = 210, 
#        height = 140, 
#        units = "mm")

# Preds.Fig ecoregion ####
hab.ecoregion<-predicts(model=hab2,values="0;0;0;0;F;F",set.seed=1)

colnames(hab.ecoregion)[10]<-"habitat"
hab.ecoregion$habitat<-as.factor(hab.ecoregion$habitat)
levels(hab.ecoregion$habitat)[levels(hab.ecoregion$habitat)=="conserved"] <- "Conserved"
levels(hab.ecoregion$habitat)[levels(hab.ecoregion$habitat)=="degraded"] <- "Degraded"
levels(hab.ecoregion$habitat)[levels(hab.ecoregion$habitat)=="degraded_mixed_agriculture"] <- "Degraded mixed with agriculture"
levels(hab.ecoregion$habitat)[levels(hab.ecoregion$habitat)=="rural_settlement"] <- "Rural settlement"

hab.ecoregion$occurrence<-as.factor(hab.ecoregion$occurrence)
levels(hab.ecoregion$occurrence)[levels(hab.ecoregion$occurrence)=="0"] <- "No record"
levels(hab.ecoregion$occurrence)[levels(hab.ecoregion$occurrence)=="1"] <- "Core"
levels(hab.ecoregion$occurrence)[levels(hab.ecoregion$occurrence)=="2"] <- "Historically occupied"
levels(hab.ecoregion$occurrence)[levels(hab.ecoregion$occurrence)=="3"] <- "Recently occupied"
hab.ecoregion$occurrence<-factor(hab.ecoregion$occurrence, levels = c("No record","Historically occupied","Recently occupied","Core"))

hab.ecoregion$presence<-rep(c("Absent","Present","Absent","Present"),each=8)

hab.ecoregion<-hab.ecoregion[-c(25:28),]

hab.degraded.5<-subset(hab.ecoregion,habitat=="Degraded")
hab.degraded.agri.5<-subset(hab.ecoregion,habitat=="Degraded mixed with agriculture")
hab.rural.5<-subset(hab.ecoregion,habitat=="Rural settlement")
hab.conserved.5<-subset(hab.ecoregion,habitat=="Conserved")


# hab5.norecord<-subset(hab.ecoregion,occurrence=="No record")
# hab5.core<-subset(hab.ecoregion,occurrence=="Core")
# hab5.historic<-subset(hab.ecoregion,occurrence=="Historically occupied")
# hab5.recent<-subset(hab.ecoregion,occurrence=="Recently occupied")

p5.hab.degraded<-ggplot(data=hab.degraded.5)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["degraded habitat"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p5.hab.degraded

p5.hab.degraded.agri<-ggplot(data=hab.degraded.agri.5)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["degraded habitat with cultivation"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p5.hab.degraded.agri

p5.hab.rural<-ggplot(data=hab.rural.5)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records")+
  scale_shape_discrete(name= "Lear's Macaw occurrence")+
  labs(y=expression(psi["rural settlement"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.position = "none")+
  ylim(0,1)
p5.hab.rural

p5.hab.conserved<-ggplot(data=hab.conserved.5)+
  geom_errorbar(mapping = aes(x=ecoregion, ymin=lower, ymax=upper,color=occurrence),position=position_dodge(width=0.5),width=0.05,size=0.5)+
  geom_point( mapping=aes(x=ecoregion,y=mean, color=occurrence,shape=presence), position=position_dodge(width=0.5),size=2)+
  scale_color_discrete(name= "Lear's Macaw records",guide = guide_legend(override.aes = list(color = "white")))+
  scale_shape_discrete(name= "Lear's Macaw occurrence",guide = guide_legend(override.aes = list(color = "white")))+
  labs(y=expression(psi["conserved habitat"]))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,angle = 90,vjust = 0.5),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.text = element_text(color = "white",size=8),
        legend.title = element_text(color = "white",size=8),
        legend.key = element_rect(fill = "white",size=8),
        legend.key.size=unit(0.30,"cm"))+
  ylim(0,1)
p5.hab.conserved

grid_hab_ecoregion<-grid.arrange(p5.hab.degraded,p5.hab.degraded.agri,p5.hab.rural,p5.hab.conserved,nrow=2,widths=c(1,1.45))

ggsave(filename="C:/Users/voeroesd/Dropbox/habitat_ecoregion.pdf", 
       plot = grid_hab_ecoregion, 
       width = 210, 
       height = 140, 
       units = "mm")

# ggsave(filename="~/Dropbox/habitat_ecoregion.pdf", 
#        plot = grid_hab_ecoregion, 
#        width = 210, 
#        height = 140, 
#        units = "mm")
