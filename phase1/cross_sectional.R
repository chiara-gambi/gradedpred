pred<-read.table("prediction_indeces.txt",header=T)
names(pred)[3]<-"Pred"
unpred<-read.table("prediction_indeces_mildy-unpred.txt",header=T)
names(unpred)[3]<-"UnPred"
speed<-read.table("recognition_indeces.txt",header=T,sep=";")
names(speed)[2]<-"Speed"
reccost<-read.table("recognition_cost_indeces.txt",header=T,sep=";")
names(reccost)[2]<-"RCost"
recbenefit<-read.table("recognition_benefit_indeces.txt",header=T,sep=";")
names(recbenefit)[2]<-"RBenefit"

indeces<-merge(pred,unpred,by=c("PartCode","Participant","Age","BPVS"),sort=F)
indeces<-merge(indeces,speed,by=c("PartCode","Participant","Age","BPVS"),sort=F)
indeces<-merge(indeces,reccost,by=c("PartCode","Participant","Age","BPVS"),sort=F)
indeces<-merge(indeces,recbenefit,by=c("PartCode","Participant","Age","BPVS"),sort=F)

head(indeces)
library(ggplot2)
library(GGally)
scatter<-ggpairs(indeces[c(3:4,6,8:11)])
ggsave("cross-sectional.png",plot=scatter,width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

indeces$PredC<-indeces$Pred-indeces$UnPred
scatter<-ggpairs(indeces[c(3:4,9:12)])
ggsave("cross-sectional_comb.png",plot=scatter,width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#controlling for age
omnibus_reg<-lm(BPVS~PredC+Speed+RCost+RBenefit+Age,data=indeces)
summary(omnibus_reg)

#Age interaction
omnibus_reg<-lm(BPVS~(PredC+Speed+RCost+RBenefit)*scale(Age,T,F),data=indeces)
summary(omnibus_reg)

#Age interaction median split
ma1<-median(indeces$Age)
indeces$AgeM<-ifelse(indeces$Age<=ma1,-.5,.5)

omnibus_reg<-lm(BPVS~(PredC+Speed+RCost+RBenefit)*scale(AgeM,T,F),data=indeces)
summary(omnibus_reg)#conistent with longitudinal analyese, Speed comes out as the only sifnificant one in interaction with Age
#marginal interaction with prediction composite measure and age

#no control for Age
omnibus_reg<-lm(BPVS~PredC+Speed+RCost+RBenefit,data=indeces)
summary(omnibus_reg)

omnibus_reg<-lm(Age~PredC+Speed+RCost+RBenefit,data=indeces)
summary(omnibus_reg)

#only younger kids
omnibus_reg<-lm(BPVS~(PredC+Speed+RCost+RBenefit),data=subset(indeces,AgeM==-.5))
summary(omnibus_reg)

#only older kids
#only younger kids
omnibus_reg<-lm(BPVS~(PredC+Speed+RCost+RBenefit),data=subset(indeces,AgeM==.5))
summary(omnibus_reg)
