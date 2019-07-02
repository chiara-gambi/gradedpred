# Longitudinal analyses of Vocabulary Growth
# Load all the eye-tracking indeces
setwd("~/OneDrive - Cardiff University/ED/gradedpred/data_backup_310818/Retest/longitudinal/an2018")
pred<-read.table("prediction_indeces.txt",header=T)
names(pred)[3]<-"Pred"
# This is the intercept contrast Pred-Mildly Pred (prediction window, combined analyses as reported in Figure 2 and Table 2 in the main paper)

unpred<-read.table("prediction_indeces_mildy-unpred.txt",header=T)
names(unpred)[3]<-"UnPred"
# This is the intercept contrast Pred-Mildly Pred (prediction window, combined analyses as reported in Figure 2 and Table 2 in the main paper)


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
### now replace the old indeces in all_test1_indeces.csv with the new ones from indeces
### in this file, the ETCode for part 215 was down as P215Task1, but in fact in the ET file it is P215Task2
### This has now been corrected

data_all<-read.csv("all_test1_indeces.csv",header=T,sep=";")# old indeces
#retest columns tells you which participants were retested
# the mistake has been conrrected so it should all be fine (99 was tested, not 89)
#discard old indeces
head(data_all)
data_all<-data_all[,c(1:16,21)]
# add new indeces by merginf data_all with indeces
names(data_all)[14]<-"Participant"

all<-merge(data_all,indeces,by=c("Participant","Age","BPVS"))

head(all)

all$PrecC<-all$Pred -all$UnPred
corr_test1<-ggpairs(all,columns=c("PrecC","Pred","Speed","UnPred","Age","BPVS"), lower = list(continuous = my_fn))
ggsave("Corr_test1.png", plot=corr_test1, width=30, height=15, unit="cm", dpi=300, path=getwd())


# now mergee with the dataset long_data_final.csv using Numb in all and ParticipantNumber in data
data<-read.csv("long_data_final.csv",header=T)
# Note that it contains 56 rows, but eye-tracking data from one participant are missing, so in effect it is 55 participants
data55<-data[is.na(data$Rec)==F,]

head(data)
data<-data[,c(1,4:19,22:24)]
head(all)
names(all)[4]<-"ParticipantNumber"
all<-all[,c(1,4,20,22:25)]
head(all)

data.wi<-merge(data,all,by=c("ParticipantNumber"))
head(data.wi)

# Note that there is one outlier with very large percentage linguistic change (>200)
# remove it now
data.wi<-data.wi[data.wi$LanCh<200,]
library(GGally)
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    #geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
corr_test2<-ggpairs(data.wi,columns=c("PrecC","Pred","Speed","UnPred","Ageatfirsttest","RawBPVSScore1","SIMD16Vigintile"), lower = list(continuous = my_fn))
ggsave("Corr_test2.png", plot=corr_test2, width=30, height=15, unit="cm", dpi=300, path=getwd())


# combined prediction score
# with median split of age
ma1<-median(data.wi$Ageatfirsttest)
data.wi$Age<-ifelse(data.wi$Ageatfirsttest<=ma1,-.5,.5)
data.wi$PrecC<-data.wi$Pred -data.wi$UnPred
mC<-lm(LanCh~1+(PrecC+RCost+Speed+RBenefit)*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mC)

mC.1<-lm(LanCh~1+PrecC+RCost+Speed+RBenefit+RawBPVSScore1,data=data.wi)
summary(mC.1)

mC.B<-lm(LanCh~1+RBenefit*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mC.B)

mC.S<-lm(LanCh~1+Speed*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mC.S)

mC.C<-lm(LanCh~1+RCost*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mC.C)

mC.P<-lm(LanCh~1+PrecC*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mC.P)

mC.all<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1+Ageatfirsttest+SIMD16Vigintile,data=data.wi)
summary(mC.all)

mc.res<-lm(RawBPVSScore1~1+Ageatfirsttest+SIMD16Vigintile,data=data.wi)
data.wi$VocRes<-residuals(mc.res)
mC.all<-lm(LanCh~1+PrecC+Speed+residuals(mc.res),data=data.wi)
summary(mC.all)

mC.all<-lm(LanCh~1+UnPred+Speed+residuals(mc.res),data=data.wi)
summary(mC.all)

mC.all.y<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1+Ageatfirsttest+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
summary(mC.all.y)

mC.all.y<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC.all.y)

mC.all.y<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
summary(mC.all.y)

# mc.res<-lm(RawBPVSScore1~1+Ageatfirsttest+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
#mC.all.y<-lm(LanCh~1+PrecC+Speed+residuals(mc.res),data=subset(data.wi,AgeCat=="Younger"))
#only combined prediction scores
mC2<-lm(LanCh~1+(PrecC)*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mC2)

#only combined prediction scores, without controlling for voc
mC2<-lm(LanCh~1+(PrecC)*scale(Age,T,F),data=datat)
summary(mC2)

#only combined prediction scores, without controlling for voc
mC2<-lm(LanCh~1+(PrecC)*Age,data=datat)
summary(mC2)

data.wi$AgeCat<-"Younger"
data.wi$AgeCat[data.wi$Age==.5]<-"Older"
CompP_medianAge<-ggplot(data.wi,aes(PrecC,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth (%")+xlab("Composite Prediction Index")
ggsave("composite_pred_index_median_age_split.png", plot=CompP_medianAge, width=30, height=15, unit="cm", dpi=300, path=getwd())

Speed_medianAge<-ggplot(data.wi,aes(Speed,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth (%")+xlab("Recognition Speed Index")
ggsave("speed_index_median_age_split.png", plot=Speed_medianAge, width=30, height=15, unit="cm", dpi=300, path=getwd())


# only younger kids
mC2<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC2)

# only younger kids, without recognition (prediction composite)
mC2<-lm(LanCh~1+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC2)

# only younger kids, without recognition (prediction)
mC2<-lm(LanCh~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC2)

# only younger kids, without prediction composite
mC2<-lm(LanCh~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC2)

# only younger kids, prediction composite predicting speed
mC2<-lm(Speed~1+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC2)

# only younger kids, prediction predicting speed
mC2<-lm(Speed~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
summary(mC2)

# only older kids, without recognition
mC3<-lm(LanCh~1+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Older"))
summary(mC3)

# only older kids, without prediction
mC3<-lm(LanCh~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Older"))
summary(mC3)


##### Replicate mediation analysis run in SPSS with Process on Younger kids (see mediation_composite_prediction.spv); controlling only for voc at test 1
library(mediation)

### Replicate mediation analyses with incorrect indeces (Younger.sav)
inc<-read.csv("Younger.csv",header=T)
# First  try with the composite prediction index
med.fit.inc<-lm(Rec~1+CompPred+BPVS1,data=inc)
out.fit.inc<-lm(LanCh~1+Rec+CompPred+BPVS1,data=inc)
set.seed(15)
med.out.inc<-mediate(med.fit.inc,out.fit.inc,treat="CompPred",mediator="Rec",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out.inc)

# Then with the basic Pred index
med.fit.inc2<-lm(Rec~1+Pred+BPVS1,data=inc)
out.fit.inc2<-lm(LanCh~1+Rec+Pred+BPVS1,data=inc)
set.seed(15)
med.out.inc2<-mediate(med.fit.inc2,out.fit.inc2,treat="Pred",mediator="Rec",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out.inc2) ## This replicates the analysis in "mediation_composite_prediction.spv", which shows that the mediation analysis did not use the composite prediction index but just the basic Pred index

# Below is using the correct indeces, with all the controls (age at test 1 ans SES in addition to voc at test 1)
## PrecC as the mediator
med.fit<-lm(PrecC~1+Speed+RawBPVSScore1+Ageatfirsttest+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
out.fit<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1+Ageatfirsttest+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out<-mediate(med.fit,out.fit,treat="Speed",mediator="PrecC",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out)
## Speed as the mediator
med.fit2<-lm(Speed~1+PrecC+RawBPVSScore1+Ageatfirsttest+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
out.fit2<-lm(LanCh~1+Speed+PrecC+RawBPVSScore1+Ageatfirsttest+SIMD16Vigintile,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out2<-mediate(med.fit2,out.fit2,treat="PrecC",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out2)

# Below is using the correct indeces, with no controls
## PrecC as the mediator
med.fit3<-lm(PrecC~1+Speed,data=subset(data.wi,AgeCat=="Younger"))
out.fit3<-lm(LanCh~1+PrecC+Speed,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out3<-mediate(med.fit3,out.fit3,treat="Speed",mediator="PrecC",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out3)
## Speed as the mediator
med.fit4<-lm(Speed~1+PrecC,data=subset(data.wi,AgeCat=="Younger"))
out.fit4<-lm(LanCh~1+Speed+PrecC,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out4<-mediate(med.fit4,out.fit4,treat="PrecC",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out4)

# Below is using the correct indeces, with only voc at test 1 as control
## PrecC as the mediator
med.fit5<-lm(PrecC~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit5<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out5<-mediate(med.fit5,out.fit5,treat="Speed",mediator="PrecC",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5)
## Speed as the mediator
med.fit6<-lm(Speed~1+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit6<-lm(LanCh~1+Speed+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out6<-mediate(med.fit6,out.fit6,treat="PrecC",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6)

# Below is using the correct indeces, with only voc at test 1 as control, but using Prediction instead of the prediction composite
## Pred as the mediator
med.fit5<-lm(Pred~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit5<-lm(LanCh~1+Pred+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out5<-mediate(med.fit5,out.fit5,treat="Speed",mediator="Pred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5)
## Speed as the mediator
med.fit6<-lm(Speed~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit6<-lm(LanCh~1+Speed+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out6<-mediate(med.fit6,out.fit6,treat="Pred",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6)
summary(lm(LanCh~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
summary(lm(Speed~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
summary(lm(LanCh~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
# Below is using the correct indeces, with only voc at test 1 as control, but using UnPred instead of the prediction composite
## Pred as the mediator
med.fit5<-lm(UnPred~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit5<-lm(LanCh~1+UnPred+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out5<-mediate(med.fit5,out.fit5,treat="Speed",mediator="UnPred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5)
## Speed as the mediator
med.fit6<-lm(Speed~1+UnPred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit6<-lm(LanCh~1+Speed+UnPred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out6<-mediate(med.fit6,out.fit6,treat="UnPred",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6)
summary(lm(Speed~1+UnPred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
# Below is using the correct indeces, with VocRes
## PrecC as the mediator
med.fit5<-lm(PrecC~1+Speed+VocRes,data=subset(data.wi,AgeCat=="Younger"))
out.fit5<-lm(LanCh~1+PrecC+Speed+VocRes,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out5<-mediate(med.fit5,out.fit5,treat="Speed",mediator="PrecC",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5)
## Speed as the mediator
med.fit6<-lm(Speed~1+PrecC+VocRes,data=subset(data.wi,AgeCat=="Younger"))
out.fit6<-lm(LanCh~1+Speed+PrecC+VocRes,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out6<-mediate(med.fit6,out.fit6,treat="PrecC",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6)

# Below is using the correct indeces, with VocRes, but using simple Prediction index
## Pred as the mediator
med.fit5<-lm(Pred~1+Speed+VocRes,data=subset(data.wi,AgeCat=="Younger"))
out.fit5<-lm(LanCh~1+Pred+Speed+VocRes,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out5<-mediate(med.fit5,out.fit5,treat="Speed",mediator="Pred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5)
## Speed as the mediator
med.fit6<-lm(Speed~1+Pred+VocRes,data=subset(data.wi,AgeCat=="Younger"))
out.fit6<-lm(LanCh~1+Speed+Pred+VocRes,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out6<-mediate(med.fit6,out.fit6,treat="Pred",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6)

# Below is using the correct indeces, with VocRes, but using simple Unpred index
## UnPred as the mediator
med.fit5<-lm(UnPred~1+Speed+VocRes,data=subset(data.wi,AgeCat=="Younger"))
out.fit5<-lm(LanCh~1+UnPred+Speed+VocRes,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out5<-mediate(med.fit5,out.fit5,treat="Speed",mediator="UnPred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5)
## Speed as the mediator
med.fit6<-lm(Speed~1+UnPred+VocRes,data=subset(data.wi,AgeCat=="Younger"))
out.fit6<-lm(LanCh~1+Speed+UnPred+VocRes,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out6<-mediate(med.fit6,out.fit6,treat="UnPred",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6)

#################### OLD
#############################################################################
library(doBy)
summaryBy(Ageatfirsttest+Ageatsecondtest~1,data=data55, FUN=c(mean,range))
data55$count<-1
summaryBy(count~Gender,data=data55, FUN=sum)

# SES distribution (Vigintile)
library(ggplot2)
ggplot(data,aes(SIMD16Vigintile))+geom_histogram(binwidth=0.5)+ggtitle("Distribution of SES")+xlab("Scottish Index of Multiple Deprivation")+ylim(c(0,13))
#by age
data$age_brks<- cut(data$Ageatfirsttest, c(seq(24, 72, 12), max(data$Ageatfirsttest)))
ggplot(data=data, aes(x = SIMD16Vigintile, fill=age_brks)) + geom_histogram(binwidth=0.5) + scale_fill_brewer(palette="RdYlBu")+ggtitle("Distribution of SES by Age")+xlab("Scottish Index of Multiple Deprivation")+ylim(c(0,13))
library(GGally)
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    #geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

# matrix correlation plot amongst prediction and recognition score
ggpairs(data,columns=c("Pred","Rec","Unpred","RecCost"), lower = list(continuous = my_fn))

# Note that there is one outlier with very large percentage linguistic change (>200)
# remove it now
datat<-data[data$LanCh<200,]

#now get correlations between LanCh and all eye-track variables
library(psych)
corr.test(datat[,c("Unpred","RecCost","Pred","Rec","LanCh")])
corr.test(data[,c("Unpred","RecCost","Pred","Rec","LanCh")])

# multiple regression with lanCh as DV and Unpred, RecCost and Voc at time 1 as IV
m<-lm(LanCh~1+Unpred+RecCost+RawBPVSScore1,data=datat)
summary(m)
m.1<-lm(LanCh~1+RecCost+RawBPVSScore1,data=datat)
summary(m.1)

m2<-lm(LanCh~1+Unpred+RecCost+RawBPVSScore1,data=data)
summary(m2)
m2.1<-lm(LanCh~1+RecCost+RawBPVSScore1,data=data)
summary(m2.1)

# RecCost is a significant predicotr of %linguistchange when including the outlier
# not otherwise
m3<-lm(LanCh~1+Unpred+RecCost+Pred+Rec+RawBPVSScore1,data=datat)
summary(m3)

m3.1<-lm(LanCh~1+Unpred+RecCost+Pred+Rec+RawBPVSScore1,data=data)
summary(m3.1)

# median split?
ma1<-median(datat$Ageatfirsttest)
datat$Age<-ifelse(datat$Ageatfirsttest<=ma1,-.5,.5)
m4<-lm(LanCh~1+(Unpred+RecCost+Pred+Rec)*scale(Ageatfirsttest,T,T),data=datat)
summary(m4)
m5<-lm(LanCh~1+(Unpred+RecCost+Pred+Rec)*Age+RawBPVSScore1,data=datat)
summary(m5)

mv1<-median(datat$RawBPVSScore1)
datat$Voc<-ifelse(datat$Ageatfirsttest<=mv1,-.5,.5)
m6<-lm(LanCh~1+(Unpred+RecCost+Pred+Rec)*Voc,data=datat)
summary(m6)

#this is data from test 1, including all the eye-tracking indeces for 215 kids
all_data<-read.csv("all_test1_indeces.csv",header=T,sep=";")
summary(all_data)
# remove DISCARD
all_data<-all_data[all_data$AGEYEAR!="DISCARD",]#216
# remove additional NA for Prediction Index
all_data<-all_data[is.na(all_data$PredictionIndex)==F,]#215
summary(all_data)
# there is still an NA in RecCostIndex

# now plot distribution of Age and BPVS as a function of whether retested or not
all_data$Test<-all_data$retest
all_data$Test<-factor(all_data$Test,labels=c("Test 1","Test 2"))
age_distr<-ggplot(data=all_data, aes(x = Age, fill=Test)) + geom_histogram(binwidth=0.5) + scale_fill_brewer(palette="RdYlBu")+ggtitle("Distribution of Age at Test 1 and Test 2")+xlab("Age in months")
ggsave("Age_distr_test_retest.png", plot=age_distr, width=30, height=15, unit="cm", dpi=300, path=getwd())

voc_distr<-ggplot(data=all_data, aes(x = BPVS, fill=Test)) + geom_histogram(binwidth=1) + scale_fill_brewer(palette="RdYlBu")+ggtitle("Distribution of Vocabulary at Test 1 and Test 2")+xlab("BPVS raw score")
ggsave("Voc_distr_test_retest.png", plot=voc_distr, width=30, height=15, unit="cm", dpi=300, path=getwd())

library(GGally)
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    #geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

names(all_data)[17:20]<-c("PHM","RS","PMU","RC")
# matrix correlation plot amongst prediction and recognition score
ggpairs(all_data,columns=c("PHM","PMU","RS","RC"), lower = list(continuous = my_fn))
corr.test(all_data[,c("PHM","RS","PMU","RC")])


# reported regression
# controlling for voc at test 1
m3<-lm(LanCh~1+Unpred+RecCost+Pred+Rec+RawBPVSScore1,data=datat)
summary(m3)

m3.noRC<-lm(LanCh~1+Unpred+Pred+Rec+RawBPVSScore1,data=datat)
summary(m3.noRC)

m3.noUnpred<-lm(LanCh~1+RecCost+Pred+Rec+RawBPVSScore1,data=datat)
summary(m3.noUnpred)

# individually
m3.Unpred<-lm(LanCh~1+Unpred+RawBPVSScore1,data=datat)
summary(m3.Unpred)

m3.Cost<-lm(LanCh~1+RecCost+RawBPVSScore1,data=datat)
summary(m3.Cost)

m3.Pred<-lm(LanCh~1+Pred+RawBPVSScore1,data=datat)
summary(m3.Pred)

m3.Rec<-lm(LanCh~1+Rec+RawBPVSScore1,data=datat)
summary(m3.Rec)

# with median split of age
ma1<-median(datat$Ageatfirsttest)
datat$Age<-ifelse(datat$Ageatfirsttest<=ma1,-.5,.5)
m5<-lm(LanCh~1+(Unpred+RecCost+Pred+Rec)*scale(Age,T,F)+RawBPVSScore1,data=datat)
summary(m5)

summary(datat)
datat$AgeCat<-"Younger"
datat$AgeCat[datat$Age==.5]<-"Older"
ggplot(datat,aes(Rec,LanCh))+geom_point()+geom_smooth(method="lm")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth")+xlab("Recognition Speed")

m5.Y<-lm(LanCh~1+(Unpred+RecCost+Pred+Rec)+RawBPVSScore1,data=subset(datat,AgeCat=="Younger"))
summary(m5.Y)

m5.O<-lm(LanCh~1+(Unpred+RecCost+Pred+Rec)+RawBPVSScore1,data=subset(datat,AgeCat=="Older"))
summary(m5.O)

# Fernald style analysis
# Raw BPVS at time 2 predicted by Raw BPVS at time 1, controlling for Age at time 1, and with all the indeces
mF<-lm(RawBPVSScore2~1+Unpred+RecCost+Pred+Rec+scale(RawBPVSScore1,T,F) + scale(Ageatfirsttest,T,F),data=datat)
summary(mF)

# raw BPVS at time 2 predicted by indeces in interaction with BPVS at time 1

mF1<-lm(RawBPVSScore2~1+(Unpred+RecCost+Pred+Rec)*scale(RawBPVSScore1,T,F)+Ageatfirsttest,data=datat)
summary(mF1)

#only prediction strength and recognition
mF2<-lm(RawBPVSScore2~1+(Pred+Rec+RawBPVSScore1)*scale(Ageatfirsttest,T,F),data=datat)
summary(mF2)

#only graded prediction and recognition cost
mF3<-lm(RawBPVSScore2~1+(Unpred+RecCost+RawBPVSScore1)*scale(Ageatfirsttest,T,F),data=datat)
summary(mF3)

#interactions with cvocabulary, controlling for age
#only prediction strength and recognition
mF4<-lm(RawBPVSScore2~1+(Pred+Rec)*scale(RawBPVSScore1,T,F)+Ageatfirsttest,data=datat)
summary(mF4)

#only graded prediction and recognition cost
mF5<-lm(RawBPVSScore2~1+(Unpred+RecCost)*scale(RawBPVSScore1,T,F)+Ageatfirsttest,data=datat)
summary(mF5)


# combined prediction score
# with median split of age
ma1<-median(datat$Ageatfirsttest)
datat$Age<-ifelse(datat$Ageatfirsttest<=ma1,-.5,.5)
datat$PrecC<-datat$Pred -datat$Unpred
mC<-lm(LanCh~1+(PrecC+RecCost+Rec)*scale(Age,T,F)+RawBPVSScore1,data=datat)
summary(mC)

#only combined prediction scores
mC2<-lm(LanCh~1+(PrecC)*scale(Age,T,F)+RawBPVSScore1,data=datat)
summary(mC2)

#only combined prediction scores, without controlling for voc
mC2<-lm(LanCh~1+(PrecC)*scale(Age,T,F),data=datat)
summary(mC2)

#only combined prediction scores, without controlling for voc
mC2<-lm(LanCh~1+(PrecC)*Age,data=datat)
summary(mC2)

summary(datat)
datat$AgeCat<-"Younger"
datat$AgeCat[datat$Age==.5]<-"Older"
ggplot(datat,aes(PrecC,LanCh))+geom_point()+geom_smooth(method="lm")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth")+xlab("Composite Prediction Score")


# only younger kids
mC2<-lm(LanCh~1+PrecC+Rec+RawBPVSScore1,data=subset(datat,AgeCat=="Younger"))
summary(mC2)

# only younger kids, without recognition
mC2<-lm(LanCh~1+PrecC+RawBPVSScore1,data=subset(datat,AgeCat=="Younger"))
summary(mC2)

# only older kids, without recognition
mC3<-lm(LanCh~1+PrecC+RawBPVSScore1,data=subset(datat,AgeCat=="Older"))
summary(mC3)

