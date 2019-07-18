# Longitudinal analyses of Vocabulary Growth
# Load packages and prepare for graphs
library(ggplot2)
library(GGally)
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    #geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
library(mediation)

#1. Repeat the analyses (Vocabulary Growth) with the new indeces
# For prediction
# A. raw difference scores in the short prediction window (average)
# B. Elog difference scores in the short prediction window (avearge)
# C. Growth Curber Model predicted  values in the short prediction window (average)
# For recognition speed
# D. raw mean first fix in neutral trials
# E. Lmer model estimates (fixef + ranef) of first fix in neutral trials

#2. Predict raw Vocabulary at time 2 based on the indeces, while controlling for Age and Voc at time 1

# Load all the eye-tracking indeces
setwd("~/OneDrive - Cardiff University/ED/gradedpred/data_backup_310818/Retest/longitudinal/an2018")
pred<-read.table("prediction_indeces.txt",header=T)
names(pred)[3]<-"Pred"
pred<-pred[,-2]
# This is the intercept contrast Pred-Mildly Pred (prediction window, combined analyses as reported in Figure 2 and Table 2 in the main paper)

unpred<-read.table("prediction_indeces_mildy-unpred.txt",header=T)
names(unpred)[3]<-"UnPred"
unpred<-unpred[,-2]
# This is the intercept contrast Unpred-Mildly Pred (prediction window, combined analyses as reported in Figure 2 and Table 2 in the main paper)

speed<-read.table("recognition_indeces.txt",header=T,sep=";")
names(speed)[2]<-"Speed"
# This is the recognition speed index, as defined in the main paper (see Longitudinal analyses)

raw<-read.table("rawshort_predindeces.txt",header=T)

elog<-read.table("elogshort_predindeces.txt",header=T)

model<-read.table("predshort_predindeces.txt",header=T)
names(model)[2:3]<-c("modelPred","modelUnpred")

tff_raw<-read.table("raw_tff_recspeedindeces.txt",header=T)
names(tff_raw)[4]<-"tff_raw"

tff_model<-read.table("model_tff_recspeedindeces.txt",header=T)
names(tff_model)[2]<-"tff_model"

# Merge all the indeces together
indeces<-merge(pred,unpred,by=c("PartCode","Participant","Age","BPVS"),sort=F)
indeces<-merge(indeces,speed,by=c("PartCode","Participant","Age","BPVS"),sort=F)
indeces<-merge(indeces,raw,by=c("Participant"),sort=F)
indeces<-merge(indeces,elog,by=c("Participant"),sort=F)
indeces<-merge(indeces,model,by=c("Participant"),sort=F)
indeces<-merge(indeces,tff_raw,by=c("Participant","Age","BPVS"),sort=F)
indeces<-merge(indeces,tff_model,by=c("Participant"),sort=F)

head(indeces)

#Load participant info, including which children were retested 

part_info<-read.csv("part_info.csv",header=T,sep=",")
# add new indeces by merging part_info with indeces
names(part_info)[14]<-"Participant"

all<-merge(part_info,indeces,by=c("Participant","Age","BPVS"))

# Composite prediction index
all$PrecC<-all$Pred -all$UnPred
all$PrecC_raw<-all$rawPred -all$rawUnpred
all$PrecC_elog<-all$elogPred -all$elogUnpred
all$PrecC_model<-all$modelPred -all$modelUnpred

# Correlations between original indeces, Age and Vocabulary at first test
graph<-all
names(graph)[19]<-"Pos Pred"
names(graph)[20]<-"Neg Pred"
names(graph)[21]<-"Recognition Speed"
names(graph)[30]<-"Graded Pred"
corr_test1<-ggpairs(graph,columns=c("Graded Pred","Pos Pred","Neg Pred","Recognition Speed","Age","BPVS"), lower = list(continuous = my_fn))
print(corr_test1)
ggsave("Corr_test1.png", plot=corr_test1, width=30, height=15, unit="cm", dpi=300, path=getwd())

# Correlations between new indeces, Age and Vocabulary at first test
graph.raw<-all
names(graph.raw)[22]<-"Pos Pred"
names(graph.raw)[23]<-"Neg Pred"
names(graph.raw)[28]<-"Recognition Speed"
names(graph.raw)[31]<-"Graded Pred"
corr_test1.raw<-ggpairs(graph.raw,columns=c("Graded Pred","Pos Pred","Neg Pred","Recognition Speed","Age","BPVS"), lower = list(continuous = my_fn))
print(corr_test1.raw)
ggsave("Corr_test1.raw.png", plot=corr_test1.raw, width=30, height=15, unit="cm", dpi=300, path=getwd())

cor.test(all$rawPred, all$Age) #0.1930198 , t = 2.871, df = 213, p-value = 0.004504
cor.test(all$rawPred, all$BPVS) #0.214173, t = 3.2, df = 213, p-value = 0.001584
cor.test(all$rawUnpred, all$Age) #-0.06369757, t = -0.93153, df = 213, p-value = 0.3526
cor.test(all$rawUnpred, all$BPVS) #-0.01097391, t = -0.16017, df = 213, p-value = 0.8729
cor.test(all$PrecC_raw, all$Age) #0.368942, t = 5.7932, df = 213, p-value = 2.459e-08
cor.test(all$PrecC_raw, all$BPVS) #0.3257412 , t = 5.0283, df = 213, p-value = 1.049e-06

cor.test(all$tff_raw, all$Age) #-0.2965319, t = -4.5316, df = 213, p-value = 9.755e-06
cor.test(all$tff_raw, all$BPVS) #-0.2942354  , t = -4.4931, df = 213, p-value = 1.151e-05

cor.test(all$Age, all$BPVS) #0.8032239, t = 19.68, df = 213, p-value < 2.2e-16

# Multiple regression predicting Vocabulary at time 1
# First regress Age out of Vocabulary
mr1<-lm(BPVS~1+Age,data=all)
summary(mr1)
all$VocRes<-residuals(mr1)
mr2<-lm(VocRes~1+PrecC+Speed,data=all)
summary(mr2)
mr3<-lm(VocRes~1+PrecC,data=all)
summary(mr3)
mr4<-lm(VocRes~1+Speed,data=all)
summary(mr4)

mr5<-lm(BPVS~1+PrecC+Speed,data=all)
summary(mr5)
mr6<-lm(BPVS~1+PrecC,data=all)
mr7<-lm(BPVS~1+Speed,data=all)
summary(mr6)
summary(mr7)
anova(mr5,mr7)
anova(mr5,mr6)

mr.all<-lm(BPVS~1+Age+PrecC+Speed,data=all)
summary(mr.all)

mr.all.raw<-lm(BPVS~1+Age+PrecC_raw+tff_raw,data=all)
summary(mr.all.raw)

mr5.raw<-lm(BPVS~1+PrecC_raw+tff_raw,data=all)
summary(mr5.raw)

# Now restrict the analysis to the sub-sample that was tested twice
# load vocabulary, grammar, SES etc. data for this subsample
data<-read.csv("longitudinal_part_info.csv",header=T)
# Note that it contains 56 rows, but eye-tracking data from one participant are missing, so in effect it is 55 participants
data55<-data[data$Eyetrack_data=="Y",]
# combine with the eye-tracking indeces
names(all)[4]<-"ParticipantNumber"
data.wi<-merge(data55,all,by=c("ParticipantNumber"))
head(data.wi)
# Note that there is one outlier with very large percentage linguistic change (>200)
# remove it now
data.wi<-data.wi[data.wi$LanCh<200,]

#Correlations between Vocabulary at Test 2, Age at Test 2, SES, TROG and indeces (from Test 1)
graph2<-data.wi[,-c(23:24)] #(remove Age and BPVS to avoid duplicates)
names(graph2)[37]<-"Pos Pred"
names(graph2)[38]<-"Neg Pred"
names(graph2)[39]<-"Rec Speed"
names(graph2)[48]<-"Graded Pred"
names(graph2)[8]<-"BPVS"
names(graph2)[5]<-"Age"
names(graph2)[18]<-"SES"
names(graph2)[11]<-"TROG"
corr_test2<-ggpairs(graph2,columns=c("Graded Pred","Pos Pred","Neg Pred","Rec Speed","Age","BPVS","TROG","SES"), lower = list(continuous = my_fn))
print(corr_test2)
ggsave("corr_test2.png", plot=corr_test2, width=30, height=15, unit="cm", dpi=300, path=getwd())

#raw data
graph2.raw<-data.wi[,-c(23:24)] #(remove Age and BPVS to avoid duplicates)
names(graph2.raw)[40]<-"Pos Pred"
names(graph2.raw)[41]<-"Neg Pred"
names(graph2.raw)[46]<-"Rec Speed"
names(graph2.raw)[49]<-"Graded Pred"
names(graph2.raw)[8]<-"BPVS"
names(graph2.raw)[5]<-"Age"
names(graph2.raw)[18]<-"SES"
names(graph2.raw)[11]<-"TROG"
corr_test2.raw<-ggpairs(graph2.raw,columns=c("Graded Pred","Pos Pred","Neg Pred","Rec Speed","Age","BPVS","TROG","SES"), lower = list(continuous = my_fn))
print(corr_test2.raw)
ggsave("corr_test2.raw.png", plot=corr_test2.raw, width=30, height=15, unit="cm", dpi=300, path=getwd())

cor.test(data.wi$RawBPVSScore1,data.wi$RawBPVSScore2)#0.7390874 , t = 7.9121, df = 52, p-value = 1.754e-10
cor.test(data.wi$RawBPVSScore1,data.wi$Ageatfirsttest)#0.7545337, t = 8.2909, df = 52, p-value = 4.429e-11
cor.test(data.wi$RawBPVSScore2,data.wi$Ageatsecondtest)#0.6271358, t = 5.806, df = 52, p-value = 3.892e-07

#multiple regression predicting vocabulary at test 2 as a function of the indeces at test 1 while controlling for vocabulary at test 1
mr.all2.voc<-lm(RawBPVSScore2~1+PrecC+Speed+RawBPVSScore1+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc)

mr.all2.voc.raw<-lm(RawBPVSScore2~1+PrecC_raw+tff_raw+RawBPVSScore1+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc.raw)

mr.all2.voc.raw.P<-lm(RawBPVSScore2~1+PrecC_raw+RawBPVSScore1+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc.raw.P)

mr.all2.voc.raw.R<-lm(RawBPVSScore2~1+tff_raw+RawBPVSScore1+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc.raw.R)

#no voc 1
mr.all2.voc.raw2<-lm(RawBPVSScore2~1+PrecC_raw+tff_raw+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc.raw2)

mr.all2.voc.raw2.P<-lm(RawBPVSScore2~1+PrecC_raw+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc.raw2.P)

mr.all2.voc.raw2.R<-lm(RawBPVSScore2~1+tff_raw+Ageatsecondtest+Ageatfirsttest,data=data.wi)
summary(mr.all2.voc.raw2.R)

#no ages
mr.all2.voc.raw2<-lm(RawBPVSScore2~1+PrecC_raw+tff_raw,data=data.wi)
summary(mr.all2.voc.raw2)

mr.all2.voc.raw2.P<-lm(RawBPVSScore2~1+PrecC_raw,data=data.wi)
summary(mr.all2.voc.raw2.P)

mr.all2.voc.raw2.R<-lm(RawBPVSScore2~1+tff_raw,data=data.wi)
summary(mr.all2.voc.raw2.R)

#predicting Voc 1 in sub-sample
mr.all2.voc.raw3<-lm(RawBPVSScore1~1+PrecC_raw+tff_raw,data=data.wi)
summary(mr.all2.voc.raw3)

mr.all2.voc.raw3.P<-lm(RawBPVSScore1~1+PrecC_raw,data=data.wi)
summary(mr.all2.voc.raw3.P)

mr.all2.voc.raw3.R<-lm(RawBPVSScore1~1+tff_raw,data=data.wi)
summary(mr.all2.voc.raw3.R)

# correlation between raw pred and recognition
cor.test(data.wi$PrecC_raw,data.wi$tff_raw)#-0.2311003 , t = -1.7129, df = 52, p-value = 0.0927

# multiple regression models involving our measure of grammar knowledge
# Predicting vocabulary at test 2, while controlling for age at test 2
mr.all2<-lm(RawBPVSScore2~1+Ageatsecondtest+PrecC+Speed+RawTROGScore2,data=data.wi)
summary(mr.all2)

mr.all2.raw<-lm(RawBPVSScore2~1+Ageatsecondtest+PrecC_raw+tff_raw+RawTROGScore2,data=data.wi)
summary(mr.all2.raw)

# Predicting vocabulary at test 1, while controlling for age at test 1
mr.all3<-lm(RawBPVSScore1~1+Ageatfirsttest+PrecC+Speed+RawTROGScore2,data=data.wi)
summary(mr.all3)

mr.all3.raw<-lm(RawBPVSScore1~1+Ageatfirsttest+PrecC_raw+tff_raw+RawTROGScore2,data=data.wi)
summary(mr.all3.raw)

#Predicting grammar
mr.all2.grammar<-lm(RawTROGScore2~1+Ageatsecondtest+PrecC+Speed,data=data.wi)
summary(mr.all2.grammar)

#### Do recognition and prediction correlate with Vocabulary change?
# recognition
mVC<-lm(LanCh~1+Speed+RawBPVSScore1,data=data.wi)
summary(mVC)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    -6.1895    12.9027  -0.480    0.633
# Speed           1.9257     1.7233   1.117    0.269
# RawBPVSScore1  -0.2072     0.2358  -0.879    0.384

mVC.raw<-lm(LanCh~1+tff_raw+RawBPVSScore1,data=data.wi)
summary(mVC.raw)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   38.14979   19.29314   1.977   0.0534 .
# tff_raw       -0.04077    0.01894  -2.152   0.0362 *
#   RawBPVSScore1 -0.23147    0.22507  -1.028   0.3086

#combined prediction score
mVCp<-lm(LanCh~1+PrecC+RawBPVSScore1,data=data.wi)
summary(mVCp)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)     0.5157    10.1539   0.051   0.9597  
# PrecC           8.4135     4.8507   1.734   0.0889 .
# RawBPVSScore1  -0.1977     0.2268  -0.872   0.3874  

mVCp.raw<-lm(LanCh~1+PrecC_raw+RawBPVSScore1,data=data.wi)
summary(mVCp.raw)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    -2.1750    10.2473  -0.212   0.8328  
# PrecC_raw      35.7716    17.5560   2.038   0.0468 *
#   RawBPVSScore1  -0.1527     0.2221  -0.687   0.4950 

# excluding outlier
mVCp.raw.out<-lm(LanCh~1+PrecC_raw+RawBPVSScore1,data=subset(data.wi,PrecC_raw>-.3))
summary(mVCp.raw.out)

mVC.raw.out<-lm(LanCh~1+tff_raw+RawBPVSScore1,data=subset(data.wi,PrecC_raw>-.3))
summary(mVC.raw.out)

mVCp.raw.Ponly<-lm(LanCh~1+rawPred+RawBPVSScore1,data=data.wi)
summary(mVCp.raw.Ponly)

#graph
pred_raw_all<-ggplot(data.wi,aes(PrecC_raw,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+ylab("Vocabulary Growth (%)")+xlab("Combined Prediction Index")
print(pred_raw_all)
rec_raw_all<-ggplot(data.wi,aes(tff_raw,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+ylab("Vocabulary Growth (%)")+xlab("Recognition Speed")
print(rec_raw_all)

pred_rec_raw_all<-ggarrange(rec_raw_all,pred_raw_all,labels = c("A","B"))
print(pred_rec_raw_all)
ggsave("rec_pred_raw_all.png", plot=pred_rec_raw_all, width=30, height=15, unit="cm", dpi=300, path=getwd())


### Mediation analysis (raw scores)
## PrecC as the mediator
med.fit1.raw<-lm(PrecC_raw~1+tff_raw+RawBPVSScore1,data=data.wi)
out.fit1.raw<-lm(LanCh~1+PrecC_raw+tff_raw+RawBPVSScore1,data=data.wi)
set.seed(15)
med.out1.raw<-mediate(med.fit1.raw,out.fit1.raw,treat="tff_raw",mediator="PrecC_raw",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out1.raw)
## Speed as the mediator
med.fit2.raw<-lm(tff_raw~1+PrecC_raw+RawBPVSScore1,data=data.wi)
out.fit2.raw<-lm(LanCh~1+tff_raw+PrecC_raw+RawBPVSScore1,data=data.wi)
set.seed(15)
med.out2.raw<-mediate(med.fit2.raw,out.fit2.raw,treat="PrecC_raw",mediator="tff_raw",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out2.raw)
summary(med.fit2.raw)

summary(lm(LanCh~1+PrecC_raw+tff_raw+RawBPVSScore1,data=data.wi))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   28.12153   19.97833   1.408   0.1654  
# PrecC_raw     28.62830   17.68410   1.619   0.1118  
# tff_raw       -0.03362    0.01917  -1.754   0.0855 .
# RawBPVSScore1 -0.22523    0.22161  -1.016   0.3144 

summary(lm(LanCh~1+PrecC_raw+tff_raw,data=data.wi))

## Pred as the mediator
med.fit3.raw<-lm(rawPred~1+tff_raw+RawBPVSScore1,data=data.wi)
out.fit3.raw<-lm(LanCh~1+rawPred+tff_raw+RawBPVSScore1,data=data.wi)
set.seed(15)
med.out3.raw<-mediate(med.fit3.raw,out.fit3.raw,treat="tff_raw",mediator="rawPred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out3.raw)
## Speed as the mediator
med.fit4.raw<-lm(tff_raw~1+rawPred+RawBPVSScore1,data=data.wi)
out.fit4.raw<-lm(LanCh~1+tff_raw+rawPred+RawBPVSScore1,data=data.wi)
set.seed(15)
med.out4.raw<-mediate(med.fit4.raw,out.fit4.raw,treat="rawPred",mediator="tff_raw",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out4.raw)
summary(lm(LanCh~1+tff_raw+RawBPVSScore1,data=data.wi))
summary(lm(tff_raw~1+rawPred+RawBPVSScore1,data=data.wi))
summary(lm(LanCh~1+rawPred+RawBPVSScore1,data=data.wi))
summary(lm(LanCh~1+rawPred+tff_raw+RawBPVSScore1,data=data.wi))

## UnPred as the mediator
med.fit5.raw<-lm(rawUnpred~1+tff_raw+RawBPVSScore1,data=data.wi)
out.fit5.raw<-lm(LanCh~1+rawUnpred+tff_raw+RawBPVSScore1,data=data.wi)
set.seed(15)
med.out5.raw<-mediate(med.fit5.raw,out.fit5.raw,treat="tff_raw",mediator="rawUnpred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out5.raw)
## Speed as the mediator
med.fit6.raw<-lm(tff_raw~1+rawUnpred+RawBPVSScore1,data=data.wi)
out.fit6.raw<-lm(LanCh~1+tff_raw+rawUnpred+RawBPVSScore1,data=data.wi)
set.seed(15)
med.out6.raw<-mediate(med.fit6.raw,out.fit6.raw,treat="rawUnpred",mediator="tff_raw",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out6.raw)
summary(lm(tff_raw~1+rawUnpred+RawBPVSScore1,data=data.wi))

## Recognition (with median split of age), controlling for Voc at test 1
ma1<-median(data.wi$Ageatfirsttest)
data.wi$Age<-ifelse(data.wi$Ageatfirsttest<=ma1,-.5,.5)
data.wi$PrecC<-data.wi$Pred -data.wi$UnPred
mRm<-lm(LanCh~1+Speed*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mRm)
# Call:
#   lm(formula = LanCh ~ 1 + Speed * scale(Age, T, F) + RawBPVSScore1, 
#      data = data.wi)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -62.614 -11.057   0.641  12.468  48.202 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)              1.1097    15.1846   0.073  0.94204   
# Speed                    1.1862     1.6477   0.720  0.47498   
# scale(Age, T, F)        58.4250    21.5990   2.705  0.00937 **
#   RawBPVSScore1           -0.2346     0.3041  -0.771  0.44426   
# Speed:scale(Age, T, F)  -9.3063     3.2626  -2.852  0.00634 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 23.17 on 49 degrees of freedom
# Multiple R-squared:  0.1698,	Adjusted R-squared:  0.102 
# F-statistic: 2.505 on 4 and 49 DF,  p-value: 0.05405

# separately by Age category
data.wi$AgeCat<-"Younger"
data.wi$AgeCat[data.wi$Age==.5]<-"Older"

mRm.Y<-lm(LanCh~1+Speed+RawBPVSScore1,data=subset(data.wi, AgeCat=="Younger"))
summary(mRm.Y)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   -18.0279    15.7257  -1.146  0.26248   
# Speed           5.9405     1.9180   3.097  0.00477 **
#   RawBPVSScore1  -0.5568     0.3872  -1.438  0.16281  

mRm.O<-lm(LanCh~1+Speed+RawBPVSScore1,data=subset(data.wi, AgeCat=="Older"))
summary(mRm.O)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    15.9222    28.2443   0.564    0.578
# Speed          -4.0201     2.7823  -1.445    0.162
# RawBPVSScore1   0.1032     0.4724   0.219    0.829

#graph
Speed_medianAge<-ggplot(data.wi,aes(Speed,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth (%)")+xlab("Recognition Speed Index")
print(Speed_medianAge)
ggsave("speed_index_median_age_split.png", plot=Speed_medianAge, width=30, height=15, unit="cm", dpi=300, path=getwd())

# combined prediction score
# with median split of age
mCm<-lm(LanCh~1+PrecC*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mCm)
# Call:
#   lm(formula = LanCh ~ 1 + PrecC * scale(Age, T, F) + RawBPVSScore1, 
#      data = data.wi)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -64.328 -11.978  -0.337   8.666  58.177 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)              3.3312    13.9957   0.238    0.813
# PrecC                    6.2262     5.1195   1.216    0.230
# scale(Age, T, F)         9.0774    11.1360   0.815    0.419
# RawBPVSScore1           -0.2118     0.3113  -0.680    0.500
# PrecC:scale(Age, T, F) -15.1635    10.3641  -1.463    0.150
# 
# Residual standard error: 24.1 on 49 degrees of freedom
# Multiple R-squared:  0.1018,	Adjusted R-squared:  0.02843 
# F-statistic: 1.388 on 4 and 49 DF,  p-value: 0.252

#separately by age category
# Younger
mCm.Y<-lm(LanCh~1+PrecC+RawBPVSScore1,data=subset(data.wi, AgeCat=="Younger"))
summary(mCm.Y)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)     4.6339    13.9529   0.332   0.7426  
# PrecC          13.6120     5.5640   2.446   0.0218 *
#   RawBPVSScore1  -0.3862     0.4033  -0.958   0.3474  

#Older
mCm.O<-lm(LanCh~1+PrecC+RawBPVSScore1,data=subset(data.wi, AgeCat=="Older"))
summary(mCm.O)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   -1.67216   27.13359  -0.062    0.951
# PrecC         -1.51302    9.19210  -0.165    0.871
# RawBPVSScore1 -0.03026    0.48424  -0.062    0.951

#Graph
CompP_medianAge<-ggplot(data.wi,aes(PrecC,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth (%)")+xlab("Prediction Gradedness Index")
print(CompP_medianAge)
ggsave("composite_pred_index_median_age_split.png", plot=CompP_medianAge, width=30, height=15, unit="cm", dpi=300, path=getwd())

#Positive Prediction index
# with median split of age
mPm<-lm(LanCh~1+Pred*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mPm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)             6.3513    14.3898   0.441    0.661
# Pred                   -1.5242     4.5713  -0.333    0.740
# scale(Age, T, F)        5.6921    10.5724   0.538    0.593
# RawBPVSScore1          -0.2126     0.3318  -0.641    0.525
# Pred:scale(Age, T, F)  -7.3377     9.2050  -0.797    0.429

#separately by age category
# Younger
mPm.Y<-lm(LanCh~1+Pred+RawBPVSScore1,data=subset(data.wi, AgeCat=="Younger"))
summary(mPm.Y)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    10.0517    15.5060   0.648    0.523
# Pred            2.6835     5.3975   0.497    0.623
# RawBPVSScore1  -0.4173     0.4658  -0.896    0.379 

#Older
mPm.O<-lm(LanCh~1+Pred+RawBPVSScore1,data=subset(data.wi, AgeCat=="Older"))
summary(mPm.O)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   -1.04464   26.04456  -0.040    0.968
# Pred          -5.43012    7.74039  -0.702    0.490
# RawBPVSScore1 -0.01665    0.47936  -0.035    0.973

#Graph
Pred_medianAge<-ggplot(data.wi,aes(Pred,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth (%)")+xlab("Positive Prediction Index")
print(Pred_medianAge)
ggsave("positive_pred_index_median_age_split.png", plot=Pred_medianAge, width=30, height=15, unit="cm", dpi=300, path=getwd())

#Negative Prediction index (Unpred - Mildly pred)
# with median split of age
mUm<-lm(LanCh~1+UnPred*scale(Age,T,F)+RawBPVSScore1,data=data.wi)
summary(mUm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)               0.8968    14.2728   0.063    0.950
# UnPred                   -5.1106     4.1256  -1.239    0.221
# scale(Age, T, F)         -0.1579     9.6181  -0.016    0.987
# RawBPVSScore1            -0.1127     0.3248  -0.347    0.730
# UnPred:scale(Age, T, F)   2.9378     8.2950   0.354    0.725

#separately by age category
# Younger
mUm.Y<-lm(LanCh~1+UnPred+RawBPVSScore1,data=subset(data.wi, AgeCat=="Younger"))
summary(mUm.Y)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)     4.2725    15.2949   0.279    0.782
# UnPred         -6.2773     4.7161  -1.331    0.195
# RawBPVSScore1  -0.2134     0.4459  -0.479    0.636

#Older
mUm.O<-lm(LanCh~1+UnPred+RawBPVSScore1,data=subset(data.wi, AgeCat=="Older"))
summary(mUm.O)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   -4.47959   26.23851  -0.171    0.866
# UnPred        -3.67208    7.15849  -0.513    0.613
# RawBPVSScore1 -0.01321    0.48227  -0.027    0.978

#Graph
UnPred_medianAge<-ggplot(data.wi,aes(UnPred,LanCh))+geom_point()+geom_smooth(method="lm",col="violet")+facet_wrap(~AgeCat)+ylab("Vocabulary Growth (%)")+xlab("Negative Prediction Index")
print(UnPred_medianAge)
ggsave("negative_pred_index_median_age_split.png", plot=UnPred_medianAge, width=30, height=15, unit="cm", dpi=300, path=getwd())


### Mediation analysis
# Only younger children, with voc at test 1 as control
## PrecC as the mediator
med.fit1<-lm(PrecC~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit1<-lm(LanCh~1+PrecC+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out1<-mediate(med.fit1,out.fit1,treat="Speed",mediator="PrecC",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out1)
## Speed as the mediator
med.fit2<-lm(Speed~1+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit2<-lm(LanCh~1+Speed+PrecC+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out2<-mediate(med.fit2,out.fit2,treat="PrecC",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out2)
summary(med.fit2)

# Below is using the correct indeces, with only voc at test 1 as control, but using Prediction instead of the prediction composite
## Pred as the mediator
med.fit3<-lm(Pred~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit3<-lm(LanCh~1+Pred+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out3<-mediate(med.fit3,out.fit3,treat="Speed",mediator="Pred",sims=5000, boot=T,boot.ci.type = "bca")
summary(med.out3)
## Speed as the mediator
med.fit4<-lm(Speed~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
out.fit4<-lm(LanCh~1+Speed+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger"))
set.seed(15)
med.out4<-mediate(med.fit4,out.fit4,treat="Pred",mediator="Speed",sims=5000,boot=T,boot.ci.type = "bca")
summary(med.out4)
summary(lm(LanCh~1+Speed+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
summary(lm(Speed~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
summary(lm(LanCh~1+Pred+RawBPVSScore1,data=subset(data.wi,AgeCat=="Younger")))
# Below is using the correct indeces, with only voc at test 1 as control, but using UnPred instead of the prediction composite
## UnPred as the mediator
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
