######################
# Graded Predictions #
######################

### NOTE THAT DATA FOR ONE 3yo in LIST3B (P174Task2) DID NOT SAVE

######################################
#############Children###################
######################################
al<-read.table("children.txt",header=T)

length(unique(al$Participant))==216-1
library(doBy)
####Children

##### REJECTION WINDOW FOR PREDICTION ANALYSES
##### 1000 before NO to 100 ms post NO

# make sure you are summing over only those fixations that either were ongoing at DO or started after
# and either where ongoing at NO + 100 ms or ended before NO + 100 ma
al$FIXPOSTDO<-as.character(al$AOIName)
al$FIXPOSTDO[(al$FixationStart<(al$TargetOnset-1000) & al$FixationEnd< (al$TargetOnset-1000))]<-"NOHIT"
al$FIXPOSTDO[(al$FixationStart>(al$TargetOnset+100)&al$FixationEnd>(al$TargetOnset+100))]<-"NOHIT"

head(al,20)

# now compute duration of the fix within the time window

al$DURPOSTDO<-al$FixationDuration
al$DURPOSTDO[al$FIXPOSTDO=="NOHIT"]<-NA
# this is for fixations that start before Det Onset
al$DURPOSTDO[is.na(al$DURPOSTDO)==F&al$FixationStart<(al$TargetOnset-1000)]<-al$DURPOSTDO[is.na(al$DURPOSTDO)==F&al$FixationStart<(al$TargetOnset-1000)]-(al$TargetOnset[is.na(al$DURPOSTDO)==F&al$FixationStart<(al$TargetOnset-1000)]-1000-al$FixationStart[is.na(al$DURPOSTDO)==F&al$FixationStart<(al$TargetOnset-1000)])
# this is for fixations that end after Noun Onset + 100 ms
al$DURPOSTDO[is.na(al$DURPOSTDO)==F&al$FixationEnd>(al$TargetOnset+100)]<-al$DURPOSTDO[is.na(al$DURPOSTDO)==F&al$FixationEnd>(al$TargetOnset+100)]-(al$FixationEnd[is.na(al$DURPOSTDO)==F&al$FixationEnd>(al$TargetOnset+100)]-(al$TargetOnset[is.na(al$DURPOSTDO)==F&al$FixationEnd>(al$TargetOnset+100)]+100))

head(al,20)

# now discard fixations for which DURPOSTDO == NA
alD<-al[is.na(al$DURPOSTDO)==F,]
# this is to avoid NA

fixsumDT<-summaryBy(DURPOSTDO ~Participant + Trial + Bias + Named+ ItemF +TargetOnset+AGEYEAR,data=alD, FUN=sum, keep.names=T)
summary(fixsumDT$DURPOSTDO)
summary(fixsumDT$TargetOnset+100-fixsumDT$TargetOnset+1000)
head(fixsumDT)
## now calculate as a proportion of the total duration of the trial from Det Onset
fixsumDT$Perc_Dur<-fixsumDT$DURPOSTDO*100/(1100)
summary(fixsumDT)
fixsumDT$DiscardTrial<-0
fixsumDT$DiscardTrial[fixsumDT$Perc_Dur<40]<-1
fixsumDT$DiscardTrial[fixsumDT$DiscardTrial>1]<-1

## now average by subject and condition
# this gives the proportion of discarded trials oer subject and condition/det out of the total number of trials for that participant in ex
discardbySubj<-summaryBy(DiscardTrial ~ Participant + Bias + Named+AGEYEAR,data=fixsumDT, FUN=mean,keep.names=T)
write.table(discardbySubj, "discarded_trials_prediction_children.txt")

# total number of trials contributing per Subject and Condition
fixsumDTbis<-fixsumDT[fixsumDT$DiscardTrial==0,]
## note that if a condition is missing for a subject in this summary it's because the subject contributed 0 trials to that condition after removing
keptbySubj<-summaryBy(DiscardTrial ~ Participant + Bias +Named+AGEYEAR,data=fixsumDTbis, FUN=length,keep.names=T)
write.table(keptbySubj,"valid_trials_prediction_children.txt")

################################################

##### REJECTION WINDOW FOR INTEGRATION ANALYSES
##### 100 ms post Noun Onset to 1000 ms post Noun Offset (i.e., post Total Duration)

# make sure you are summing over only those fixations that either were ongoing at NO + 100 or started after
# and either where ongoing at TotDur + 1000 ms or ended before TotDur + 1000 ms
al$FIXPOSTDO2<-as.character(al$AOIName)
al$FIXPOSTDO2[(al$FixationStart<(al$TargetOnset+100) & al$FixationEnd< (al$TargetOnset+100))]<-"NOHIT"
al$FIXPOSTDO2[(al$FixationStart>(al$TotDur+1000)&al$FixationEnd>(al$TotDur+1000))]<-"NOHIT"

# now compute duration of the fix within the time window
al$DURPOSTDO2<-al$FixationDuration
al$DURPOSTDO2[al$FIXPOSTDO2=="NOHIT"]<-NA
# this is for fixations that start before TargetOnset +100
al$DURPOSTDO2[is.na(al$DURPOSTDO2)==F&al$FixationStart<(al$TargetOnset+100)]<-al$DURPOSTDO2[is.na(al$DURPOSTDO2)==F&al$FixationStart<(al$TargetOnset+100)]-(al$TargetOnset[is.na(al$DURPOSTDO2)==F&al$FixationStart<(al$TargetOnset+100)]+100-al$FixationStart[is.na(al$DURPOSTDO2)==F&al$FixationStart<(al$TargetOnset+100)])
# this is for fixations that end after TotDur + 1000 ms
al$DURPOSTDO2[is.na(al$DURPOSTDO2)==F&al$FixationEnd>(al$TotDur+1000)]<-al$DURPOSTDO2[is.na(al$DURPOSTDO2)==F&al$FixationEnd>(al$TotDur+1000)]-(al$FixationEnd[is.na(al$DURPOSTDO2)==F&al$FixationEnd>(al$TotDur+1000)]-(al$TotDur[is.na(al$DURPOSTDO2)==F&al$FixationEnd>(al$TotDur+1000)]+1000))

# now discard fixations for which DURPOSTDO == NA
alD2<-al[is.na(al$DURPOSTDO2)==F,]
# this is to avoid NA

fixsumDT2<-summaryBy(DURPOSTDO2~Participant + Trial + Bias+Named + ItemF+TargetOnset+TotDur+AGEYEAR,data=alD2, FUN=sum, keep.names=T)
summary(fixsumDT2$DURPOSTDO2)

## now calculate as a proportion of the total duration of the trial from Det Onset
fixsumDT2$Perc_Dur<-fixsumDT2$DURPOSTDO2*100/(fixsumDT2$TotDur+1000-(fixsumDT2$TargetOnset+100))
summary(fixsumDT2)
fixsumDT2$DiscardTrial<-0
fixsumDT2$DiscardTrial[fixsumDT2$Perc_Dur<40]<-1
fixsumDT2$DiscardTrial[fixsumDT2$DiscardTrial>1]<-1

## now average by subject and condition
# this gives the proportion of discarded trials oer subject and condition/det out of the total number of trials for that participant in ex
discardbySubj2<-summaryBy(DiscardTrial ~ Participant + Bias+Named+AGEYEAR,data=fixsumDT2, FUN=mean,keep.names=T)
write.table(discardbySubj2, "discarded_trials_recognition_children.txt")

# total number of trials contributing per Subject and Condition
fixsumDT2bis<-fixsumDT2[fixsumDT2$DiscardTrial==0,]
## note that if a condition is missing for a subject in this summary it's because the subject contributed 0 trials to that condition after removing
keptbySubj2<-summaryBy(DiscardTrial ~ Participant + Bias+Named+AGEYEAR,data=fixsumDT2bis, FUN=length,keep.names=T)
write.table(keptbySubj2,"valid_trials_recognition_children.txt")

##########
## Graph #
#########

## two separate graphs, one for prediction window (-1000 to +100 around targetOnset),
#and one for integration window (TargetOnset+100 to 1000 after TotDur)

# plot average fixation proportion over time for each picture (A,B,C) in 50 ms time bins
### fixation proportion; for the purprose of this analysis a fixation is counted as falling in a IA in a given bin 
#if it is ongoing when the bin starts, or starts after the bin starts and ends before or after the bin ends, 
#or if it was ongoing when the bin ends. Use this measure for the purpose of plotting

## Cretae variable FixLoc that tells you whether a given fixation was on picture A, B, or C
# you can do this using Columns A, B, c and AOIName
# how to treat White space? maybe code as fourth category (Background)

##IMPORTANT NOTE: window in Item 3 is window2; hair in item 11 is hair2
al$A<-as.character(al$A)
al$B<-as.character(al$B)
al$C<-as.character(al$C)
al$A[al$ItemF=="11"]<-"Hair2"
al$B[al$ItemF=="3"]<-"Window2"

al$FixLoc<-NA
for (i in 1:nrow(al)){
  if (al$AOIName[i]==tolower(gsub(" ","",al$A[i]))){al$FixLoc[i]<-"A"}
  if (al$AOIName[i]==tolower(gsub(" ","",al$B[i]))){al$FixLoc[i]<-"B"}
  if (al$AOIName[i]==tolower(gsub(" ","",al$C[i]))){al$FixLoc[i]<-"C"}
  if (al$AOIName[i]=="White Space"){al$FixLoc[i]<-"Background"}
}
summary(as.factor(al$FixLoc))



#### Prediction Graph
alp<-al
# Zero at noun onset
# noun onset varies by Item and Condition and is listed in the variable TargetOnset
# plotting from 1 second before Noun Onset to 100 ms after Noun Onset


for (i in (-20:2)){
  k<-paste(50*i,sep="")
  j=i-1
  alp$x<-alp$FixLoc
  alp$x[(alp$FixationStart<(alp$TargetOnset + 50*j) & alp$FixationEnd< (alp$TargetOnset + 50*j))|(alp$FixationStart>(alp$TargetOnset + 50*i) & alp$FixationEnd> (alp$TargetOnset + 50*i))]<-NA
  i=i+1
  alp$x<-as.factor(alp$x)
  names(alp)[names(alp)=="x"]<-k
}

summary(alp)


#change to long format
names(alp)[75:97] # ok
library(reshape2)

alp.l<-melt(alp,id.vars=names(alp)[1:74])

alp.l$AFIXBIN<-0
alp.l$BFIXBIN<-0
alp.l$CFIXBIN<-0
alp.l$BackgroundFIXBIN<-0
alp.l$AFIXBIN[is.na(alp.l$value)==F&alp.l$value=="A"]<-1
alp.l$BFIXBIN[is.na(alp.l$value)==F&alp.l$value=="B"]<-1
alp.l$CFIXBIN[is.na(alp.l$value)==F&alp.l$value=="C"]<-1
alp.l$BackgroundFIXBIN[is.na(alp.l$value)==F&alp.l$value=="Background"]<-1

alp.l$time<-as.numeric(as.character(alp.l$variable))

# find number of rows with fixations in more than one column
alp.l$SUM<-alp.l$AFIXBIN+alp.l$BFIXBIN+alp.l$CFIXBIN
nrow(alp.l[alp.l$SUM>1,])
range(alp.l$SUM, na.rm=T)# never more than one picture fixated at the same time

#Discard irrelevant fixations
alp.lD<-alp.l[is.na(alp.l$DURPOSTDO)==F,]

fixsumbin<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN + DURPOSTDO ~ time + Participant + Trial + ItemF+ Bias+Block+TargetOnset+Age+BPVS+AGEYEAR, data=alp.lD, FUN = sum, keep.names=T, na.rm=T)

summary(fixsumbin)

range(fixsumbin$AFIXBIN, na.rm=T) 
length(fixsumbin$AFIXBIN[fixsumbin$AFIXBIN>1]) 
range(fixsumbin$BFIXBIN, na.rm=T) 
length(fixsumbin$BFIXBIN[fixsumbin$BFIXBIN>1])
range(fixsumbin$CFIXBIN, na.rm=T) 
length(fixsumbin$CFIXBIN[fixsumbin$CFIXBIN>1])
range(fixsumbin$BackgroundFIXBIN, na.rm=T)
length(fixsumbin$BackgroundFIXBIN[fixsumbin$BackgroundFIXBIN>1])

fixsumbin$AFIXBIN[fixsumbin$AFIXBIN>1]<-1
fixsumbin$BFIXBIN[fixsumbin$BFIXBIN>1]<-1
fixsumbin$CFIXBIN[fixsumbin$CFIXBIN>1]<-1
fixsumbin$BackgroundFIXBIN[fixsumbin$BackgroundFIXBIN>1]<-1

range(fixsumbin$AFIXBIN, na.rm=T) # 0 to 1 
range(fixsumbin$BFIXBIN, na.rm=T) # 0 to 1
range(fixsumbin$CFIXBIN, na.rm=T) # 0 to 1 
range(fixsumbin$BackgroundFIXBIN, na.rm=T) # 0 to 1

## apply %40 rejection criterion
fixsumbin$Perc_Dur<-fixsumbin$DURPOSTDO*100/(1100)
fixsumbin$DiscardTrial<-0
fixsumbin$DiscardTrial[fixsumbin$Perc_Dur<40]<-1
fixsumbin$DiscardTrial[fixsumbin$DiscardTrial>1]<-1

fixsumbin<-fixsumbin[fixsumbin$DiscardTrial==0,]


#save to file
write.table(fixsumbin,"children_pred_bytrial-cleaned.txt",row.names=F)

### Looks to A/B/C across the three BIas conditions, removing Background with loess
fixpropbinABC<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN ~ time + Participant + Bias+Age+BPVS+AGEYEAR, data=subset(fixsumbin,BackgroundFIXBIN==0), FUN = mean, keep.names=T, na.rm=T)
### turn into long format
str(fixpropbinABC)
names(fixpropbinABC)[1]<-"Time.ms"
names(fixpropbinABC)[3]<-"Context"
names(fixpropbinABC)[7]<-"FIXBIN.A"
names(fixpropbinABC)[8]<-"FIXBIN.B"
names(fixpropbinABC)[9]<-"FIXBIN.C"
fixpropbinABC<-fixpropbinABC[,-10]
library(ggplot2)
fixpropbinABC_l<-reshape(fixpropbinABC,varying=names(fixpropbinABC)[7:9],direction="long")
str(fixpropbinABC_l)
names(fixpropbinABC_l)[7]<-"Picture"
propABC_loess_pred<-ggplot(fixpropbinABC_l,aes(x=Time.ms,y=(FIXBIN), color=Picture,linetype=Picture))+stat_summary(fun.y="mean", geom="point")+stat_smooth(method="loess")+facet_wrap(~Context+AGEYEAR)+ggtitle("Prediction window")+ylab("Fixation proportions")
print(propABC_loess_pred)
ggsave("propABC.loess_prediction_children.png", plot=propABC_loess_pred, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

##############################################################################
##############################################################################
## Difference curves (by subjects and items, with elogit transform)
## starts with fixsumbin (children_pred_bytrial-cleaned.txt)
fixsumbin<-read.table("children_pred_bytrial-cleaned.txt",header=T)
head(fixsumbin)
# then average by subject and bias/item and bias (excluding background only)
library(dplyr)

#(Following Brock Ferguson example)
#http://brockferguson.com/tutorials/R-gca-workshop/tutorial4-growth-curve-analysis.html#orthogonal-polynomial-growth-curve-analysis
bySubj <- fixsumbin[fixsumbin$BackgroundFIXBIN==0,] %>%
  group_by(Participant,Bias,time) %>% # aggregate within time slices
  summarise(AFIXBIN.m = mean(AFIXBIN), BFIXBIN.m = mean(BFIXBIN), CFIXBIN.m = mean(CFIXBIN), yA = sum(AFIXBIN), yB = sum(BFIXBIN), yC = sum(CFIXBIN), N_A = length(AFIXBIN),N_B = length(BFIXBIN),N_C = length(CFIXBIN)) %>%
  mutate(elogA = log( (yA + .5) / (N_A - yA + .5) ), 
         elogB = log( (yB + .5) / (N_B - yB + .5) ),
         elogC = log( (yC + .5) / (N_C - yC + .5) )) %>%  # empirical logit
  #wts = 1/(y + .5) + 1/(N - y + .5), # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup()        
#bySubj_check<-summaryBy(AFIXBIN+BFIXBIN+CFIXBIN~Participant+Bias+time, data=subset(fixsumbin,BackgroundFIXBIN==0),FUN=mean, keep.names=T,na.rm=T)

## add Age/AGEYEAR/BPVS to bySubj

str(fixsumbin)
part.info<-summaryBy(time~Participant+AGEYEAR+Age+BPVS, data=fixsumbin,FUN=mean)
part.info<-part.info[,-5]
bySubj<-merge(bySubj,part.info,by=c("Participant"),sort=FALSE)
head(bySubj)

byItem <- fixsumbin[fixsumbin$BackgroundFIXBIN==0,] %>%
  group_by(ItemF,Bias,time) %>% # aggregate within time slices
  summarise(AFIXBIN.m = mean(AFIXBIN), BFIXBIN.m = mean(BFIXBIN), CFIXBIN.m = mean(CFIXBIN), yA = sum(AFIXBIN), yB = sum(BFIXBIN), yC = sum(CFIXBIN), N_A = length(AFIXBIN),N_B = length(BFIXBIN),N_C = length(CFIXBIN)) %>%
  mutate(elogA = log( (yA + .5) / (N_A - yA + .5) ),
         elogB = log( (yB + .5) / (N_B - yB + .5) ),
         elogC = log( (yC + .5) / (N_C - yC + .5) )) %>%  # empirical logit
  #wts = 1/(y + .5) + 1/(N - y + .5), # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup()
#byItem_check<-summaryBy(AFIXBIN+BFIXBIN+CFIXBIN~ItemF+Bias+time, data=subset(fixsumbin,BackgroundFIXBIN==0),FUN=mean, keep.names=T,na.rm=T)

# then for each subject/item and for each picture take difference between the average in CbA and FLAT and the differene between the average in ABC and FLAT (in empirical logits).
# nead to turn into wide format first
library(reshape2)

################
##By SUBJECTS###
################
bySubj_tow<-as.data.frame(bySubj[,c(1:3,13:18)])
bySubj.w<-reshape(bySubj_tow,v.names=c("elogA","elogB","elogC"),timevar=c("Bias"),idvar=c("Participant","time","AGEYEAR","Age","BPVS"),direction="wide")
## compute differences
bySubj.w$A.CBA<-bySubj.w$elogA.CBA-bySubj.w$elogA.FLAT
bySubj.w$B.CBA<-bySubj.w$elogB.CBA-bySubj.w$elogB.FLAT
bySubj.w$C.CBA<-bySubj.w$elogC.CBA-bySubj.w$elogC.FLAT

bySubj.w$A.ABC<-bySubj.w$elogA.ABC-bySubj.w$elogA.FLAT
bySubj.w$B.ABC<-bySubj.w$elogB.ABC-bySubj.w$elogB.FLAT
bySubj.w$C.ABC<-bySubj.w$elogC.ABC-bySubj.w$elogC.FLAT

names(bySubj.w)
diff<-bySubj.w[,c(1:5,15:20)]
names(diff)[2]<-"Time.ms"

##turn to long format
diff<-reshape(diff,varying=names(diff)[6:11],direction="long")
names(diff)[6]<-"Contrast"
names(diff)[7:9]<-c("Picture.A","Picture.B","Picture.C")
diff<-diff[,-10]
diff<-reshape(diff,varying=names(diff)[7:9],direction="long")

## plot the difference curves over time
diff.mean<-summaryBy(Picture~Time.ms+Contrast+time,data=diff, FUN=mean,keep.names=T,na.rm=T)
names(diff.mean)[4]<-"PropLooks"
names(diff.mean)[3]<-"Picture"
diff.mean$Contrast<-as.factor(diff.mean$Contrast)
levels(diff.mean$Contrast)<-c("ABC-FLAT","CBA-FLAT")
library(ggplot2)
qplot(Time.ms,PropLooks,data=diff.mean,linetype=Picture,geom="line")+facet_wrap(~Contrast)+ggtitle("Adults")

## with CI
## taken from Micheal Frank Github
library(bootstrap)
library(lme4)
library(stringr)
library(lubridate)

## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

diff.meanCI<-summaryBy(Picture~Time.ms+Contrast+time,data=diff, FUN=c(mean,ci.low,ci.high),keep.names=F,na.rm=T)
names(diff.meanCI)[4]<-"PropLooks"
names(diff.meanCI)[3]<-"Picture"
diff.meanCI$Contrast<-as.factor(diff.meanCI$Contrast)
levels(diff.meanCI$Contrast)<-c("ABC-FLAT","CBA-FLAT")

elog_diff_CI_pred<-ggplot(diff.meanCI,aes(x=Time.ms,y=PropLooks,color=Picture,linetype=Picture))+geom_line(stat = "identity", size = 1) + geom_linerange(aes(ymin= (PropLooks - Picture.ci.low), ymax= (PropLooks + Picture.ci.high)), position = position_dodge(width=.9), size =0.5)+facet_wrap(~Contrast)+ggtitle("Prediction window")+ylab("Empirical logit Difference")
ggsave("elog_growthcurve.CI_prediction_children.png", plot=elog_diff_CI_pred, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

# with points + loess function
names(diff)[8]<-"PropLooks"
names(diff)[7]<-"Picture"
diff$Contrast<-as.factor(diff$Contrast)
levels(diff$Contrast)<-c("ABC-FLAT","CBA-FLAT")
library(ggplot2)
elog_diff_loess_pred<-ggplot(diff,aes(x=Time.ms,y=PropLooks,color=Picture,linetype=Picture))+stat_summary(fun.y="mean", geom="point")+stat_smooth(method="loess")+facet_wrap(~Contrast+AGEYEAR)+ggtitle("Prediction window")+ylab("Empirical logit Difference")
print(elog_diff_loess_pred)
ggsave("elog_growthcurve.loess_prediction_children.png", plot=elog_diff_loess_pred, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

## split by median vocabulary or by quartiles of vocabulary
median(diff$BPVS) #41
diff$Voc<-"Below median (41)"
diff$Voc[diff$BPVS>41]<-"Above median (41)"
summary(diff$BPVS)#n. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###################2.00   30.00   41.00   42.25   54.00   93.00 
diff$Voc2<-"First quartile (2-30)"
diff$Voc2[diff$BPVS>30]<-"Interquartile (31-54)"
diff$Voc2[diff$BPVS>54]<-"Third quartile (55-93)"
unique(diff$Participant[diff$Voc2=="Interquartile (31-54)"]) #108
unique(diff$Participant[diff$Voc2=="First quartile (2-30)"]) #56
unique(diff$Participant[diff$Voc2=="Third quartile (55-93)"]) #51

elog_diff_loess_pred_voc<-ggplot(diff,aes(x=Time.ms,y=PropLooks,color=Picture,linetype=Picture))+stat_summary(fun.y="mean", geom="point")+stat_smooth(method="loess")+facet_wrap(~Contrast+Voc)+ggtitle("Prediction window")+ylab("Empirical logit Difference")
print(elog_diff_loess_pred_voc)
ggsave("elog_growthcurve.loess_prediction_children_voc.png", plot=elog_diff_loess_pred_voc, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

elog_diff_loess_pred_voc2<-ggplot(diff,aes(x=Time.ms,y=PropLooks,color=Picture,linetype=Picture))+stat_summary(fun.y="mean", geom="point")+stat_smooth(method="loess")+facet_wrap(~Contrast+Voc2)+ggtitle("Prediction window")+ylab("Empirical logit Difference")
print(elog_diff_loess_pred_voc2)
ggsave("elog_growthcurve.loess_prediction_children_voc2.png", plot=elog_diff_loess_pred_voc2, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

# split by median age
median(diff$Age) #44
diff$AgeM<-"Below median (44)"
diff$AgeM[diff$Age>44]<-"Above median (44)"

elog_diff_loess_pred_ageM<-ggplot(diff,aes(x=Time.ms,y=PropLooks,color=Picture,linetype=Picture))+stat_summary(fun.y="mean", geom="point")+stat_smooth(method="loess")+facet_wrap(~Contrast+AgeM)+ggtitle("Prediction window")+ylab("Empirical logit Difference")
print(elog_diff_loess_pred_ageM)
ggsave("elog_growthcurve.loess_prediction_children_ageM.png", plot=elog_diff_loess_pred_ageM, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

# split by age group (2, 3, and 4-5)
elog_diff_loess_pred_ageG<-ggplot(diff,aes(x=Time.ms,y=PropLooks,color=Picture,linetype=Picture))+stat_summary(fun.y="mean", geom="point")+stat_smooth(method="loess")+facet_wrap(~Contrast+AGEYEAR)+ggtitle("Prediction window")+ylab("Empirical logit Difference")
print(elog_diff_loess_pred_ageM)
ggsave("elog_growthcurve.loess_prediction_children_ageG.png", plot=elog_diff_loess_pred_ageG, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

## stats
## first, we need to compute the weights for the differences.
## these weights must be the inverse of the variance of the differences
## the variance of the differences should be equal to the sum of the variances of the two empirical logits, 
# minus 2 the covariance of the empirical logits
# the variances of the empirical logits according to Barr(2008) are
# v = (1/(y+.5))+(1/(n-y+.5))
## NOTE: as I am not sure how I would compute the covariances, I am not going to compute weights
## But in theory the regression (lmer) on empirical logits should be weighted to account for the fact that mean and variance are related

## analyze
gca<-as.data.frame(diff)
names(gca)[2]<-"time"
names(gca)[8]<-"Elog"
names(gca)[7]<-"Picture"
head(gca)

t<-poly(unique(gca$time),2)
time<-as.vector(unique(gca$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gca<-gca[order(gca$time),]
gca$t1<-NA
gca$t2<-NA
for (i in (1:nrow(gca))){
  gca$t1[i]<-t[t$time==gca$time[i],1] 
  gca$t2[i]<-t[t$time==gca$time[i],2] 
}
summary(gca)
gca$Picture<-as.factor(gca$Picture)
gca$Picture<-relevel(gca$Picture,ref="B")
gca$BD1<-ifelse(gca$Picture=="A", 1,0)
gca$BD2<-ifelse(gca$Picture=="C", 1,0)
gca$AC<-scale(gca$Age,T,F)
gca$VC<-scale(gca$BPVS,T,F)
#analyse teh two contrasts separately
library(lme4)
gca$Participant<-as.factor(gca$Participant)
gca<-gca[,-9]
mdiffCBA<-lmer(Elog~1+(t1+t2)*(BD1+BD2)+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA-FLAT"),REML=F)
summary(mdiffCBA)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (BD1 + BD2) + (1 + t1 + t2 | Participant) +  
#   (1 + t1 + t2 | Participant:BD1) + (1 + t1 + t2 | Participant:BD2)
# Data: subset(gca, Contrast == "CBA-FLAT")
# 
# AIC      BIC   logLik deviance df.resid 
# 31132.5  31345.4 -15538.2  31076.5    14807 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.5002 -0.5829 -0.0033  0.5750  4.2307 
# 
# Random effects:
#   Groups          Name        Variance  Std.Dev.  Corr       
# Participant:BD2 (Intercept) 5.193e-01 7.206e-01            
# t1          6.176e+00 2.485e+00  0.01      
# t2          2.502e+00 1.582e+00 -0.39 -0.02
# Participant:BD1 (Intercept) 4.748e-01 6.890e-01            
# t1          7.261e+00 2.695e+00  0.02      
# t2          2.486e+00 1.577e+00 -0.37 -0.02
# Participant     (Intercept) 0.000e+00 0.000e+00            
# t1          1.394e-10 1.181e-05   NaN      
# t2          9.934e-12 3.152e-06   NaN -0.65
# Residual                    3.051e-01 5.523e-01            
# Number of obs: 14835, groups:  Participant:BD2, 430; Participant:BD1, 430; Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.11864    0.06845  -1.733
# t1           0.02243    0.25281   0.089
# t2           0.20700    0.15691   1.319
# BD1         -0.17731    0.06738  -2.631
# BD2          0.58257    0.07038   8.277
# t1:BD1      -0.58192    0.26529  -2.194
# t1:BD2       0.07341    0.24553   0.299
# t2:BD1      -0.32531    0.16113  -2.019
# t2:BD2      -0.41080    0.16160  -2.542
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     BD1    BD2    t1:BD1 t1:BD2 t2:BD1
# t1      0.014                                                 
# t2     -0.367 -0.015                                          
# BD1    -0.492 -0.008  0.179                                   
# BD2    -0.514 -0.005  0.186  0.013                            
# t1:BD1 -0.008 -0.525  0.007  0.016  0.000                     
# t1:BD2 -0.006 -0.486  0.007  0.000  0.011  0.022              
# t2:BD1  0.172  0.007 -0.513 -0.349  0.000 -0.014  0.000       
# t2:BD2  0.185  0.007 -0.515  0.000 -0.360  0.000 -0.014  0.054
confint(mdiffCBA, method="Wald")
# (Intercept) -0.2527931  0.015520950
# t1          -0.4730769  0.517932012
# t2          -0.1005235  0.514532872
# BD1         -0.3093665 -0.045245613
# BD2          0.4446149  0.720515126
# t1:BD1      -1.1018825 -0.061958711
# t1:BD2      -0.4078321  0.554645509
# t2:BD1      -0.6411156 -0.009509867
# t2:BD2      -0.7275254 -0.094066736
mdiffCBA.AV<-lmer(Elog~1+(t1+t2)*(BD1+BD2)*(AC+VC)+t1*(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA-FLAT"),REML=F)
summary(mdiffCBA.AV)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (BD1 + BD2) * (AC + VC) + (1 + t1 + t2 |  
#                                                              Participant) + (1 + t1 + t2 | Participant:BD1) + (1 + t1 +      t2 | Participant:BD2)
# Data: subset(gca, Contrast == "CBA-FLAT")
# 
# AIC      BIC   logLik deviance df.resid 
# 31136.9  31486.7 -15522.4  31044.9    14789 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.5028 -0.5823 -0.0006  0.5739  4.2369 
# 
# Random effects:
#   Groups          Name        Variance  Std.Dev.  Corr       
# Participant:BD2 (Intercept) 4.993e-01 7.066e-01            
# t1          6.088e+00 2.467e+00  0.01      
# t2          2.465e+00 1.570e+00 -0.39 -0.01
# Participant:BD1 (Intercept) 4.688e-01 6.847e-01            
# t1          7.226e+00 2.688e+00  0.02      
# t2          2.425e+00 1.557e+00 -0.37 -0.02
# Participant     (Intercept) 0.000e+00 0.000e+00            
# t1          7.395e-11 8.599e-06   NaN      
# t2          1.744e-11 4.176e-06   NaN -0.36
# Residual                    3.051e-01 5.524e-01            
# Number of obs: 14835, groups:  Participant:BD2, 430; Participant:BD1, 430; Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.118636   0.067559  -1.756
# t1           0.022428   0.251683   0.089
# t2           0.207005   0.155450   1.332
# BD1         -0.177306   0.066962  -2.648
# BD2          0.582565   0.069051   8.437
# AC          -0.003877   0.010981  -0.353
# VC          -0.001625   0.007035  -0.231
# t1:BD1      -0.581921   0.264676  -2.199
# t1:BD2       0.073407   0.243871   0.301
# t2:BD1      -0.325313   0.159366  -2.041
# t2:BD2      -0.410796   0.160531  -2.559
# t1:AC        0.006987   0.040907   0.171
# t1:VC       -0.013197   0.026206  -0.504
# t2:AC       -0.023732   0.025266  -0.939
# t2:VC        0.022937   0.016186   1.417
# BD1:AC      -0.016964   0.010884  -1.559
# BD1:VC       0.006836   0.006972   0.980
# BD2:AC       0.021368   0.011223   1.904
# BD2:VC       0.002130   0.007190   0.296
# t1:BD1:AC    0.038553   0.043019   0.896
# t1:BD1:VC   -0.011171   0.027559  -0.405
# t1:BD2:AC   -0.068850   0.039637  -1.737
# t1:BD2:VC    0.052832   0.025393   2.081
# t2:BD1:AC    0.060363   0.025902   2.330
# t2:BD1:VC   -0.044646   0.016594  -2.691
# t2:BD2:AC    0.024642   0.026092   0.944
# t2:BD2:VC   -0.028919   0.016715  -1.730
confint(mdiffCBA.AV, method="Wald")
# 2.5 %       97.5 %
# (Intercept) -0.2510498900  0.013777783
# t1          -0.4708629963  0.515718074
# t2          -0.0976728076  0.511682207
# BD1         -0.3085494306 -0.046062702
# BD2          0.4472281908  0.717901865
# AC          -0.0253984319  0.017644992
# VC          -0.0154120680  0.012162747
# t1:BD1      -1.1006771629 -0.063164015
# t1:BD2      -0.4045738781  0.551387331
# t2:BD1      -0.6376656561 -0.012959780
# t2:BD2      -0.7254310059 -0.096161125
# t1:AC       -0.0731898214  0.087162858
# t1:VC       -0.0645600284  0.038166360
# t2:AC       -0.0732525415  0.025788188
# t2:VC       -0.0087871529  0.054661094
# BD1:AC      -0.0382955596  0.004367382
# BD1:VC      -0.0068299562  0.020501111
# BD2:AC      -0.0006288384  0.043364757
# BD2:VC      -0.0119620778  0.016221444
# t1:BD1:AC   -0.0457627636  0.122868095
# t1:BD1:VC   -0.0651860235  0.042843596
# t1:BD2:AC   -0.1465377007  0.008838219
# t1:BD2:VC    0.0030629253  0.102601063
# t2:BD1:AC    0.0095954664  0.111131228
# t2:BD1:VC   -0.0771693177 -0.012122683
# t2:BD2:AC   -0.0264965211  0.075781046
# t2:BD2:VC   -0.0616804071  0.003841448

##### This needs updating to include also the effects of age and vocabulary.
# ## Log-Lik
# mdiffCBA.noBD1i<-lmer(Elog~1+(t1+t2)*(BD2)+BD1:t1+BD1:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
# mdiffCBA.noBD2i<-lmer(Elog~1+(t1+t2)*(BD1)+BD2:t1+BD2:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
# mdiffCBA.noBD1l<-lmer(Elog~1+(t1+t2)*(BD2)+BD1+BD1:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
# mdiffCBA.noBD2l<-lmer(Elog~1+(t1+t2)*(BD1)+BD2+BD2:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
# mdiffCBA.noBD1q<-lmer(Elog~1+(t1+t2)*(BD2)+BD1+BD1:t1+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
# mdiffCBA.noBD2q<-lmer(Elog~1+(t1+t2)*(BD1)+BD2+BD2:t1+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
# 
# #BD1
# anova(mdiffCBA.noBD1i,mdiffCBA)#7.4198      1   0.006451 **
# #BD2
# anova(mdiffCBA.noBD2i,mdiffCBA)#31.66      1  1.837e-08 ***
# #BD1:time
# anova(mdiffCBA.noBD1l,mdiffCBA)#4.0454      1    0.04429 *
# #BD2:time
# anova(mdiffCBA.noBD2l,mdiffCBA)#1.6231      1     0.2027
# #BD1:time2
# anova(mdiffCBA.noBD1q,mdiffCBA)#0.0511      1     0.8212
# #BD2:time2
# anova(mdiffCBA.noBD2q,mdiffCBA)#0.373      1     0.5414

mdiffABC<-lmer(Elog~1+(t1+t2)*(BD1+BD2)+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC-FLAT"),REML=F)
summary(mdiffABC)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (BD1 + BD2) + (1 + t1 + t2 | Participant) +  
#   (1 + t1 + t2 | Participant:BD1) + (1 + t1 + t2 | Participant:BD2)
# Data: subset(gca, Contrast == "ABC-FLAT")
# 
# AIC      BIC   logLik deviance df.resid 
# 31480.9  31693.8 -15712.4  31424.9    14807 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.9644 -0.5823  0.0012  0.5676  4.7212 
# 
# Random effects:
#   Groups          Name        Variance  Std.Dev.  Corr       
# Participant:BD2 (Intercept) 4.590e-01 6.775e-01            
# t1          6.901e+00 2.627e+00  0.01      
# t2          2.167e+00 1.472e+00 -0.43 -0.14
# Participant:BD1 (Intercept) 4.598e-01 6.781e-01            
# t1          6.524e+00 2.554e+00 -0.02      
# t2          2.058e+00 1.434e+00 -0.36 -0.10
# Participant     (Intercept) 6.076e-15 7.795e-08            
# t1          5.105e-12 2.259e-06 1.00       
# t2          5.979e-12 2.445e-06 0.20  0.20 
# Residual                    3.173e-01 5.633e-01            
# Number of obs: 14835, groups:  Participant:BD2, 430; Participant:BD1, 430; Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.08226    0.06586  -1.249
# t1          -0.07849    0.25281  -0.310
# t2           0.09208    0.14534   0.634
# BD1          0.32594    0.06637   4.911
# BD2         -0.03331    0.06632  -0.502
# t1:BD1       0.57529    0.25226   2.281
# t1:BD2      -0.58627    0.25912  -2.263
# t2:BD1      -0.01242    0.14864  -0.084
# t2:BD2      -0.35219    0.15201  -2.317
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     BD1    BD2    t1:BD1 t1:BD2 t2:BD1
# t1     -0.002                                                 
# t2     -0.378 -0.114                                          
# BD1    -0.504  0.009  0.171                                   
# BD2    -0.503 -0.007  0.205  0.015                            
# t1:BD1  0.009 -0.499  0.046 -0.019  0.000                     
# t1:BD2 -0.007 -0.512  0.067  0.000  0.013  0.023              
# t2:BD1  0.168  0.045 -0.511 -0.334  0.000 -0.090  0.000       
# t2:BD2  0.197  0.066 -0.523  0.000 -0.391  0.000 -0.128  0.065
confint(mdiffABC,method="Wald")
# (Intercept) -0.21134396  0.04682833
# t1          -0.57399700  0.41700962
# t2          -0.19277862  0.37694781
# BD1          0.19584986  0.45603166
# BD2         -0.16329110  0.09667615
# t1:BD1       0.08086596  1.06971900
# t1:BD2      -1.09413426 -0.07840471
# t2:BD1      -0.30373846  0.27890161
# t2:BD2      -0.65012415 -0.05425875

mdiffABC.AV<-lmer(Elog~1+(t1+t2)*(BD1+BD2)*(AC+VC)+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC-FLAT"),REML=F)
summary(mdiffABC.AV)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (BD1 + BD2) * (AC + VC) + (1 + t1 + t2 |  
#                                                              Participant) + (1 + t1 + t2 | Participant:BD1) + (1 + t1 +      t2 | Participant:BD2)
# Data: subset(gca, Contrast == "ABC-FLAT")
# 
# AIC      BIC   logLik deviance df.resid 
# 31486.9  31836.7 -15697.4  31394.9    14789 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.9655 -0.5822 -0.0003  0.5690  4.7302 
# 
# Random effects:
#   Groups          Name        Variance  Std.Dev.  Corr       
# Participant:BD2 (Intercept) 4.565e-01 6.757e-01            
# t1          6.643e+00 2.577e+00  0.01      
# t2          2.152e+00 1.467e+00 -0.43 -0.13
# Participant:BD1 (Intercept) 4.460e-01 6.678e-01            
# t1          6.464e+00 2.543e+00 -0.03      
# t2          2.045e+00 1.430e+00 -0.37 -0.10
# Participant     (Intercept) 2.543e-15 5.043e-08            
# t1          2.975e-10 1.725e-05  0.56      
# t2          6.477e-12 2.545e-06 -0.28  0.64
# Residual                    3.173e-01 5.633e-01            
# Number of obs: 14835, groups:  Participant:BD2, 430; Participant:BD1, 430; Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.0822578  0.0652837  -1.260
# t1          -0.0784937  0.2498822  -0.314
# t2           0.0920846  0.1449015   0.635
# BD1          0.3259408  0.0654016   4.984
# BD2         -0.0333075  0.0661428  -0.504
# AC           0.0025041  0.0106108   0.236
# VC          -0.0052151  0.0067976  -0.767
# t1:BD1       0.5752925  0.2511679   2.290
# t1:BD2      -0.5862695  0.2544574  -2.304
# t2:BD1      -0.0124184  0.1482417  -0.084
# t2:BD2      -0.3521915  0.1515549  -2.324
# t1:AC        0.0377976  0.0406143   0.931
# t1:VC       -0.0307816  0.0260186  -1.183
# t2:AC       -0.0123738  0.0235514  -0.525
# t2:VC        0.0073962  0.0150877   0.490
# BD1:AC      -0.0009736  0.0106300  -0.092
# BD1:VC       0.0132262  0.0068099   1.942
# BD2:AC      -0.0065538  0.0107504  -0.610
# BD2:VC       0.0042758  0.0068870   0.621
# t1:BD1:AC    0.0114935  0.0408232   0.282
# t1:BD1:VC    0.0114188  0.0261525   0.437
# t1:BD2:AC   -0.1426444  0.0413579  -3.449
# t1:BD2:VC    0.0802970  0.0264950   3.031
# t2:BD1:AC    0.0286221  0.0240943   1.188
# t2:BD1:VC   -0.0134749  0.0154355  -0.873
# t2:BD2:AC    0.0232943  0.0246328   0.946
# t2:BD2:VC   -0.0194151  0.0157804  -1.230
confint(mdiffABC.AV,method="Wald")
# 2.5 %     97.5 %
# (Intercept) -0.2102114560  0.045695823
# t1          -0.5682538713  0.411266485
# t2          -0.1919170748  0.376086261
# BD1          0.1977558914  0.454125628
# BD2         -0.1629450090  0.096330052
# AC          -0.0182926914  0.023300867
# VC          -0.0185381047  0.008107886
# t1:BD1       0.0830124434  1.067572514
# t1:BD2      -1.0849968871 -0.087542092
# t2:BD1      -0.3029669125  0.278130064
# t2:BD2      -0.6492336320 -0.055149270
# t1:AC       -0.0418049146  0.117400160
# t1:VC       -0.0817772165  0.020213984
# t2:AC       -0.0585336558  0.033786032
# t2:VC       -0.0221750326  0.036967528
# BD1:AC      -0.0218079385  0.019860785
# BD1:VC      -0.0001208776  0.026573266
# BD2:AC      -0.0276242993  0.014516638
# BD2:VC      -0.0092224846  0.017774172
# t1:BD1:AC   -0.0685185583  0.091505640
# t1:BD1:VC   -0.0398391731  0.062676781
# t1:BD2:AC   -0.2237044397 -0.061584414
# t1:BD2:VC    0.0283677192  0.132226319
# t2:BD1:AC   -0.0186018094  0.075846037
# t2:BD1:VC   -0.0437278599  0.016778058
# t2:BD2:AC   -0.0249850764  0.071573658
# t2:BD2:VC   -0.0503441900  0.011514022
 
###### This will need to be completed to include effects of age and vocabulary
# #Log-Lik
# mdiffABC.noBD1i<-lmer(Elog~1+(t1+t2)*(BD2)+BD1:t1+BD1:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
# mdiffABC.noBD2i<-lmer(Elog~1+(t1+t2)*(BD1)+BD2:t1+BD2:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
# mdiffABC.noBD1l<-lmer(Elog~1+(t1+t2)*(BD2)+BD1+BD1:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
# mdiffABC.noBD2l<-lmer(Elog~1+(t1+t2)*(BD1)+BD2+BD2:t2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
# mdiffABC.noBD1q<-lmer(Elog~1+(t1+t2)*(BD2)+BD1:t1+BD1+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
# mdiffABC.noBD2q<-lmer(Elog~1+(t1+t2)*(BD1)+BD2:t1+BD2+(1+t1+t2|Participant)+(1+t1+t2|Participant:BD1)+(1+t1+t2|Participant:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
# 
# #BD1
# anova(mdiffABC.noBD1i,mdiffABC)#14.504      1  0.0001398 ***
# #BD2
# anova(mdiffABC.noBD2i,mdiffABC)#1.8092      1     0.1786
# #BD1:time
# anova(mdiffABC.noBD1l,mdiffABC)#0.0044      1     0.9472
# #BD2:time
# anova(mdiffABC.noBD2l,mdiffABC)#0.961      1     0.3269
# #BD1:time2
# anova(mdiffABC.noBD1q,mdiffABC)#2.4424      1     0.1181
# #BD2:time2
# anova(mdiffABC.noBD2q,mdiffABC)#0.4775      1     0.4896
# 

##############################################################################
###############################################################################
#### By - participant indeces from the prediction models, separately by contrast

##plot by participant effects
diffbypart<-data.frame(ItemSet=rep(c("CBA-FLAT","ABC-FLAT"),each=430),Contrast=rep(c("A-B","C-B","A-B","C-B"),each=215),PartCode=rep(1:215,4),Value=NA)
head(diffbypart)
#CBA-FLAT, A-B
rCBA_BD1<-ranef(mdiffCBA)$'Participant:BD1'[1]
#select only random effects for "1"
CBA_AB<-rCBA_BD1[c(FALSE,TRUE),]-rCBA_BD1[c(TRUE,FALSE),]+as.numeric(fixef(mdiffCBA)["BD1"])
#CBA-FLAT, C-B
rCBA_BD2<-ranef(mdiffCBA)$'Participant:BD2'[1]
#select only random effects for "1"
CBA_CB<-rCBA_BD2[c(FALSE,TRUE),]-rCBA_BD2[c(TRUE,FALSE),]+as.numeric(fixef(mdiffCBA)["BD2"])


#ABC-FLAT, A-B
rABC_BD1<-ranef(mdiffABC)$'Participant:BD1'[1]
#select only random effects for "1"
ABC_AB<-rABC_BD1[c(FALSE,TRUE),]-rABC_BD1[c(TRUE,FALSE),]+as.numeric(fixef(mdiffABC)["BD1"])
#CBA-FLAT, C-B
rABC_BD2<-ranef(mdiffABC)$'Participant:BD2'[1]
#select only random effects for "1"
ABC_CB<-rABC_BD2[c(FALSE,TRUE),]-rABC_BD2[c(TRUE,FALSE),]+as.numeric(fixef(mdiffABC)["BD2"])

diffbypart$Value<-c(CBA_AB,CBA_CB,ABC_AB,ABC_CB)
summary(diffbypart)

part.info<-summaryBy(time~Participant+Age+BPVS,data=gca,FUN=mean)
part.info<-part.info[,-4]
part.info$PartCode<-c(1:nrow(part.info))
head(part.info)
head(diffbypart)
diffbypart<-merge(diffbypart,part.info,by="PartCode",sort=FALSE)
bySubj_pred_effects<-ggplot(diffbypart,aes(x=PartCode,y=Value,col=Contrast))+geom_linerange(aes(ymin=0,ymax=Value),position="jitter")+facet_wrap(~ItemSet)+coord_flip()
print(bySubj_pred_effects)
ggsave("elog_diffcurve_effects_pred_children.png", plot=bySubj_pred_effects, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#correlations with age
bySubj_pred_effects_age<-ggplot(diffbypart,aes(x=Age,y=Value,col=Contrast))+geom_point(position="jitter")+stat_smooth(method="lm")+facet_wrap(~ItemSet)
print(bySubj_pred_effects_age)
ggsave("elog_diffcurve_effects_pred_children_corrAge.png", plot=bySubj_pred_effects_age, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#correlations with vocabulary
bySubj_pred_effects_voc<-ggplot(diffbypart,aes(x=BPVS,y=Value,col=Contrast))+geom_point(position="jitter")+stat_smooth(method="lm")+facet_wrap(~ItemSet)
print(bySubj_pred_effects_voc)
ggsave("elog_diffcurve_effects_pred_children_corrVoc.png", plot=bySubj_pred_effects_voc, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

##########################################################################################
##########################################################################################

#### By subjects, collapsing across A and C
## analyze
gca<-diff
names(gca)[2]<-"time"
names(gca)[8]<-"Elog"
names(gca)[7]<-"Picture"
head(gca)

gca$PictureP<-"UnPred"
gca$PictureP[gca$Picture=="B"]<-"Mildly Pred"
gca$PictureP[gca$Picture=="A"&gca$Contrast=="ABC-FLAT"]<-"Pred"
gca$PictureP[gca$Picture=="C"&gca$Contrast=="CBA-FLAT"]<-"Pred"

t<-poly(unique(gca$time),2)
time<-as.vector(unique(gca$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gca<-gca[order(gca$time),]
gca$t1<-NA
gca$t2<-NA
for (i in (1:nrow(gca))){
  gca$t1[i]<-t[t$time==gca$time[i],1] 
  gca$t2[i]<-t[t$time==gca$time[i],2] 
}
summary(gca)
gca$PictureP<-as.factor(gca$PictureP)
gca$PictureP<-relevel(gca$PictureP,ref="Mildly Pred")
gca$Pred<-ifelse(gca$PictureP=="Pred", 1,0)
gca$Unpred<-ifelse(gca$PictureP=="UnPred", 1,0)
gca$AC<-scale(gca$Age,T,F)
gca$VC<-scale(gca$BPVS,T,F)

#analyse together
mdiffPredUnpred<-lmer(Elog~1+(t1+t2)*(Pred+Unpred)+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
summary(mdiffPredUnpred)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (Pred + Unpred) + (1 + t1 + t2 | Participant) +  
#   (1 + t1 + t2 | Participant:Pred) + (1 + t1 + t2 | Participant:Unpred)
# Data: gca
# 
# AIC      BIC   logLik deviance df.resid 
# 82311.1  82543.5 -41127.6  82255.1    29642 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.4645 -0.6168  0.0008  0.6215  4.2018 
# 
# Random effects:
#   Groups             Name        Variance  Std.Dev.  Corr       
# Participant:Unpred (Intercept) 2.719e-01 5.215e-01            
# t1          3.758e+00 1.939e+00 -0.01      
# t2          8.023e-01 8.957e-01 -0.53 -0.15
# Participant:Pred   (Intercept) 2.835e-01 5.325e-01            
# t1          3.563e+00 1.888e+00 -0.03      
# t2          8.090e-01 8.994e-01 -0.48 -0.20
# Participant        (Intercept) 1.136e-14 1.066e-07            
# t1          2.893e-11 5.378e-06  1.00      
# t2          1.735e-12 1.317e-06 -0.50 -0.50
# Residual                       7.991e-01 8.939e-01            
# Number of obs: 29670, groups:  Participant:Unpred, 430; Participant:Pred, 430; Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.10045    0.05162  -1.946
# t1          -0.02803    0.18950  -0.148
# t2           0.14954    0.09671   1.546
# Pred         0.45425    0.05291   8.586
# Unpred      -0.10531    0.05188  -2.030
# t1:Pred      0.32435    0.19199   1.689
# t1:Unpred   -0.58410    0.19666  -2.970
# t2:Pred     -0.21161    0.10603  -1.996
# t2:Unpred   -0.33875    0.10573  -3.204
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     Pred   Unpred t1:Prd t1:Unp t2:Prd
# t1        -0.021                                                 
# t2        -0.446 -0.154                                          
# Pred      -0.513  0.016  0.210                                   
# Unpred    -0.502  0.005  0.230  0.029                            
# t1:Pred    0.016 -0.507  0.087 -0.031  0.000                     
# t1:Unpred  0.005 -0.519  0.063  0.000 -0.009  0.049              
# t2:Pred    0.196  0.080 -0.548 -0.382  0.000 -0.158  0.000       
# t2:Unpred  0.211  0.060 -0.547  0.000 -0.420  0.000 -0.116  0.166
confint(mdiffPredUnpred, method="Wald")
# (Intercept) -0.20161502  0.0007211534
# t1          -0.39944362  0.3433774628
# t2          -0.04000033  0.3390896266
# Pred         0.35055490  0.5579508918
# Unpred      -0.20698016 -0.0036333879
# t1:Pred     -0.05195207  0.7006512715
# t1:Unpred   -0.96953861 -0.1986514660
# t2:Pred     -0.41942007 -0.0037944239
# t2:Unpred   -0.54598757 -0.1315165997

mdiffPredUnpred.AV<-lmer(Elog~1+(t1+t2)*(Pred+Unpred)*(AC+VC)+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
summary(mdiffPredUnpred.AV)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (Pred + Unpred) * (AC + VC) + (1 + t1 +  
#                                                                  t2 | Participant) + (1 + t1 + t2 | Participant:Pred) + (1 +  
#                                                                                                                            t1 + t2 | Participant:Unpred)
# Data: gca
# 
# AIC      BIC   logLik deviance df.resid 
# 82307.6  82689.3 -41107.8  82215.6    29624 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.4339 -0.6178  0.0007  0.6214  4.1905 
# 
# Random effects:
#   Groups             Name        Variance  Std.Dev.  Corr       
# Participant:Unpred (Intercept) 2.676e-01 5.173e-01            
# t1          3.712e+00 1.927e+00 -0.02      
# t2          7.726e-01 8.790e-01 -0.54 -0.13
# Participant:Pred   (Intercept) 2.676e-01 5.173e-01            
# t1          3.521e+00 1.876e+00 -0.05      
# t2          7.905e-01 8.891e-01 -0.49 -0.19
# Participant        (Intercept) 0.000e+00 0.000e+00            
# t1          1.737e-10 1.318e-05   NaN      
# t2          9.433e-12 3.071e-06   NaN -0.80
# Residual                       7.993e-01 8.941e-01            
# Number of obs: 29670, groups:  
#   Participant:Unpred, 430; Participant:Pred, 430; Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)  -0.1004469  0.0506975  -1.981
# t1           -0.0280331  0.1884187  -0.149
# t2            0.1495446  0.0955448   1.565
# Pred          0.4542529  0.0514904   8.822
# Unpred       -0.1053068  0.0514865  -2.045
# AC           -0.0006863  0.0082401  -0.083
# VC           -0.0034199  0.0052788  -0.648
# t1:Pred       0.3243496  0.1909728   1.698
# t1:Unpred    -0.5840950  0.1955774  -2.987
# t2:Pred      -0.2116072  0.1052185  -2.011
# t2:Unpred    -0.3387521  0.1044249  -3.244
# t1:AC         0.0223921  0.0306244   0.731
# t1:VC        -0.0219892  0.0196188  -1.121
# t2:AC        -0.0180530  0.0155293  -1.163
# t2:VC         0.0151666  0.0099485   1.525
# Pred:AC       0.0101972  0.0083689   1.218
# Pred:VC       0.0076779  0.0053614   1.432
# Unpred:AC    -0.0117590  0.0083683  -1.405
# Unpred:VC     0.0055557  0.0053610   1.036
# t1:Pred:AC   -0.0286781  0.0310395  -0.924
# t1:Pred:VC    0.0321254  0.0198848   1.616
# t1:Unpred:AC -0.0520459  0.0317879  -1.637
# t1:Unpred:VC  0.0345629  0.0203642   1.697
# t2:Pred:AC    0.0266322  0.0171016   1.557
# t2:Pred:VC   -0.0211972  0.0109557  -1.935
# t2:Unpred:AC  0.0418288  0.0169726   2.464
# t2:Unpred:VC -0.0320305  0.0108731  -2.946
confint(mdiffPredUnpred.AV, method="Wald")
# (Intercept)  -0.199812224 -0.0010816465
# t1           -0.397327029  0.3412608749
# t2           -0.037719726  0.3368090190
# Pred          0.353333631  0.5551721567
# Unpred       -0.206218522 -0.0043950221
# AC           -0.016836525  0.0154638927
# VC           -0.013766158  0.0069263882
# t1:Pred      -0.049950117  0.6986493223
# t1:Unpred    -0.967419700 -0.2007703785
# t2:Pred      -0.417831689 -0.0053828003
# t2:Unpred    -0.543421082 -0.1340830869
# t1:AC        -0.037630645  0.0824147857
# t1:VC        -0.060441447  0.0164629962
# t2:AC        -0.048489767  0.0123837785
# t2:VC        -0.004332034  0.0346652528
# Pred:AC      -0.006205591  0.0265999734
# Pred:VC      -0.002830140  0.0181860171
# Unpred:AC    -0.028160521  0.0046426010
# Unpred:VC    -0.004951586  0.0160630068
# t1:Pred:AC   -0.089514421  0.0321582211
# t1:Pred:VC   -0.006848041  0.0710988391
# t1:Unpred:AC -0.114349059  0.0102572977
# t1:Unpred:VC -0.005350247  0.0744760523
# t2:Pred:AC   -0.006886236  0.0601506115
# t2:Pred:VC   -0.042670025  0.0002756445
# t2:Unpred:AC  0.008563208  0.0750944300
# t2:Unpred:VC -0.053341418 -0.0107196664

#### This will have to be completed to include effects of age and vocabulary.
# #log-lik
# # Pred
# mdiffPredUnpred.noi<-lmer(Elog~1+(t1+t2)*(Unpred)+t1:Pred+t2:Pred+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
# mdiffPredUnpred.nol<-lmer(Elog~1+(t1+t2)*(Unpred)+Pred+t2:Pred+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
# mdiffPredUnpred.noq<-lmer(Elog~1+(t1+t2)*(Unpred)+t1:Pred+Pred+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
# 
# anova(mdiffPredUnpred.noi,mdiffPredUnpred)#31.021      1  2.553e-08 ***
# anova(mdiffPredUnpred.nol,mdiffPredUnpred)#0.882      1     0.3477
# anova(mdiffPredUnpred.noq,mdiffPredUnpred)#2.3155      1     0.1281
# 
# # Unpred
# mdiffPredUnpred.noi2<-lmer(Elog~1+(t1+t2)*(Pred)+t1:Unpred+t2:Unpred+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
# mdiffPredUnpred.nol2<-lmer(Elog~1+(t1+t2)*(Pred)+Unpred+t2:Unpred+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
# mdiffPredUnpred.noq2<-lmer(Elog~1+(t1+t2)*(Pred)+t1:Unpred+Unpred+(1+t1+t2|Participant)+(1+t1+t2|Participant:Pred)+(1+t1+t2|Participant:Unpred),data=gca,REML=F)
# 
# anova(mdiffPredUnpred.noi2,mdiffPredUnpred)#7.215      1    0.00723 **
# anova(mdiffPredUnpred.nol2,mdiffPredUnpred)#3.5734      1    0.05871 .
# anova(mdiffPredUnpred.noq2,mdiffPredUnpred)#0.3735      1     0.5411


###plot by participant effects (intercept)
diffbypart<-data.frame(Contrast=rep(c("Pred"),each=215),PartCode=rep(1:215,1),Value=NA)
head(diffbypart)
#Pred intercept
Pred_i<-ranef(mdiffPredUnpred)$'Participant:Pred'[1]
#select only random effects for "1", that is for 
#Pred<-Pred_i[c(FALSE,TRUE),]+as.numeric(fixef(mdiffPredUnpred_noA)["Pred"])
# Given Pred as a fixed effects is a difference between Predictive (1) and Neutral (0, the reference level),
# it is more appropriate to take the difference between the ranef for 1 and 0 as in Predn below
# this is equivalent to Predn2, confirming my interpretation.

Predn<-(Pred_i[c(FALSE,TRUE),]-Pred_i[c(TRUE,FALSE),])+as.numeric(fixef(mdiffPredUnpred)["Pred"])
#Predn2<-Pred_i[c(FALSE,TRUE),]+as.numeric(fixef(mdiffPredUnpred)["Pred"])+as.numeric(fixef(mdiffPredUnpred)["(Intercept)"]) - (as.numeric(fixef(mdiffPredUnpred)["(Intercept)"])+Pred_i[c(TRUE,FALSE),])
# use Predn
diffbypart$Value<-Predn
summary(diffbypart)

part.info<-summaryBy(time~Participant+Age+BPVS,data=gca,FUN=mean)
part.info<-part.info[,-4]
part.info$PartCode<-c(1:nrow(part.info))
head(part.info)
diffbypart<-merge(diffbypart,part.info,by="PartCode",sort=FALSE)

#correlation with vocabulary
bySubj_pred_effects_voc_comb<-ggplot(diffbypart,aes(x=BPVS,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between vocabulary size and prediction index (intercept)")+ylab("Prediction index (Highly-Mildly predictable, intercept")+xlab("raw BPVS score")
print(bySubj_pred_effects_voc_comb)
ggsave("elog_diffcurve_effects_pred_comb_children_corrVoc.png", plot=bySubj_pred_effects_voc_comb, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")


#correlations with age
bySubj_pred_effects_age_comb<-ggplot(diffbypart,aes(x=Age,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and prediction index (intercept)")+ylab("Prediction index (Highly-Mildly predictable, intercept")+xlab("Age in months")
print(bySubj_pred_effects_age_comb)
ggsave("elog_diffcurve_effects_pred_comb_children_corrAge.png", plot=bySubj_pred_effects_age_comb, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")


# This is the prediction index
# That is the Pred effect collapsing across ABC and CBA contexts (and A and C pictures)
# The Pred effect is a difference on the intercept
# computed as Predictve - Neutral
# In practice, for each participatn we sum the fixed effect
# to the random effect, that is the 1 in
# (1+t1+t2|Participant:Pred)
# (in practice, two random effects are computed for the reference level (neutral)
# and the comparison level (predictive) of the dummy factor Pred)
# since the fixed effects is a difference, I also take the difference in the random effects
# I have checked this is the same as summing fixed effect and random effect separately for each level, and only then taking the difference.

# save the prediction indeces to file
write.table(diffbypart,"prediction_indeces.txt",row.names=F)

# regression
Pred.AV<-lm(Value~Age+BPVS,data=diffbypart)
summary(Pred.AV)
# Call:
#   lm(formula = Value ~ Age + BPVS, data = diffbypart)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.24176 -0.56026  0.07659  0.59730  2.40572 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.257413   0.252306  -1.020    0.309
# Age          0.009444   0.009259   1.020    0.309
# BPVS         0.007272   0.005932   1.226    0.222
# 
# Residual standard error: 0.8353 on 212 degrees of freedom
# Multiple R-squared:  0.05706,	Adjusted R-squared:  0.04816 
# F-statistic: 6.414 on 2 and 212 DF,  p-value: 0.001974

# only age
Pred.A<-lm(Value~Age,data=diffbypart)
summary(Pred.A)# 0.018561   0.005522   3.361 0.000919 ***

#only voc
Pred.V<-lm(Value~BPVS,data=diffbypart)
summary(Pred.V)#0.012131   0.003534   3.433 0.000717 ***

cor.test(diffbypart$Age,diffbypart$BPVS)
# Pearson's product-moment correlation
# 
# data:  diffbypart$Age and diffbypart$BPVS
# t = 19.68, df = 213, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.7500291 0.8460942
# sample estimates:
# cor 
# 0.8032239

# now the Unpredictability Index (i.e., size of the graded prediction effect)
diffbypart2<-data.frame(Contrast=rep(c("UnPred"),each=215),PartCode=rep(1:215,1),Value=NA)
head(diffbypart2)
#UnPred intercept
Unpred_i<-ranef(mdiffPredUnpred)$'Participant:Unpred'[1]

Unpredn<-(Unpred_i[c(FALSE,TRUE),]-Unpred_i[c(TRUE,FALSE),])+as.numeric(fixef(mdiffPredUnpred)["Unpred"])
diffbypart2$Value<-Unpredn
summary(diffbypart2)

part.info<-summaryBy(time~Participant+Age+BPVS,data=gca,FUN=mean)
part.info<-part.info[,-4]
part.info$PartCode<-c(1:nrow(part.info))
head(part.info)
diffbypart2<-merge(diffbypart2,part.info,by="PartCode",sort=FALSE)

#correlation with vocabulary
bySubj_unpred_effects_voc_comb<-ggplot(diffbypart2,aes(x=BPVS,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between vocabulary size and prediction index (intercept)")+ylab("Prediction index (Mildly- Unpredictable, intercept")+xlab("raw BPVS score")
print(bySubj_unpred_effects_voc_comb)
ggsave("elog_diffcurve_effects_unpred_comb_children_corrVoc.png", plot=bySubj_unpred_effects_voc_comb, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")


#correlations with age
bySubj_unpred_effects_age_comb<-ggplot(diffbypart2,aes(x=Age,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and prediction index (intercept)")+ylab("Prediction index (Mildly- Unpredictable, intercept")+xlab("Age in months")
print(bySubj_unpred_effects_age_comb)
ggsave("elog_diffcurve_effects_unpred_comb_children_corrAge.png", plot=bySubj_unpred_effects_age_comb, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")


write.table(diffbypart2,"prediction_indeces_mildy-unpred.txt",row.names=F)

# regression
UnPred.AV<-lm(Value~Age+BPVS,data=diffbypart2)
summary(UnPred.AV)
# Call:
#   lm(formula = Value ~ Age + BPVS, data = diffbypart2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5536 -0.5149 -0.0424  0.5737  3.2038 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.175493   0.251905   0.697    0.487
# Age         -0.011831   0.009245  -1.280    0.202
# BPVS         0.005347   0.005922   0.903    0.368
# 
# Residual standard error: 0.834 on 212 degrees of freedom
# Multiple R-squared:  0.007871,	Adjusted R-squared:  -0.001489 
# F-statistic: 0.841 on 2 and 212 DF,  p-value: 0.4327

# only age
UnPred.A<-lm(Value~Age,data=diffbypart2)
summary(UnPred.A)#-0.005127   0.005504  -0.931    0.353

#only voc
UnPred.V<-lm(Value~BPVS,data=diffbypart2)
summary(UnPred.V)#BPVS        -0.0007408  0.0035331  -0.210    0.834

####################
## BY ITEMS ########
####################
byItem_tow<-as.data.frame(byItem[,c(1:3,13:15)])
byItem.w<-reshape(byItem_tow,v.names=c("elogA","elogB","elogC"),timevar=c("Bias"),idvar=c("ItemF","time"),direction="wide")
## compute differences
byItem.w$A.CBA<-byItem.w$elogA.CBA-byItem.w$elogA.FLAT
byItem.w$B.CBA<-byItem.w$elogB.CBA-byItem.w$elogB.FLAT
byItem.w$C.CBA<-byItem.w$elogC.CBA-byItem.w$elogC.FLAT

byItem.w$A.ABC<-byItem.w$elogA.ABC-byItem.w$elogA.FLAT
byItem.w$B.ABC<-byItem.w$elogB.ABC-byItem.w$elogB.FLAT
byItem.w$C.ABC<-byItem.w$elogC.ABC-byItem.w$elogC.FLAT

names(byItem.w)
diff.I<-byItem.w[,c(1:2,12:17)]
names(diff.I)[2]<-"Time.ms"

##turn to long format
diff.I<-reshape(diff.I,varying=names(diff.I)[3:8],direction="long")
names(diff.I)[3]<-"Contrast"
names(diff.I)[4:6]<-c("Picture.A","Picture.B","Picture.C")
diff.I<-diff.I[,-7]
diff.I<-reshape(diff.I,varying=names(diff)[4:6],direction="long")

#analyze
gca<-as.data.frame(diff.I)
names(gca)[2]<-"time"
names(gca)[5]<-"Elog"
names(gca)[4]<-"Picture"
head(gca)

t<-poly(unique(gca$time),2)
time<-as.vector(unique(gca$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gca<-gca[order(gca$time),]
gca$t1<-NA
gca$t2<-NA
for (i in (1:nrow(gca))){
  gca$t1[i]<-t[t$time==gca$time[i],1] 
  gca$t2[i]<-t[t$time==gca$time[i],2] 
}
summary(gca)
gca$Picture<-as.factor(gca$Picture)
gca$Picture<-relevel(gca$Picture,ref="B")
gca$BD1<-ifelse(gca$Picture=="A", 1,0)
gca$BD2<-ifelse(gca$Picture=="C", 1,0)
# cannot include age or voc
#analyse teh two contrasts separately
library(lme4)
mdiffCBA.I<-lmer(Elog~1+(t1+t2)*(BD1+BD2)+(1+t1+t2|ItemF)+(1+t1+t2|ItemF:BD1)+(1+t1+t2|ItemF:BD2),data=subset(gca,Contrast=="CBA"),REML=F)
summary(mdiffCBA.I) # only Pred-mildly!
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (BD1 + BD2) + (1 + t1 + t2 | ItemF) +      (1 + t1 + t2 | ItemF:BD1) + (1 + t1 + t2 | ItemF:BD2)
# Data: subset(gca, Contrast == "CBA")
# 
# AIC      BIC   logLik deviance df.resid 
# -395.6   -257.2    225.8   -451.6     1007 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7728 -0.6271 -0.0287  0.6582  2.8561 
# 
# Random effects:
#   Groups    Name        Variance  Std.Dev.  Corr       
# ItemF:BD2 (Intercept) 1.048e-01 3.237e-01            
# t1          1.101e+00 1.049e+00  0.02      
# t2          3.607e-01 6.006e-01 -0.13 -0.35
# ItemF:BD1 (Intercept) 9.653e-02 3.107e-01            
# t1          1.455e+00 1.206e+00  0.12      
# t2          3.806e-01 6.169e-01 -0.25  0.16
# ItemF     (Intercept) 8.496e-15 9.217e-08            
# t1          1.687e-11 4.107e-06 -0.30      
# t2          4.250e-13 6.519e-07  0.47 -0.98
# Residual              2.155e-02 1.468e-01            
# Number of obs: 1035, groups:  ItemF:BD2, 30; ItemF:BD1, 30; ItemF, 15
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.16107    0.11612  -1.387
# t1           0.08325    0.41455   0.201
# t2           0.17254    0.22551   0.765
# BD1         -0.16715    0.11400  -1.466
# BD2          0.69192    0.11872   5.828
# t1:BD1      -0.67105    0.44369  -1.512
# t1:BD2       0.05054    0.38693   0.131
# t2:BD1      -0.28266    0.23155  -1.221
# t2:BD2      -0.40688    0.22576  -1.802
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     BD1    BD2    t1:BD1 t1:BD2 t2:BD1
# t1      0.072                                                 
# t2     -0.187 -0.076                                          
# BD1    -0.491 -0.064  0.126                                   
# BD2    -0.511 -0.009  0.062  0.005                            
# t1:BD1 -0.058 -0.535 -0.077  0.119  0.000                     
# t1:BD2 -0.010 -0.467  0.170  0.000  0.020  0.008              
# t2:BD1  0.120 -0.080 -0.513 -0.245  0.000  0.150  0.000       
# t2:BD2  0.063  0.159 -0.501  0.000 -0.124  0.000 -0.340  0.027
confint(mdiffCBA.I, method="Wald")
# (Intercept) -0.3886534 0.06651224
# t1          -0.7292475 0.89574778
# t2          -0.2694510 0.61453271
# BD1         -0.3905808 0.05628923
# BD2          0.4592338 0.92461342
# t1:BD1      -1.5406689 0.19856048
# t1:BD2      -0.7078189 0.80890446
# t2:BD1      -0.7364898 0.17116153
# t2:BD2      -0.8493631 0.03560746

mdiffABC.I<-lmer(Elog~1+(t1+t2)*(BD1+BD2)+(1+t1+t2|ItemF)+(1+t1+t2|ItemF:BD1)+(1+t1+t2|ItemF:BD2),data=subset(gca,Contrast=="ABC"),REML=F)
summary(mdiffABC.I)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (BD1 + BD2) + (1 + t1 + t2 | ItemF) +      (1 + t1 + t2 | ItemF:BD1) + (1 + t1 + t2 | ItemF:BD2)
# Data: subset(gca, Contrast == "ABC")
# 
# AIC      BIC   logLik deviance df.resid 
# -252.8   -114.4    154.4   -308.8     1007 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.2889 -0.5747 -0.0100  0.6156  2.9100 
# 
# Random effects:
#   Groups    Name        Variance  Std.Dev.  Corr       
# ItemF:BD2 (Intercept) 9.064e-02 3.011e-01            
# t1          9.215e-01 9.599e-01  0.22      
# t2          7.867e-01 8.870e-01 -0.31  0.34
# ItemF:BD1 (Intercept) 7.505e-02 2.740e-01            
# t1          1.010e+00 1.005e+00 -0.30      
# t2          5.712e-01 7.558e-01 -0.22  0.43
# ItemF     (Intercept) 9.281e-15 9.634e-08            
# t1          8.671e-13 9.312e-07 -1.00      
# t2          2.757e-12 1.661e-06 -0.60  0.60
# Residual              2.534e-02 1.592e-01            
# Number of obs: 1035, groups:  ItemF:BD2, 30; ItemF:BD1, 30; ItemF, 15
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.100165   0.105448  -0.950
# t1          -0.113851   0.361216  -0.315
# t2           0.073592   0.303677   0.242
# BD1          0.359087   0.100764   3.564
# BD2         -0.068210   0.110599  -0.617
# t1:BD1       0.703726   0.371607   1.894
# t1:BD2      -0.715684   0.355305  -2.014
# t2:BD1      -0.009843   0.282030  -0.035
# t2:BD2      -0.410520   0.329056  -1.248
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     BD1    BD2    t1:BD1 t1:BD2 t2:BD1
# t1     -0.034                                                 
# t2     -0.270  0.374                                          
# BD1    -0.478  0.151  0.101                                   
# BD2    -0.524 -0.105  0.165  0.007                            
# t1:BD1  0.140 -0.514 -0.195 -0.293  0.000                     
# t1:BD2 -0.112 -0.492 -0.176  0.000  0.213  0.013              
# t2:BD1  0.104 -0.216 -0.464 -0.218  0.000  0.420  0.000       
# t2:BD2  0.159 -0.160 -0.542  0.000 -0.304  0.000  0.325  0.018
confint(mdiffABC.I,method="Wald")
# (Intercept) -0.30683926  0.10650943
# t1          -0.82182221  0.59411992
# t2          -0.52160445  0.66878839
# BD1          0.16159254  0.55658139
# BD2         -0.28497933  0.14855918
# t1:BD1      -0.02461153  1.43206306
# t1:BD2      -1.41206920 -0.01929966
# t2:BD1      -0.56261125  0.54292620
# t2:BD2      -1.05545804  0.23441824

# combined
gca<-diff.I
names(gca)[2]<-"time"
names(gca)[5]<-"Elog"
names(gca)[4]<-"Picture"
head(gca)

gca$PictureP<-"UnPred"
gca$PictureP[gca$Picture=="B"]<-"Mildly Pred"
gca$PictureP[gca$Picture=="A"&gca$Contrast=="ABC"]<-"Pred"
gca$PictureP[gca$Picture=="C"&gca$Contrast=="CBA"]<-"Pred"

t<-poly(unique(gca$time),2)
time<-as.vector(unique(gca$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gca<-gca[order(gca$time),]
gca$t1<-NA
gca$t2<-NA
for (i in (1:nrow(gca))){
  gca$t1[i]<-t[t$time==gca$time[i],1] 
  gca$t2[i]<-t[t$time==gca$time[i],2] 
}
summary(gca)
gca$PictureP<-as.factor(gca$PictureP)
gca$PictureP<-relevel(gca$PictureP,ref="Mildly Pred")
gca$Pred<-ifelse(gca$PictureP=="Pred", 1,0)
gca$Unpred<-ifelse(gca$PictureP=="UnPred", 1,0)
#analyse together
mdiffPredUnpred.I<-lmer(Elog~1+(t1+t2)*(Pred+Unpred)+(1+t1+t2|ItemF)+(1+t1+t2|ItemF:Pred)+(1+t1+t2|ItemF:Unpred),data=gca,REML=F)
summary(mdiffPredUnpred.I)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: Elog ~ 1 + (t1 + t2) * (Pred + Unpred) + (1 + t1 + t2 | ItemF) +  
#   (1 + t1 + t2 | ItemF:Pred) + (1 + t1 + t2 | ItemF:Unpred)
# Data: gca
# 
# AIC      BIC   logLik deviance df.resid 
# 2403.9   2561.7  -1174.0   2347.9     2042 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.6877 -0.5355  0.0129  0.5618  3.8870 
# 
# Random effects:
#   Groups       Name        Variance  Std.Dev.  Corr       
# ItemF:Unpred (Intercept) 3.236e-02 1.799e-01            
# t1          5.489e-01 7.409e-01 -0.12      
# t2          2.314e-01 4.810e-01 -0.65  0.43
# ItemF:Pred   (Intercept) 4.419e-02 2.102e-01            
# t1          3.629e-01 6.024e-01 -0.26      
# t2          1.319e-01 3.632e-01 -0.38  0.52
# ItemF        (Intercept) 0.000e+00 0.000e+00            
# t1          1.605e-12 1.267e-06  NaN       
# t2          1.446e-13 3.802e-07  NaN  0.45 
# Residual                 1.583e-01 3.978e-01            
# Number of obs: 2070, groups:  ItemF:Unpred, 30; ItemF:Pred, 30; ItemF, 15
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.13062    0.07302  -1.789
# t1          -0.01530    0.25702  -0.060
# t2           0.12307    0.17174   0.717
# Pred         0.52551    0.07969   6.594
# Unpred      -0.11768    0.06909  -1.703
# t1:Pred      0.37713    0.24277   1.553
# t1:Unpred   -0.69337    0.28937  -2.396
# t2:Pred     -0.20836    0.16773  -1.242
# t2:Unpred   -0.34659    0.20348  -1.703
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     Pred   Unpred t1:Prd t1:Unp t2:Prd
# t1        -0.173                                                 
# t2        -0.454  0.404                                          
# Pred      -0.546  0.108  0.141                                   
# Unpred    -0.473  0.058  0.317  0.042                            
# t1:Pred    0.125 -0.472 -0.182 -0.229  0.000                     
# t1:Unpred  0.049 -0.563 -0.206  0.000 -0.103  0.075              
# t2:Pred    0.158 -0.176 -0.488 -0.289  0.000  0.372  0.000       
# t2:Unpred  0.253 -0.196 -0.592  0.000 -0.536  0.000  0.348  0.155

confint(mdiffPredUnpred.I, method="Wald")
# (Intercept) -0.27374155  0.01250607
# t1          -0.51905647  0.48845549
# t2          -0.21353128  0.45966413
# Pred         0.36931557  0.68169498
# Unpred      -0.25308757  0.01773172
# t1:Pred     -0.09867658  0.85294512
# t1:Unpred   -1.26053205 -0.12620658
# t2:Pred     -0.53711458  0.12039424
# t2:Unpred   -0.74539765  0.05221361

##### need to do LogLik for the by-items analyses

####################
#### Recognition or Integration Graph
####################
alr<-al
# Zero at noun onset +100
# noun onset varies by Item and Condition and is listed in the variable TargetOnset
# plotting from Target Onset + 100 to about TotDur+1000
#mean(time$TotDur-time$TargetOnset)*1000
# on average End of sentence is about 630 ms after TargetOnset
# so TotDur+1000 is about TargetOnset+1650

####################################################################################
##### Run the following code to restrict recognition window analyses to trials were participants were not looking at B at the onset of the recognition window
## If a fixation was NOT ongoing between 0ms (i.e. Target Onset) and 100 ms mark it as "NOHIT"
summary(alr)
alr$FIXATONSET<-as.character(alr$FixLoc)
alr$FIXATONSET[(alr$FixationStart<alr$TargetOnset&alr$FixationEnd<alr$TargetOnset)]<-"NOHIT"
alr$FIXATONSET[(alr$FixationStart>(alr$TargetOnset+100)&alr$FixationEnd>(alr$TargetOnset+100))]<-"NOHIT"
summary(as.factor(alr$FIXATONSET))

## Now create variable that tells you if FIXATONSET==B
alr$DISCARDBONSET<-0
alr$DISCARDBONSET[alr$FIXATONSET=="B"]<-1

## Now create variable that tells you if FIXATONSET==A
alr$DISCARDAONSET<-0
alr$DISCARDAONSET[alr$FIXATONSET=="A"]<-1

## Now create variable that tells you if FIXATONSET==C
alr$DISCARDCONSET<-0
alr$DISCARDCONSET[alr$FIXATONSET=="C"]<-1
####################################################################################

for (i in (2:33)){
  k<-paste(50*i,sep="")
  j=i-1
  alr$x<-alr$FixLoc
  alr$x[(alr$FixationStart<(alr$TargetOnset + 50*j) & alr$FixationEnd< (alr$TargetOnset + 50*j))|(alr$FixationStart>(alr$TargetOnset + 50*i) & alr$FixationEnd> (alr$TargetOnset + 50*i))]<-NA
  i=i+1
  alr$x<-as.factor(alr$x)
  names(alr)[names(alr)=="x"]<-k
}

summary(alr)


#change to long format
names(alr)[79:110]
library(reshape2)

alr.l<-melt(alr,id.vars=names(alr)[1:78])

alr.l$AFIXBIN<-0
alr.l$BFIXBIN<-0
alr.l$CFIXBIN<-0
alr.l$BackgroundFIXBIN<-0
alr.l$AFIXBIN[is.na(alr.l$value)==F&alr.l$value=="A"]<-1
alr.l$BFIXBIN[is.na(alr.l$value)==F&alr.l$value=="B"]<-1
alr.l$CFIXBIN[is.na(alr.l$value)==F&alr.l$value=="C"]<-1
alr.l$BackgroundFIXBIN[is.na(alr.l$value)==F&alr.l$value=="Background"]<-1

alr.l$time<-as.numeric(as.character(alr.l$variable))

# find number of rows with fixations in more than one column
alr.l$SUM<-alr.l$AFIXBIN+alr.l$BFIXBIN+alr.l$CFIXBIN
nrow(alr.l[alr.l$SUM>1,])
range(alr.l$SUM, na.rm=T)# never more than one picture fixated at the same time

#Discard irrelevant fixations
alr.lD<-alr.l[is.na(alr.l$DURPOSTDO2)==F,]
library(doBy)
fixsumbin2<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN + DURPOSTDO2+DISCARDBONSET+DISCARDAONSET+DISCARDCONSET ~ time + Participant + Trial + ItemF+ Named+Bias+Block+TargetOnset+TotDur+Age+BPVS+AGEYEAR, data=alr.lD, FUN = sum, keep.names=T, na.rm=T)

fixsumbin2$AFIXBIN[fixsumbin2$AFIXBIN>1]<-1
fixsumbin2$BFIXBIN[fixsumbin2$BFIXBIN>1]<-1
fixsumbin2$CFIXBIN[fixsumbin2$CFIXBIN>1]<-1
fixsumbin2$BackgroundFIXBIN[fixsumbin2$BackgroundFIXBIN>1]<-1

range(fixsumbin2$AFIXBIN, na.rm=T) # 0 to 1 
range(fixsumbin2$BFIXBIN, na.rm=T) # 0 to 1
range(fixsumbin2$CFIXBIN, na.rm=T) # 0 to 1 
range(fixsumbin2$BackgroundFIXBIN, na.rm=T) # 0 to 1

## apply %40 rejection criterion
fixsumbin2$Perc_Dur<-fixsumbin2$DURPOSTDO2*100/(fixsumbin2$TotDur+1000-(fixsumbin2$TargetOnset+100))
fixsumbin2$DiscardTrial<-0
fixsumbin2$DiscardTrial[fixsumbin2$Perc_Dur<40]<-1
fixsumbin2$DiscardTrial[fixsumbin2$DiscardTrial>1]<-1

fixsumbin2<-fixsumbin2[fixsumbin2$DiscardTrial==0,]
write.table(fixsumbin2,"children_rec_bytrial-cleaned.txt",row.names=F)

fixsumbin2<-read.table("children_rec_bytrial-cleaned.txt",header=T)
###now discard trials were participants were looking at B at Onset (i.e DISSCARDBONSET>=1)
summary(fixsumbin2)
fixsumbin3<-fixsumbin2[fixsumbin2$DISCARDBONSET==0,]
write.table(fixsumbin3,"children_rec_bytrial-cleaned_noBonset.txt",row.names=F)

###now discard trials were participants were looking at A at Onset (i.e DISSCARDAONSET>=1)
summary(fixsumbin2)
fixsumbin4<-fixsumbin2[fixsumbin2$DISCARDAONSET==0,]
write.table(fixsumbin4,"children_rec_bytrial-cleaned_noAonset.txt",row.names=F)

###now discard trials were participants were looking at C at Onset (i.e DISSCARDCONSET>=1)
summary(fixsumbin2)
fixsumbin5<-fixsumbin2[fixsumbin2$DISCARDCONSET==0,]
write.table(fixsumbin5,"children_rec_bytrial-cleaned_noConset.txt",row.names=F)

#by subjects
fixpropbin2<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN  ~ time + Participant + Named+Bias + Block+AGEYEAR+Age+BPVS, data=subset(fixsumbin2,BackgroundFIXBIN==0), FUN = mean, keep.names=T, na.rm=T)
fixpropbin3<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN  ~ time + Participant + Named+Bias + Block+AGEYEAR+Age+BPVS, data=subset(fixsumbin3,BackgroundFIXBIN==0), FUN = mean, keep.names=T, na.rm=T)
fixpropbin4<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN  ~ time + Participant + Named+Bias + Block+AGEYEAR+Age+BPVS, data=subset(fixsumbin4,BackgroundFIXBIN==0), FUN = mean, keep.names=T, na.rm=T)
fixpropbin5<-summaryBy(AFIXBIN + BFIXBIN + CFIXBIN + BackgroundFIXBIN  ~ time + Participant + Named+Bias + Block+AGEYEAR+Age+BPVS, data=subset(fixsumbin5,BackgroundFIXBIN==0), FUN = mean, keep.names=T, na.rm=T)

require(ggplot2)

#excluding looks to B at onset
fixpropbinB2<-summaryBy(BFIXBIN ~ time + Named+ Bias + AGEYEAR+Block, data=fixpropbin3, FUN = mean, keep.names=T, na.rm=T)
fixpropbinB_pred2<-fixpropbinB2[fixpropbinB2$Named=="B",]
fixpropbinB_pred2<-fixpropbinB_pred2[fixpropbinB_pred2$Block=="First",]
fixpropbinB_pred2$Pred<-"Unexpected"
fixpropbinB_pred2$Pred[fixpropbinB_pred2$Bias=="FLAT"]<-"Neutral"
qplot(time,BFIXBIN,data=fixpropbinB_pred2,color=Bias,linetype=Pred,geom="line",ylim=c(0,1))+facet_wrap(~AGEYEAR)+ggtitle("Children")

## graph with CIs
## taken from Micheal Frank Github
library(bootstrap)
library(lme4)
library(stringr)
library(lubridate)
require(longitudinalData)
# library(plyr) # first load plyr then dplyr from https://github.com/hadley/dplyr
#library(dplyr)
#library(tidyr)

## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}

ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

fixpropbinB2<-summaryBy(BFIXBIN ~ time + Named+ Bias + Block+AGEYEAR, data=fixpropbin3, FUN = c(mean,ci.high,ci.low), keep.names=T, na.rm=T)
fixpropbinB_pred2<-fixpropbinB2[fixpropbinB2$Named=="B",]
fixpropbinB_pred2$Predictability<-"Unexpected"
fixpropbinB_pred2$Predictability[fixpropbinB_pred2$Bias=="FLAT"]<-"Neutral"
fixpropbinB_pred2<-fixpropbinB_pred2[fixpropbinB_pred2$Block=="First",]
names(fixpropbinB_pred2)[3]<-"Context"
fixpropbinB_pred2$Context<-factor(fixpropbinB_pred2$Context,labels=c("A-biasing","C-biasing","Neutral"))
rec_cost<-ggplot(fixpropbinB_pred2,aes(x=time,y=BFIXBIN.mean,color=Context,linetype=Context))+geom_line(stat = "identity", size = 1) + geom_linerange(aes(ymin= (BFIXBIN.mean - BFIXBIN.ci.low), ymax= (BFIXBIN.mean + BFIXBIN.ci.high)), position = position_dodge(width=.9), size =0.5)+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to B")+xlab("Time.ms")+facet_wrap(~AGEYEAR)
print(rec_cost)
ggsave("prop_growthcurve.CI_recognition_B_children.png", plot=rec_cost, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

# points and loess function
fixpropbinB_pred2<-fixpropbin3[fixpropbin3$Named=="B",]
fixpropbinB_pred2$Predictability<-"Unexpected"
fixpropbinB_pred2$Predictability[fixpropbinB_pred2$Bias=="FLAT"]<-"Neutral"
fixpropbinB_pred2<-fixpropbinB_pred2[fixpropbinB_pred2$Block=="First",]
names(fixpropbinB_pred2)[4]<-"Context"
fixpropbinB_pred2$Context<-factor(fixpropbinB_pred2$Context,labels=c("A-biasing","C-biasing","Neutral"))

rec_cost_loess<-ggplot(fixpropbinB_pred2,aes(x=time,y=BFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess") +ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to B")+xlab("Time.ms")+facet_wrap(~AGEYEAR)
print(rec_cost_loess)
ggsave("prop_growthcurve.loess_recognition_B_children.png", plot=rec_cost_loess, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

## by median split of age
median(fixpropbinB_pred2$Age) #44
fixpropbinB_pred2$AgeM<-"Below median (44)"
fixpropbinB_pred2$AgeM[fixpropbinB_pred2$Age>44]<-"Above median (44)"

rec_cost_loess.ageM<-ggplot(fixpropbinB_pred2,aes(x=time,y=BFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess") +ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to B")+xlab("Time.ms")+facet_wrap(~AgeM)
print(rec_cost_loess.ageM)
ggsave("prop_growthcurve.loess_recognition_B_children_medianAge.png", plot=rec_cost_loess.ageM, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

## by median split of voc
median(fixpropbinB_pred2$BPVS) #42
fixpropbinB_pred2$VocM<-"Below median (42)"
fixpropbinB_pred2$VocM[fixpropbinB_pred2$BPVS>42]<-"Above median (42)"

rec_cost_loess.VocM<-ggplot(fixpropbinB_pred2,aes(x=time,y=BFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess") +ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to B")+xlab("Time.ms")+facet_wrap(~VocM)
print(rec_cost_loess.VocM)
ggsave("prop_growthcurve.loess_recognition_B_children_medianVoc.png", plot=rec_cost_loess.VocM, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

## by quartile of vocabulary
summary(fixpropbinB_pred2$BPVS)
fixpropbinB_pred2$Voc2<-"First quartile (2-30)"
fixpropbinB_pred2$Voc2[fixpropbinB_pred2$BPVS>30]<-"Interquartile (31-54)"
fixpropbinB_pred2$Voc2[fixpropbinB_pred2$BPVS>54]<-"Third quartile (55-93)"

rec_cost_loess.VocQ<-ggplot(fixpropbinB_pred2,aes(x=time,y=BFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess") +ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to B")+xlab("Time.ms")+facet_wrap(~Voc2)
print(rec_cost_loess.VocQ)
ggsave("prop_growthcurve.loess_recognition_B_children_quartileVoc.png", plot=rec_cost_loess.VocQ, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()


# ## Looks to A/C when they are expected versus not particularly expected
fixsumbinAPred<-fixsumbin4[fixsumbin4$Named=="A",]
fixsumbinAPred<-fixsumbinAPred[fixsumbinAPred$Bias!="CBA",]
fixsumbinAPred<-fixsumbinAPred[fixsumbinAPred$Block=="Second",]
fixsumbinAPred$Pred<-"Expected"
fixsumbinAPred$Pred[fixsumbinAPred$Bias=="FLAT"]<-"Neutral"
#by participants
fixpropbinAPred<-summaryBy(AFIXBIN ~ time +Participant+ Pred+AGEYEAR+BPVS, data=fixsumbinAPred, FUN = mean, keep.names=T, na.rm=T)
# fixpropbinAPredCI<-summaryBy(AFIXBIN ~ time +Pred , data=fixpropbinAPred, FUN = c(mean,ci.high,ci.low), keep.names=T, na.rm=T)
# fixpropbinAPredCI$Context<-factor(fixpropbinAPredCI$Pred,labels=c("A_biasing","Neutral"))
# rec_benA<-ggplot(fixpropbinAPredCI,aes(x=time,y=AFIXBIN.mean,color=Context,linetype=Context))+geom_line(stat = "identity", size = 1) + geom_linerange(aes(ymin= (AFIXBIN.mean - AFIXBIN.ci.low), ymax= (AFIXBIN.mean + AFIXBIN.ci.high)), position = position_dodge(width=.9), size =0.5)+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to A")+xlab("Time.ms")+scale_color_manual(values=c("#F8766D","#619CFF"))+scale_linetype_manual(values=c("solid",42))
# ggsave("prop_growthcurve.CI_recognition_A_adults.png", plot=rec_benA, width=30, height=15, unit="cm", dpi=300, path="M:/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL_ADULTS_ADDED")
# dev.off()

# with points and loess
# by age group
fixpropbinAPred$Context<-factor(fixpropbinAPred$Pred,labels=c("A_biasing","Neutral"))
rec_benA_loess<-ggplot(fixpropbinAPred,aes(x=time,y=AFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess")+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to A")+xlab("Time.ms")+scale_color_manual(values=c("#F8766D","#619CFF"))+scale_linetype_manual(values=c("solid",42))+facet_wrap(~AGEYEAR)
print(rec_benA_loess)
ggsave("prop_growthcurve.loess_recognition_A_children.png", plot=rec_benA_loess, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

# by quantile of Voc
summary(fixpropbinAPred$BPVS)
fixpropbinAPred$Voc2<-"First quartile (2-30)"
fixpropbinAPred$Voc2[fixpropbinAPred$BPVS>30]<-"Interquartile (31-54)"
fixpropbinAPred$Voc2[fixpropbinAPred$BPVS>54]<-"Third quartile (55-93)"

rec_benA_loess.VocQ<-ggplot(fixpropbinAPred,aes(x=time,y=AFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess")+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to A")+xlab("Time.ms")+scale_color_manual(values=c("#F8766D","#619CFF"))+scale_linetype_manual(values=c("solid",42))+facet_wrap(~Voc2)
print(rec_benA_loess)
ggsave("prop_growthcurve.loess_recognition_A_children_VocQuartile.png", plot=rec_benA_loess.VocQ, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()


## Looks to C when it is expected versus not particularly expected (excluding trials on which FIXATONSET==C)
fixsumbinCPred<-fixsumbin5[fixsumbin5$Named=="C",]
fixsumbinCPred<-fixsumbinCPred[fixsumbinCPred$Bias!="ABC",]
fixsumbinCPred<-fixsumbinCPred[fixsumbinCPred$Block=="Second",]
fixsumbinCPred$Pred<-"Expected"
fixsumbinCPred$Pred[fixsumbinCPred$Bias=="FLAT"]<-"Neutral"
#by participants
fixpropbinCPred<-summaryBy(CFIXBIN ~ time +Participant+ Pred+AGEYEAR+BPVS, data=fixsumbinCPred, FUN = mean, keep.names=T, na.rm=T)
# fixpropbinCPredCI<-summaryBy(CFIXBIN ~ time +Pred , data=fixpropbinCPred, FUN = c(mean,ci.high,ci.low), keep.names=T, na.rm=T)
# fixpropbinCPredCI$Context<-factor(fixpropbinCPredCI$Pred,labels=c("C_biasing","Neutral"))
# rec_benC<-ggplot(fixpropbinCPredCI,aes(x=time,y=CFIXBIN.mean,color=Context,linetype=Context))+geom_line(stat = "identity", size = 1) + geom_linerange(aes(ymin= (CFIXBIN.mean - CFIXBIN.ci.low), ymax= (CFIXBIN.mean + CFIXBIN.ci.high)), position = position_dodge(width=.9), size =0.5)+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to C")+xlab("Time.ms")+scale_color_manual(values=c("#00BA38","#619CFF"))+scale_linetype_manual(values=c(22,42))
# ggsave("prop_growthcurve.CI_recognition_C_adults.png", plot=rec_benC, width=30, height=15, unit="cm", dpi=300, path="M:/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL_ADULTS_ADDED")
# dev.off()

# with points and loess
# by age year
fixpropbinCPred$Context<-factor(fixpropbinCPred$Pred,labels=c("C_biasing","Neutral"))
rec_benC_loess<-ggplot(fixpropbinCPred,aes(x=time,y=CFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y = "mean", geom = "point") + stat_smooth(method="loess")+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to C")+xlab("Time.ms")+scale_color_manual(values=c("#00BA38","#619CFF"))+scale_linetype_manual(values=c(22,42))+facet_wrap(~AGEYEAR)
print(rec_benC_loess)
ggsave("prop_growthcurve.loess_recognition_C_children.png", plot=rec_benC_loess, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

#by quanrtile of Voc
summary(fixpropbinCPred$BPVS)
fixpropbinCPred$Voc2<-"First quartile (2-30)"
fixpropbinCPred$Voc2[fixpropbinCPred$BPVS>30]<-"Interquartile (31-54)"
fixpropbinCPred$Voc2[fixpropbinCPred$BPVS>54]<-"Third quartile (55-93)"

rec_benC_loess.VocQ<-ggplot(fixpropbinCPred,aes(x=time,y=CFIXBIN,color=Context,linetype=Context))+stat_summary(fun.y = "mean", geom = "point") + stat_smooth(method="loess")+ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to C")+xlab("Time.ms")+scale_color_manual(values=c("#00BA38","#619CFF"))+scale_linetype_manual(values=c(22,42))+facet_wrap(~Voc2)
print(rec_benC_loess.VocQ)
ggsave("prop_growthcurve.loess_recognition_C_children.quartileVoc.png", plot=rec_benC_loess.VocQ, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

###combine A, B and C (only neutral trials)
fixsumbinBPred<-fixsumbin3[fixsumbin3$Named=="B"&fixsumbin3$BackgroundFIXBIN==0,]
fixsumbinBPred<-fixsumbinBPred[fixsumbinBPred$Bias=="FLAT",]
fixsumbinBPred<-fixsumbinBPred[fixsumbinBPred$Block=="First",]
fixsumbinBPred$TARGETFIXBIN<-fixsumbinBPred$BFIXBIN
fixsumbinBPred<-fixsumbinBPred[,c("time","Participant","Trial","ItemF","Named","Bias","Block","TARGETFIXBIN","AGEYEAR","Age","BPVS")]
fixsumbinBPred$Picture<-"B"


fixsumbinAPred<-fixsumbin4[fixsumbin4$Named=="A"&fixsumbin4$BackgroundFIXBIN==0,]
fixsumbinAPred<-fixsumbinAPred[fixsumbinAPred$Bias=="FLAT",]
fixsumbinAPred<-fixsumbinAPred[fixsumbinAPred$Block=="Second",]
fixsumbinAPred$TARGETFIXBIN<-fixsumbinAPred$AFIXBIN
fixsumbinAPred<-fixsumbinAPred[,c("time","Participant","Trial","ItemF","Named","Bias","Block","TARGETFIXBIN","AGEYEAR","Age","BPVS")]
fixsumbinAPred$Picture<-"A"

fixsumbinCPred<-fixsumbin5[fixsumbin5$Named=="C"&fixsumbin5$BackgroundFIXBIN==0,]
fixsumbinCPred<-fixsumbinCPred[fixsumbinCPred$Bias=="FLAT",]
fixsumbinCPred<-fixsumbinCPred[fixsumbinCPred$Block=="Second",]
fixsumbinCPred$TARGETFIXBIN<-fixsumbinCPred$CFIXBIN
fixsumbinCPred<-fixsumbinCPred[,c("time","Participant","Trial","ItemF","Named","Bias","Block","TARGETFIXBIN","AGEYEAR","Age","BPVS")]
fixsumbinCPred$Picture<-"C"

fixsumbinABCPred<-rbind(fixsumbinAPred,fixsumbinCPred,fixsumbinBPred)
summary(fixsumbinABCPred)

#by participants
#by age year
fixpropbinABCPred<-summaryBy(TARGETFIXBIN ~ time +Participant+AGEYEAR+Age+BPVS, data=fixsumbinABCPred, FUN = mean, keep.names=T, na.rm=T)
rec_neutral_loess<-ggplot(fixpropbinABCPred,aes(x=time,y=TARGETFIXBIN))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess") +ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to the Named picture in Neutral contexts")+xlab("Time.ms")+facet_wrap(~AGEYEAR)
print(rec_neutral_loess)
ggsave("prop_growthcurve_loess_recognition_named_neutral_children.png", plot=rec_neutral_loess, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

# by quartile of Voc
summary(fixpropbinABCPred$BPVS)
fixpropbinABCPred$Voc2<-"First quartile (2-30)"
fixpropbinABCPred$Voc2[fixpropbinABCPred$BPVS>30]<-"Interquartile (31-54)"
fixpropbinABCPred$Voc2[fixpropbinABCPred$BPVS>54]<-"Third quartile (55-93)"

rec_neutral_loess.VocQ<-ggplot(fixpropbinABCPred,aes(x=time,y=TARGETFIXBIN))+stat_summary(fun.y="mean",geom="point")+stat_smooth(method="loess") +ylim(c(0,1))+ggtitle("Integration window")+ylab("Proportion of Looks to the Named picture in Neutral contexts")+xlab("Time.ms")+facet_wrap(~Voc2)
print(rec_neutral_loess.VocQ)
ggsave("prop_growthcurve_loess_recognition_named_neutral_children_quartileVoc.png", plot=rec_neutral_loess.VocQ, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")
dev.off()

##################
#Recognition Speed
#################

####### ELogit analyses of recognition speed (by subjects ONLY), with index derivation
bySubj_rec <- fixsumbinABCPred %>%
  group_by(Participant,Bias,time) %>% # aggregate within time slices
  summarise(TARGETFIXBIN.m = mean(TARGETFIXBIN), yT = sum(TARGETFIXBIN), N_T = length(TARGETFIXBIN)) %>%
  mutate(elogT = log( (yT + .5) / (N_T - yT + .5) )) %>%  # empirical logit
  #wts = 1/(y + .5) + 1/(N - y + .5), # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup()        

part.info<-summaryBy(time~Participant+AGEYEAR+Age+BPVS, data=fixsumbinABCPred,FUN=mean)
part.info<-part.info[,-5]
bySubj_rec<-merge(bySubj_rec,part.info,by=c("Participant"),sort=FALSE)
summary(bySubj_rec)

#model of elogit by Subj
# growth curve

gca<-bySubj_rec
head(gca)

t<-poly(unique(gca$time),2)
time<-as.vector(unique(gca$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gca<-gca[order(gca$time),]
gca$t1<-NA
gca$t2<-NA
for (i in (1:nrow(gca))){
  gca$t1[i]<-t[t$time==gca$time[i],1] 
  gca$t2[i]<-t[t$time==gca$time[i],2] 
}
summary(gca)

gca$AC<-scale(gca$Age,T,F)
gca$VC<-scale(gca$BPVS,T,F)

# base model (without age or vocabulary)
mrecABC<-lmer(elogT~1+t1+t2+(1+t1+t2|Participant),data=gca,REML=F)
summary(mrecABC)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: elogT ~ 1 + t1 + t2 + (1 + t1 + t2 | Participant)
# Data: gca
# 
# AIC      BIC   logLik deviance df.resid 
# 13431.0  13499.3  -6705.5  13411.0     6870 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.2285 -0.5804  0.0082  0.5880  3.4280 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# Participant (Intercept) 0.2785   0.5277              
# t1          4.8031   2.1916    0.25      
# t2          3.5531   1.8850   -0.48  0.16
# Residual                0.3176   0.5636              
# Number of obs: 6880, groups:  Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.32367    0.03662   -8.84
# t1           5.44938    0.15433   35.31
# t2          -1.85975    0.13418  -13.86
# 
# Correlation of Fixed Effects:
#   (Intr) t1    
# t1  0.237       
# t2 -0.455  0.151

##compute by participant effects (t1, slope) and correlate with vocabulary
slopebypart<-data.frame(PartCode=rep(1:215,1),Value=NA)
#rec slope
rec_s<-ranef(mrecABC)$'Participant'[2]
#select only random effects for t1, which is the second column
Rec<-rec_s+as.numeric(fixef(mrecABC)["t1"])

slopebypart$Value<-Rec$t1
#diffbypart$Value<-c(CBA_AB,CBA_CB,ABC_AB,ABC_CB)
summary(slopebypart)
head(slopebypart)

part.info<-summaryBy(time~Participant+Age+BPVS,data=gca,FUN=mean)
part.info<-part.info[,-4]
part.info$PartCode<-c(1:nrow(part.info))
head(part.info)
slopebypart<-merge(slopebypart,part.info,by=c("PartCode"),sort=FALSE)

#correlation with vocabulary
bySubj_rec_effects_voc<-ggplot(slopebypart,aes(x=BPVS,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between vocabulary and recognition speed (slope)")+xlab("BPVS raw score")+ylab("recognition speed (slope) in neutral contexts")
print(bySubj_rec_effects_voc)
ggsave("bysubj_recspeed_effects_voc.png", plot=bySubj_rec_effects_voc, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#correlation with age
bySubj_rec_effects_age<-ggplot(slopebypart,aes(x=Age,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and recognition speed (slope)")+xlab("Age in months")+ylab("recognition speed (slope) in neutral contexts")
print(bySubj_rec_effects_age)
ggsave("bysubj_recspeed_effects_age.png", plot=bySubj_rec_effects_age, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

## regression
RecSpeed<-lm(Value~Age+BPVS, data =slopebypart)
summary(RecSpeed)
# Call:
#   lm(formula = Value ~ Age + BPVS, data = slopebypart)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6.414 -1.354 -0.050  1.398  5.644 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.18103    0.63620   6.572 3.79e-10 ***
#   Age          0.01645    0.02335   0.705    0.482    
# BPVS         0.01335    0.01496   0.892    0.373    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.106 on 212 degrees of freedom
# Multiple R-squared:  0.0297,	Adjusted R-squared:  0.02055 
# F-statistic: 3.245 on 2 and 212 DF,  p-value: 0.04093

### separately for age and vocabulary
RecSpeed.V<-lm(Value~BPVS, data =slopebypart)
summary(RecSpeed.V)#BPVS        0.021811   0.008899   2.451   0.0151 *

RecSpeed.A<-lm(Value~Age, data =slopebypart)
summary(RecSpeed.A)#Age          0.03318    0.01390   2.387   0.0179 *

# This is the recognition index
# it was computed by looking at only Neutral trials
# and taking the slope of increase in looks to the named picture
# when this was not already fixed at trial onset
# save to file
write.table(slopebypart, "recognition_indeces.txt",sep=";",dec=".",row.name=F)

################################
### Elog Integration cost and Integration benefit analyses 
### (FIRST by subjects, with indeces AND THEN by items)
################################
### Prediction costs (B)
fixsumbin3$Pred<-"Unexpected"
fixsumbin3$Pred[fixsumbin3$Bias=="FLAT"]<-"Neutral"
fixsumbin3B<-fixsumbin3[fixsumbin3$Named=="B",]
fixsumbin3B<-fixsumbin3B[fixsumbin3B$Block=="First",]
bySubjB <- fixsumbin3B[fixsumbin3B$BackgroundFIXBIN==0,] %>%
  group_by(Participant,Pred,time) %>% # aggregate within time slices
  summarise(BFIXBIN.m = mean(BFIXBIN), yB = sum(BFIXBIN),N_B = length(BFIXBIN)) %>%
  mutate(elogB = log( (yB + .5) / (N_B - yB + .5) )) %>%  # empirical logit
  #wts = 1/(yB + .5) + 1/(N_B - yB + .5)) %>% # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup()   

## add Age/AGEYEAR/BPVS to bySubj

part.info<-summaryBy(time~Participant+AGEYEAR+Age+BPVS, data=fixsumbin3B,FUN=mean)
part.info<-part.info[,-5]
bySubjB<-merge(bySubjB,part.info,by=c("Participant"),sort=FALSE)
head(bySubjB)

byItemB <- fixsumbin3B[fixsumbin3B$BackgroundFIXBIN==0,] %>%
  group_by(ItemF,Pred,time) %>% # aggregate within time slices
  summarise(BFIXBIN.m = mean(BFIXBIN), yB = sum(BFIXBIN), N_B = length(BFIXBIN)) %>%
  mutate(elogB = log( (yB + .5) / (N_B - yB + .5) )) %>%  # empirical logit
  #wts = 1/(yB + .5) + 1/(N_B - yB + .5)) %>% # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup() 

##Prediction benefits (A and C combined)
#### For the purpose of analyses, you should combine A and B to get better power, even thought the figures show them separately

###combine A and C
fixsumbinAPred<-fixsumbin4[fixsumbin4$Named=="A",]
fixsumbinAPred<-fixsumbinAPred[fixsumbinAPred$Bias!="CBA",]
fixsumbinAPred<-fixsumbinAPred[fixsumbinAPred$Block=="Second",]
fixsumbinAPred$Pred<-"Expected"
fixsumbinAPred$Pred[fixsumbinAPred$Bias=="FLAT"]<-"Neutral"
fixsumbinAPred$TARGETFIXBIN<-fixsumbinAPred$AFIXBIN
fixsumbinAPred<-fixsumbinAPred[,c("time","Participant","Trial","ItemF","Named","Bias","Block","Pred","TARGETFIXBIN","BackgroundFIXBIN","Age","AGEYEAR","BPVS")]
fixsumbinAPred$Picture<-"A"

fixsumbinCPred<-fixsumbin5[fixsumbin5$Named=="C",]
fixsumbinCPred<-fixsumbinCPred[fixsumbinCPred$Bias!="ABC",]
fixsumbinCPred<-fixsumbinCPred[fixsumbinCPred$Block=="Second",]
fixsumbinCPred$Pred<-"Expected"
fixsumbinCPred$Pred[fixsumbinCPred$Bias=="FLAT"]<-"Neutral"
fixsumbinCPred$TARGETFIXBIN<-fixsumbinCPred$CFIXBIN
fixsumbinCPred<-fixsumbinCPred[,c("time","Participant","Trial","ItemF","Named","Bias","Block","Pred","TARGETFIXBIN","BackgroundFIXBIN","Age","AGEYEAR","BPVS")]
fixsumbinCPred$Picture<-"C"

fixsumbinACPred<-rbind(fixsumbinAPred,fixsumbinCPred)
summary(fixsumbinACPred)

bySubjAC <- fixsumbinACPred[fixsumbinACPred$BackgroundFIXBIN==0,] %>%
  group_by(Participant,Pred,time,Picture) %>% # aggregate within time slices
  summarise(TARGETFIXBIN.m = mean(TARGETFIXBIN), yT = sum(TARGETFIXBIN),N_T = length(TARGETFIXBIN)) %>%
  mutate(elogT = log( (yT + .5) / (N_T - yT + .5) )) %>%  # empirical logit
  #wts = 1/(y + .5) + 1/(N - y + .5), # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup() 

## add Age/AGEYEAR/BPVS to bySubj

part.info<-summaryBy(time~Participant+AGEYEAR+Age+BPVS, data=fixsumbinACPred,FUN=mean)
part.info<-part.info[,-5]
bySubjAC<-merge(bySubjAC,part.info,by=c("Participant"),sort=FALSE)
head(bySubjAC)


byItemAC <- fixsumbinACPred[fixsumbinACPred$BackgroundFIXBIN==0,] %>%
  group_by(ItemF,Pred,time,Picture) %>% # aggregate within time slices
  summarise(TARGETFIXBIN.m = mean(TARGETFIXBIN), yT = sum(TARGETFIXBIN), N_T = length(TARGETFIXBIN)) %>%
  mutate(elogT = log( (yT + .5) / (N_T - yT + .5) )) %>%  # empirical logit
  #wts = 1/(y + .5) + 1/(N - y + .5), # optional weights
  #Arcsin = asin(sqrt(PropAnimal))) # arcsin-sqrt
  ungroup() 

## gca analysis

##### below I use the logit transformation; note that results are very different if we model proportions directly without a trasnform
######################
## Prediction costs##
####################

####################
#### By Subject ####
###################
gcar<-bySubjB

t<-poly(unique(gcar$time),2)
time<-as.vector(unique(gcar$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gcar<-gcar[order(gcar$time),]
gcar$t1<-NA
gcar$t2<-NA
for (i in (1:nrow(gcar))){
  gcar$t1[i]<-t[t$time==gcar$time[i],1] 
  gcar$t2[i]<-t[t$time==gcar$time[i],2] 
}
summary(gcar)
gcar_pred<-gcar
gcar_pred$P<-ifelse(gcar_pred$Pred=="Neutral",-.5,.5)
gcar_pred$PC<-scale(gcar_pred$P,T,F)
head(gcar_pred)

mGCAr_B<-lmer(elogB~1+(t1+t2)*PC+(1+(t1+t2)*PC||Participant), data=gcar_pred,REML=F)
summary(mGCAr_B)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: elogB ~ 1 + (t1 + t2) * PC + ((1 | Participant) + (0 + t1 | Participant) +  
#                                          (0 + t2 | Participant) + (0 + PC | Participant) + (0 + t1:PC |  
#                                                                                               Participant) + (0 + t2:PC | Participant))
# Data: gcar_pred
# 
# AIC      BIC   logLik deviance df.resid 
# 28137.6  28235.3 -14055.8  28111.6    13614 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.5970 -0.5787  0.0137  0.5846  3.8455 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# Participant   (Intercept)  0.1692  0.4114  
# Participant.1 t1           4.0303  2.0076  
# Participant.2 t2           2.0751  1.4405  
# Participant.3 PC           0.5945  0.7710  
# Participant.4 t1:PC       10.4022  3.2252  
# Participant.5 t2:PC        7.1439  2.6728  
# Residual                   0.3490  0.5907  
# Number of obs: 13627, groups:  Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.42981    0.02857 -15.043
# t1           4.42398    0.14014  31.569
# t2          -1.07480    0.10257 -10.478
# PC          -0.21678    0.05369  -4.038
# t1:PC        0.18710    0.22794   0.821
# t2:PC        0.91869    0.19159   4.795
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     PC     t1:PC 
# t1     0.000                            
# t2     0.000  0.000                     
# PC     0.005  0.000  0.000              
# t1:PC  0.000  0.004 -0.001  0.000       
# t2:PC  0.000 -0.001  0.005  0.000  0.000

########################
#recognition cost index
quadbypart<-data.frame(PartCode=rep(c(1:215),1),Value=NA)
#rec quadratic effect
rec_q<-ranef(mGCAr_B)$'Participant'[6]
#select only random effects for quadratic term, which is the sixth column
Recq<-rec_q+as.numeric(fixef(mGCAr_B)["t2:PC"])

quadbypart$Value<-Recq$`t2:PC`

summary(quadbypart)
head(quadbypart)

part.info<-summaryBy(time~Participant+Age+BPVS,data=gca,FUN=mean)
part.info<-part.info[,-4]
part.info$PartCode<-c(1:nrow(part.info))
head(part.info)
quadbypart<-merge(quadbypart,part.info,by=c("PartCode"),sort=FALSE)

#correlation with vocabulary
bySubj_rec_cost_effects_voc<-ggplot(quadbypart,aes(x=BPVS,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between vocabulary and recognition cost (quadratic)")+xlab("BPVS raw score")+ylab("recognition cost (quadratic) for B pictures")
print(bySubj_rec_cost_effects_voc)
ggsave("bysubj_reccost_effects_voc.png", plot=bySubj_rec_cost_effects_voc, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#correlation with age
bySubj_rec_costs_effects_age<-ggplot(quadbypart,aes(x=Age,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and recognition cost (quadratic)")+xlab("Age in months")+ylab("recognition cost (quadratic) in neutral contexts")
print(bySubj_rec_costs_effects_age)
ggsave("bysubj_reccost_effects_age.png", plot=bySubj_rec_costs_effects_age, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#regression 
RecCost<-lm(Value~Age+BPVS, data=quadbypart)
summary(RecCost)
# Call:
#   lm(formula = Value ~ Age + BPVS, data = quadbypart)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.5647 -1.4978 -0.1188  1.5744  6.0002 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -1.331143   0.753344  -1.767   0.0787 .
# Age          0.042913   0.027647   1.552   0.1221  
# BPVS         0.009752   0.017711   0.551   0.5825  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.494 on 212 degrees of freedom
# Multiple R-squared:  0.05151,	Adjusted R-squared:  0.04256 
# F-statistic: 5.757 on 2 and 212 DF,  p-value: 0.003676

# age only
RecCost.A<-lm(Value~Age, data=quadbypart)
summary(RecCost.A)# 0.05514    0.01644   3.354 0.000944 ***

# voc only
RecCost.V<-lm(Value~BPVS, data=quadbypart)
summary(RecCost.V)# 0.03183    0.01058   3.007  0.00295 **

# This is the recognition cost index

# save to file
write.table(quadbypart, "recognition_cost_indeces.txt",sep=";",dec=".",row.name=F)

######################
## Recognition benefit
######################

####################
#### By Subject ####
###################
gcar<-bySubjAC

t<-poly(unique(gcar$time),2)
time<-as.vector(unique(gcar$time))
t<-cbind(t,time)
t<-as.data.frame(t)
gcar<-gcar[order(gcar$time),]
gcar$t1<-NA
gcar$t2<-NA
for (i in (1:nrow(gcar))){
  gcar$t1[i]<-t[t$time==gcar$time[i],1] 
  gcar$t2[i]<-t[t$time==gcar$time[i],2] 
}
summary(gcar)
gcar_pred<-gcar
gcar_pred$P<-ifelse(gcar_pred$Pred=="Neutral",-.5,.5)
gcar_pred$PC<-scale(gcar_pred$P,T,F)
head(gcar_pred)

mGCAr_AC<-lmer(elogT~1+(t1+t2)*PC+(1+(t1+t2)*PC||Participant), data=gcar_pred,REML=F)
summary(mGCAr_AC)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: elogT ~ 1 + (t1 + t2) * PC + ((1 | Participant) + (0 + t1 | Participant) +  
#                                          (0 + t2 | Participant) + (0 + PC | Participant) + (0 + t1:PC |  
#                                                                                               Participant) + (0 + t2:PC | Participant))
# Data: gcar_pred
# 
# AIC      BIC   logLik deviance df.resid 
# 60096.5  60201.4 -30035.2  60070.5    23617 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5183 -0.6446  0.0138  0.6747  3.2908 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# Participant   (Intercept) 0.1581   0.3976  
# Participant.1 t1          2.2791   1.5097  
# Participant.2 t2          1.5777   1.2561  
# Participant.3 PC          0.3475   0.5895  
# Participant.4 t1:PC       6.2047   2.4909  
# Participant.5 t2:PC       3.8744   1.9683  
# Residual                  0.6518   0.8073  
# Number of obs: 23630, groups:  Participant, 215
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.13297    0.02781  -4.782
# t1           2.89136    0.10826  26.707
# t2          -1.21656    0.09158 -13.285
# PC           0.02912    0.04207   0.692
# t1:PC       -1.30433    0.18267  -7.140
# t2:PC       -0.54570    0.14910  -3.660
# 
# Correlation of Fixed Effects:
#   (Intr) t1     t2     PC     t1:PC 
# t1    -0.003                            
# t2     0.001 -0.003                     
# PC     0.004 -0.004  0.003              
# t1:PC -0.003  0.007 -0.003 -0.005       
# t2:PC  0.003 -0.004  0.006  0.003 -0.004

########################
#recognition benefit index
quadbypart2<-data.frame(PartCode=rep(c(1:215),1),Value=NA)
#rec benefit quadratic effect
rec_q2<-ranef(mGCAr_AC)$'Participant'[6]
#select only random effects for quadratic term, which is the sixth column
Recq2<-rec_q2+as.numeric(fixef(mGCAr_AC)["t2:PC"])

quadbypart2$Value<-Recq2$`t2:PC`
summary(quadbypart2)
head(quadbypart2)

part.info<-summaryBy(time~Participant+Age+BPVS,data=gca,FUN=mean)
part.info<-part.info[,-4]
part.info$PartCode<-c(1:nrow(part.info))
head(part.info)
quadbypart2<-merge(quadbypart2,part.info,by=c("PartCode"),sort=FALSE)

#correlation with vocabulary
bySubj_rec_benefit_effects_voc<-ggplot(quadbypart2,aes(x=BPVS,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between vocabulary and recognition benefit (quadratic)")+xlab("BPVS raw score")+ylab("recognition benefit (quadratic) for A/C pictures")
print(bySubj_rec_benefit_effects_voc)
ggsave("bysubj_recbenefit_effects_voc.png", plot=bySubj_rec_benefit_effects_voc, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#correlation with age
bySubj_rec_benefit_effects_age<-ggplot(quadbypart2,aes(x=Age,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and recognition benefit (quadratic)")+xlab("Age in months")+ylab("recognition benefit (quadratic) for A/C pictures")
print(bySubj_rec_benefit_effects_age)
ggsave("bysubj_recbenefit_effects_age.png", plot=bySubj_rec_benefit_effects_age, width=30, height=15, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/ExpectationDrivenLanguageLearning/ExpectationLearning/Experiment4_gradedpredictions/mainexp/FINAL/children_2018an")

#regression 
RecBen<-lm(Value~Age+BPVS, data=quadbypart2)
summary(RecBen)
# Call:
#   lm(formula = Value ~ Age + BPVS, data = quadbypart2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.5952 -1.1456 -0.0364  1.3129  4.4061 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.6288442  0.5390484  -1.167    0.245
# Age          0.0020877  0.0197822   0.106    0.916
# BPVS        -0.0001483  0.0126730  -0.012    0.991
# 
# Residual standard error: 1.785 on 212 degrees of freedom
# Multiple R-squared:  0.0001235,	Adjusted R-squared:  -0.009309 
# F-statistic: 0.01309 on 2 and 212 DF,  p-value: 0.987

# age only
RecBen.A<-lm(Value~Age, data=quadbypart2)
summary(RecBen.A)# 0.001902   0.011756   0.162    0.872

# voc only
RecBen.V<-lm(Value~BPVS, data=quadbypart2)
summary(RecBen.V)# 0.000926   0.007532   0.123   0.9023 

# This is the recognition cost index

# save to file
write.table(quadbypart2, "recognition_benefit_indeces.txt",sep=";",dec=".",row.name=F)

### below median age
# RecBen.mA<-lm(Value~Age+BPVS, data=subset(quadbypart2,Age<=42))
# summary(RecBen.mA)
# bySubj_rec_benefit_effects_age<-ggplot(subset(quadbypart2,Age<=42),aes(x=Age,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and recognition benefit (quadratic)")+xlab("Age in months")+ylab("recognition benefit (quadratic) for A/C pictures")
# print(bySubj_rec_benefit_effects_age)
# bySubj_rec_benefit_effects_voc<-ggplot(subset(quadbypart2,Age<=42),aes(x=BPVS,y=Value))+geom_point(position="jitter")+stat_smooth(method="lm")+ggtitle("Correlation between age and recognition benefit (quadratic)")+xlab("Age in months")+ylab("recognition benefit (quadratic) for A/C pictures")
# print(bySubj_rec_benefit_effects_voc)
