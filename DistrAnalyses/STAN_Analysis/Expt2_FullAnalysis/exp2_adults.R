setwd("M:/Projects/turn-taking/turn-takingS1/Laura/Experiment2")
data<-read.csv("Exp2Dataset.csv",header=T)
head(data)
summary(data)
library(doBy)
# Pred = Predictablity (between subjects): whether the question is predictable or not (i.e., there is only one animal or two animals hiding behind characters)
# early.late = whether the information that allows participants to prepare their response is provided early or late in the question.
# Right.Wrong = whether the question requires yes or no as an answer
# Length = whether the character name is long or short
# CharacterLength = the actual length of the last word (whether character name or the parrot/the tiger)

# remove inconsistencies in coding of Response
summary(data$Response)
data$Response[data$Response=="n "]<-"n"
summary(data$Response)
data$Response[data$Response=="y "]<-"y"
summary(data$Response)
data$Response[data$Response=="Y"]<-"y"
summary(data$Response)
data$Response<-factor(data$Response, levels=c("n","y"))
summary(data$Response)

#plot response against Right.Wrong
table(data$Response,data$Right.Wrong)

#remove incorrect responses
data$Correct<-"Correct"
data$Correct[data$Response=="y"&data$Right.Wrong=="wrong"|data$Response=="n"&data$Right.Wrong=="right"]<-"Incorrect"
summary(as.factor(data$Correct))
data<-data[data$Correct=="Correct",]# removed 32 data points

#means
bySubj<-summaryBy(RT.ms~Participant+Pred+Early.Late+Right.Wrong+Length,FUN=mean,keep.names=T,data=data)
summaryBy(RT.ms~+Pred+Early.Late,FUN=mean,keep.names=T,data=data)
# Pred Early.Late    RT.ms
# 1   Pred      early 303.1491
# 2   Pred       late 433.3194
# 3 Unpred      early 458.2910
# 4 Unpred       late 536.7646

#plot
library(ggplot2)
se.bar<-function(x, ...){
  sd(x, ...)/sqrt(length(x))
}

sumtot <- function(x, ...){
  c(m=mean(x, ...), se=se.bar(x, ...))
}

#main plot: Predictability by Early.Late
collapsed<-summaryBy(RT.ms~+Pred+Early.Late,FUN=sumtot,data=data)
library(ggplot2)
main<-ggplot(collapsed,aes(Pred,RT.ms.m,fill=Early.Late)) + geom_bar(stat="identity", position="dodge",colour="black")+geom_errorbar(aes(ymax = RT.ms.m + RT.ms.se, ymin=RT.ms.m - RT.ms.se), position=position_dodge(width=0.9), width=0.2) 
plot(main)

#correlation between Length and RT
corr<-ggplot(data,aes(CharacterLength.ms,RT.ms,col=Early.Late)) + geom_point()
plot(corr)

# stats
library(lme4)
# remove any value above 2 seconds
data<-data[data$RT.ms<=2000,]
data$P<-ifelse(data$Pred=="Pred",.5,-.5)
data$PC<-scale(data$P,T,F)
data$EL<-ifelse(data$Early.Late=="early",.5,-.5)
data$ELC<-scale(data$EL,T,F)
data$RW<-ifelse(data$Right.Wrong=="right",.5,-.5)
data$RWC<-scale(data$RW,T,F)
data$LC<-ifelse(data$Length=="Long",.5,-.5)
data$LCC<-scale(data$LC,T,F)
data$LLW<-scale(data$CharacterLength.ms,T,T)
m0<-lmer(RT.ms~1+LCC+RWC+PC*ELC+(1+ELC||Subject)+(1|SoundFile),data=data)
summary(m0)
m1<-lmer(RT.ms~1+LLW+LCC+RWC+PC*ELC+(1+ELC||Subject)+(1|SoundFile),data=data)
summary(m1)

#P == Pred
data$P<-ifelse(data$Pred=="Pred",0,1)
# now the main effect of ELC refer to the effect of Early.Late when the question is predictable (because Pred is the reference level)
m1.pred<-lmer(RT.ms~1+LLW+LCC+RWC+P*ELC+(1+ELC||Subject)+(1|SoundFile),data=data)
summary(m1.pred)
#P == Unpred
data$P<-ifelse(data$Pred=="Pred",1,0)
# now the main effect of ELC refer to the effect of Early.Late when the question is unpredictable (because Unpred is the reference level)
m1.unpred<-lmer(RT.ms~1+LLW+LCC+RWC+P*ELC+(1+ELC||Subject)+(1|SoundFile),data=data)
summary(m1.unpred)

# now try adding an interaction between length of teh last word and early.late
data$P<-ifelse(data$Pred=="Pred",.5,-.5)
data$PC<-scale(data$P,T,F)
m3<-lmer(RT.ms~1+LLW*ELC+LCC+RWC+PC+ELC:PC+(1+ELC||Subject)+(1|SoundFile),data=data)
summary(m3)

#try adding Qduration as covariate
data$QD<-scale(data$QuLength.ms,T,T)
m4<-lmer(RT.ms~1+LLW+LCC+QD+RWC+PC*ELC+(1+ELC||Subject)+(1|SoundFile),data=data)
summary(m4)
