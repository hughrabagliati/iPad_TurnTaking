# NOTE THAT FOR THESE ANALYSES YOU TYPICALLY NEED TO INCLUDE THE LONG RT TAIL. I recommend rt >0 & rt <16000
library(ggplot2)
library(reshape2)
library(retimes)
library(doBy)
library(ez)
library(plyr)
ads <- read.csv("Adult_R.csv")
fives <- read.csv("5yo_R.csv")
threes <- read.csv("3yo_R.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

tt <- rbind(ads,fives,threes)
tt$Age <- as.factor(tt$Age)
tt$Subject <- paste(tt$Age,tt$Participant, sep = "")

tt<- subset(tt, RTms <= 6000)
tt$RTms <- tt$RTms + abs(mins(tt$RTms))
d = data.frame(Subject = rep(NA, times = length(unique(tt$Subject))*4), Pred = NA, Match = NA, Age = NA, Mu = NA, Sigma = NA, Tau = NA)
index = 1
for (k in unique(tt$Pred)){
	for (j in unique(subset(tt, Pred == k)$Subj)){
		for (i in unique(subset(tt, Pred == k & Subject == j)$Match)){
			a = timefit(subset(tt, Pred == k & Subject == j & Match == i )$RTms)
			
			d$Age[index] <- as.character(unique(subset(tt, Pred == k & Subject == j & Match == i )$Age)[1])
			d$Subject[index] <- j
			d$Pred[index] <- k
			d$Match[index] <- i
			d$Mu[index] <- a@par[1]
			d$Sigma[index] <- a@par[2]
			d$Tau[index] <- a@par[3]
			index <- index + 1

			}
		}
	}
re.summary <- summaryBy(Mu + Sigma+ Tau ~ Age+Pred + Match, data = d, FUN = c(mean))

ezANOVA(d, dv = Mu, wid = Subject, within = .(Pred,Match), between = .(Age))
ezANOVA(d, dv = Sigma, wid = Subject, within = .(Pred,Match), between = .(Age))
ezANOVA(d, dv = Tau, wid = Subject, within = .(Pred,Match), between = .(Age))

re.summary.gg <- melt(re.summary,id.vars = c("Age","Pred","Match"), variable.name = "Parameter",value.name = "Value")
ggplot(data = re.summary.gg, aes(x = Pred, y = Value)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Match)) +
  facet_wrap(~Parameter+Age)
  
 # Now for all individuals
#tt<- subset(tt, Incorrect == 0)
d = data.frame(Pred = rep(NA, length = 12), Match = NA, Age = NA, Mu = NA, Sigma = NA, Tau = NA)
index = 1
for (k in unique(tt$Pred)){
	for (j in unique(subset(tt, Pred == k)$Age)){
		for (i in unique(subset(tt, Pred == k & Age == j)$Match)){

			a = timefit(subset(tt, Pred == k & Age == j & Match == i )$RTms)
			d$Age[index] <- j
			d$Pred[index] <- k
			d$Match[index] <- i
			d$Mu[index] <- a@par[1]
			d$Sigma[index] <- a@par[2]
			d$Tau[index] <- a@par[3]
			index <- index + 1

			}
		}
	}
re.summary <- summaryBy(Mu + Sigma+ Tau ~ Age+Pred + Match, data = d, FUN = c(mean))
re.summary.gg <- melt(re.summary,id.vars = c("Age","Pred","Match"), variable.name = "Parameter",value.name = "Value")
ggplot(data = re.summary.gg, aes(x = Pred, y = Value)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Match)) +
  facet_wrap(~Parameter+Age)