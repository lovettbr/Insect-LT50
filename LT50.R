####Insect LT50 Analysis 5/15/15 Lovett####

####Load packages####

#install.packages(c("reshape", "plyr", "ggplot2", "xlsx"))
library(reshape)
library(plyr)
library(ggplot2)
library("xlsx")

####Import and format data####

location="~/GitHub/Example_Data.csv"
dat <- read.csv(location)

#Remove NA values and melt data
dat[is.na(dat)]=0
dat=melt(dat, colnames(dat)[1:2], colnames(dat)[3:length(colnames(dat))])
colnames(dat)=c("Trial", "Day", "Treatment", "Value")

####Compute LT50 relevant statistics and create data tables####
#Calculate n values for each trial
n.vals=tapply(dat$Value, dat[c("Treatment", "Trial")], FUN=sum)
n.vals=melt(n.vals, n.vals[1], n.vals[1:length(colnames(n.vals))])
colnames(n.vals)=c("Treatment", "Trial", "n")

#Add n values to data, remove alive row, and calculate daily percent mortality
dat=merge(dat, n.vals, by=c("Treatment", "Trial"))
dat=dat[dat$Day!="Alive",]
dat=ddply(dat, .(Treatment, Trial), transform, Percent=(n-cumsum(Value))/(n)*100)
dat.per=recast(dat[c(1:3, 6)], Trial+Day~Treatment)
dat$Day=as.numeric(as.character(dat$Day))

#Find minimum trial duration
trial.min=min(ddply(dat, c("Trial"), summarize, Duration=max(Day))$Duration)

#Subset data to maximum duration in all trials
dat.min=dat[dat$Day<=trial.min,]
dat.min$se=ddply(dat.min, c("Day", "Treatment"), summarize, se=sd(Percent)/sqrt(length(Percent)))$se

#Aggregate data with maximum duration for all trials and calculate standard error
dat.agg=ddply(dat.min, c("Treatment", "Day"), summarize, per.mean=mean(Percent), se=sd(Percent)/sqrt(length(Percent)))

#Calculate LT50 for each replicate with an LT50
LT50=ddply(subset(dat.min, Percent <= 50), c("Trial", "Treatment"), summarize, LT50=min(Day))
#Calculate means and standard error
LT50.Error=ddply(LT50, "Treatment", summarize, "LT50 Mean"=mean(LT50), se=sd(LT50)/sqrt(length(LT50)), Replicates=length(LT50))
LT50=recast(LT50, Treatment~Trial)
LT50=cbind(LT50, LT50.Error[2:4])

####Find Abbott's Mortality####
ab.val=12
dat.ab=subset(dat, Day<=ab.val)
dat.ab=ddply(dat.ab, .(Treatment, Trial), transform, Alive=n-cumsum(Value))
dat.ab=subset(dat.ab, Day==12)
ctrl.tab=subset(dat.ab, Treatment=="Control")[c(2,7)]
colnames(ctrl.tab)[2]="ctrl.n"
dat.ab=merge(dat.ab, ctrl.tab, "Trial")
dat.ab$Abbott=(1-dat.ab$Alive/dat.ab$ctrl.n)*100
Abbott.Error=ddply(dat.ab, "Treatment", summarize, "Abbott Mean"=mean(Abbott), se=sd(Abbott)/sqrt(length(Abbott)), Replicates=length(Abbott))
dat.ab=recast(dat.ab[c(1,2,9)], Treatment~Trial)
Abbott=cbind(dat.ab, Abbott.Error[2:4])

####Generate plots####

#Plot data by replicate
attach(dat)
theme = theme_bw()+theme(text = element_text(size=20), axis.title.x = element_text(size=25), axis.title.x = element_text(size=25), title = element_text(size=30))
Rep.S.Plot=ggplot(dat, aes(Day, Percent, color=Treatment)) + geom_line(size=2)+facet_wrap(~Trial)+scale_x_continuous(breaks=0:max(Day))+theme(text = element_text(size=20))+ylab("Mean Survival (%)")+xlab("Time (Days)")+ggtitle("Field Isolate Replicate Survival for Anopheles gambiae")+theme
Rep.S.Plot

#Plot aggreggated data
attach(dat.agg)
limits = aes(ymax = per.mean + se, ymin= per.mean - se)
theme = theme_bw()+theme(text = element_text(size=25), axis.title.x = element_text(size=25), axis.title.x = element_text(size=25), title = element_text(size=30))
Agg.S.Plot.Error=ggplot(dat.agg, aes(Day, per.mean, color=Treatment))+geom_line(size=2)+ geom_errorbar(limits, width=0.25, size=1)+theme+scale_color_brewer(palette="Dark2")+xlab("Time (Days)")+ylab("Mean Survival (%)")+ggtitle("Field Isolate Survival for Anopheles gambiae")+scale_x_continuous(breaks=0:max(Day))
Agg.S.Plot.Error
Agg.S.Plot=ggplot(dat.agg, aes(Day, per.mean, color=Treatment))+geom_line(size=2)+ theme+scale_color_brewer(palette="Dark2")+ylab("Mean Survival (%)")+xlab("Time (Days)")+ggtitle("Field Isolate Survival for Anopheles gambiae")+scale_x_continuous(breaks=0:max(Day))
Agg.S.Plot

####Save plots and tables####

#Save plots as png
ggsave(Rep.S.Plot, file="Replicate Survival Plot.png", width=18, height=9)
ggsave(Agg.S.Plot, file="Aggregated Survival Plot.png", width=15, height=9)
ggsave(Agg.S.Plot.Error, file="Aggregated Survival Plot With Error Bars.png", width=15, height=9)

#Save tables in excel sheet
write.xlsx(dat, "LT50 Tables.xls", "Full Data", row.names=F, showNA=T)
write.xlsx(dat.per, "LT50 Tables.xls", "Percent Data", row.names=F, showNA=T, append=T)
colnames(dat.agg)[3]="Percent Mean"
write.xlsx(dat.agg, "LT50 Tables.xls", "Aggregated Data", row.names=F, showNA=T, append=T)
write.xlsx(LT50, "LT50 Tables.xls", "LT50", row.names=F, showNA=T, append=T)
write.xlsx(Abbott, "LT50 Tables.xls", "Abbott's Mortality", row.names=F, showNA=T, append=T)
write.xlsx(n.vals, "LT50 Tables.xls", "N Values", row.names=F, showNA=T, append=T)
