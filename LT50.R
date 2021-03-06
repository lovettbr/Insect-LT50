####Insect LT50 Analysis 12/6/16 Lovett####

####Load packages####

#install.packages(c("reshape", "plyr", "ggplot2", "xlsx"))
library(reshape)
library(plyr)
library(ggplot2)
library("xlsx")
library(MASS)

####Import and format data####
location="~/Downloads/Example_Data.csv"
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
dat=ddply(dat, .(Treatment, Trial), transform, Percent=(n-cumsum(Value))/(n)*100, Alive=n-cumsum(Value), Dead=cumsum(Value))
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
surv.per=0.50
LT50=ddply(dat.min,.(Trial, Treatment), summarize,
             LT=as.numeric(dose.p(glm(cbind(Alive,Dead)~Day,binomial),p=surv.per)))
LT50[LT50$LT<0,]$LT=NA
LT50.Error=ddply(LT50, .(Treatment), summarize, "LT50 Mean"=mean(LT,na.rm=T), se=sd(LT, na.rm=T)/sqrt(length(LT[!is.na(LT)])), Replicates=length(LT[!is.na(LT)]))

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
write.xlsx(LT50.Error, "LT50 Tables.xls", "Abbott's Mortality", row.names=F, showNA=T, append=T)
write.xlsx(n.vals, "LT50 Tables.xls", "N Values", row.names=F, showNA=T, append=T)
