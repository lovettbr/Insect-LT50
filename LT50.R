####Insect LT50 Analysis 4/13/15 Lovett####

library(reshape)
library(plyr)
library(ggplot2)
library(xtable)

####Import Data####
dat <- read.csv("~/Desktop/Etienne Mosquito Mortality Isolates 1-6.csv")
dat[is.na(dat)]=0
dat2=dat
dat2=melt(dat, colnames(dat)[1:2], colnames(dat)[3:length(colnames(dat))])

n.vals=tapply(dat2$value, dat2[c("variable", "Trial")], FUN=sum)
n.vals=melt(n.vals, n.vals[1], n.vals[1:length(colnames(n.vals))])
colnames(n.vals)=c("variable", "Trial", "n")
View(n.vals)

dat2=merge(dat2, n.vals, by=c("variable", "Trial"))
dat2=ddply(dat2,.(variable, Trial),transform,csum=cumsum(value))
dat2$csub=dat2$n-dat2$csum
dat2$per=dat2$csub/dat2$n*100
dat2=dat2[dat2$day!="Alive",]
dat2$day=as.numeric(as.character(dat2$day))

trial.min=min(ddply(dat2, c("Trial"), summarize, duration=max(day))$duration)
LT50=ddply(subset(dat.min, per<= 50), c("Trial", "variable"), summarize, LT50=min(day))
LT50.compiled=ddply(LT50, "variable", summarize, LT50.mean=mean(LT50), se=sd(LT50)/sqrt(length(LT50)))
View(LT50.compiled)
dat.min=dat2[dat2$day<=trial.min,]
dat.min$se=ddply(dat.min, c("day", "variable"), summarize, se=sd(per)/sqrt(length(per)))$se

attach(dat2)
limits <- aes(ymax = per + se, ymin= per - se)
ggplot(dat2, aes(day, per, color=variable)) + geom_line()+facet_wrap(~Trial)
ggplot(dat2, aes(day, per, color=variable)) + geom_line()+facet_wrap(~Trial)+ geom_errorbar(limits, position="dodge", width=0.25)

dat.agg=ddply(dat.min, c("variable", "day"), summarize, per.mean=mean(per), se=sd(per)/sqrt(length(per)))
#dat.agg=aggregate(per~variable+day, data=dat.min, mean)

attach(dat.agg)
limits <- aes(ymax = per.mean + se, ymin= per.mean - se)
ggplot(dat.agg, aes(day, per.mean, color=variable))+geom_line()+ geom_errorbar(limits, position="dodge", width=0.25)

print.xtable.booktabs <- function(x){
  
  print(xtable(x),
        floating=F,
        hline.after=NULL, 
        add.to.row=list(pos=list(-1,0, nrow(x)), 
                        command=c(
                          '\\toprule ',
                          '\\midrule ',
                          '\\bottomrule ')))
  
}
print.xtable.booktabs(LT50.compiled)