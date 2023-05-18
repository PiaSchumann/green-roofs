#packages
library(readxl)
library(nlme)
library(multcomp)
library(emmeans)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(vegan)

#load data: Vegetation 
D <- read_excel("~/Masterarbeit/Vegetation.xlsx")

#species list total
specieslisttotal <- aggregate(D$species, list("species"=D$species, "family"=D$family, "seedmixture"=D$seedmixture), FUN=length)
specieslisttotal <- specieslisttotal[,-4]


####finalizing data####

# adjusting the spcover values relative to the vegcover values on the subplots
sumCoverlist <- aggregate(D$spcover, list("date"=D$date, "plot"=D$plot, "subplot"=D$subplot), FUN=sum)
names(sumCoverlist)[4] <- "sumCover"
Dmerge <- merge(D, sumCoverlist, by=c("date", "plot", "subplot"), all=TRUE)
Dmerge[is.na(Dmerge$sumCover)]
Dmerge$newCover <- (Dmerge$spcover*Dmerge$"vegcover")/Dmerge$sumCover

# adding heterogeneity, elements and substrate as columns to the data, rename columns
plotinfo <- read_excel("~/Masterarbeit/plotinfo_withseeds.xlsx")
D <- merge(Dmerge, plotinfo, by=c("plot", "subplot"), all=TRUE)
names(D)[names(D)=="spcover"] <- "originalcover"
names(D)[names(D)=="newCover"] <- "spcover"

#separate seeding experiment (heahtland plots) and main experiment (Optigruen plots)
Dseed <- subset(D, D$elements=="seed")
D <- subset(D, D$elements!="seed")
D$substrate <- as.factor(D$substrate)
D$substrate <- factor(D$substrate, levels = c("5", "10", "15"))

#species list Optigruen plots
specieslist2 <- aggregate(D$species, list("species"=D$species, "family"=D$family, "seedmixture"=D$seedmixture), FUN=length)
specieslist2 <- specieslist2[,-4]

#species list heathland plots
specieslist3 <- aggregate(Dseed$species, list("species"=Dseed$species, "family"=Dseed$family, "seedmixture"=Dseed$seedmixture), FUN=length)
specieslist3 <- specieslist3[,-4]


##adding columns##

##plot and heterogeneity
plotinfo_plot <- plotinfo[,-c(2,3,5,6)] 
plotinfo_plot <- plotinfo_plot[!duplicated(plotinfo_plot[,c("plot", "heterogeneity")]),]

##blooming
#converting blooming to cover of blooming on the plot
D$spcoverblooming <- D$spcover*D$blooming/100
Dseed$spcoverblooming <- Dseed$spcover*Dseed$blooming/100

#adding cover of blooming per subplot
#Optigruen plots
coverblooming <- aggregate(D$spcoverblooming, list("round"=D$round, "plot"=D$plot, "subplot"=D$subplot), FUN=sum)
names(coverblooming)[4] <- "coverblooming"
coblo <- merge(coverblooming, plotinfo, by=c("subplot", "plot"))

#heathland plots
coverbloomingseed <- aggregate(Dseed$spcoverblooming, list("round"=Dseed$round, "plot"=Dseed$plot, "subplot"=Dseed$subplot), FUN=sum)
names(coverbloomingseed)[4] <- "coverblooming"

#converting cover values on plot level 
#Optigruen plots
cov2 <- data.frame(coverblooming)
cov2[cov2$subplot!="B", "coverblooming"] <- cov2[cov2$subplot!="B", "coverblooming"]*1.5 #Fläche A bzw C ist 1m*1,5m
cov2[cov2$subplot=="B", "coverblooming"] <- cov2[cov2$subplot=="B", "coverblooming"]*0.75 #Fläche B ist 0,5m*1,5m
Dnewbl <- aggregate(cov2$coverblooming, list("plot"=cov2$plot, "round"=cov2$round), FUN=sum)
names(Dnewbl)[3] <- "coverblooming"
Dnewbl$coverblooming <- Dnewbl$coverblooming/3.75 #Gesamtfläche ist 2,5m*1,5m
Dnewbl <- merge(Dnewbl, plotinfo_plot, by=c("plot"), all=TRUE)
Dnewbl <- na.omit(Dnewbl) #remove columns with NA (seed plots)

#heathland plots
covseed2 <- data.frame(coverbloomingseed)
covseed2[covseed2$subplot!="B", "coverblooming"] <- covseed2[covseed2$subplot!="B", "coverblooming"]*1.5 #Fläche A bzw C ist 1m*1,5m
covseed2[covseed2$subplot=="B", "coverblooming"] <- covseed2[covseed2$subplot=="B", "coverblooming"]*0.75 #Fläche B ist 0,5m*1,5m
Dnewblseed <- aggregate(covseed2$coverblooming, list("plot"=covseed2$plot, "round"=covseed2$round), FUN=sum)
names(Dnewblseed)[3] <- "coverblooming"
Dnewblseed$coverblooming <- Dnewblseed$coverblooming/3.75 #Gesamtfläche ist 2,5m*1,5m
Dnewblseed <- merge(Dnewblseed, plotinfo_plot, by=c("plot"), all=TRUE)
Dnewblseed <- na.omit(Dnewblseed) #remove columns with NA (seed plots)


##species richness
SR <- aggregate(D$species, list("round"=D$round, "plot"=D$plot, "subplot"=D$subplot), FUN=length)
names(SR)[4] <- "speciesrichness"
SR <- merge(plotinfo, SR, by=c( "plot", "subplot"), all=TRUE)

#separate seeding experiment and main experiment
SR_seed <- subset(SR, SR$elements=="seed") 
SR <- subset(SR, SR$elements!="seed")

#species richness on plot level
PlotSR <- aggregate(D[,c("species")], by=list("plot"=D$plot, "round"=D$round), FUN=function(x) length(unique(x)))
names(PlotSR)[3] <- "SR"
PlotSR <- merge(PlotSR, plotinfo_plot)


##vegetation cover
#mean of total vegcover on subplot level
Dvegco2 <- unique(D[,c("vegcover", "plot", "subplot", "round")])
Dvegco2 <- merge(Dvegco2, plotinfo, by=c("plot", "subplot"), all=TRUE)
Dvegco2$subplotID <- paste(Dvegco2$plot, Dvegco2$subplot, sep="")
Dvegco2$roof <- substr(Dvegco2$plot,1,1)
Dvegco2$round <- as.factor(Dvegco2$round)

#separate seeding experiment and main experiment
Dvegco2_seed <- subset(Dvegco2, Dvegco2$elements=="seed") 
Dvegco2 <- subset(Dvegco2, Dvegco2$elements!="seed")

#vegetation cover on plot level 
Dv <- unique(D[,c("vegcover", "plot", "subplot", "round")])
Dv[Dv$subplot!="B","vegcover"] <- Dv[Dv$subplot!="B","vegcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dv[Dv$subplot=="B", "vegcover"] <- Dv[Dv$subplot=="B", "vegcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dv$vegcover <- Dv$vegcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dnewvegco <- aggregate(Dv$vegcover, list("plot"=Dv$plot, "round"=Dv$round), FUN=sum)
names(Dnewvegco)[3] <- "vegcover"
Dnewvegco <- merge(Dnewvegco, plotinfo_plot, by=c("plot"), all=TRUE)
Dnewvegco <- na.omit(Dnewvegco) #remove colums with NA (seed plots)


##phenology: community weighted mean
Dcwm_phen <- D %>%                                           
  group_by(subplotID, round) %>% 
  summarise(weighted.mean(phenology, spcover, na.rm=TRUE))
names(Dcwm_phen)[3] <- "cwm_phen"
Dcwm_phen$plot <- substr(Dcwm_phen$subplotID, 1,3)
Dcwm_phen$subplot <- substr(Dcwm_phen$subplotID, 4,4)
Dphen <- merge(Dcwm_phen, plotinfo, by=c("subplot", "plot"), all=TRUE)
Dphen <- na.omit(Dphen) #remove colums with NA (seed plots)

##height: community weighted mean
D$height <- as.numeric(D$height)
Dcwm_height <- D %>%                                           
  group_by(subplotID, round) %>% 
  summarise(weighted.mean(height, spcover, na.rm=TRUE))
names(Dcwm_height)[3] <- "cwm_height"
Dcwm_height$plot <- substr(Dcwm_height$subplotID, 1,3)
Dcwm_height$subplot <- substr(Dcwm_height$subplotID, 4,4)
Dhe <- merge(Dcwm_height, plotinfo, by=c("subplot", "plot"), all=TRUE)
Dhe <- na.omit(Dhe) #remove colums with NA (seed plots)


### visualisation of seasonal patterns on plot level ###

colblo <- c("#CC3399","#CC3399","#CC3399","#CC3399","#CC3399","#CC3399","#CC3399","#CC3399","#CC3399")
colphen <- c("#33CC00","#33CC00","#33CC00","#33CC00","#33CC00","#33CC00","#33CC00","#33CC00","#33CC00")

#cover of blooming vegetation Optigruen plots
Dbloomdate <- data.frame(Dnewbl)
Dbloomdate$roof <- substr(Dbloomdate$plot,1,1)
Dbloomdate$roundroof <- paste(Dbloomdate$round,Dbloomdate$roof, sep="/")
coblodate1 <- ggplot(Dbloomdate, aes(y=coverblooming, x=roundroof, fill=roundroof)) + 
  geom_boxplot() + scale_fill_manual(values=colblo) + ylim(c(0,60)) +
  scale_y_continuous(breaks=seq(0, 40, 10)) +
  xlab("seasonal pattern") + ylab("cover blooming (Optigruen plots) [%]") + theme_classic()

#cover of blooming vegetation heathland plots
Dbloomdateseed <- data.frame(Dnewblseed)
Dbloomdateseed$roof <- substr(Dbloomdateseed$plot,1,1)
Dbloomdateseed$roundroof <- paste(Dbloomdateseed$round,Dbloomdateseed$roof, sep="/")
coblodate2 <- ggplot(Dbloomdateseed, aes(y=coverblooming, x=roundroof, fill=roundroof)) + 
  geom_boxplot() + scale_fill_manual(values=colblo) + ylim(c(0,60)) +
  scale_y_continuous(breaks=seq(0, 40, 10)) +
  coord_cartesian(ylim = c(0,40))  +
  xlab("seasonal pattern") + ylab("cover blooming (heathland plots) [%]") + theme_classic()

#phenology
Dphendate <- aggregate(Dphen$cwm_phen, list("round"=Dphen$round, "plot"=Dphen$plot), FUN=mean)
Dphendate$roof <- substr(Dphendate$plot,1,1)
Dphendate$roundroof <- paste(Dphendate$round,Dphendate$roof, sep="/")
names(Dphendate)[3] <- "cwm_phen"
phen1 <- ggplot(Dphendate, aes(y=cwm_phen, x=roundroof, fill=roundroof)) + 
  geom_boxplot() +  scale_fill_manual(values=colphen) +
  xlab("seasonal pattern") + ylab("phenology status") + theme_classic()

#save plots combined
plotcombo1 <- ggarrange(coblodate1 + rremove("xlab")+border(), coblodate2+ rremove("xlab") +border(), phen1+border(), ncol=1, nrow=3, labels=c("a", "b", "c"))%>%
  ggexport(filename = "season.pdf", width=8, height=10)


##diversity
#subplot level
D$site <- paste(D$plot, D$subplot, D$round, sep="/")
Ddiv <- data.frame(D$site, D$species, D$spcover, D$round)
colnames(Ddiv)[colnames(Ddiv) == "D.species"] <- "species"
colnames(Ddiv)[colnames(Ddiv) == "D.spcover"] <- "spcover"
colnames(Ddiv)[colnames(Ddiv) == "D.site"] <- "site"
Ddiv2 <- reshape(data=Ddiv,idvar="site",
                 v.names = "spcover",
                 timevar = "species",
                 direction="wide")
Ddiv2[is.na(Ddiv2)] <- 0
Ddiv2$shannon <- diversity(Ddiv2[,c(3:81)], index = "shannon") 
Ddiv2$simpson <- diversity(Ddiv2[,c(3:81)], index = "simpson") 
Ddiv2$plot <- substr(Ddiv2$site,1,3) 
Ddiv2$subplot <- substr(Ddiv2$site,5,5) 
Ddiv2$round <- substr(Ddiv2$site,7,8) 
Ddiv3 <- unique(Ddiv2[,c("shannon", "simpson", "plot", "subplot", "round")])
Ddiv3 <- merge(Ddiv3, plotinfo, by=c( "plot", "subplot"), all=TRUE)
Ddiv3 <- na.omit(Ddiv3) #remove colums with NA (seed plots)

#plot level
D$plotsite <- paste(D$plot, D$round, sep="/")
Ddivplot <- data.frame(D$plotsite, D$subplot, D$species, D$spcover)
colnames(Ddivplot)[colnames(Ddivplot) == "D.species"] <- "species"
colnames(Ddivplot)[colnames(Ddivplot) == "D.spcover"] <- "spcover"
colnames(Ddivplot)[colnames(Ddivplot) == "D.plotsite"] <- "plotsite"
colnames(Ddivplot)[colnames(Ddivplot) == "D.subplot"] <- "subplot"
Ddivplot[Ddivplot$subplot!="B", "spcover"] <- Ddivplot[Ddivplot$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Ddivplot[Ddivplot$subplot=="B", "spcover"] <- Ddivplot[Ddivplot$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dnewdiv <- aggregate(Ddivplot$spcover, list("plotsite"=Ddivplot$plotsite, "species"=Ddivplot$species), FUN=sum)
names(Dnewdiv)[3] <- "spcover"
Dnewdiv$spcover <- Dnewdiv$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Ddivplot2 <- reshape(data=Dnewdiv,idvar="plotsite",
                     v.names = "spcover",
                     timevar = "species",
                     direction="wide")
Ddivplot2[is.na(Ddivplot2)] <- 0
Ddivplot2$shannon <- diversity(Ddivplot2[,c(2:80)], index = "shannon") 
Ddivplot2$simpson <- diversity(Ddivplot2[,c(2:80)], index = "simpson") 
Ddivplot2$plot <- substr(Ddivplot2$plotsite,1,3) 
Ddivplot2$round <- substr(Ddivplot2$plotsite,5,6) 
Ddivplot3 <- unique(Ddivplot2[,c("shannon", "simpson", "plot", "round")])
Ddivplot3 <- merge(Ddivplot3, plotinfo_plot, by=c("plot"), all=TRUE)
Ddivplot3 <- na.omit(Ddivplot3) #remove columns with NA (seed plots)


##cover of species with dead individuals
#subplot level
Ddead <- subset(D, dead=="YES")
coverdead <- aggregate(Ddead$spcover, list("round"=Ddead$round, "plot"=Ddead$plot, "subplot"=Ddead$subplot), FUN=mean)
names(coverdead)[4] <- "coverdead"
coverdead <- merge(coverdead, plotinfo, by=c( "plot", "subplot"), all=TRUE)  
coverdead <- na.omit(coverdead)
coverdead$substrate <- as.factor(coverdead$substrate)
coverdead$elements <- as.factor(coverdead$elements)

#plot level
Ddeadplot <- data.frame(coverdead$round, coverdead$plot, coverdead$subplot, coverdead$coverdead)
names(Ddeadplot)[1] <- "round"
names(Ddeadplot)[2] <- "plot"
names(Ddeadplot)[3] <- "subplot"
names(Ddeadplot)[4] <- "coverdead"
Ddeadplot[Ddeadplot$subplot!="B", "coverdead"] <- Ddeadplot[Ddeadplot$subplot!="B", "coverdead"]*1.5 #Fläche A bzw C ist 1m*1,5m
Ddeadplot[Ddeadplot$subplot=="B", "coverdead"] <- Ddeadplot[Ddeadplot$subplot=="B", "coverdead"]*0.75 #Fläche B ist 0,5m*1,5m
Ddeadplot <- aggregate(Ddeadplot$coverdead, list("plot"=Ddeadplot$plot, "round"=Ddeadplot$round), FUN=sum)
names(Ddeadplot)[3] <- "coverdead"
Ddeadplot$coverdead <- Ddeadplot$coverdead/3.75 #Gesamtfläche ist 2,5m*1,5m
Ddeadplot <- merge(Ddeadplot, plotinfo_plot, by=c("plot"), all=TRUE)
Ddeadplot <- na.omit(Ddeadplot) #remove columns with NA (seed plots)


##cover of species with seedlings
#subplot level
Dseedlings <- subset(D, D$seedlings=="1")
Ds <- aggregate(Dseedlings$spcover, list("round"=Dseedlings$round, "subplotID"=Dseedlings$subplotID), FUN=mean)
names(Ds)[3] <- "meanspcover"
Ds$plot <- substr(Ds$subplotID,1,3)
Ds$subplot <- substr(Ds$subplotID,4,4)
Dseedlings <- merge(Ds, plotinfo, by=c("plot", "subplot"), all=TRUE)
Dseedlings <- Dseedlings[rowSums(is.na(Dseedlings))==0,]
Dseedlings$round <- as.factor(Dseedlings$round)
Dseedlings$substrate <- as.factor(Dseedlings$substrate)
Dseedlings$elements <- as.factor(Dseedlings$elements)

#plot level
Dsed <- data.frame(D)
Dsed[Dsed$subplot!="B", "spcover"] <- Dsed[Dsed$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dsed[Dsed$subplot=="B", "spcover"] <- Dsed[Dsed$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dsed <- aggregate(Dsed$spcover, list("plot"=Dsed$plot, "round"=Dsed$round), FUN=sum)
names(Dsed)[3] <- "spcover"
Dsed$spcover <- Dsed$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dsed <- merge(Dsed, plotinfo_plot, by=c("plot"), all=TRUE)
Dsed <- na.omit(Dsed) #remove columns with NA (seed plots)
Dsed$heterogeneity <- as.factor(Dsed$heterogeneity)

##parameters as factor
Dvegco2$substrate <- as.factor(Dvegco2$substrate)
Dvegco2$elements <- as.factor(Dvegco2$elements)
Dnewvegco$heterogeneity <- as.factor(Dnewvegco$heterogeneity)
Dhe$substrate <- as.factor(Dhe$substrate)
Dhe$elements <- as.factor(Dhe$elements)
Dhe$heterogeneity <- as.factor(Dhe$heterogeneity)
Dphen$elements <- as.factor(Dphen$elements)
Dphen$substrate <- as.factor(Dphen$substrate)
Dphen$heterogeneity <- as.factor(Dphen$heterogeneity)
Ddiv3$elements <- as.factor(Ddiv3$elements)
Ddiv3$substrate <- as.factor(Ddiv3$substrate)
Ddiv3$round <- as.factor(Ddiv3$round)
Ddivplot3$heterogeneity <- as.factor(Ddivplot3$heterogeneity)
Ddivplot3$round <- as.factor(Ddivplot3$round)
coblo$substrate <- as.factor(coblo$substrate)
coblo$elements <- as.factor(coblo$elements)
Dnewbl$heterogeneity <- as.factor(Dnewbl$heterogeneity)
coverdead$substrate <- as.factor(coverdead$substrate)
coverdead$elements <- as.factor(coverdead$elements)
Ddeadplot$heterogeneity <- as.factor(Ddeadplot$heterogeneity)
SR$substrate <- as.factor(SR$substrate)
SR$elements <- as.factor(SR$elements)
PlotSR$heterogeneity <- as.factor(PlotSR$heterogeneity)


### plots & models ### 

level_elements <- c("stones","deadwood","refuges","sand","control")
col_elements <- c("grey77","tan3","gray47","goldenrod2","darkseagreen")

#vegetation cover ~ substrate depth
modvegco1 <- lme(vegcover ~ substrate*round, random=~1|subplot/plot/round, data=Dvegco2[Dvegco2$subplot!="B",])
plot(modvegco1)
qqnorm(modvegco1)
anova(modvegco1)
summary(glht(modvegco1, linfct=mcp(substrate="Tukey")))
summary(glht(modvegco1, linfct=mcp(round="Tukey")))
summary(glht(modvegco1, lsm(pairwise ~ substrate | round)))

labelveg1 <- Dvegco2 %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("b", "b", "a", "c", "b","a", "c", "b", "a"))
round_veg <- data.frame(x = unique(Dvegco2$round), label = c("b", "a", "a"))
plotvegco1 <-  ggplot(Dvegco2, aes(x=round, y=vegcover, fill=factor(substrate, levels=c("5","10","15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("vegetation cover [%]") +  
  scale_y_continuous(breaks=seq(20, 80, 20)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelveg1, mapping=aes(x=round,  y=95,label=label), 
            position = position_dodge(width = .75)) + 
  geom_text(data = round_veg, aes(x = x, y = 105, label = label), inherit.aes = FALSE)+ theme_classic()

#vegetation cover ~ elements
modvegco2 <- lme(vegcover ~ elements*round, random=~1|subplot/plot/round, data=Dvegco2[Dvegco2$subplot=="B",])
plot(modvegco2)
qqnorm(modvegco2)
anova(modvegco2)
summary(glht(modvegco2, linfct=mcp(elements="Tukey")))
summary(glht(modvegco2, linfct=mcp(round="Tukey")))
summary(glht(modvegco2, lsm(pairwise ~ elements | round)))

labelveg2 <- Dvegco2 %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("a", "bc", "ad", "bd", "ce",  "a","c","ab","bc","c",  "a", "bc", "ad", "be", "cf"))
round_veg2 <- data.frame(x = unique(Dvegco2$round), label = c("a", "b", "a"))
plotvegco2 <- ggplot(Dvegco2, aes(x=round, y=vegcover, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("vegetation cover [%]") +  
  scale_y_continuous(breaks=seq(20, 80, 20)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelveg2, mapping=aes(x=round,  y=95,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_veg2, aes(x = x, y = 105, label = label), inherit.aes = FALSE)+ theme_classic()

#vegetation cover ~ heterogeneity
modvegco3 <- lme(vegcover ~ heterogeneity*round, random=~1|plot/round, data=Dnewvegco)
plot(modvegco3)
qqnorm(modvegco3)
anova(modvegco3)
summary(glht(modvegco3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modvegco3, linfct=mcp(round="Tukey")))
summary(glht(modvegco3, lsm(pairwise ~ heterogeneity | round)))

labelveg3 <- Dnewvegco %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a", "a", "b", "a", "b", "a"))
round_veg3 <- data.frame(x = unique(Dvegco2$round), label = c("b", "a", "a"))
plotvegco3 <- ggplot(Dnewvegco, aes(x=round, y=vegcover, fill=factor(heterogeneity, levels=c("homo", "hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("vegetation cover [%]") +  
  scale_y_continuous(breaks=seq(20, 100, 20)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelveg3, mapping=aes(x=round,  y=110,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_veg3, aes(x = x, y = 115, label = label), inherit.aes = FALSE)+ theme_classic()

#SR ~ substrate
modsr1 <- lme(speciesrichness ~ substrate*round, random=~1|subplot/plot/round, data=SR[SR$subplot!="B",])
plot(modsr1)
qqnorm(modsr1)
anova(modsr1)
summary(glht(modsr1, linfct=mcp(substrate="Tukey")))
summary(glht(modsr1, linfct=mcp(round="Tukey")))
summary(glht(modsr1, lsm(pairwise ~ substrate | round)))

labelsr1 <- SR %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("c","b", "a", "c","b", "a", "c","b", "a"))
round_sr <- data.frame(x = unique(SR$round), label = c("b", "b", "a"))
plotsr1 <- ggplot(SR, aes(x=round, y=speciesrichness, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("species richness") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelsr1, mapping=aes(x=round,  y=22.2,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_sr, aes(x = x, y = 23.8, label = label), inherit.aes = FALSE)+ theme_classic()

#SR ~ elements
modsr2 <- lme(speciesrichness ~ elements*round, random=~1|subplot/plot/round, data=SR[SR$subplot=="B",])
plot(modsr2)
qqnorm(modsr2)
anova(modsr2)
summary(glht(modsr2, linfct=mcp(elements="Tukey")))
summary(glht(modsr2, linfct=mcp(round="Tukey")))
summary(glht(modsr2, lsm(pairwise ~ elements | round)))

labelsr2 <- SR %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("bc", "ac", "b", "b", "a",  "ab", "ab", "b", "b", "a",  "ab", "a", "ab", "b", "a"))
plotsr2 <- ggplot(SR, aes(x=round, y=speciesrichness, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("species richness") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelsr2, mapping=aes(x=round,  y=22.2,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_sr, aes(x = x, y = 23.5, label = label), inherit.aes = FALSE)+ theme_classic()

#SR ~ heterogeneity
modsr3 <- lme(SR ~ heterogeneity*round, random=~1|plot/round, data=PlotSR)
plot(modsr3)
qqnorm(modsr3)
anova(modsr3)
summary(glht(modsr3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modsr3, linfct=mcp(round="Tukey")))
summary(glht(modsr3, lsm(pairwise ~ heterogeneity | round)))

labelsr3 <- PlotSR %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a","a", "a", "a", "a", "a"))
plotsr3 <- ggplot(PlotSR, aes(x=round, y=SR, fill=factor(heterogeneity, levels=c("homo", "hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("species richness") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelsr3, mapping=aes(x=round,  y=29,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_sr, aes(x = x, y = 31, label = label), inherit.aes = FALSE)+ theme_classic()

#coverblooming ~ substrate
modbloom1 <- lme(coverblooming ~ substrate*round, random=~1|plot/subplot/round, data=coblo[coblo$subplot!="B",])
plot(modbloom1)
qqnorm(modbloom1)
anova(modbloom1)
summary(glht(modbloom1, linfct=mcp(substrate_f="Tukey")))
summary(glht(modbloom1, linfct=mcp(round="Tukey")))
summary(glht(modbloom1, lsm(pairwise ~ substrate | round)))

labelcoblo1 <- coblo %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("b", "a","a", "b", "ab","a", "b", "b", "a"))
round_coblo <- data.frame(x = unique(coblo$round), label = c("a", "b", "b"))
plotcoblo1 <- ggplot(coblo, aes(x=round, y=coverblooming, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("blooming vegetation [%]") + 
  scale_y_continuous(breaks=seq(0, 40, 10)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelcoblo1, mapping=aes(x=round,  y=55,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_coblo, aes(x = x, y = 60, label = label), inherit.aes = FALSE)+ theme_classic()

#coverblooming ~ elements
modbloom2 <- lme(coverblooming ~ elements*round, random=~1|plot/subplot/round, data=coblo[coblo$subplot=="B",])
plot(modbloom2) 
qqnorm(modbloom2)
anova(modbloom2)
summary(glht(modbloom2, linfct=mcp(elements="Tukey")))
summary(glht(modbloom2, linfct=mcp(round="Tukey")))
summary(glht(modbloom2, lsm(pairwise ~ elements | round)))

labelcoblo2 <- coblo %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("a", "b", "ac","bc", "b",  "a", "a", "a","a", "a",  "a", "a", "a","a", "a"))
plotcoblo2 <- ggplot(coblo, aes(x=round, y=coverblooming, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("blooming vegetation [%]") +  
  scale_y_continuous(breaks=seq(0, 40, 10)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelcoblo2, mapping=aes(x=round,  y=55,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_coblo, aes(x = x, y = 60, label = label), inherit.aes = FALSE)+ theme_classic()

#coverblooming ~ heterogeneity
modbloom3 <- lme(coverblooming ~ heterogeneity*round, random=~1|plot/round, data=Dnewbl)
plot(modbloom3)
qqnorm(modbloom3)
anova(modbloom3)
summary(glht(modbloom3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modbloom3, linfct=mcp(round="Tukey")))
summary(glht(modbloom3, lsm(pairwise ~ heterogeneity | round)))

labelcoblo3 <- Dnewbl %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a", "a", "a","a", "a","a"))
plotcoblo3 <- ggplot(Dnewbl, aes(x=round, y=coverblooming, fill=factor(heterogeneity, levels=c("homo","hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("blooming vegetation [%]") +  
  scale_y_continuous(breaks=seq(0, 40, 10)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelcoblo3, mapping=aes(x=round,  y=55,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_coblo, aes(x = x, y = 60, label = label), inherit.aes = FALSE)+ theme_classic()

#seedlings ~ substrate 
modseedlings1 <- lme(log(meanspcover) ~ substrate*round, random=~1|subplot/plot/round, data=Dseedlings[Dseedlings$subplot!="B",])
plot(modseedlings1) 
qqnorm(modseedlings1)
anova(modseedlings1)
summary(glht(modseedlings1, linfct=mcp(substrate="Tukey")))
summary(glht(modseedlings1, linfct=mcp(round="Tukey")))
summary(glht(modseedlings1, lsm(pairwise ~ substrate | round)))

labelsed1 <- Dseedlings %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("a", "a", "a", "b", "a", "a","a", "a", "a"))
round_sed <- data.frame(x = unique(Dseedlings$round), label = c("a", "a", "b"))
plotsed1 <- ggplot(Dseedlings, aes(x=round, y=meanspcover, fill=factor(substrate, levels=c("5","10","15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("cover of seedlings [%]") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  scale_y_continuous(breaks=seq(0, 30, 10)) +
  geom_text(data=labelsed1, mapping=aes(x=round,  y=45,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_sed, aes(x = x, y = 48, label = label), inherit.aes = FALSE) + theme_classic()

#seedlings ~ elements
modseedlings2 <- lme(log(meanspcover) ~ elements*round, random=~1|subplot/plot/round, data=Dseedlings[Dseedlings$subplot=="B",])
plot(modseedlings2)
qqnorm(modseedlings2)
anova(modseedlings2)
summary(glht(modseedlings2, linfct=mcp(elements="Tukey")))
summary(glht(modseedlings2, linfct=mcp(round="Tukey")))
summary(glht(modseedlings2, lsm(pairwise ~ elements | round)))

labelsed2 <- Dseedlings %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("a", "ab", "a", "a", "b", "a", "a", "a", "a", "a","a", "b", "a", "ab", "b"))
round_sed2 <- data.frame(x = unique(Dseedlings$round), label = c("a", "a", "a"))
plotsed2 <- ggplot(Dseedlings, aes(x=round, y=meanspcover, fill=factor(elements, level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("cover of seedlings [%]") +  
  scale_y_continuous(breaks=seq(0, 30, 10)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelsed2, mapping=aes(x=round,  y=45,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_sed2, aes(x = x, y = 48, label = label), inherit.aes = FALSE) + theme_classic()

#seedlings ~ heterogeneity
modseedlings3 <- lme(log(spcover) ~ heterogeneity*round, random=~1|plot/round, data=Dsed)
plot(modseedlings3)
qqnorm(modseedlings3)
anova(modseedlings3)
summary(glht(modseedlings3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modseedlings3, linfct=mcp(round="Tukey")))
summary(glht(modseedlings3, lsm(pairwise ~ heterogeneity | round)))

labelsed3 <- Dsed %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a", "a", "a", "a", "a", "a"))
plotsed3 <- ggplot(Dsed, aes(x=round, y=spcover, fill=factor(heterogeneity, levels=c("homo", "hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("cover of seedlings [%]") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) + 
  scale_y_continuous(breaks=seq(40, 100, 20)) +
  geom_text(data=labelsed3, mapping=aes(x=round,  y=150,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_sed, aes(x = x, y = 165, label = label), inherit.aes = FALSE)+ theme_classic()

#height ~ substrate 
modheight1 <- lme(cwm_height ~ substrate*round, random=~1|subplot/plot/round, data=Dhe[Dhe$subplot!="B",])
plot(modheight1)
qqnorm(modheight1)
anova(modheight1)
summary(glht(modheight1, linfct=mcp(substrate="Tukey")))
summary(glht(modheight1, linfct=mcp(round="Tukey")))
summary(glht(modheight1, lsm(pairwise ~ substrate | round)))

labelhe1 <- Dhe %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("c", "b", "a", "c", "b", "a", "c", "b", "a"))
round_he <- data.frame(x = unique(Dhe$round), label = c("a", "b", "c"))
plotheight1 <- ggplot(Dhe, aes(x=round, y=cwm_height, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("plant height [cm]") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelhe1, mapping=aes(x=round,  y=40,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_he, aes(x = x, y = 45, label = label), inherit.aes = FALSE)+ theme_classic()

#height ~ elements
modheight2 <- lme(cwm_height ~ elements*round, random=~1|subplot/plot/round, data=Dhe[Dhe$subplot=="B",])
plot(modheight2)
qqnorm(modheight2)
anova(modheight2)
summary(glht(modheight2, linfct=mcp(elements="Tukey")))
summary(glht(modheight2, linfct=mcp(round="Tukey")))
summary(glht(modheight2, lsm(pairwise ~ elements | round)))

labelhe2 <- Dhe %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("b", "a", "b", "b", "a",  "b", "a", "b", "b", "a",  "b", "a", "b", "b", "a"))
round_he2 <- data.frame(x = unique(Dhe$round), label = c("a", "a", "b"))
plotheight2 <- ggplot(Dhe, aes(x=round, y=cwm_height, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("plant height [cm]") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelhe2, mapping=aes(x=round,  y=40,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_he2, aes(x = x, y = 43, label = label), inherit.aes = FALSE)+ theme_classic()

#height ~ heterogeneity
modheight3 <- lme(cwm_height ~ heterogeneity*round, random=~1|plot/round, data=Dhe)
plot(modheight3)
qqnorm(modheight3)
anova(modheight3)
summary(glht(modheight3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modheight3, linfct=mcp(round="Tukey")))
summary(glht(modheight3, lsm(pairwise ~ heterogeneity | round)))

labelhe3 <- Dhe %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a", "a","a", "a","a", "a"))
plotheight3 <- ggplot(Dhe, aes(x=round, y=cwm_height, fill=factor(heterogeneity, levels=c("homo","hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1", "orangered2")) + 
  xlab("round") + ylab("plant height [cm]") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelhe3, mapping=aes(x=round,  y=40,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_he2, aes(x = x, y = 45, label = label), inherit.aes = FALSE)+ theme_classic()

#phenology ~ substrate
modphen1 <- lme(cwm_phen ~ substrate*round, random=~1|subplotID/round, data=Dphen[Dphen$subplot!="B",])
plot(modphen1) 
qqnorm(modphen1) 
anova(modphen1)
summary(glht(modphen1, linfct=mcp(substrate="Tukey")))
summary(glht(modphen1, linfct=mcp(round="Tukey")))
summary(glht(modphen1, lsm(pairwise ~ substrate | round)))

labelphen1 <- Dphen %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("a", "a", "a","a", "a", "a","a", "a", "a"))
round_phen <- data.frame(x = unique(Dphen$round), label = c("b", "a", "ab"))
plotphen1 <- ggplot(Dphen, aes(x=round, y=cwm_phen, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("phenology status") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelphen1, mapping=aes(x=round,  y=5,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_phen, aes(x = x, y = 5.3, label = label), inherit.aes = FALSE)+ theme_classic()

#phenology ~ elements
modphen2 <- lme(cwm_phen ~ elements*round, random=~1|subplotID/round, data=Dphen[Dphen$subplot=="B",])
plot(modphen2) 
qqnorm(modphen2) 
anova(modphen2)
summary(glht(modphen2, linfct=mcp(elements="Tukey")))
summary(glht(modphen2, linfct=mcp(round="Tukey")))
summary(glht(modphen2, lsm(pairwise ~ elements | round)))

labelphen2 <- Dphen %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("a", "a", "a","a", "a", "a", "a", "a", "a","a", "a", "a", "a", "a","a"))
plotphen2 <- ggplot(Dphen, aes(x=round, y=cwm_phen, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("phenology status") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelphen2, mapping=aes(x=round,  y=5,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_phen, aes(x = x, y = 5.3, label = label), inherit.aes = FALSE)+ theme_classic()

#phenology ~ heterogeneity
modphen3 <- lme(cwm_phen ~ heterogeneity*round, random=~1|plot/round, data=Dphen)
plot(modphen3) 
qqnorm(modphen3) 
anova(modphen3)
summary(glht(modphen3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modphen3, linfct=mcp(round="Tukey")))
summary(glht(modphen3, lsm(pairwise ~ heterogeneity | round)))

labelphen3 <- Dphen %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a", "a", "a","a", "a", "a"))
round_phen3 <- data.frame(x = unique(Dphen$round), label = c("c", "a", "b"))
plotphen3 <- ggplot(Dphen, aes(x=round, y=cwm_phen, fill=factor(heterogeneity, levels=c("homo","hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("phenology status") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelphen3, mapping=aes(x=round,  y=5,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_phen3, aes(x = x, y = 5.3, label = label), inherit.aes = FALSE)+ theme_classic()

#shannon ~ substrate
modshan1 <- lme(shannon ~ substrate*round, random=~1|subplot/plot/round, data=Ddiv3[Ddiv3$subplot!="B",])
plot(modshan1)
qqnorm(modshan1)
anova(modshan1)
summary(glht(modshan1, linfct=mcp(substrate="Tukey")))
summary(glht(modshan1, linfct=mcp(round="Tukey")))
summary(glht(modshan1, lsm(pairwise ~ substrate | round)))

labelshan1 <- Ddiv3 %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("c", "b", "a","c", "b", "a","b", "a", "a"))
round_shan <- data.frame(x = unique(Ddiv3$round), label = c("b", "b", "a"))
plotshan1 <- ggplot(Ddiv3, aes(x=round, y=shannon, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("shannon index") +  
  scale_y_continuous(breaks=seq(1.0, 2.6, 0.4)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelshan1, mapping=aes(x=round,  y=3,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_shan, aes(x = x, y = 3.3, label = label), inherit.aes = FALSE)+ theme_classic()

#shannon ~ elements
modshan2 <- lme(shannon ~ elements*round, random=~1|subplot/plot/round, data=Ddiv3[Ddiv3$subplot=="B",])
plot(modshan2)
qqnorm(modshan2)
anova(modshan2)
summary(glht(modshan2, linfct=mcp(elements="Tukey")))
summary(glht(modshan2, linfct=mcp(round="Tukey")))
summary(glht(modshan2, lsm(pairwise ~ elements | round)))

labelshan2 <- Ddiv3 %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("ab", "a", "b", "b", "a",  "ab", "a", "b", "b", "a",  "ab", "ab", "ab", "b", "a"))
plotshan2 <- ggplot(Ddiv3, aes(x=round, y=shannon, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("shannon index") +  
  scale_y_continuous(breaks=seq(1.0, 2.6, 0.4)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelshan2, mapping=aes(x=round,  y=3,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_shan, aes(x = x, y = 3.3, label = label), inherit.aes = FALSE)+ theme_classic()

#shannon ~ heterogeneity
modshan3 <- lme(shannon ~ heterogeneity*round, random=~1|plot/round, data=Ddivplot3)
plot(modshan3)
qqnorm(modshan3)
anova(modshan3)
summary(glht(modshan3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modshan3, linfct=mcp(round="Tukey")))
summary(glht(modshan3, lsm(pairwise ~ heterogeneity | round)))

labelshan3 <- Ddivplot3 %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c( "a",  "a", "a", "a", "a", "a"))
plotshan3 <- ggplot(Ddivplot3, aes(x=round, y=shannon, fill=factor(heterogeneity, levels=c("homo", "hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("shannon index") +  
  scale_y_continuous(breaks=seq(1.6, 2.8, 0.4)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelshan3, mapping=aes(x=round,  y=3,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_shan, aes(x = x, y = 3.2, label = label), inherit.aes = FALSE)+ theme_classic()

#simpson ~ substrate
modsimp1 <- lme(simpson ~ substrate*round, random=~1|subplot/plot/round, data=Ddiv3[Ddiv3$subplot!="B",])
plot(modsimp1)
qqnorm(modsimp1)
anova(modsimp1)
summary(glht(modsimp1, linfct=mcp(substrate="Tukey")))
summary(glht(modsimp1, linfct=mcp(round="Tukey")))
summary(glht(modsimp1, lsm(pairwise ~ substrate | round)))

labelsimp1 <- Ddiv3 %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("c", "b", "a","b", "a", "a","b", "a", "a"))
round_simp <- data.frame(x = unique(Ddiv3$round), label = c("b", "c", "a"))
plotsimp1 <- ggplot(Ddiv3, aes(x=round, y=simpson, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("simpson index") +  
  scale_y_continuous(breaks=seq(0.4, 1.0, 0.2)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelsimp1, mapping=aes(x=round,  y=1.2,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_simp, aes(x = x, y = 1.3, label = label), inherit.aes = FALSE)+ theme_classic()

#simpson ~ elements
modsimp2 <- lme(simpson ~ elements*round, random=~1|subplot/plot/round, data=Ddiv3[Ddiv3$subplot=="B",])
plot(modsimp2)
qqnorm(modsimp2)
anova(modsimp2)
summary(glht(modsimp2, linfct=mcp(elements="Tukey")))
summary(glht(modsimp2, linfct=mcp(round="Tukey")))
summary(glht(modsimp2, lsm(pairwise ~ elements | round)))

labelsimp2 <- Ddiv3 %>% 
  distinct(round, elements) %>% 
  arrange(round, elements) %>% 
  mutate(label = c("ab", "a","bc","b","ac",  "ab", "a", "b", "b", "a",  "a", "a", "a", "a", "a"))
round_simp2 <- data.frame(x = unique(Ddiv3$round), label = c("a", "a", "a"))
plotsimp2 <- ggplot(Ddiv3, aes(x=round, y=simpson, fill=factor(elements, levels=level_elements))) + 
  geom_boxplot() + scale_fill_manual("structural elements", values=col_elements) + 
  xlab("round") + ylab("simpson index") +  
  scale_y_continuous(breaks=seq(0.4, 1.0, 0.2)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labelsimp2, mapping=aes(x=round,  y=1.2,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_simp2, aes(x = x, y = 1.3, label = label), inherit.aes = FALSE)+ theme_classic()

#simpson ~ heterogeneity
modsimp3 <- lme(simpson ~ heterogeneity*round, random=~1|plot/round, data=Ddivplot3)
plot(modsimp3)
qqnorm(modsimp3)
anova(modsimp3)
summary(glht(modsimp3, linfct=mcp(heterogeneity="Tukey")))
summary(glht(modsimp3, linfct=mcp(round="Tukey")))
summary(glht(modsimp3, lsm(pairwise ~ heterogeneity | round)))

labelsimp3 <- Ddivplot3 %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c( "a",  "a", "a", "a", "a", "a"))
round_simp3 <- data.frame(x = unique(Ddivplot3$round), label = c("a", "b", "b"))
plotsimp3 <- ggplot(Ddivplot3, aes(x=round, y=simpson, fill=factor(heterogeneity, levels=c("homo", "hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("simpson index") +  
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  scale_y_continuous(breaks=seq(0.6, 1.0, 0.1)) +
  geom_text(data=labelsimp3, mapping=aes(x=round,  y=1.1,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_simp3, aes(x = x, y = 1.15, label = label), inherit.aes = FALSE)+ theme_classic()

#coverdead ~ substrate
moddead1 <- lme(log(coverdead) ~ substrate*round, random=~1|plot/subplot/round, data=coverdead[coverdead$subplot!="B",])
plot(moddead1) 
qqnorm(moddead1)
anova(moddead1)
summary(glht(moddead1, linfct=mcp(round="Tukey")))
summary(glht(moddead1, lsm(pairwise ~ substrate | round)))

labeldead1 <- coverdead %>% 
  distinct(round, substrate) %>% 
  arrange(round, substrate) %>% 
  mutate(label = c("a", "a", "a", "a", "a", "a", "a", "a", "a"))
round_dead1 <- data.frame(x = unique(coverdead$round), label = c("a", "a", "a"))
plotdead1 <- ggplot(coverdead, aes(x=round, y=coverdead, fill=factor(substrate, levels=c("5", "10", "15")))) + 
  geom_boxplot() + scale_fill_brewer("substrate height [cm]") + 
  xlab("round") + ylab("cover of dead vegetation [%]") +    
  scale_y_continuous(breaks=seq(0, 10, 2.5)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labeldead1, mapping=aes(x=round,  y=13,label=label), 
            position = position_dodge(width = .75))+ theme_classic() +
  geom_text(data = round_dead1, aes(x = x, y = 14, label = label), inherit.aes = FALSE)+ theme_classic()

#coverdead ~ elements
moddead2 <- lme(log(coverdead) ~ elements*round, random=~1|subplot/plot/round, data=coverdead[coverdead$subplot=="B",], method="ML")

#coverdead ~ heterogeneity
moddead3 <- lme(log(coverdead) ~ heterogeneity*round, random=~1|plot/round, data=Ddeadplot)
plot(moddead3)
qqnorm(moddead3)
anova(moddead3)
summary(glht(moddead3, linfct=mcp(round="Tukey")))
summary(glht(moddead3, lsm(pairwise ~ heterogeneity | round)))

labeldead3 <- Ddeadplot %>% 
  distinct(round, heterogeneity) %>% 
  arrange(round, heterogeneity) %>% 
  mutate(label = c("a", "a", "a", "a", "a", "a"))
round_dead3 <- data.frame(x = unique(Ddeadplot$round), label = c("a", "a", "a"))
plotdead3 <- ggplot(Ddeadplot, aes(x=round, y=coverdead, fill=factor(heterogeneity, levels=c("homo","hetero")))) + 
  geom_boxplot() + scale_fill_manual("grade of heterogeneity", values=c("orange1","orangered2")) + 
  xlab("round") + ylab("cover of dead vegetation [%]") +  
  scale_y_continuous(breaks=seq(0, 10, 2.5)) +
  theme(legend.position = "bottom",legend.box = "vertical",legend.title.align = 0) +
  geom_text(data=labeldead3, mapping=aes(x=round,  y=11,label=label), 
            position = position_dodge(width = .75)) +
  geom_text(data = round_dead3, aes(x = x, y = 12, label = label), inherit.aes = FALSE)+ theme_classic()


##combined plots

#substrate depth
ggarrange(plotvegco1 + rremove("xlab") + border(), plotsr1+ rremove("xlab")+ border(), plotcoblo1 + rremove("xlab")+ border(), plotsed1+border()+ rremove("xlab"), plotheight1 + rremove("xlab")+ border(), plotphen1 +border()+ rremove("xlab"), plotshan1+ border(), plotsimp1+ border() ,
          ncol=2, nrow=4, 
          labels=c("a", "b", "c", "d", "e", "f","g","h"),
          common.legend=TRUE, legend="bottom") %>%
  ggexport(filename = "substr.pdf", width=8, height=10)

#elements
plotcombo3 <- ggarrange(plotvegco2 + rremove("xlab") + border(), plotsr2+ rremove("xlab")+ border(), plotcoblo2 + rremove("xlab")+ border(), plotsed2 +border()+ rremove("xlab"), plotheight2 + rremove("xlab")+ border(), plotphen2 +border()+ rremove("xlab"), plotshan2 + border(), plotsimp2+ border() ,
                        ncol=2, nrow=4, 
                        labels=c("a", "b", "c", "d", "e", "f","g","h"),
                        common.legend=TRUE, legend="bottom")%>%
  ggexport(filename = "elem.pdf", width=8, height=10)

#heterogeneity
plotcombo4 <- ggarrange(plotvegco3 + rremove("xlab")+ border(), plotsr3+ rremove("xlab")+ border(), plotcoblo3 + rremove("xlab")+ border(), plotsed3 +border()+ rremove("xlab"), plotheight3 + rremove("xlab")+ border(), plotphen3 +border()+ rremove("xlab"), plotshan3 + border(), plotsimp3+ border() ,
                        ncol=2, nrow=4, 
                        labels=c("a", "b", "c", "d", "e", "f","g","h"),
                        common.legend=TRUE, legend="bottom")%>%
  ggexport(filename = "het.pdf", width=8, height=10)

#deadcover appendix
plotcombo5 <- ggarrange(plotdead1+ border(), plotdead3+ border(), 
                        ncol=2, nrow=1, 
                        labels=c("a", "b"),
                        legend="bottom")%>%
  ggexport(filename = "dead.pdf", width=8, height=3.5)

