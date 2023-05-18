#packages
library(readxl)
library(vegan)

## substrate height (subplot level)
Dsubstrate <- subset(D, D$subplot!="B")
Dsubstrate <- Dsubstrate[,c("species", "spcover", "substrate", "plot")]

comp6 <- reshape(Dsubstrate, 
                 timevar = "species", idvar = c("substrate","plot"),
                 direction = "wide", drop=NULL)
comp6[is.na(comp6)] <- 0
comp6.1 <- round(comp6[,3:74], digit=1)
comp6.2 <- comp6[1:2]
comp6.1 <- as.matrix(comp6.1)

run6 <- metaMDS(comp6.1, distance="bray", k=2)
plot(run6)
stressplot(run6)

comp6$col<-c("lightcyan2", "lightskyblue2","dodgerblue3")[as.numeric(factor(comp6$substrate))]
plot6 <- plot(run6, display = "sites", type = "n", main="Vegetation composition sorted by substrate depth")
plot6 + points(run6, choices = c(1,2), display = "sites", col = comp6$col, pch = 19)
plot6 + ordihull(run6,comp6$substrate,display = "sites",draw = c("polygon"),col = NULL,border = c("lightcyan2", "lightskyblue2","dodgerblue3"),lty = c(1, 2),lwd = 1)
plot6 + legend("topleft", legend = levels(factor(comp6$substrate)),col = c("lightcyan2", "lightskyblue2","dodgerblue3"),bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.07))

anosim(comp6.1, comp6$substrate, permutations = 999, distance = "bray", strata = NULL)


## structural elements (subplot level)
Delements <- subset(D, D$subplot=="B")
Delements <- Delements[,c("species", "spcover", "elements", "plot")]

comp5 <- reshape(Delements, 
                 timevar = "species", idvar = c("elements","plot"),
                 direction = "wide", drop=NULL)
comp5[is.na(comp5)] <- 0
comp5.1 <- round(comp5[,3:66], digit=1)
comp5.2 <- comp5[1:2]
comp5.1 <- as.matrix(comp5.1)

run5 <- metaMDS(comp5.1, distance="bray", k=2)
plot(run5)
stressplot(run5)

comp5$col<-c("darkseagreen", "tan3","gray47","goldenrod2","grey77")[as.numeric(factor(comp5$elements))]
plot5 <- plot(run5, display = "sites", type = "n", main="Vegetation composition sorted by habitat elements")
plot5 + points(run5, choices = c(1,2), display = "sites", col = comp5$col, pch = 19)
plot5 + ordihull(run5,comp5$elements,display = "sites",draw = c("polygon"),col = NULL,border = c("darkseagreen", "tan3","gray47","goldenrod2","grey77"),lty = c(1, 2),lwd = 1)
plot5 + legend("topleft", legend = levels(factor(comp5$elements)),col = c("darkseagreen", "tan3","gray47","goldenrod2","grey77"),bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.07))

anosim(comp5.1, comp5$elements, permutations = 999, distance = "bray", strata = NULL)


## seed mixtures (plot level)
#Optigruen plots (only homogenous plots and B plots control group)
Dhomo <- subset(D, D$heterogeneity=="homo")
Dhomo <- subset(Dhomo, Dhomo$elements=="control")
Dhomo <- Dhomo[c("subplot", "plot", "spcover", "species", "round", "Seedmixture")]
Dhomo[Dhomo$subplot!="B", "spcover"] <- Dhomo[Dhomo$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dhomo[Dhomo$subplot=="B", "spcover"] <- Dhomo[Dhomo$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dhomo$spcover <- Dhomo$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dhomo2 <- aggregate(Dhomo$spcover, list("round"=Dhomo$round, "plot"=Dhomo$plot, "Seedmixture"=Dhomo$Seedmixture, "species"=Dhomo$species), FUN=mean)
names(Dhomo2)[5] <- "meanspcover"
#heathland plots
Dseed <- Dseed[c("subplot", "plot", "spcover", "species", "round", "Seedmixture")]
Dseed[Dseed$subplot!="B", "spcover"] <- Dseed[Dseed$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dseed[Dseed$subplot=="B", "spcover"] <- Dseed[Dseed$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dseed$spcover <- Dseed$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dseed2 <- aggregate(Dseed$spcover, list("round"=Dseed$round, "plot"=Dseed$plot, "Seedmixture"=Dseed$Seedmixture, "species"=Dseed$species), FUN=mean)
names(Dseed2)[5] <- "meanspcover"

comp1 <- rbind(Dhomo2[,c("species","meanspcover", "plot", "round", "Seedmixture")], Dseed2[,c("species","meanspcover", "plot", "round", "Seedmixture")])
comp1 <- reshape(comp1, 
                 timevar = "species", idvar = c("plot", "round","Seedmixture"),
                 direction = "wide")
comp1[is.na(comp1)] <- 0
comp1.1 <- round(comp1[,4:73], digit=1)
comp1.2 <- comp1[,1:3]

run1 <- metaMDS(comp1.1, distance="bray", k=2)
plot(run1)
stressplot(run1)

comp1$col<-c("dodgerblue2", "deeppink2","orange1", "chartreuse3")[as.numeric(factor(comp1$Seedmixture))]
plot1 <- plot(run1, display = "sites", type = "n", main="Vegetation composition sorted by seed mixture")
plot1 + points(run1, choices = c(1,2), display = "sites", col = comp1$col, pch = 19)
plot1 + ordihull(run1,comp1$Seedmixture,display = "sites",draw = c("polygon"),col = NULL,border = c("dodgerblue2", "deeppink2","orange1", "chartreuse3"), lty = c(1, 2),lwd = 1)
plot1 + legend("topleft", legend = levels(factor(comp1$Seedmixture)) , col = c("dodgerblue2", "deeppink2","orange1", "chartreuse3") , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.07))

anosim(comp1.1, comp1$Seedmixture, permutations = 999, distance = "bray", strata = NULL)

## round (plot level)
Dround <- data.frame(D)
Dround <- Dround[,c("species", "spcover", "round", "plot")]
Dround[Dround$subplot!="B", "spcover"] <- Dround[Dround$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dround[Dround$subplot=="B", "spcover"] <- Dround[Dround$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dround$spcover <- Dround$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
DRound <- aggregate(Dround$spcover, list("round"=Dround$round, "plot"=Dround$plot, "species"=Dround$species), FUN=mean)
names(DRound)[4] <- "meanspcover"

comp7 <- reshape(DRound, 
                 timevar = "species", idvar = c("round","plot"),
                 direction = "wide", drop=NULL)
comp7[is.na(comp7)] <- 0
comp7.1 <- round(comp7[,3:81], digit=1)
comp7.2 <- comp7[1:2]
comp7.1 <- as.matrix(comp7.1)

run7 <- metaMDS(comp7.1, distance="bray", k=2)
plot(run7)
stressplot(run7)

comp7$col<-c("darksalmon", "firebrick2","firebrick4")[as.numeric(factor(comp7$round))]
plot7 <- plot(run7, display = "sites", type = "n", main="Vegetation composition sorted by round")
plot7 + points(run7, choices = c(1,2), display = "sites", col = comp7$col, pch = 19)
plot7 + ordihull(run7,comp7$round,display = "sites",draw = c("polygon"),col = NULL,border = c("darksalmon", "firebrick2","firebrick4"),lty = c(1, 2),lwd = 1)
plot7 + legend("topleft", legend = levels(factor(comp7$round)),col = c("darksalmon", "firebrick2","firebrick4"),bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.07))

anosim(comp7.1, comp7$round, permutations = 999, distance = "bray", strata = NULL)

## comparison year (Optigruen plots) (plot level)
#load data: Daten_Rena
Daten_Rena <- read_excel("Masterarbeit/Daten_Rena.xlsx")
Daten_Rena$year <- "2020"
Daten_Rena$year <- "2020"
names(Daten_Rena)[1] <- "subplotID"
names(Daten_Rena)[4] <- "species"
names(Daten_Rena)[6] <- "spcover"
names(Daten_Rena)[2] <- "heterogeneity"
Daten_Rena$plot <- substr(Daten_Rena$subplotID, 1,3)
Daten_Rena$subplot <- substr(Daten_Rena$subplotID, 4,4)
DR <- Daten_Rena[c("subplot", "plot", "spcover", "species", "year")]
DR[DR$subplot!="B", "spcover"] <- DR[DR$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
DR[DR$subplot=="B", "spcover"] <- DR[DR$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
DRnew <- aggregate(DR$spcover, list("plot"=DR$plot, "year"=DR$year, "species"=DR$species), FUN=sum)
names(DRnew)[4] <- "spcover"
DRnew$spcover <- DRnew$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m

#converting species cover  
Dnew <- subset(D, D$Seedmixture=="Optigruen")
Dnew$year <- "2022"
Dnew[Dnew$subplot!="B", "spcover"] <- Dnew[Dnew$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dnew[Dnew$subplot=="B", "spcover"] <- Dnew[Dnew$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dnewspcover <- aggregate(Dnew$spcover, list("plot"=Dnew$plot, "round"=Dnew$round, "year"=Dnew$year, "species"=Dnew$species), FUN=sum)
names(Dnewspcover)[5] <- "spcover"
Dnewspcover$spcover <- Dnewspcover$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dround2 <- subset(Dnewspcover, Dnewspcover$round=="R2")
Dround1 <- subset(Dnewspcover, Dnewspcover$round=="R1")

#NMDS with round 1
comp2 <- rbind(Dround1[,c("species","spcover", "plot", "year")], DRnew[,c("species","spcover", "plot", "year")])
comp2 <- reshape(comp2, 
                 timevar = "species", idvar = c("year","plot"),
                 direction = "wide", drop=NULL)
comp2[is.na(comp2)] <- 0
comp2 <- subset(comp2,year != 0 )
comp2.1 <- round(comp2[,3:84], digit=1)
comp2.2 <- comp2[1:2]
comp2.1 <- as.matrix(comp2.1)

run2 <- metaMDS(comp2.1, distance="bray", k=2)
plot(run2)
stressplot(run1)

comp2$col<-c("slateblue", "seagreen1")[as.numeric(factor(comp2$year))]
plot2 <- plot(run2, display = "sites", type = "n", main="Vegetation composition of Optigruen plots")
plot2 + points(run2, choices = c(1,2), display = "sites", col = comp2$col, pch = 19)
plot2 + ordihull(run2,comp2$year,display = "sites",draw = c("polygon"),col = NULL,border = c("slateblue", "seagreen1"), lty = c(1, 2),lwd = 1)
plot2 + legend("topleft", legend = levels(factor(comp2$year)) , col = c("slateblue", "seagreen1") , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.07))

anosim(comp2.1, comp2$year, permutations = 999, distance = "bray", strata = NULL)

## comparison year (heahtland plots) (plotebene)
#load data: Vegetationsaufnahmen_Brantstraße_Arten
Daten_Lukas <- read_excel("Masterarbeit/Vegetationsaufnahmen_Brantstraße_Arten.xlsx")
Daten_Lukas$year <- "2020"
Daten_Lukas <- subset(Daten_Lukas,Biotopebene != "Boden" )
Daten_Lukas <- subset(Daten_Lukas,Saatgut != "Optigruen" )
names(Daten_Lukas)[7] <- "spcover"
Daten_Lukas[Daten_Lukas$spcover=="r", "spcover"] <- "1"
Daten_Lukas[Daten_Lukas$spcover=="+", "spcover"] <- "2"
Daten_Lukas[Daten_Lukas$spcover=="1", "spcover"] <- "3"
Daten_Lukas[Daten_Lukas$spcover=="2", "spcover"] <- "12.5"
Daten_Lukas$spcover <- as.numeric(Daten_Lukas$spcover)
names(Daten_Lukas)[3] <- "plot"
names(Daten_Lukas)[6] <- "species"

#converting species cover 
Dnewgesamt <- data.frame(Dseed)
Dnewgesamt$year <- "2022"
Dnewgesamt[Dnewgesamt$subplot!="B", "spcover"] <- Dnewgesamt[Dnewgesamt$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dnewgesamt[Dnewgesamt$subplot=="B", "spcover"] <- Dnewgesamt[Dnewgesamt$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
DnewG <- aggregate(Dnewgesamt$spcover, list("plot"=Dnewgesamt$plot, "round"=Dnewgesamt$round, "year"=Dnewgesamt$year, "species"=Dnewgesamt$species), FUN=sum)
names(DnewG)[5] <- "spcover"
DnewG$spcover <- DnewG$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
DgesamtR1 <- subset(DnewG, DnewG$round=="R1")

#NMDS with round 1
comp4 <- rbind(DgesamtR1[,c("species","spcover", "plot", "year")], Daten_Lukas[,c("species","spcover", "plot", "year")])
comp4 <- reshape(comp4, 
                 timevar = "species", idvar = c("year","plot"),
                 direction = "wide", drop=NULL)
comp4[is.na(comp4)] <- 0
comp4 <- subset(comp4,year != 0 )
comp4.1 <- round(comp4[,3:88], digit=1)
comp4.2 <- comp4[1:2]
comp4.1 <- as.matrix(comp4.1)

run4 <- metaMDS(comp4.1, distance="bray", k=2)
plot(run4)
stressplot(run4)

comp4$col<-c("slateblue", "seagreen1")[as.numeric(factor(comp4$year))]
plot4 <- plot(run4, display = "sites", type = "n", main="Vegetation composition of heathland plots")
plot4 + points(run4, choices = c(1,2), display = "sites", col = comp4$col, pch = 19)
plot4 + ordihull(run4,comp4$year,display = "sites",draw = c("polygon"),col = NULL,border = c("slateblue", "seagreen1"), lty = c(1, 2),lwd = 1)
plot4 + legend("topleft", legend = levels(factor(comp4$year)),col = c("slateblue", "seagreen1"),bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0.03, 0.07))

anosim(comp4.1, comp4$year, permutations = 999, distance = "bray", strata = NULL)
