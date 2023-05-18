## total SR

# Optigruen plots 2020
PlotSR_R <- aggregate(Daten_Rena[,c("species")], by=list("plot"=Daten_Rena$plot), FUN=function(x) length(unique(x)))
names(PlotSR_R)[2] <- "SR"
PlotSR_R$year <- "2020"
length(unique(Daten_Rena[["species"]])) #52
mean(PlotSR_R$SR) #15

# heathland plots 2020
PlotSR_L <- aggregate(Daten_Lukas[,c("species")], by=list("plot"=Daten_Lukas$plot), FUN=function(x) length(unique(x)))
names(PlotSR_L)[2] <- "SR"
PlotSR_L$year <- "2020"
length(unique(Daten_Lukas[["species"]])) #58
mean(PlotSR_L$SR) #16.1

# Optigruen plots 2022 (all rounds)
PlotSR <- aggregate(D[,c("species")], by=list("plot"=D$plot, "round"=D$round), FUN=function(x) length(unique(x)))
names(PlotSR)[3] <- "SR"
PlotSR <- merge(PlotSR, plotinfo_plot)
mean(PlotSR$SR) #16.7

# heathland plots 2022 (all rounds)
PlotSRseed <- aggregate(Dseed[,c("species")], by=list("plot"=Dseed$plot, "round"=Dseed$round), FUN=function(x) length(unique(x)))
names(PlotSRseed)[3] <- "SR"
PlotSRseed$year <- "2022"
PlotSRseed <- PlotSRseed[,-2]
mean(PlotSRseed$SR) #17.3


## total species list

splist <- aggregate(plot ~ species, D, function(species) length(unique(species)))
names(splist)[2] <- "Optigruen 2022"
splistseed <- aggregate(plot ~ species, Dseed, function(species) length(unique(species)))
names(splistseed)[2] <- "heathland 2022"
splist_R <- aggregate(plot ~ species, Daten_Rena, function(species) length(unique(species)))
names(splist_R)[2] <- "Optigruen 2020"
splist_L <- aggregate(plot ~ species, Daten_Lukas, function(species) length(unique(species)))
names(splist_L)[2] <- "heathland 2020"

list1 <- merge(splist, splistseed, by=c("species"), all=TRUE)
list2 <- merge(splist_R, splist_L, by=c("species"), all=TRUE)
totallist <- merge(list2, list1, by=c("species"), all=TRUE)
totallist[is.na(totallist)] <- 0
totallist <- merge(totallist, specieslist1, by=c("species"), all=TRUE)


## cover of succulents, herbs, grass (plot level)

#optigruen plots 2022
Dsedum <- subset(D, D$family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "plot", "subplot", "round")])
Dsedum[Dsedum$subplot!="B", "spcover"] <- Dsedum[Dsedum$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dsedum[Dsedum$subplot=="B", "spcover"] <- Dsedum[Dsedum$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dsedum$spcover <- Dsedum$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot, "round"=Dsedum$round), FUN=sum)
mean(Dsedum$x) #14.7 

Dgrass <- subset(D, D$family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot", "subplot", "round")])
Dgrass[Dgrass$subplot!="B", "spcover"] <- Dgrass[Dgrass$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dgrass[Dgrass$subplot=="B", "spcover"] <- Dgrass[Dgrass$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dgrass$spcover <- Dgrass$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot, "round"=Dgrass$round), FUN=sum)
mean(Dgrass$x) #4.1 

Dherb <- subset(D, D$family!="Poaceae")
Dherb <- subset(Dherb, Dherb$family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot", "subplot", "round")])
Dherb[Dherb$subplot!="B", "spcover"] <- Dherb[Dherb$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dherb[Dherb$subplot=="B", "spcover"] <- Dherb[Dherb$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dherb$spcover <- Dherb$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot, "round"=Dherb$round), FUN=sum)
mean(Dherb$x) #23.7 

#heathland plots 2022
Dsedum <- subset(Dseed, Dseed$family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "plot", "subplot", "round")])
Dsedum[Dsedum$subplot!="B", "spcover"] <- Dsedum[Dsedum$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dsedum[Dsedum$subplot=="B", "spcover"] <- Dsedum[Dsedum$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dsedum$spcover <- Dsedum$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot, "round"=Dsedum$round), FUN=sum)
mean(Dsedum$x) #4.0

Dgrass <- subset(Dseed, Dseed$family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot", "subplot", "round")])
Dgrass[Dgrass$subplot!="B", "spcover"] <- Dgrass[Dgrass$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dgrass[Dgrass$subplot=="B", "spcover"] <- Dgrass[Dgrass$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dgrass$spcover <- Dgrass$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot, "round"=Dgrass$round), FUN=sum)
mean(Dgrass$x) #8.6

Dherb <- subset(Dseed, Dseed$family!="Poaceae")
Dherb <- subset(Dherb, Dherb$family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot", "subplot", "round")])
Dherb[Dherb$subplot!="B", "spcover"] <- Dherb[Dherb$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dherb[Dherb$subplot=="B", "spcover"] <- Dherb[Dherb$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dherb$spcover <- Dherb$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot, "round"=Dherb$round), FUN=sum)
mean(Dherb$x) #36.1

#Optigruen plots 2020
Daten_Rena$subplot <- substr(Daten_Rena$subplotID,4,4)
Daten_Rena$plot <- substr(Daten_Rena$subplotID,1,3)
Dsedum <- subset(Daten_Rena, Daten_Rena$Family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "subplot", "plot")])
Dsedum[Dsedum$subplot!="B", "spcover"] <- Dsedum[Dsedum$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dsedum[Dsedum$subplot=="B", "spcover"] <- Dsedum[Dsedum$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dsedum$spcover <- Dsedum$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot), FUN=sum)
mean(Dsedum$x) #7.5

Dgrass <- subset(Daten_Rena, Daten_Rena$Family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot", "subplot")])
Dgrass[Dgrass$subplot!="B", "spcover"] <- Dgrass[Dgrass$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dgrass[Dgrass$subplot=="B", "spcover"] <- Dgrass[Dgrass$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dgrass$spcover <- Dgrass$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot), FUN=sum)
mean(Dgrass$x) #1.3

Dherb <- subset(Daten_Rena, Daten_Rena$Family!="Poaceae")
Dherb <- subset(Dherb, Dherb$Family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot", "subplot")])
Dherb[Dherb$subplot!="B", "spcover"] <- Dherb[Dherb$subplot!="B", "spcover"]*1.5 #Fläche A bzw C ist 1m*1,5m
Dherb[Dherb$subplot=="B", "spcover"] <- Dherb[Dherb$subplot=="B", "spcover"]*0.75 #Fläche B ist 0,5m*1,5m
Dherb$spcover <- Dherb$spcover/3.75 #Gesamtfläche ist 2,5m*1,5m
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot), FUN=sum)
mean(Dherb$x) #9.8

#heathland plots 2020
Lukas <- read_delim("Lukas.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Daten_Lukas <- merge(Lukas, Daten_Lukas, by=c("plot", "species"), all=TRUE)
Daten_Lukas <- subset(Daten_Lukas,Saatgut.x != "Optigruen" )
Dsedum <- subset(Daten_Lukas, Daten_Lukas$family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "plot")])
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot), FUN=sum)
mean(Dsedum$x) #9.9

Dgrass <- subset(Daten_Lukas, Daten_Lukas$family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot")])
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot), FUN=sum)
mean(Dgrass$x) #10.3

Dherb <- subset(Daten_Lukas, Daten_Lukas$family!="Poaceae")
Dherb <- subset(Dherb, Dherb$family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot")])
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot), FUN=sum)
mean(Dherb$x) #15.5

#substrate comparison 
#15cm
D15 <- subset(D, D$substrate=="15")
Dsedum <- subset(D15, D15$family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "subplot", "plot")])
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot, "subplot"=Dsedum$subplot), FUN=sum)
mean(Dsedum$x) #36.96

Dgrass <- subset(D15, D15$family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot", "subplot")])
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot, "subplot"=Dgrass$subplot), FUN=sum)
mean(Dgrass$x) #14.52

Dherb <- subset(D15, D15$family!="Poaceae")
Dherb <- subset(Dherb, Dherb$family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot", "subplot")])
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot, "subplot"=Dherb$subplot), FUN=sum)
mean(Dherb$x) #102

#10cm
D10 <- subset(D, D$substrate=="10")
D10 <- subset(D10, D10$subplot!="B")
Dsedum <- subset(D10, D10$family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "subplot", "plot")])
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot, "subplot"=Dsedum$subplot), FUN=sum)
mean(Dsedum$x) #48.3

Dgrass <- subset(D10, D10$family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot")])
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot), FUN=sum)
mean(Dgrass$x) #29

Dherb <- subset(D10, D10$family!="Poaceae")
Dherb <- subset(Dherb, Dherb$family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot", "subplot")])
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot, "subplot"=Dherb$subplot), FUN=sum)
mean(Dherb$x) #74.2

#5cm
D5 <- subset(D, D$substrate=="5")
Dsedum <- subset(D5, D5$family=="Crassulaceae")
Dsedum <- unique(Dsedum[,c("spcover", "subplot", "plot")])
Dsedum <- aggregate(Dsedum$spcover, list("plot"=Dsedum$plot, "subplot"=Dsedum$subplot), FUN=sum)
mean(Dsedum$x) #51

Dgrass <- subset(D5, D5$family=="Poaceae")
Dgrass <- unique(Dgrass[,c("spcover", "plot", "subplot")])
Dgrass <- aggregate(Dgrass$spcover, list("plot"=Dgrass$plot, "subplot"=Dgrass$subplot), FUN=sum)
mean(Dgrass$x) #6.3

Dherb <- subset(D5, D5$family!="Poaceae")
Dherb <- subset(Dherb, Dherb$family!="Crassulaceae")
Dherb <- unique(Dherb[,c("spcover", "plot", "subplot")])
Dherb <- aggregate(Dherb$spcover, list("plot"=Dherb$plot, "subplot"=Dherb$subplot), FUN=sum)
mean(Dherb$x) #46.4
