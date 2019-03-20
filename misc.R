library(rpart)
library(ggplot2)
library(rpart.plot)
library(plyr)
Biomeclimate <- readRDS(file='data/RadBiomeclimate.RDS')
Biomeclimate <- Biomeclimate[Biomeclimate$Norm == 1990,]
Biomeclimate$a01 <- 0
Biomeclimate$a02 <- 0
Biomeclimate$a03 <- 0
Biomeclimate$a04 <- 0
Biomeclimate$a05 <- 0
Biomeclimate$a06 <- 0
Biomeclimate$a07 <- 0
Biomeclimate$a08 <- 0
Biomeclimate$a09 <- 0
Biomeclimate$a10 <- 0
Biomeclimate$a11 <- 0
Biomeclimate$a12 <- 0
for (i in 0:11){
  Biomeclimate[,which(colnames(Biomeclimate)=='a01')+i] <- 
    pmin(Biomeclimate[,which(colnames(Biomeclimate)=='e01')+i], Biomeclimate[,which(colnames(Biomeclimate)=='p01')+i])
}

Biomeclimate$pAET <- apply(Biomeclimate[,which(colnames(Biomeclimate)=='a01'):which(colnames(Biomeclimate)=='a12')], MARGIN = 1, FUN='max')

Biomeclimate$PET <- apply(Biomeclimate[,which(colnames(Biomeclimate)=='e01'):which(colnames(Biomeclimate)=='e12')], MARGIN = 1, FUN='sum')

Biomeclimate$MAP <- apply(Biomeclimate[,which(colnames(Biomeclimate)=='p01'):which(colnames(Biomeclimate)=='p12')], MARGIN = 1, FUN='sum')

Biomeclimate$AET <- apply(Biomeclimate[,which(colnames(Biomeclimate)=='a01'):which(colnames(Biomeclimate)=='a12')], MARGIN = 1, FUN='sum')
Biomeclimate$MAAT <- apply(Biomeclimate[,which(colnames(Biomeclimate)=='t01'):which(colnames(Biomeclimate)=='t12')], MARGIN = 1, FUN='mean')

Biomeclimate$Deficit <- pmax(Biomeclimate$PET - Biomeclimate$AET, 0)
Biomeclimate$Surplus <- pmax(Biomeclimate$MAP - Biomeclimate$AET, 0)
Biomeclimate$M <- (Biomeclimate$MAP/Biomeclimate$PET +0.0001)
Biomeclimate$Cindex <- pmin(Biomeclimate$Tc,Biomeclimate$Tclx+15)

Biomeclimate$Seasonalilty <- ifelse(Biomeclimate$Deficit < 150 & Biomeclimate$M>=1, "Isopluvial",
                       ifelse(Biomeclimate$Surplus < 25 & Biomeclimate$M < 0.5 & Biomeclimate$pAET < 75, "Isoxeric",
                              ifelse(Biomeclimate$pAET < 75,"Xerothermic","Pluviothermic")))



Biomeclimate$MRegime <- ifelse(Biomeclimate$M>=2,"Perhumid",
                  ifelse(Biomeclimate$M>=1.414,"Moist-Humid",
                         ifelse(Biomeclimate$M>=1,"Dry-Humid",
                                ifelse(Biomeclimate$M>=0.707,"Moist-Subhumid",
                                       ifelse(Biomeclimate$M>=0.5,"Dry-Subhumid",
                                              ifelse(Biomeclimate$M>=0.25,"Semiarid",
                                                     ifelse(Biomeclimate$M>=0.125,"Arid","Perarid"
                                                     )))))))


Biomeclimate$BioTemperatureC <- 
  ifelse(Biomeclimate$Tc >= 20 & Biomeclimate$Tclx >=5,"Meso-Tropical",
         ifelse(Biomeclimate$Tc >= 15 & Biomeclimate$Tclx >=0,"Cryo-Tropical",
                ifelse(Biomeclimate$Tc >= 10 & Biomeclimate$Tclx >=-5,"Thermo-Sutropical",
                       ifelse(Biomeclimate$Tc >= 5 & Biomeclimate$Tclx >=-10,"Meso-Subtropical",
                              ifelse(Biomeclimate$Tc >= 0 & Biomeclimate$Tclx >=-15,"Cryo-Subtropical",
                                     ifelse(Biomeclimate$Tc >= -5 & Biomeclimate$Tclx >=-20,"Thermo-Temperate",
                                            ifelse(Biomeclimate$Tc >= -10 & Biomeclimate$Tclx >=-25,"Meso-Temperate",
                                                   ifelse(Biomeclimate$Tc >= -25 & Biomeclimate$Tclx >=-40,"Cryo-Temperate","Polar"
                                                   ))))))))

Biomeclimate$BioTemperatureW <- ifelse(Biomeclimate$Tg >= 24,"Hot (Lowland)",
                          ifelse(Biomeclimate$Tg >= 18,"Warm (Premontane)",
                                 ifelse(Biomeclimate$Tg >= 15,"Warm-Mild (Lower-Montane)",
                                        ifelse(Biomeclimate$Tg >= 12,"Cool-Mild (Upper-Montane)",
                                               ifelse(Biomeclimate$Tg >= 6,"Cool (Subalpine)","Cold (Alpine)"
                                               )))))

Biomeclimate$summer <- pmax(apply(Biomeclimate[,c('t01','t02','t12')], MARGIN = 1, FUN = 'mean'),apply(Biomeclimate[,c('t06','t07','t08')], MARGIN = 1, FUN = 'mean'))
Biomeclimate$winter <- pmin(apply(Biomeclimate[,c('t01','t02','t12')], MARGIN = 1, FUN = 'mean'),apply(Biomeclimate[,c('t06','t07','t08')], MARGIN = 1, FUN = 'mean'))

cryic <- subset(Biomeclimate, MAAT <= 7 & MAAT >= -1 & summer <= 16.2 & summer >= 15.8)
frigidmesic <- subset(Biomeclimate, MAAT >= 6.8 & MAAT <= 7.2 )
mesicthermic <- subset(Biomeclimate, MAAT >= 14.8 & MAAT <= 15.2)
thermichypertherm <- subset(Biomeclimate, MAAT >= 20.8 & MAAT <= 21.2)
iso <- subset(Biomeclimate, summer - winter > 6.8 & summer - winter < 7.2)
gelic <- subset(Biomeclimate, MAAT >= -1.2 & MAAT <= -0.8)

ggplot()+
  geom_smooth(cryic, mapping = aes(x=Cindex, y= Tg))+
  geom_smooth(frigidmesic, mapping = aes(x=Cindex, y= Tg))+
  geom_smooth(mesicthermic, mapping = aes(x=Cindex, y= Tg))+
  geom_smooth(thermichypertherm, mapping = aes(x=Cindex, y= Tg))+
  geom_smooth(iso, mapping = aes(x=Cindex, y= Tg))

a1=data.frame(x=c(-50,-50,0,0), y=c(0,6,6,0))
a2=data.frame(x=c(-50,-50,0,0), y=c(6,12,12,6))
a3=data.frame(x=c(-50,-50,0,0), y=c(12,36,36,12))
a4=data.frame(x=c(0,0,6,0), y=c(0,6,6,0))
a5=data.frame(x=c(0,0,18,6), y=c(6,18,18,6))
a6=data.frame(x=c(0,0,15,15), y=c(18,36,36,18))
a7=data.frame(x=c(15,15,36,18), y=c(18,36,36,18))

ll1 <- data.frame(x=c(-50,6), y=c(6,6))
ll2 <- data.frame(x=c(0,0), y=c(0,36))
ll3 <- data.frame(x=c(15,15), y=c(18,36))
l1 <- data.frame(x=c(-50,12), y=c(12,12))
l2 <- data.frame(x=c(-50,0), y=c(15,15))
l3 <- data.frame(x=c(-50,18), y=c(18,18))
l4 <- data.frame(x=c(0,24), y=c(24,24))
l5 <- data.frame(x=c(-10,-10), y=c(0,36))
l6 <- data.frame(x=c(-25,-25), y=c(0,36))

climplot2 <-  ggplot() +
  geom_polygon(data=a1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.5)+
  geom_polygon(data=a2, mapping=aes(x=x, y=y, fill='boreal'),alpha = 0.5)+
  geom_polygon(data=a3, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.5)+
  geom_polygon(data=a4, mapping=aes(x=x, y=y, fill='andean'),alpha = 0.5)+
  geom_polygon(data=a5, mapping=aes(x=x, y=y, fill='oceanic'),alpha = 0.5)+
  geom_polygon(data=a6, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.5)+
  geom_polygon(data=a7, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.5)+
  
  geom_line(data=ll1, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=ll2, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=ll3, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l1, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l2, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l3, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l4, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l5, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l6, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_smooth(cryic, mapping = aes(x=Cindex, y= Tg, color = 'cryic-frigid'))+
  geom_smooth(gelic, mapping = aes(x=Cindex, y= Tg, color = 'gelic-cryic'))+
  geom_smooth(frigidmesic, mapping = aes(x=Cindex, y= Tg, color = 'frigid-mesic'))+
  geom_smooth(mesicthermic, mapping = aes(x=Cindex, y= Tg, color = 'mesic-thermic'))+
  geom_smooth(thermichypertherm, mapping = aes(x=Cindex, y= Tg, color = 'thermic-hyperthermic'))+
  geom_smooth(iso, mapping = aes(x=Cindex, y= Tg, color = 'iso'))+

  scale_fill_manual("Air", values = c("alpine" = "pink",
                                         "boreal" = "darkgreen",
                                         "temperate" = "greenyellow",
                                         "andean" = "lightblue",
                                         "oceanic" = "darkcyan",
                                         "subtropical" = "orange",
                                         "tropical" = "darkred"
                                         
  ))+ 
  scale_color_manual("Soil", values = c("iso" = "yellow",
                                             "frigid-mesic" = "green",
                                         
                                        "gelic-cryic" = "cyan",
                                             "cryic-frigid" = "blue",
                                             "mesic-thermic" = "orange",
                                             "thermic-hyperthermic" = "darkred"
                                             
  ))+
  scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", 
                     breaks=c(-45,-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                     labels=c('-45 (-60)','-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                              '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
  scale_y_continuous(name= "Growing Season Temperature", breaks=c(0,6,12,18,24,30))+
  coord_fixed(ratio = 1/1,xlim = c(-45,30), ylim = c(0, 33))+
  labs(title = paste("Temperature Regimes", sep=""))+
  theme_bw()+
  theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
        panel.grid.major = element_line(), panel.grid.minor = element_blank())








Biomeclimate$TM10 <- 0
for (i in 0:11){
  Biomeclimate$TM10  <- Biomeclimate$TM10 + (Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i] >= 10)
}

Biomeclimate$SunPPT <- ifelse(Biomeclimate$Latitude>=0,(apply(Biomeclimate[,which(colnames(Biomeclimate)=='p04'):which(colnames(Biomeclimate)=='p09')], MARGIN = 1, FUN='sum')+0.0001)/(Biomeclimate$MAP+0.0001),1- (apply(Biomeclimate[,which(colnames(Biomeclimate)=='p04'):which(colnames(Biomeclimate)=='p09')], MARGIN = 1, FUN='sum')+0.0001)/(Biomeclimate$MAP+0.0001))

Biomeclimate$DryThresh <- ifelse(Biomeclimate$SunPPT < 1/3, 2*Biomeclimate$MAAT, ifelse(Biomeclimate$SunPPT >= 2/3, 2*Biomeclimate$MAAT + 28,2*Biomeclimate$MAAT + 14))

Biomeclimate$Pd <- apply(Biomeclimate[,which(colnames(Biomeclimate)=='p01'):which(colnames(Biomeclimate)=='p12')], MARGIN = 1, FUN='min')
Biomeclimate$Pds <- ifelse(Biomeclimate$Latitude >= 0,
                           apply(Biomeclimate[,which(colnames(Biomeclimate)=='p04'):which(colnames(Biomeclimate)=='p09')], MARGIN = 1, FUN='min'),
                           apply(Biomeclimate[,c(which(colnames(Biomeclimate)=='p01'):which(colnames(Biomeclimate)=='p03'),which(colnames(Biomeclimate)=='p10'):which(colnames(Biomeclimate)=='p12'))], MARGIN = 1, FUN='min'))

Biomeclimate$Pws <- ifelse(Biomeclimate$Latitude >= 0,
                           apply(Biomeclimate[,which(colnames(Biomeclimate)=='p04'):which(colnames(Biomeclimate)=='p09')], MARGIN = 1, FUN='max'),
                           apply(Biomeclimate[,c(which(colnames(Biomeclimate)=='p01'):which(colnames(Biomeclimate)=='p03'),which(colnames(Biomeclimate)=='p10'):which(colnames(Biomeclimate)=='p12'))], MARGIN = 1, FUN='max'))

Biomeclimate$Pdw <- ifelse(Biomeclimate$Latitude < 0,
                           apply(Biomeclimate[,which(colnames(Biomeclimate)=='p04'):which(colnames(Biomeclimate)=='p09')], MARGIN = 1, FUN='min'),
                           apply(Biomeclimate[,c(which(colnames(Biomeclimate)=='p01'):which(colnames(Biomeclimate)=='p03'),which(colnames(Biomeclimate)=='p10'):which(colnames(Biomeclimate)=='p12'))], MARGIN = 1, FUN='min'))

Biomeclimate$Pww <- ifelse(Biomeclimate$Latitude < 0,
                           apply(Biomeclimate[,which(colnames(Biomeclimate)=='p04'):which(colnames(Biomeclimate)=='p09')], MARGIN = 1, FUN='max'),
                           apply(Biomeclimate[,c(which(colnames(Biomeclimate)=='p01'):which(colnames(Biomeclimate)=='p03'),which(colnames(Biomeclimate)=='p10'):which(colnames(Biomeclimate)=='p12'))], MARGIN = 1, FUN='max'))

Biomeclimate$Koeppen <- as.character('unk')

Biomeclimate$Koeppen <- ifelse(Biomeclimate$Tw < 10, 'E',
                               ifelse(Biomeclimate$MAP < (10 * Biomeclimate$DryThresh),'B',
                                      ifelse(Biomeclimate$Tc >= 18, 'A',
                                             ifelse(Biomeclimate$Tc < -3, 'D', 'C'))))

Biomeclimate$Koeppen <- ifelse(Biomeclimate$Koeppen %in% 'A',
                               ifelse(Biomeclimate$Pd >= 60, 'Af',
                                      ifelse(Biomeclimate$MAP >=  25*(100-Biomeclimate$Pd),'Am',
                                             ifelse(Biomeclimate$Pdw <60,'Aw','As'))),Biomeclimate$Koeppen )
                                                    
Biomeclimate$Koeppen <- ifelse(Biomeclimate$Koeppen %in% 'B',
                               ifelse(Biomeclimate$MAP <= 5*Biomeclimate$DryThresh,
                                      ifelse(Biomeclimate$MAAT >= 18,'BWh','BWk'),
                                      ifelse(Biomeclimate$MAAT >= 18,'BSh','BSk')),Biomeclimate$Koeppen)

Biomeclimate$Koeppen <- ifelse(Biomeclimate$Koeppen %in% 'C',
                               ifelse(Biomeclimate$Pds < Biomeclimate$Pdw &
                                        Biomeclimate$Pww > 3*Biomeclimate$Pds & Biomeclimate$Pds < 40, 'Cs',
                                      ifelse(Biomeclimate$Pww < Biomeclimate$Pds &
                                               Biomeclimate$Pws > 10*Biomeclimate$Pdw, 'Cw', 'Cf')),
                               Biomeclimate$Koeppen)

Biomeclimate$Koeppen <- ifelse(Biomeclimate$Koeppen %in% 'D',
                               ifelse(Biomeclimate$Pds < Biomeclimate$Pdw &
                                        Biomeclimate$Pww > 3*Biomeclimate$Pds & Biomeclimate$Pds < 40, 'Ds',
                                      ifelse(Biomeclimate$Pww < Biomeclimate$Pds &
                                               Biomeclimate$Pws > 10*Biomeclimate$Pdw, 'Dw', 'Df')),
                               Biomeclimate$Koeppen)

Biomeclimate$Koeppen <- ifelse(Biomeclimate$Koeppen %in% 'E',
                               ifelse(Biomeclimate$Tw > 0, 'ET','EF'),
                               Biomeclimate$Koeppen)

Biomeclimate$Koeppen <- ifelse(Biomeclimate$Koeppen %in% c('Cf','Cw','Cs','Df','Dw','Ds'),
                               ifelse(Biomeclimate$Tc < -38 & Biomeclimate$TM10 < 4,paste0(Biomeclimate$Koeppen,'d'),
                                      ifelse(Biomeclimate$TM10 < 4, paste0(Biomeclimate$Koeppen,'c'),
                                             ifelse(Biomeclimate$Tw < 22, paste0(Biomeclimate$Koeppen,'b'),paste0(Biomeclimate$Koeppen,'a')))),
                                                    Biomeclimate$Koeppen)


summaryKoeppen <- ddply(Biomeclimate,"Koeppen", summarise,Tg = median(Tg),  Cindex = median(Cindex), Tc = median(Tc), Tcl = median(Tcl), Tclx = median(Tclx), M=median(M), Deficit=median(Deficit), Surplus=median(Surplus), pAET=median(pAET))

#Classification Tree

selectBiome <- subset(Biomeclimate, !Koeppen %in% c('As', 'Cwa', 'Cwb', 'Cwc', 'Dwa', 'Dwb', 'Dwc'))
selectBiomecount<-aggregate(selectBiome[,c("Koeppen")], by=list(selectBiome$Koeppen),FUN=length)
colnames(selectBiomecount)<-c("Koeppen","x")
selectBiomecount$wt <- 1/(selectBiomecount$x+1)
#selectBiome <- subset(selectBiome, select = -c(wt) )
selectBiome<-merge(selectBiome,selectBiomecount, by="Koeppen")
colnames(selectBiome)

biomeclass <- rpart(Koeppen ~  Tg + Cindex +  M + Deficit + Surplus + pAET, data = selectBiome,weights=selectBiome$wt, method="class", control = list(maxdepth = 7, cp=0.0005, minsplit=1000))
# Make plot

png(filename="biomeclass.png",width = 10, height = 3, units = 'in', res = 600)

rpart.plot(biomeclass, extra=108) # Make plot

dev.off()

#Classification Tree

selectBiome <- subset(Biomeclimate, !BIOME %in% c("9","14"))
selectBiome <- subset(Biomeclimate, (BIOME %in% c('1') & Koeppen %in% c('Af', 'Am'))|
                        (BIOME %in% c('4') & Koeppen %in% c('Cfa', 'Dfa', 'Cfb', 'Dfb', 'Dwa', 'Dwb', 'Cwa', 'Cwb'))|
                        (BIOME %in% c('12') & Koeppen %in% c('Csa', 'Csb'))|
                        (BIOME %in% c('13') & Koeppen %in% c('BWh', 'BWk'))|
                        (BIOME %in% c('8') & Koeppen %in% c( 'BSk'))|
                        (BIOME %in% c('7') & Koeppen %in% c( 'BSh'))|
                        (BIOME %in% c('2') & Koeppen %in% c( 'Aw'))|
                        (BIOME %in% c('6') & Koeppen %in% c( 'Dfc'))|
                        (BIOME %in% c('11') & Koeppen %in% c( 'ET'))|
                        (BIOME %in% c('99') & Koeppen %in% c( 'EF'))
)

selectBiomecount<-aggregate(selectBiome[,c("biomname")], by=list(selectBiome$BIOME,selectBiome$biomname),FUN=length)
colnames(selectBiomecount)<-c("BIOME","biomname","x")
selectBiomecount$wt <- 100/(selectBiomecount$x+100)
#selectBiome <- subset(selectBiome, select = -c(wt) )
selectBiome<-merge(selectBiome,selectBiomecount, by=c("BIOME","biomname"))
colnames(selectBiome)

biomeclass <- rpart(biomname ~  Tg + Cindex +  M + Deficit + Surplus + pAET, data = selectBiome,weights=selectBiome$wt, method="class", control = list(maxdepth = 4, cp=0.0005, minsplit=100))
# Make plot

png(filename="biomeclass.png",width = 10, height = 3, units = 'in', res = 600)

rpart.plot(biomeclass, extra=108) # Make plot

dev.off()

#Classification Tree

selectBiome <- Biomeclimate
selectBiome$synclm <- paste(selectBiome$BioTemperatureC, selectBiome$BioTemperatureW, selectBiome$MRegime, selectBiome$Seasonalilty)
selectBiomecount<-aggregate(selectBiome[,c("synclm")], by=list(selectBiome$synclm),FUN=length)
colnames(selectBiomecount)<-c("synclm","x")
selectBiomecount$wt <- 1000/(selectBiomecount$x+1000)
#selectBiome <- subset(selectBiome, select = -c(wt) )
selectBiome<-merge(selectBiome,selectBiomecount, by=c("synclm"))
colnames(selectBiome)

biomeclass <- rpart(synclm ~  Tg + Cindex +  M + Deficit + Surplus + pAET, data = selectBiome,weights=selectBiome$wt, method="class", control = list(maxdepth = 5, cp=0.0005, minsplit=1000))
# Make plot

png(filename="biomeclass.png",width = 10, height = 3, units = 'in', res = 600)

rpart.plot(biomeclass, extra=108) # Make plot

dev.off()

