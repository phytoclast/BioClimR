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
Biomeclimate$PPETRatio <- (Biomeclimate$MAP/Biomeclimate$PET +0.0001)

Biomeclimate$Seasonalilty <- ifelse(Biomeclimate$Deficit < 150 & Biomeclimate$PPETRatio>=1, "Isopluvial",
                       ifelse(Biomeclimate$Surplus < 25 & Biomeclimate$PPETRatio < 0.5 & Biomeclimate$pAET < 75, "Isoxeric",
                              ifelse(Biomeclimate$pAET < 75,"Xerothermic","Pluviothermic")))



Biomeclimate$MRegime <- ifelse(Biomeclimate$PPETRatio>=2,"Perhumid",
                  ifelse(Biomeclimate$PPETRatio>=1.414,"Moist-Humid",
                         ifelse(Biomeclimate$PPETRatio>=1,"Dry-Humid",
                                ifelse(Biomeclimate$PPETRatio>=0.707,"Moist-Subhumid",
                                       ifelse(Biomeclimate$PPETRatio>=0.5,"Dry-Subhumid",
                                              ifelse(Biomeclimate$PPETRatio>=0.25,"Semiarid",
                                                     ifelse(Biomeclimate$PPETRatio>=0.125,"Arid","Perarid"
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
Biomeclimate$T10 <- 0
for (i in 0:11){
  Biomeclimate$T10  <- Biomeclimate$T10 + (Biomeclimate[,which(colnames(Biomeclimate)=='t01')+i] >= 10)
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

Biomeclimate$Koeppen <- ifelse(Biomeclimate$MAP < (10 * Biomeclimate$DryThresh),'B',
                               ifelse(Biomeclimate$Tc >= 18, 'A',
                                      ifelse(Biomeclimate$Tw < 10, 'E',
                                             ifelse(Biomeclimate$Tc < 0, 'D', 'C'))))