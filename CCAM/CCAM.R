install.packages("RColorBrewer")

library(ggplot2)
library(tibble)
library(dplyr)
library(devtools)
library("tidyr")
library(RColorBrewer)



display.brewer.all()

#data rearranging and organizing
CCAM_Data_Gale_Terrestrial_Regorganized<-CCAM_Data_Gale_Terrestrial%>%gather(ID,Norm_Int,cl5_402549621ccs_f0043474ccam01057p3_norm_Rocknest:Aug_7_154707_2019_CCS_POX6_Loc5_norm)
CCAM_AllAttributes_Terrestrial_Gale<-left_join(CCAM_Data_Gale_Terrestrial_Regorganized,JoinTable_CCAM_Gale_Terrestrial,by="ID")

  #filtering MnO 2 stnadard dev above mean 
Stats_StandDev_sol2000<-filter(JoinTable_CCAM_Gale_Terrestrial,MnO>1.24024)
write.csv(x=Stats_StandDev_sol2000, file="Stats_StandDev_sol2000.csv")
  #to write csvs
write.csv(x=CCAM_Data_Gale_Terrestrial_Regorganized, file="CCAM_Data_Gale_Terrestrial_Regorganized.csv")
write.csv(x=CCAM_AllAttributes_Terrestrial_Gale,file="CCAM_AllAttributes_Terrestrial_Gale.csv")

#filtering between Earth and Gale samples with Gale samples being >2 stand dev
Earth_Samps<-filter(CCAM_AllAttributes_Terrestrial_Gale,Type=="Terrestrial")
Gale_Samps<-filter(CCAM_AllAttributes_Terrestrial_Gale,Type=="Gale")
Gale_Samps_HighMn<-filter(Gale_Samps,MnO>1.24024)





#Plots - reccomend in increments of 15 nm
  #wavelength filter
Earth_Samps_wl_filter<-filter(Earth_Samps,wavelength>286 & wavelength<288)
  #plot
ES_457<-ggplot(Earth_Samps_wl_filter, aes(wavelength, Norm_Int, group=ID, color=Mineral))+
  geom_line()+
  geom_vline(xintercept = c(287.415

  ),linetype="dashed")+
  theme_classic()+
  theme(legend.position = "none")
ES_457


  #Gale crater wavelength filter
Gale_Samps_wl_filter<-filter(Gale_Samps_HighMn,wavelength>650 & wavelength<665)
  #factor by location
Gale_Samps_wl_filter$Location <- factor(Gale_Samps_wl_filter$Location,levels = c("Yellowknife Bay","Yellowknife Bay - Float Rock","Kimberley","Pahrump Hills","Between Marias Pass and Bridgar Basin","Approaching Vera Rubin Ridge","Glen Torridon"))
  #Gale crater plot
GS_plot<-ggplot(Gale_Samps_wl_filter, aes(wavelength, Norm_Int, group=ID, color=Location))+
  geom_line()+
  scale_color_manual(values = c("#DDCC77","#117733","#CC6677","#661100","#6699CC","#332288","#888888"))+
                 geom_vline(xintercept = c(657.987

                 ),linetype="dashed")+
                 theme_classic()+
  theme(legend.position = "none")
GS_plot

#Display Spectra Elements
Pb <- c(257.737,	257.803,	261.444,	261.496,	266.395,	280.282,	282.402,	283.389,	287.415,	
        294.829,	294.939,	308.998,	313.872,	317.742,	322.146,	402.077,	405.895,	406.328,	
        416.921,	424.612,	438.769,	500.681,	500.797,	504.399,	507.199,	507.594,	507.777,	
        589.726,	666.204,	723.096)

Sr<-c(407.886,	416.297,	421.671,	430.666,	460.862,	496.364,	496.933,	515.751,	522.656,	525.836,	550.57,	553.635,	641.024,	644.846,	646.758,	650.58,	664.537,	689.449)


##Comparing Gale and Terrestrial
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Yellowknife Bay"|Mineral=="3+/4+"|Location=="Yellowknife Bay - Float Rock"|Legend_Name=="CB_14")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))

Earth_Gale_Compare_CarbLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Approaching Vera Rubin Ridge"|Location=="Glen Torridon"|Mineral=="2+"|Legend_Name=="LW_Nod")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_CarbLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))

Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>450 & wavelength<465)





#"darkgoldenrod1","darkslategray4","#332288","firebrick2","darkred","coral3","lightsalmon4","darkorange3","green"
######OXIDES GALE - Yellowknife Bay

Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Yellowknife Bay"|Mineral=="3+/4+"|Location=="Yellowknife Bay - Float Rock"|Legend_Name=="CB_14")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>287 & wavelength<288)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Caribou","Little_Dal","Duncan Lake","Rocknest","LW_Nod","LW_Mass","LV_FM","CB_14"))
#for oxides 
Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(287.415
  ),linetype="dashed")+
  scale_color_manual(values = c("darkgoldenrod1","#CC6677","#332288","#888888","chartreuse4","palegreen","limegreen","red"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot


###Kimberley
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Kimberley"|Mineral=="2+"|Legend_Name=="LW_Nod"|Legend_Name=="Caribou")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>650 & wavelength<665)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Mondooma","Neil","Stephen","CB_14","CB_38","CY_25","MO5","O99","Caribou","LW_Nod"))

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(657.987
  ),linetype="dashed")+
  scale_color_manual(values = c("darkgoldenrod1","darkslategray4","#332288","firebrick2","darkred","coral3","lightsalmon4","darkorange3","grey","green"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot


#Pahrump Hills and Between Marias and Bridgar
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Pahrump Hills"|Location=="Between Marias Pass and Bridgar Basin"|Mineral=="2+"|Legend_Name=="LW_Nod"|Legend_Name=="Caribou")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>650 & wavelength<665)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Bald Mountain","Gordon","CB_14","CB_38","CY_25","MO5","O99","LW_Nod","Caribou"))

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(657.987
  ),linetype="dashed")+
  scale_color_manual(values = c("purple","darkslategray4","firebrick2","darkred","coral3","lightsalmon4","darkorange3","green","black"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot


#Approaching Vera Rubin Ridge and GT
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Approaching Vera Rubin Ridge"|Mineral=="2+"|Legend_Name=="LW_Nod"|Legend_Name=="Caribou")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>650 & wavelength<665)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Newport Ledge","Denning Brook","Knight Nubble","CB_14","CB_38","CY_25","MO5","O99","LW_Nod","Caribou"))

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(657.987
  ),linetype="dashed")+
  scale_color_manual(values = c("purple","darkslategray4","gold2","firebrick2","darkred","coral3","lightsalmon4","darkorange3","green","black"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot



#Groken
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Glen Torridon"|Mineral=="2+"|Legend_Name=="LW_Nod"|Legend_Name=="Caribou"|Legend_Name=="LW_Mass")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>650 & wavelength<665)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Groken","CB_14","CB_38","CY_25","MO5","O99","LW_Nod","Caribou","LW_Mass"))

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(657.987
  ),linetype="dashed")+
  scale_color_manual(values = c("gold2","firebrick2","darkred","coral3","lightsalmon4","darkorange3","green","black","chartreuse4"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot





Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Approaching Vera Rubin Ridge"|Location=="Glen Torridon"|Mineral=="2+"|Legend_Name=="LW_Nod"|Legend_Name=="Caribou")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>650 & wavelength<665)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Newport Ledge","Denning Brook","Knight Nubble","Groken","CB_14","CB_38","CY_25","MO5","O99","LW_Nod","Caribou"))

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(657.987
  ),linetype="dashed")+
  scale_color_manual(values = c("purple","darkslategray4","gold2","#332288","firebrick2","darkred","coral3","lightsalmon4","darkorange3","green","black"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot




####To customize
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Legend_Name=="Gordon"|Legend_Name=="Stephen"|Legend_Name=="Denning Brook"|Legend_Name=="LW_Mass"|Legend_Name=="CB_14")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>650 & wavelength<665)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Gordon","Denning Brook","Stephen","LW_Mass","CB_14"))

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(657.987
  ),linetype="dashed")+
  scale_color_manual(values = c("gold2","purple","darkslategray4","chartreuse4","firebrick2"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot


Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Newport Ledge","Denning Brook","Knight Nubble","Groken","CB_14","CB_38","CY_25","MO5","O99","LW_Nod"))
#for oxides "Caribou","Little_Dal","Duncan Lake","Rocknest","LW_Nod","LW_Mass","LV_FM","CB_14"

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(287.415
  ),linetype="dashed")+
  scale_color_manual(values = c("purple","darkslategray4","#332288","firebrick2","darkred","coral3","darkorange3","black","grey","green"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot


#Carobpi
Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Yellowknife Bay"|Mineral=="3+/4+"|Location=="Yellowknife Bay - Float Rock"|Legend_Name=="CB_14")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))

Earth_Gale_Compare_CarbLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Location=="Yellowknife Bay"|Location=="Yellowknife Bay - Float Rock"|Mineral=="3+/4+"|Legend_Name=="CB_14")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_CarbLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))

Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>286 & wavelength<288)




Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("Newport Ledge","Denning Brook","Knight Nubble","Groken","CB_14","CB_38","CY_25","MO5","O99","LW_Nod"))
#for oxides "Caribou","Little_Dal","Duncan Lake","Rocknest","LW_Nod","LW_Mass","LV_FM","CB_14"

Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(	402.077
  ),linetype="dashed")+
  scale_color_manual(values = c("purple","darkslategray4","#332288","firebrick2","darkred","coral3","darkorange3","black","grey","green"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot









ES_403<-ggplot(Earth_Samps_wl_filter, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "blue", high = "red", na.value = NA)+
  theme_classic()
ES_403

Gale_Samps<-filter(CCAM_AllAttributes_Terrestrial_Gale,Type=="Gale")
Gale_Samps_403<-filter(Gale_Samps,wavelength>402.5 & wavelength<404)

GS_403<-ggplot(Gale_Samps_403, aes(wavelength, Norm_Int, group=ID,color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "blue", high = "red", na.value = NA)+
  theme_classic()
GS_403

#-----455-----
Earth_Samps_455<-filter(Earth_Samps,wavelength>660 & wavelength<675)

ES_457<-ggplot(Earth_Samps_455, aes(wavelength, Norm_Int, group=ID, color=Mineral))+
  geom_line()+
  geom_vline(xintercept = c(664.537,	666.204
                            ),linetype="dashed")+
  theme_classic()+
  theme(legend.position = "none")
ES_457



GS<-ggplot(Gale_Samps_455, aes(wavelength, Norm_Int,group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
GS


CCAM_All_455<-filter(CCAM_AllAttributes_Terrestrial_Gale,wavelength>450 & wavelength<460)
GS<-ggplot(CCAM_All_455, aes(wavelength, Norm_Int,group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
GS

Dunc_Oxides2<-filter(CCAM_AllAttributes_Terrestrial_Gale,Legend_Name=="Caribou"|Legend_Name=="Duncan Lake"|Legend_Name=="LW_Mass"|Legend_Name=="LW_Nod"|Legend_Name=="LV_FM")
Dunc_Oxides2_407<-filter(Dunc_Oxides2,wavelength>402 & wavelength<412)
Ex<-ggplot(Dunc_Oxides2_407, aes(wavelength, Norm_Int,group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
Ex


#-----675-----

Earth_Samps_514<-filter(Earth_Samps,wavelength>650 & wavelength<665)
ES<-ggplot(Earth_Samps_514, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
ES
Earth_Samps_514<-filter(Earth_Samps,wavelength>650 & wavelength<665)
ES<-ggplot(Earth_Samps_514, aes(wavelength, Norm_Int, group=ID, color=Mineral))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")
ES

#GaleSamples
Gale_Samps_657<-filter(Gale_Samps,wavelength>300 & wavelength<345)
GS_657<-ggplot(Gale_Samps_657, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()+
  theme(legend.position = "none")
GS_657
GS<-ggplot(Gale_Samps_657, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
GS




Gale_Samps_514<-filter(Gale_Samps,wavelength>513 & wavelength<516)
GS<-ggplot(Gale_Samps_514, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
GS


#-----330 Zn-----
Earth_Samps_330<-filter(Earth_Samps,wavelength>328.5 & wavelength<331.5)
ES<-ggplot(Earth_Samps_330, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
ES

ES<-ggplot(Earth_Samps_330, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
ES


#-----480 Zn-----
Earth_Samps_480<-filter(Earth_Samps,wavelength>465 & wavelength<495)
ES<-ggplot(Earth_Samps_480, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
ES

ES<-ggplot(Earth_Samps_480, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
ES

Gale_Samps_480<-filter(Gale_Samps,wavelength>465 & wavelength<495)
GS<-ggplot(Gale_Samps_480, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
GS


#-----660 Pb-----
Earth_Samps_666<-filter(Earth_Samps,wavelength>660 & wavelength<670)
ES<-ggplot(Earth_Samps_666, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
ES

ES<-ggplot(Earth_Samps_480, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  theme_classic()
ES

Gale_Samps_666<-filter(Gale_Samps,wavelength>660 & wavelength<670)
GS<-ggplot(Gale_Samps_666, aes(wavelength, Norm_Int, group=ID, color=MnO))+
  geom_line()+
  scale_colour_gradient(low = "palegoldenrod", high = "darkred", na.value = NA)+
  theme_classic()
GS



Earth_Gale_Compare_OxideLike<-filter(CCAM_AllAttributes_Terrestrial_Gale, Mineral=="3+/4+"|Legend_Name=="CB_14")
Earth_Gale_Comp_HighMnGale<-filter(Earth_Gale_Compare_OxideLike,(Type=="Gale"& MnO>1.24024)|(Type=="Terrestrial"))
Earth_Gale_Comp_filter<-filter(Earth_Gale_Comp_HighMnGale,wavelength>286 & wavelength<288)
Earth_Gale_Comp_filter$Legend_Name <- factor(Earth_Gale_Comp_filter$Legend_Name, levels = c("LW_Nod","LW_Mass","LV_FM","CB_14"))
#for oxides 
Earth_Gale_plot<-ggplot(Earth_Gale_Comp_filter, aes(wavelength, Norm_Int, group=ID, color=Legend_Name))+
  geom_line()+
  geom_vline(xintercept = c(287.415
  ),linetype="dashed")+
  scale_color_manual(values = c("chartreuse4","palegreen","limegreen","red"))+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size=1.5)))
Earth_Gale_plot
