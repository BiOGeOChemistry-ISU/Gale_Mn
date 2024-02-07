library(ggplot2)
library(tibble)
library(dplyr)
library(devtools)
library("tidyr")
library('ggrepel')

Range_Data_Mn<-filter(XANES,eV>6520 & eV<6600)
Range_Data_Mn_Carb<-filter(Range_Data_Mn,Type=="Carbonate"| Type=="Standard")
Range_Data_Mn_Oxyhy<-filter(Range_Data_Mn,Type=="Oxy(hydroxide)"| Type=="Phyllomanganite")
Range_Data_Mn_BL<-filter(Range_Data_Mn, Type=="Standard"|Type=="BL 16 to 18cm"| Type=="BL 18 to 20cm" | Type=="BL 28 to 30cm"| Type=="BL 6 to 8cm"| Type=="Mn(aq)"|Type=="Thin Section")

Range_Data_Mn_Carb$Sample<-factor(Range_Data_Mn_Carb$Sample,levels = c("Rhodochrosite","CB_38","CB_14","CY_25","MO5","O99"))

Mn_XANES_Carb<-ggplot(Range_Data_Mn_Carb, aes(eV, Adjusted_Norm_Abs, label=Sample))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Sample),size=1.2)+
  ylim(0, 4.5)+
  scale_color_manual(values = c("Grey","Black","Black","Black","Black","Black"))+
  ylab("Normalized Absorbance")+
  theme_classic()+
  theme(legend.position = "none")
Mn_XANES_Carb

Range_Data_Mn_Oxyhy$Sample<-factor(Range_Data_Mn_Oxyhy$Sample,levels = c("dBi4","LV_FM","LW_Mass","LW_Nod"))

Mn_XANES_Oxyhy<-ggplot(Range_Data_Mn_Oxyhy, aes(eV, Adjusted_Norm_Abs, label=Sample))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Sample),size=1.2)+
  ylim(0, 4.5)+
  scale_color_manual(values = c("Grey","Black","Black","Black"))+
  ylab("Normalized Absorbance")+
  theme_classic()+
  theme(legend.position = "none")
Mn_XANES_Oxyhy

Range_Data_Mn_BL$Type<-factor(Range_Data_Mn_BL$Type,levels = c("Mn(aq)","BL 6 to 8cm","BL 16 to 18cm", "BL 18 to 20cm","BL 28 to 30cm","Thin Section","Standard"))
 
Mn_XANES_BL<-ggplot(Range_Data_Mn_BL, aes(eV, Adjusted_Norm_Abs, label=Type))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Type),size=1.2)+
  scale_color_manual(values = c("Grey","Black","Black","Black","Black","Black","Grey"))+
  ylim(0, 5.5)+
  ylab("Normalized Absorbance")+
  theme_classic()+
  theme(legend.position = "none") 
Mn_XANES_BL
 
