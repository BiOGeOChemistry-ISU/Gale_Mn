library(ggplot2)
library(tibble)
library(dplyr)
library(devtools)
library("tidyr")
library('ggrepel')
library(readxl)
require(ggpubr)
library(plotly)

# Set WD
setwd("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/XANES_Figure")

# Import dataset
XANES <- read_excel("XANES.xlsx")


Range_Data_Mn<-filter(XANES,eV>6520 & eV<6600)
Range_Data_Mn_Carb<-filter(Range_Data_Mn,Type=="Carbonate"| Type=="Standard")
Range_Data_Mn_Oxyhy<-filter(Range_Data_Mn,Type=="Oxy(hydroxide)"| Type=="Phyllomanganite")
Range_Data_Mn_BL<-filter(Range_Data_Mn, Type=="Standard2"|Type=="BL 16 to 18cm"| Type=="BL 18 to 20cm" | Type=="BL 28 to 30cm"| Type=="BL 6 to 8cm"| Type=="Phosphate"|Type=="Thin Section")

Range_Data_Mn_Carb$Sample<-factor(Range_Data_Mn_Carb$Sample,levels = c("Rhodochrosite0","CB_38","CB_14","CY_25","MO5","O99"))

Mn_XANES_Carb<-ggplot(Range_Data_Mn_Carb, aes(eV, Adjusted_Norm_Abs, label=Sample))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Sample),size=0.75)+
  ylim(0, 4.5)+
  scale_color_manual(values = c("Grey","Black","Black","Black","Black","Black"))+
  ylab(NULL)+
  xlab(NULL) +
  theme_classic()+
  theme(legend.position = "none", #remove legend
      strip.text.x = element_text(size = 8), #change font size of facet label
      axis.text = element_text(size = 8), #change font size of axis grid label
      axis.title = element_text(size = 9),  #change font size of axis label
      axis.ticks.y = element_blank(), # remove y axis ticks
      axis.text.y = element_blank()) # remove y axis tick labels
Mn_XANES_Carb

Range_Data_Mn_Oxyhy$Sample<-factor(Range_Data_Mn_Oxyhy$Sample,levels = c("dBi4","LV_FM","LW_Mass","LW_Nod"))

Mn_XANES_Oxyhy<-ggplot(Range_Data_Mn_Oxyhy, aes(eV, Adjusted_Norm_Abs, label=Sample))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Sample),size=0.75)+
  ylim(0, 3)+
  scale_color_manual(values = c("Grey","Black","Black","Black"))+
  ylab("Normalized Absorbance")+
  xlab(NULL) +
  theme_classic()+
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9),  #change font size of axis label
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.text.y = element_blank()) # remove y axis tick labels
Mn_XANES_Oxyhy


Range_Data_Mn_BL$Type<-factor(Range_Data_Mn_BL$Type,levels = c("Phosphate","BL 28 to 30cm", "BL 18 to 20cm", "BL 16 to 18cm", "BL 6 to 8cm","Thin Section","Standard2"))
 
Mn_XANES_BL<-ggplot(Range_Data_Mn_BL, aes(eV, Adjusted_Norm_Abs, label=Type))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Type),size=0.75)+
  scale_color_manual(values = c("Grey","Black","Black","Black","Black","Black","Grey"))+
  ylim(0, 5.5)+
  ylab(NULL)+
  theme_classic()+
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9),  #change font size of axis label
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.text.y = element_blank()) # remove y axis tick labels
Mn_XANES_BL
 
# line up plots in a row and label them
XANES<-ggarrange(Mn_XANES_Carb, Mn_XANES_Oxyhy, Mn_XANES_BL, # names of three plots in order desired
                       labels = c("A", "B", "C"), # labels plots
                      label.x = 0.15, # x position of labels
                 label.y = 0.9, # x position of labels
                       ncol = 1, nrow = 3, align = "v", # 3 down; align vertically
                      font.label = list(size = 9, face = "plain"))

XANES

#save plot in WD as pdf with dimensions that you want
ggsave("XANES.pdf", device = "pdf", plot=XANES, width=50, height=90, units="mm") 

