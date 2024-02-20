#Install packages needed
packages = c('ggtern', 'plotly', 'readr', 'dplyr', 'tidyr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


#Open packages
library(ggplot2)
library("ggtern")
library(dplyr)
library(tidyverse)
library(dplyr)

#If filtering data - do here
Full_Data<-filter(ManganeseData_ForRCode,
                  Formation=="Marine") #add in whatever column and catergory you want to filter by
  #if not filtering, run this code:
Full_Data<-ManganeseData_ForRCode

#If you want to plot an element at a different scale - do this step following order of legend, then cleaning- otherwise skip
  #MoE-1
Full_Data$Mo_x0.1=Full_Data$Mo*0.1
  #CuE-1
Full_Data$Cu_x0.1=Full_Data$Cu*0.1
  #NiE-1
Full_Data$Ni_x0.1=Full_Data$Ni*0.1
  #BaE-2
Full_Data$Ba_x0.1=Full_Data$Ba*0.01
  #MgE1
Full_Data$Mg_wt_x10=Full_Data$Mg_wt*10

#To set order of legend - important step as this will also organize the order that you assign colors
Full_Data$Mn_Species <- factor(Full_Data$Mn_Species,levels = c("Oxide","Carbonate","Gale Crater","Other"))
Full_Data$Formation <- factor(Full_Data$Formation,levels = c("Diagenetic","Hydrogenetic","Mixed","Freshwater","This Study (Oxides)", "Carbonates","This Study (Carbonates)","Gale Crater"))
Full_Data$Type <- factor(Full_Data$Type,levels = c("Marine","Freshwater","Carbonates","Gale Crater"))



#Cleaning Data (excluding individual data from Shebandown Lakes):
  #taking out individual Shebandowan Lakes and only keeping avg
Clean_Data_1<-filter(Full_Data,!Location=="Shebandowan Lakes")
  #data with Shebandowan lakes averaged
Clean_Data_2<-filter(Full_Data,!Location=="Shebandowan Lakes Averaged")


#Base Ternary Plot
  #add in elements you want to plot in this line (make sure to have the correct dataset)
Element_Base=ggtern(data=Clean_Data_2,aes(x=Fe_ppm, y=Co_Cu_Ni, z=Mn_ppm))

  #ternary plot - plotting by Mn-species
Element_Plot<- Element_Base+ geom_point(aes(color=Mn_Species, shape=Mn_Species),size=2.5,stroke=1.5)+
  scale_color_manual(values = c("#1F968BFF","#404788FF","black","grey"))+
  scale_shape_manual(values=c(1,18,2,1))+
  labs(shape="",color="")+
  theme_void()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
Element_Plot

  
  #ternary plot - plotting by Type (splits oxide into: marine,freshwater)
Element_Plot<- Element_Base+ geom_point(aes(color=Type, shape=Type),size=2.5,stroke=1.5)+
  scale_color_manual(values = c("#1F968BFF","#55C667FF","#404788FF","Black"))+
  scale_shape_manual(values=c(1, 3,18,2))+
  labs(shape="",color="")+
  theme_void()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
Element_Plot


#ternary plot - plotting by Formation (splits marine into:hydrogenetic, diagenetic, mixed)
Element_Plot<- Element_Base+ geom_point(aes(color=Formation, shape=Formation),size=2.5,stroke=1.5)+
  scale_color_manual(values = c("#440154FF","#1F968BFF","#FDE725FF","#55C667FF","#404788FF","Black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="")+
  theme_void()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
Element_Plot