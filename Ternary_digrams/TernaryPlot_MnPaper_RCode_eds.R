########################################## Load and install packages ################################

# List of required packages
packages <- c(
  'ggtern', 'plotly', 'readr', 'dplyr', 'tidyr', 'ggpubr', 'ggplot2'
)

# Install any missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Ensure specific version of ggplot2 for compatibility with ggpubr
if (packageVersion("ggplot2") != "3.2.1") {
  devtools::install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
}

##################### Loading data ###################

#set directory to export plots
setwd("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Ternary_diagrams")

#Bringing in datasheet
Full_Data <- read_excel("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Compiled_Lit_chemistry_v2_plotting.xlsx", 
                        sheet = "Data")
########################################################
#If filtering data - do here
Full_Data<-filter(ManganeseData_ForRCode,
                  Type=="Marine") #add in whatever column and catergory you want to filter by
  #if not filtering, run this code:
Full_Data<-ManganeseData_ForRCode
Full_Data <-Full_Data %>% drop_na(Type)

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

################################################# MgFeMn ########################################################
#Base Ternary Plot
  #add in elements you want to plot in this line (make sure to have the correct dataset)
MgFeMn = ggtern(data=Full_Data,aes(x=Mg_wt_x10, y=Fe_wt, z=Mn_wt))

  #ternary plot - plotting by Mn-species
MgFeMn_species <- MgFeMn + geom_point(aes(color=Mn_Species, shape=Mn_Species),size=2.5,stroke=1.5)+
  scale_color_manual(values = c("#1F968BFF","#404788FF","black","grey"))+
  scale_shape_manual(values=c(1,18,2,1))+
  labs(shape="",color="",
       x="Mgx10 wt%",
       y="Fe wt%",
       z="Mn wt%")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
MgFeMn_species


  #ternary plot - plotting by Type (splits oxide into: marine,freshwater)
MgFeMn_type <- MgFeMn + geom_point(aes(color=Type, shape=Type),size=2.5,stroke=1.5)+
  scale_color_manual(values = c("#1F968BFF","#55C667FF","#404788FF","Black"))+
  scale_shape_manual(values=c(1, 3,18,2))+
  labs(shape="",color="",
       x="Mgx10 wt%",
       y="Fe wt%",
       z="Mn wt%")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
MgFeMn_type


#ternary plot - plotting by Formation (splits marine into:hydrogenetic, diagenetic, mixed)
MgFeMn_formation<- MgFeMn + geom_point(aes(color=Formation, shape=Formation),size=1,stroke=1)+
  scale_color_manual(values = c("#440154FF","#1F968BFF","#FDE725FF","#55C667FF","#404788FF","Black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="", x="Mgx10 wt%",
       y="Fe wt%",
       z="Mn wt%")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
MgFeMn_formation

################################################# Conly ########################################################
#Base Ternary Plot
#add in elements you want to plot in this line (make sure to have the correct dataset)
CoCuNi_Fe_Mn=ggtern(data=Full_Data,aes(x=Fe_ppm, y=Co_Cu_Ni, z=Mn_ppm))

#ternary plot - plotting by Formation (splits marine into:hydrogenetic, diagenetic, mixed)
CoCuNi_Fe_Mn_formation<- CoCuNi_Fe_Mn + geom_point(aes(color=Formation, shape=Formation),size=1,stroke=1)+
  scale_color_manual(values = c("#440154FF","#1F968BFF","#FDE725FF","#55C667FF","#404788FF","Black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="", x="Fe ppm",
       y="Co+Cu+Ni ppm",
       z="Mn ppm")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
CoCuNi_Fe_Mn_formation

################################################# Trace ########################################################
#Base Ternary Plot
#add in elements you want to plot in this line (make sure to have the correct dataset)
CoCuZn=ggtern(data=Full_Data,aes(x=Cu, y=Co, z=Zn))

#ternary plot - plotting by Formation (splits marine into:hydrogenetic, diagenetic, mixed)
CoCuZn_formation<- CoCuZn + geom_point(aes(color=Formation, shape=Formation),size=1,stroke=1)+
  scale_color_manual(values = c("#440154FF","#1F968BFF","#FDE725FF","#55C667FF","#404788FF","Black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="", x="Cu ppm",
       y="Co ppm",
       z="Zn ppm")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
CoCuZn_formation



################################################# Figure #######################################################
#ggtern and ggarrange are incompatible so using grid.arrange from ggplot instead
#not using this because I don't have the patience to figure out how to adjust the sizes of everything to get an appropriately proportioned figure

Tern<-ggtern::grid.arrange(MgFeMn_formation, CoCuNi_Fe_Mn_formation, CoCuZn_formation, 
                           ncol=3)
Tern

#set WD
setwd("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Ternary_digrams")

#save plot in WD as pdf with dimensions that you want
ggsave("CoCuZn_formation.pdf", device = "pdf", plot=CoCuZn_formation, width=100, height=90, units="mm") 
ggsave("CoCuNi_Fe_Mn_formation.pdf", device = "pdf", plot=CoCuNi_Fe_Mn_formation, width=100, height=90, units="mm") 
ggsave("MgFeMn_formation.pdf", device = "pdf", plot=MgFeMn_formation, width=100, height=90, units="mm") 
