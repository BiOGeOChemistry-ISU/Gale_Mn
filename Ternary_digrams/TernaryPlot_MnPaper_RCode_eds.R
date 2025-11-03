########################################## Load and install packages ################################

# List of required packages
packages <- c(
  'ggtern', 'plotly', 'readr', 'dplyr', 'tidyr', 'ggpubr', 
  'ggplot2', 'readxl'
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

######################################## Loading data ##############################################

#set directory to export plots
setwd("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Ternary_digrams")

#Bringing in datasheet
Full_Data <- read_excel("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Compiled_Lit_chemistry_v2_plotting.xlsx", 
                        sheet = "Data")

########################################### unit conversions #########################################

#convert to numeric
wtpct <- c("Fe",
           "FeO",
           "Fe2O3",
           "MnO",
           "MgO")

Full_Data <- Full_Data %>%
  mutate(across(all_of(wtpct), ~ as.numeric(.x)))


#add missing weight percent Fe data
Full_Data <- Full_Data %>%
  mutate(
    Fe = case_when(
      !is.na(Fe) ~ Fe, # Keep existing Fe values
      !is.na(Fe2O3) ~ Fe2O3 * 0.6994, # Convert from Fe2O3 if Fe is NA
      !is.na(FeO) ~ FeO * 0.7773, # Convert from FeO if Fe and Fe2O3 are NA
      TRUE ~ NA_real_ # If all are NA, keep NA
    )
  )

#add missing weight percent Mn data
Full_Data <- Full_Data %>%
  mutate(
    Mn = case_when(
      !is.na(Mn) ~ Mn, # Keep existing Fe values
      !is.na(MnO) ~ MnO * 0.7744, # Convert from MnO if Mn is NA
      TRUE ~ NA_real_ # If all are NA, keep NA
    )
  )


#add missing weight percent Mg data from MgO into a new column and make another new column that has 10x Mg
Full_Data <- Full_Data %>%
  mutate(
    Mg = case_when(
      !is.na(MgO) ~ MgO * 0.603, # Convert from MgO to Mg
      TRUE ~ NA_real_
    ),
    Mgx10 = Mg * 10 # Multiply Mg by 10
  )

#convert to numeric
ppm <- c("Cu",
         "Co",
         "Ni",
         "Zn")


#################################### Filtering dataset #############################################



All_Data <- Full_Data %>%
  filter(
    Mn_species == "carbonate" |
      (Mn_species == "oxide" & Formation %in% c("Freshwater", "Marine")) |
      Pathway %in% c("Gale crater", "Mixed", "Hydrogenetic", "Diagenetic")
  )

All_Data <- All_Data %>%
  mutate(across(all_of(ppm), ~ as.numeric(.x)))


All_Data <- All_Data %>%
  mutate(
    Fe_ppm = Fe * 10000,
    Mn_ppm = Mn * 10000
  )


All_Data <- All_Data %>%
  mutate(Co_Cu_Ni = 10*(Co + Ni + Cu)) 

#To set order of legend - important step as this will also organize the order that you assign colors
All_Data$Pathway <- factor(All_Data$Pathway,levels = c("Diagenetic","Hydrogenetic","Mixed","Freshwater", "Carbonates", "Gale crater"))

################################################# MgFeMn ########################################################
#Base Ternary Plot
  #add in elements you want to plot in this line (make sure to have the correct dataset)
MgFeMn = ggtern(data=All_Data,aes(x=Mgx10, y=Fe, z=Mn))

  #ternary plot - plotting by Mn-species
MgFeMn_species <- MgFeMn + geom_point(aes(color=Pathway, shape=Pathway),size=1,stroke=1)+
  scale_color_manual(values = c("#404788FF","#1F968BFF","#FDE725FF","#55C667FF","grey","black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="",
       x="10xMg",
       y="Fe",
       z="Mn")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
MgFeMn_species


################################################# Conly ########################################################



#Base Ternary Plot
#add in elements you want to plot in this line (make sure to have the correct dataset)
CoCuNi_Fe_Mn=ggtern(data=All_Data,aes(x=Fe_ppm, y=Co_Cu_Ni, z=Mn_ppm))

#ternary plot - plotting by Formation (splits marine into:hydrogenetic, diagenetic, mixed)
CoCuNi_Fe_Mn_formation<- CoCuNi_Fe_Mn + geom_point(aes(color=Pathway, shape=Pathway),size=1,stroke=1)+
  scale_color_manual(values = c("#440154FF","#1F968BFF","#FDE725FF","#55C667FF","grey","Black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="", x="Fe",
       y="10*(Co+Cu+Ni)",
       z="Mn")+
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
CoCuZn=ggtern(data=All_Data,aes(x=Cu, y=Co, z=Zn))

#ternary plot - plotting by Formation (splits marine into:hydrogenetic, diagenetic, mixed)
CoCuZn_formation<- CoCuZn + geom_point(aes(color=Pathway, shape=Pathway),size=1,stroke=1)+
  scale_color_manual(values = c("#440154FF","#1F968BFF","#FDE725FF","#55C667FF","grey","Black"))+
  scale_shape_manual(values=c(0, 1, 2, 3,18,2))+
  labs(shape="",color="", x="Cu",
       y="Co",
       z="Zn")+
  theme_void()+
  theme_showarrows()+
  theme_hidetitles()+
  theme(tern.panel.mask.show = FALSE,legend.spacing.y = unit(-0.2, 'cm'))+
  theme(tern.panel.mask.show = FALSE,legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1), legend.box.just = "left", 
        legend.background = element_rect(fill = "transparent", color = NA))
CoCuZn_formation


######################################## Exporting Plot #################################################

# Assuming these are your ggplot objects:
# MgFeMn_species, CoCuNi_Fe_Mn_formation, CoCuZn_formation

# Arrange the plots
combined_plot <- ggarrange(
  MgFeMn_species, CoCuZn_formation,
  labels = c("A.", "B."),
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)

# Save to PDF
ggsave("Ternary.pdf", combined_plot, width = 8.5, height = 4, units = "in")

# Save as TIFF
ggsave("Ternary.tiff", combined_plot, width = 8.5, height = 4, units = "in", dpi = 300)

# Save as JPEG
ggsave("Ternary.jpeg", combined_plot, width = 8.5, height = 4, units = "in", dpi = 300)

