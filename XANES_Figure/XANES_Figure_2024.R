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


Range_Data_Mn<-filter(XANES,eV>6530 & eV<6580)
Range_Data_Mn_Carb<-filter(Range_Data_Mn,Type=="Carbonate"| Type=="Standard")
Range_Data_Mn_Oxyhy<-filter(Range_Data_Mn,Type=="Oxy(hydroxide)"| Type=="Phyllomanganite")
Range_Data_Mn_BL<-filter(Range_Data_Mn, Type=="Standard2"|Type=="BL 16 to 18cm"| Type=="BL 18 to 20cm" | Type=="BL 28 to 30cm"| Type=="BL 6 to 8cm"| Type=="Phosphate")
Range_Data_Mn_BL$Type[is.na(Range_Data_Mn_BL$Type)] <- "Rhodocrosite"

Range_Data_Mn_Carb$Sample<-factor(Range_Data_Mn_Carb$Sample,levels = c("Rhodochrosite0","CB_38","CB_14","CY_25","MO5","O99"))


sample_labels <- c(
  "Rhodochrosite0" = "Rhodochrosite",
  "CB_38" = "CB-38",
  "CB_14" = "CB-14",
  "CY_25" = "CY-25",
  "MO5" = "MO5",
  "O99" = "O99"
)

label_data <- Range_Data_Mn_Carb |>
  group_by(Sample) |>
  slice_max(eV, n = 1) |>
  mutate(Sample_label = sample_labels[as.character(Sample)])

Mn_XANES_Carb<-ggplot(Range_Data_Mn_Carb, aes(eV, Adjusted_Norm_Abs, label=Sample))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Sample),size=0.5)+
  geom_text(
    data = label_data,
    aes(x = eV, y = Adjusted_Norm_Abs, label = Sample_label, color = Sample),
    hjust = -0.1,
    size = 2.5,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 6539, color= "black", size=0.5) +
  ylim(0, 4.5)+
  scale_color_manual(
    values = c("Black","#E64B35FF","#E64B35FF","#E64B35FF","#E64B35FF","#E64B35FF"),
    labels = sample_labels
  ) +
  scale_x_continuous(
    breaks = seq(6530, 6580, by = 15)  # adjust to your range
  ) +
  coord_cartesian(clip = "off") +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic()+
  theme(legend.position = "none", #remove legend
        plot.margin = margin(5.5, 50, 5.5, 5.5),
      strip.text.x = element_text(size = 8), #change font size of facet label
      axis.text = element_text(size = 8), #change font size of axis grid label
      axis.title = element_text(size = 8),  #change font size of axis label
      axis.ticks.y = element_blank(), # remove y axis ticks
      axis.text.y = element_blank()) # remove y axis tick labels
Mn_XANES_Carb

Range_Data_Mn_Oxyhy$Sample<-factor(Range_Data_Mn_Oxyhy$Sample,levels = c("dBi4","LV_FM","LW_Mass","LW_Nod"))

sample_labels_2 <- c(
  "dBi4" = "Phyllomang.",
  "LV_FM" = "LV_FM",
  "LW_Mass" = "LW_Mass",
  "LW_Nod" = "LW_Nod"
)

label_data2 <- Range_Data_Mn_Oxyhy |>
  group_by(Sample) |>
  slice_max(eV, n = 1) |>
  mutate(Sample_label_2 = sample_labels_2[as.character(Sample)])


Mn_XANES_Oxyhy<-ggplot(Range_Data_Mn_Oxyhy, aes(eV, Adjusted_Norm_Abs, label=Sample))+
  geom_line(aes(x=eV,y=Adjusted_Norm_Abs,color=Sample),size=0.5)+
  geom_text(
    data = label_data2,
    aes(x = eV, y = Adjusted_Norm_Abs, label = Sample_label_2, color = Sample),
    hjust = -0.1,
    size = 2.5,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 6560.5, color= "black", size=0.5) +
  ylim(0, 3)+
  scale_color_manual(
    values = c("Black","#404788FF","#404788FF","#404788FF"),
    labels = sample_labels
  ) +
  scale_x_continuous(
    breaks = seq(6530, 6580, by = 15)  # adjust to your range
  ) +
  coord_cartesian(clip = "off") +
  ylab("Normalized Absorbance")+
  xlab(NULL) +
  theme_classic()+
  theme(legend.position = "none", #remove legend
        plot.margin = margin(5.5, 50, 5.5, 5.5),
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 8),  #change font size of axis label
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.text.y = element_blank()) # remove y axis tick labels
Mn_XANES_Oxyhy


Range_Data_Mn_BL$Type<-factor(Range_Data_Mn_BL$Type,levels = c("Phosphate","BL 28 to 30cm", "BL 18 to 20cm", "BL 16 to 18cm", "BL 6 to 8cm","Rhodocrosite"))
 
sample_labels_3 <- c(
  "Phosphate" = "Hurealite",
  "BL 28 to 30cm" = "BL_28-30",
  "BL 18 to 20cm" = "BL_18-20",
  "BL 16 to 18cm" = "BL_16-18",
  "BL 6 to 8cm" = "BL_6-8",
  "Rhodocrosite" = "Rhodocrosite"
)

label_data3 <- Range_Data_Mn_BL |>
  group_by(Type) |>
  slice_max(eV, n = 1, with_ties = FALSE) |>
  mutate(Sample_label_3 = sample_labels_3[as.character(Type)])

Mn_XANES_BL <- ggplot(Range_Data_Mn_BL, aes(eV, Adjusted_Norm_Abs, color = Type)) +
  geom_line(size = 0.5) +
  scale_x_continuous(
    breaks = seq(6530, 6580, by = 15)  # adjust to your range
  ) +
  geom_text(
    data = label_data3,
    aes(label = Sample_label_3, color = Type),
    hjust = -0.1,
    size = 2.5,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 6539, color = "black", size = 0.5) +
  geom_vline(xintercept = 6551, color = "black", size = 0.5) +
  scale_color_manual(
    values = c("Black","#1F968BFF","#1F968BFF","#1F968BFF","#1F968BFF","Black"),
    labels = sample_labels_3
  ) +
  ylim(0, 5.5) +
  coord_cartesian(clip = "off") +
  ylab(NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 50, 5.5, 5.5),
    strip.text.x = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
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

#save plot in WD as pdf with dimensions that you want
ggsave("XANES.jpeg", device = "jpeg", plot=XANES, width=50, height=90, units="mm") 

#save plot in WD as pdf with dimensions that you want
ggsave("XANES.tiff", device = "tiff", plot=XANES, width=50, height=90, units="mm") 
