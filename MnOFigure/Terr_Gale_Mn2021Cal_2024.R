install.packages("ggplot2")
install.packages("tibble")
install.packages("dyplr")
install.packages("tidyr")

library("ggplot2")
library(tibble)
library(dplyr)
library("tidyr")
library(readxl)
require(ggpubr)
library(plotly)

############################################## Sol figure ########################################################
#import dataset
Gale_Mn_Concentration_2021 <- read_excel("MnOFigure/Gale_Mn_Concentration_2021.xlsx")

#MnO vs sol (figure 1a)
  #to filter and only use data from sol 2000 and before
GaleTarget_2000<-filter(Gale_Mn_Concentration_2021,Sol<2000)

#plot that distinguishes by color the targets looked at for study
Figure_1a<- ggplot(GaleTarget_2000, aes(x=MnO, y=Sol, color=Targets_for_study))+
  geom_point(aes(MnO,Sol),shape=21, fill="skyblue", color="steelblue4", size=1) +
  theme_classic()+
  theme(axis.text = element_text(size = 8),
        axis.title=element_text(size=8)) +
  labs(x="wt% MnO", y="Sol")
Figure_1a

#plot without distinguishing by color the targets looked at for study
Figure_1a_no_color<- ggplot(GaleTarget_2000, aes(x=MnO, y=Sol))+
  geom_point(aes(colour=factor(Targets_for_study), fill = factor(Targets_for_study)), shape=21, size = 1.5)+
  scale_color_manual(values=c("black","black"))+
  scale_fill_manual(values=c("black","grey"))+
  theme_classic()+
  theme(axis.text = element_text(size = 8),
        axis.title=element_text(size=8)) +
  labs( x="wt% MnO", y="Sol")+
  theme(legend.position = "none") +
  geom_vline(xintercept = c(1.24),linetype="dashed") #add line to show Mn enrichment
Figure_1a_no_color


#to add a log scale - add to one of the plots
#scale_x_continuous(trans='log10')

############################################## Box and whisker figure###################################################
#import dataset
MnO_BoxWhisker <- read_excel("MnOFigure/MnO_BoxWhisker.xlsx")
  
#for box and whisker plot - use MnO_BoxWhisker
GaleTarget_BW<-filter(MnO_BoxWhisker, Type == "Gale")
Gale_box_whisker<-ggplot(GaleTarget_BW,aes(x=MnO,y=Target))+
  geom_point(size=0.5)+
  geom_boxplot(aes(group=Target),outlier.color="black",outlier.size = 1, alpha=0.5)+
  theme_classic()+
  theme(legend.position="none",
        axis.text = element_text(size = 8),
        axis.title=element_text(size=8)) +
  labs(x="wt% MnO", y=NULL)
Gale_box_whisker

Terrestrial_BW<-filter(MnO_BoxWhisker, Type == "Terrestrial")
Terrestrial_box_whisker<-ggplot(Terrestrial_BW,aes(x=MnO,y=Target))+
  geom_point(size=0.5)+
  geom_boxplot(aes(group=Target),outlier.color="black",outlier.size = 1, alpha=0.5)+
  theme_classic()+
  theme(legend.position="none",
        axis.text = element_text(size = 8),
        axis.title=element_text(size=8)) +
  labs(x="wt% MnO", y=NULL)
Terrestrial_box_whisker

############################################### Export plots #######################################################
# line up plots in a row and label them
Gale<-ggarrange(Figure_1a_no_color, Gale_box_whisker, Terrestrial_box_whisker, # names of three plots in order desired
                 labels = c("A", "B", "C"), # labels plots
                 label.x = 0, # x position of labels
                 label.y = 1, # x position of labels
                 heights = c(2, 1, 1),
                 ncol = 1, nrow = 3, align = "v", # 3 down; align vertically
                 font.label = list(size = 9, face = "plain"))

Gale

#save plot in WD as pdf with dimensions that you want
ggsave("Gale.pdf", device = "pdf", plot=Gale, width=90, height=150, units="mm") 
