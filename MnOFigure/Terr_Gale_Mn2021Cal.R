install.packages("ggplot2")
install.packages("tibble")
install.packages("dyplr")
install.packages("tidyr")

library("ggplot2")
library(tibble)
library(dplyr)
library("tidyr")

#MnO vs sol (figure 1a)
  #to filter and only use data from sol 2000 and before
GaleTarget_2000<-filter(Gale_Mn_Concentration_2021,Sol<2000)

#plot without distinguishing by color the targets looked at for study
Figure_1a<- ggplot(GaleTarget_2000, aes(x=MnO, y=Sol, color=Targets_for_study))+
  geom_point(aes(MnO,Sol),shape=21, fill="skyblue", color="steelblue4", size=1) +
  theme_classic()+
  labs( x="wt% MnO", y="Sol")
Figure_1a_no_color

#plot that distinguishes by color the targets looked at for study
Figure_1a<- ggplot(GaleTarget_2000, aes(x=MnO, y=Sol))+
  geom_point(aes(colour=factor(Targets_for_study), fill = factor(Targets_for_study)), shape=21, size = 1.5)+
  scale_color_manual(values=c("steelblue4","tomato4"))+
  scale_fill_manual(values=c("skyblue","salmon1"))+
  theme_classic()+
  labs( x="wt% MnO", y="Sol")+
  theme(legend.position = "none")
Figure_1a

#add line to show Mn enrichment
Figure_1a+ geom_vline(xintercept = c(1.24),linetype="dashed")

#to add a log scale - add to one of the plots
scale_x_continuous(trans='log10')

  
#for box and whisker plot - use MnO_BoxWhisker
GaleTarget_BW<-filter(MnO_BoxWhisker, Type == "Gale")
Majors_box_whisker<-ggplot(GaleTarget_BW,aes(x=MnO,y=Target))+
  geom_point(size=0.5)+
  geom_boxplot(aes(group=Target),outlier.color="black",outlier.size = 1, alpha=0.5)+
  theme_bw()+
  theme(legend.position="none")+
  labs(x="wt%", x="")
Majors_box_whisker
