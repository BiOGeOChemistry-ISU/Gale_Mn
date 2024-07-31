
packages = c('ggtern', 'plotly', 'readr', 'dplyr', 'tidyr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

require(devtools)
install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggstatsplot")

library(ggplot2)
library("viridis")
library("ggtern")
library(dplyr)
library(ggrepel)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dplyr)
library(readr)
library("ggstatsplot")



########Two Sample T-Test#######

#Bringing in datasheet
library(readxl)
Full_Data <- read_excel("ManganeseData_ForRCode.xlsx", 
                        sheet = "Sheet1")
#Filtering Data to prep for T-test





  
#transforming elements to log
ManganeseData_ForRCode$log_Zn_Mn=log10(ManganeseData_ForRCode$Zn/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Mn=log10(ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Fe_Mn=log10(ManganeseData_ForRCode$Fe_Mn)
ManganeseData_ForRCode$log_Ba_Mn=log10(ManganeseData_ForRCode$Ba/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Cu_Mn=log10(ManganeseData_ForRCode$Cu/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Co_Mn=log10(ManganeseData_ForRCode$Co/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Mo_Mn=log10(ManganeseData_ForRCode$Mo/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Ni_Mn=log10(ManganeseData_ForRCode$Ni/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Sr_Mn=log10(ManganeseData_ForRCode$Sr/ManganeseData_ForRCode$Mn_ppm)
ManganeseData_ForRCode$log_Mg_Mn=log10(ManganeseData_ForRCode$Mg_wt/ManganeseData_ForRCode$Mn_ppm)

#filtering to only include Mn-species of oxides and carbonates
Oxides_Carbonates<-filter(Full_Data,Mn_Species=="Oxide"|Mn_Species=="Carbonate")


#checking normality (for these make sure to change the element you are interested in)
  #OXIDES
Oxides<-filter(Oxides_Carbonates,Mn_Species=="Oxide") #filter
shapiro.test(Oxides$log_Zn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Oxides<-Oxides$log_Zn #setting histogram  !!!Change element in this line!!!
hist(Oxides) #histogram
 
  #CARBONATES
Carbonates<-filter(Oxides_Carbonates,Mn_Species=="Carbonate") #filter
shapiro.test(Carbonates$log_Zn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Carbonates<-Carbonates$log_Zn #setting histogram
hist(Carbonates) #histogram !!!Change element in this line!!!
  
  #FRESHWATER
Freshwater<-filter(Oxides_Carbonates,Type=="Freshwater") #filter
shapiro.test(Freshwater$log_Zn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Freshwater<-Freshwater$log_Zn #setting histogram !!!Change element in this line!!!
hist(Freshwater) #histogram

  #MARINE
Marine<-filter(Oxides_Carbonates,Type=="Marine") #filter
shapiro.test(Marine$log_Zn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Marine<-Marine$log_Zn #setting histogram !!!Change element in this line!!!
hist(Marine) #histogram 


#Pulling Summary and Doing T-Test
  
  #OXIDES AND CARBONATES
Oxides_Carbonates%>%
  group_by(Mn_Species) %>%
  get_summary_stats(log_Fe_Mn, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Fe_Mn ~ Mn_Species,data=Oxides_Carbonates) #t-test !!!Change element in this line!!!
res

  #MARINE, CARBONATES
Marine_Carbonates<-filter(Oxides_Carbonates,Type=="Marine"|Type=="Carbonates") #filtering

Marine_Carbonates%>%
  group_by(Type) %>%
  get_summary_stats(log_Sr, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Sr ~ Type,data=Marine_Carbonates) #t-test !!!Change element in this line!!!
res
  
  #FRESHWATER_CARBONATES
Freshwater_Carbonates<-filter(Oxides_Carbonates,Type=="Freshwater"|Type=="Carbonates") #filtering

Freshwater_Carbonates%>%
  group_by(Type) %>%
  get_summary_stats(log_Zn, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Zn ~ Type,data=Freshwater_Carbonates) #t-test
res
  
  #FRESHWATER_MARINE
Freshwater_Marine<-filter(Oxides_Carbonates,Type=="Freshwater"|Type=="Marine") #filtering

Freshwater_Marine%>%
  group_by(Type) %>%
  get_summary_stats(log_Mg, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Mg ~ Type,data=Freshwater_Marine) #t-test
res


Freshwater_Marine%>%
  group_by(Type) %>%
  get_summary_stats(log_Mg, type="mean_sd") #pulling stats !!!Change element in this line!!!
#boxplot
bxp<-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Mg", #!!!Change element in this line!!!
  ylab = "log(Mg/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Mg ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))




#CREATING BOXPLOT WITH STATS - using categories of: Oxides and Carbonates
  #pulling stats
Oxides_Carbonates%>%
  group_by(Mn_Species) %>%
  get_summary_stats(log_Mg, type="mean_sd") #pulling stats
  #boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Mg",
  ylab = "log(Mg/Mn)", xlab = "Mn-Species", add = "jitter"
)
  #add stat test
  #stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Mg ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))


#CREATING BOXPLOT WITH STATS  using categories by formation

  #pulling stats
Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Sr, type="mean_sd") #pulling stats
  #boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Sr",
  ylab = "log(Sr)", xlab = "Mn-Species", add = "jitter"
)
bxp

#add stat test
  #stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Zn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))



#CREATING BOXPLOT WITH STATS using Marine Only
#pulling stats
Marine<-filter(Oxides_Carbonates,Type=="Marine")
  #ordering categories
Oxides_Carbonates$Formation <- factor(Oxides_Carbonates$Formation , levels=c("Hydrogenetic", "Mixed", "Diagenetic", "Freshwater","Carbonates"))
Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(Mg_wt, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "Mg_wt",
  ylab = "Mg (wt%)", xlab = "Mn-Species", add = "jitter"
)
bxp 


#CREATING BOXPLOT WITH STATS using Gale Crater Data
  #grabbing stats
Gale<-filter(ManganeseData_ForRCode,Type=="Marine"|Type=="Freshwater"|Type=="Gale Crater"|Type=="Carbonates")
Gale$Formation <- factor(Gale$Formation , levels=c("Hydrogenetic", "Mixed", "Diagenetic", "Freshwater","Carbonates","Gale Crater"))

Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Mg, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Mg",
  ylab = "log(Mg)", xlab = "Mn-Species", add = "jitter"
)
bxp
#add stat test
#stat test
stat.test <- Freshwater %>% 
  t_test(log_Zn ~ Formation) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp + 
  stat_ +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))
