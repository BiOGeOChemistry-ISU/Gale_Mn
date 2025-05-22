
packages = c('devtools','ggplot2', 'plotly', 'rstatix', 'readr', 'dplyr', 'tidyr', 'readxl', 'ggpubr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# need this version of ggplot for ggboxplot()
install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")

require("ggpubr")
require("devtools")
require("readxl")
require("dplyr")


##################### Loading and manipulating data ###################

#Bringing in datasheet
Full_Data <- read_excel("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Stats/ManganeseData_ForRCode.xlsx", 
                        sheet = "Sheet1")

#replace zero values with NA
Full_Data[Full_Data == 0] <- NA

#converting to molar units
Full_Data$Zn_M=Full_Data$Zn/(65.38*1000)
Full_Data$Mn_M=Full_Data$Mn_ppm/(54.938*1000)
Full_Data$Mg_M=Full_Data$Mg_wt/(24.305)*10
Full_Data$Fe_M=Full_Data$Fe_ppm/(55.845*1000)
Full_Data$Co_M=Full_Data$Co/(58.933*1000)
Full_Data$Cu_M=Full_Data$Cu/(63.646*1000)
Full_Data$Mo_M=Full_Data$Mo/(95.95*1000)
Full_Data$Ni_M=Full_Data$Ni/(58.693*1000)
Full_Data$Sr_M=Full_Data$Sr/(87.62*1000)
Full_Data$Ba_M=Full_Data$Ba/(137.33*1000)
Full_Data$Li_M=Full_Data$Li/(6.941*1000)

##calculating ratios and transforming elements to log
Full_Data$log_Mn=log10(Full_Data$Mn_M)
Full_Data$log_Fe_Mn=log10(Full_Data$Fe_M/Full_Data$Mn_M)
Full_Data$log_Zn_Mn=log10(Full_Data$Zn_M/Full_Data$Mn_M)
Full_Data$log_Ba_Mn=log10(Full_Data$Ba_M/Full_Data$Mn_M)
Full_Data$log_Cu_Mn=log10(Full_Data$Cu_M/Full_Data$Mn_M)
Full_Data$log_Co_Mn=log10(Full_Data$Co_M/Full_Data$Mn_M)
Full_Data$log_Mo_Mn=log10(Full_Data$Mo_M/Full_Data$Mn_M)
Full_Data$log_Ni_Mn=log10(Full_Data$Ni_M/Full_Data$Mn_M)
Full_Data$log_Sr_Mn=log10(Full_Data$Sr_M/Full_Data$Mn_M)
Full_Data$log_Mg_Mn=log10(Full_Data$Mg_M/Full_Data$Mn_M)
Full_Data$log_Li_Mn=log10(Full_Data$Li_M/Full_Data$Mn_M)

#filtering to only include Mn-species of oxides and carbonates
Oxides_Carbonates<-filter(Full_Data,Mn_Species=="Oxide"|Mn_Species=="Carbonate")

#MARINE, CARBONATES
Marine_Carbonates<-filter(Oxides_Carbonates,Type=="Marine"|Type=="Carbonates") #filtering

#FRESHWATER_CARBONATES
Freshwater_Carbonates<-filter(Oxides_Carbonates,Type=="Freshwater"|Type=="Carbonates") #filtering

#FRESHWATER_MARINE
Freshwater_Marine<-filter(Oxides_Carbonates,Type=="Freshwater"|Type=="Marine") #filtering

########################## Oxides normality plots ###############################

#checking normality (for these make sure to change the element you are interested in)
  #OXIDES
Oxides<-filter(Oxides_Carbonates,Mn_Species=="Oxide") #filter
shapiro.test(Oxides$log_Li_Mn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Oxides_Li_Mn<-Oxides$log_Li_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Li_Mn,
     xlab = "log Li/Mn",
     main = "W = 0.96235, p-value = 0.03003") #histogram

Oxides_Mg_Mn<-Oxides$log_Mg_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Mg_Mn,
     xlab = "log Mg/Mn",
     main = "W = 0.87214, p-value = 1.123e-13") #histogram

Oxides_Zn_Mn<-Oxides$log_Zn_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Zn_Mn,
     xlab = "log Zn/Mn",
     main = "W = 0.83605, p-value < 2.2e-16") #histogram

Oxides_Mn<-Oxides$log_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Mn,
     xlab = "log Mn",
     main = "W = 0.83676, p-value < 2.2e-16") #histogram

Oxides_Fe_Mn<-Oxides$log_Fe_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Fe_Mn,
     xlab = "log Fe/Mn",
     main = "W = 0.95158, p-value = 1.515e-08") #histogram

Oxides_Ba_Mn<-Oxides$log_Ba_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Ba_Mn,
     xlab = "log Ba/Mn",
     main = "W = 0.89738, p-value = 5.727e-11") #histogram

Oxides_Cu_Mn<-Oxides$log_Cu_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Cu_Mn,
     xlab = "log Cu/Mn",
     main = "W = 0.93693, p-value = 4.393e-10") #histogram

Oxides_Co_Mn<-Oxides$log_Co_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Co_Mn,
     xlab = "log Co/Mn",
     main = "W = 0.93082, p-value = 1.098e-10") #histogram

Oxides_Mo_Mn<-Oxides$log_Mo_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Mo_Mn,
     xlab = "log Mo/Mn",
     main = "W = 0.62424, p-value < 2.2e-16") #histogram

Oxides_Ni_Mn<-Oxides$log_Ni_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Ni_Mn,
     xlab = "log Ni/Mn",
     main = "W = 0.8448, p-value < 2.2e-16") #histogram

Oxides_Sr_Mn<-Oxides$log_Sr_Mn #setting histogram  !!!Change element in this line!!!
hist(Oxides_Sr_Mn,
     xlab = "log Sr/Mn",
     main = "W = 0.97825, p-value = 0.01102") #histogram

################ CARBONATES normality plots ##################################
Carbonates<-filter(Oxides_Carbonates,Mn_Species=="Carbonate") #filter

shapiro.test(Carbonates$log_Li_Mn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Carbonates_Li_Mn<-Carbonates$log_Li_Mn #setting histogram
hist(Carbonates_Li_Mn,
     xlab = "log Li/Mn",
     main = "W = 0.93241, p-value = 0.6129") #histogram !!!Change element in this line!!!

Carbonates_Mg_Mn<-Carbonates$log_Mg_Mn #setting histogram
hist(Carbonates_Mg_Mn,
     xlab = "log Mg/Mn",
     main = "W = 0.89064, p-value = 0.0194") #histogram !!!Change element in this line!!!

Carbonates_Zn_Mn<-Carbonates$log_Zn_Mn #setting histogram
hist(Carbonates_Zn_Mn,
     xlab = "log Zn/Mn",
     main = "W = 0.9603, p-value = 0.5223") #histogram !!!Change element in this line!!!
  
Carbonates_Mn<-Carbonates$log_Mn #setting histogram
hist(Carbonates_Mn,
     xlab = "log Mn",
     main = "W = 0.77779, p-value = 0.0002293") #histogram !!!Change element in this line!!!

Carbonates_Fe_Mn<-Carbonates$log_Fe_Mn #setting histogram
hist(Carbonates_Fe_Mn,
     xlab = "log Fe/Mn",
     main = "W = 0.82969, p-value = 0.001516") #histogram !!!Change element in this line!!!

Carbonates_Ba_Mn<-Carbonates$log_Ba_Mn #setting histogram
hist(Carbonates_Ba_Mn,
     xlab = "log Ba/Mn",
     main = "W = 0.97619, p-value = 0.8619") #histogram !!!Change element in this line!!!

Carbonates_Cu_Mn<-Carbonates$log_Cu_Mn #setting histogram
hist(Carbonates_Cu_Mn,
     xlab = "log Cu/Mn",
     main = "W = 0.93751, p-value = 0.1945") #histogram !!!Change element in this line!!!

Carbonates_Co_Mn<-Carbonates$log_Co_Mn #setting histogram
hist(Carbonates_Co_Mn,
     xlab = "log Co/Mn",
     main = "W = 0.88737, p-value = 0.01675") #histogram !!!Change element in this line!!!

Carbonates_Mo_Mn<-Carbonates$log_Mo_Mn #setting histogram
hist(Carbonates_Mo_Mn,
     xlab = "log Mo/Mn",
     main = "W = 0.93799, p-value = 0.4314") #histogram !!!Change element in this line!!!

Carbonates_Ni_Mn<-Carbonates$log_Ni_Mn #setting histogram
hist(Carbonates_Ni_Mn,
     xlab = "log Ni/Mn",
     main = "W = 0.96899, p-value = 0.6878") #histogram !!!Change element in this line!!!

Carbonates_Sr_Mn<-Carbonates$log_Sr_Mn #setting histogram
hist(Carbonates_Sr_Mn,
     xlab = "log Sr/Mn",
     main = "W = 0.96242, p-value = 0.6773") #histogram !!!Change element in this line!!!

##################### FRESHWATER normality plots ###############
Freshwater<-filter(Oxides_Carbonates,Type=="Freshwater") #filter
shapiro.test(Freshwater$log_Li_Mn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Freshwater_Li_Mn<-Freshwater$log_Li_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Li_Mn,
     xlab = "log Li/Mn",
     main = "W = 0.90335, p-value = 0.203") #histogram

Freshwater_Mg_Mn<-Freshwater$log_Mg_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Mg_Mn,
     xlab = "log Mg/Mn",
     main = "W = 0.83599, p-value = 1.98e-06") #histogram

Freshwater_Mn<-Freshwater$log_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Mn,
     xlab = "log Mn",
     main = "W = 0.97237, p-value = 0.07693") #histogram

Freshwater_Fe_Mn<-Freshwater$log_Fe_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Fe_Mn,
     xlab = "log Fe/Mn",
     main = "W = 0.94285, p-value = 0.001267") #histogram

Freshwater_Ba_Mn<-Freshwater$log_Ba_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Ba_Mn,
     xlab = "log Ba/Mn",
     main = "W = 0.81061, p-value = 0.001249") #histogram

Freshwater_Co_Mn<-Freshwater$log_Co_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Co_Mn,
     xlab = "log Co/Mn",
     main = "W = 0.96009, p-value = 0.01638") #histogram

Freshwater_Cu_Mn<-Freshwater$log_Cu_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Cu_Mn,
     xlab = "log Cu/Mn",
     main = "W = 0.84971, p-value = 2.403e-07") #histogram

Freshwater_Mo_Mn<-Freshwater$log_Mo_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Mo_Mn,
     xlab = "log Mo/Mn",
     main = "W = 0.9463, p-value = 0.5971") #histogram

Freshwater_Ni_Mn<-Freshwater$log_Ni_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Ni_Mn,
     xlab = "log Ni/Mn",
     main = "W = 0.90136, p-value = 1.244e-05") #histogram

Freshwater_Sr_Mn<-Freshwater$log_Sr_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Sr_Mn,
     xlab = "log Sr/Mn",
     main = "W = 0.96577, p-value = 0.8409") #histogram

Freshwater_Zn_Mn<-Freshwater$log_Zn_Mn #setting histogram !!!Change element in this line!!!
hist(Freshwater_Zn_Mn,
     xlab = "log Zn/Mn",
     main = "W = 0.95706, p-value = 0.008369") #histogram

##################### MARINE normality plots ################################
Marine<-filter(Oxides_Carbonates,Type=="Marine") #filter
shapiro.test(Marine$log_Li_Mn) #Shapiro-Wilk normality test !!!Change element in this line!!!

Marine_Li_Mn <-Marine$log_Li_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Li_Mn, 
     xlab = "log Li/Mn",
     main = "W = 0.93243, p-value = 0.002288") #histogram 

Marine_Mg_Mn <-Marine$log_Mg_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Mg_Mn, 
     xlab = "log Mg/Mn",
     main = "W = 0.9462, p-value = 1.093e-06") #histogram 

Marine_Mn <-Marine$log_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Mn, 
     xlab = "log Mn",
     main = "W = 0.92386, p-value = 2.016e-09") #histogram 

Marine_Fe_Mn <-Marine$log_Fe_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Fe_Mn, 
     xlab = "log Fe/Mn",
     main = "W = 0.83622, p-value = 9.566e-15") #histogram 

Marine_Ba_Mn <-Marine$log_Ba_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Ba_Mn, 
     xlab = "log Ba/Mn",
     main = "W = 0.9132, p-value = 2.7e-09") #histogram 

Marine_Co_Mn <-Marine$log_Co_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Co_Mn, 
     xlab = "log Co/Mn",
     main = "W = 0.84183, p-value = 1.785e-14") #histogram 

Marine_Cu_Mn <-Marine$log_Cu_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Cu_Mn, 
     xlab = "log Cu/Mn",
     main = "W = 0.91461, p-value = 3.919e-10") #histogram 

Marine_Mo_Mn <-Marine$log_Mo_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Mo_Mn, 
     xlab = "log Mo/Mn",
     main = "W = 0.80799, p-value = 9.386e-15") #histogram 

Marine_Ni_Mn <-Marine$log_Ni_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Ni_Mn, 
     xlab = "log Ni/Mn",
     main = "W = 0.89756, p-value = 2.518e-11") #histogram 

Marine_Sr_Mn <-Marine$log_Sr_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Sr_Mn, 
     xlab = "log Sr/Mn",
     main = "W = 0.97497, p-value = 0.006855") #histogram 

Marine_Zn_Mn <-Marine$log_Zn_Mn #setting histogram !!!Change element in this line!!!
hist(Marine_Zn_Mn, 
     xlab = "log Zn/Mn",
     main = "W = 0.74226, p-value < 2.2e-16") #histogram 


##################### Pulling Summary and Doing T-Test #######################

## not using this because it will be built into the plots
  
  #OXIDES AND CARBONATES
Oxides_Carbonates%>%
  group_by(Mn_Species) %>%
  get_summary_stats(log_Mn, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Mn ~ Mn_Species,data=Oxides_Carbonates) #t-test !!!Change element in this line!!!
res
  
  #Marine and Carbonates
Marine_Carbonates%>%
  group_by(Type) %>%
  get_summary_stats(log_Sr_Mn, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Sr_Mn ~ Type,data=Marine_Carbonates) #t-test !!!Change element in this line!!!
res
  
  #Freshwater and Carbonates
Freshwater_Carbonates%>%
  group_by(Type) %>%
  get_summary_stats(log_Zn_Mn, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Zn_Mn ~ Type,data=Freshwater_Carbonates) #t-test
res
  
  #Freshwater and Marine
Freshwater_Marine%>%
  group_by(Type) %>%
  get_summary_stats(log_Mg_Mn, type="mean_sd") #pulling stats !!!Change element in this line!!!
res<-t.test(log_Mg_Mn ~ Type,data=Freshwater_Marine) #t-test
res


################################# Boxplots with stats for Freshwater vs. Marine #####################################

#boxplot
bxp_FwM_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Mn", #!!!Change element in this line!!!
  ylab = "log(Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Mg_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Mg_Mn", #!!!Change element in this line!!!
  ylab = "log(Mg/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Mg_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Mg_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Fe_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Fe_Mn", #!!!Change element in this line!!!
  ylab = "log(Fe/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Fe_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Fe_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Ba_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Ba_Mn", #!!!Change element in this line!!!
  ylab = "log(Ba/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Ba_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Ba_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Co_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Co_Mn", #!!!Change element in this line!!!
  ylab = "log(Co/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Co_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Co_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Cu_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Cu_Mn", #!!!Change element in this line!!!
  ylab = "log(Cu/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Cu_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Cu_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Mo_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Mo_Mn", #!!!Change element in this line!!!
  ylab = "log(Mo/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Mo_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Mo_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Ni_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Ni_Mn", #!!!Change element in this line!!!
  ylab = "log(Ni/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Ni_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Ni_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Sr_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Sr_Mn", #!!!Change element in this line!!!
  ylab = "log(Sr/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Sr_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Sr_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Zn_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Zn_Mn", #!!!Change element in this line!!!
  ylab = "log(Zn/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Zn_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Zn_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_FwM_Li_Mn <-ggboxplot(
  Freshwater_Marine, x = "Type", y = "log_Li_Mn", #!!!Change element in this line!!!
  ylab = "log(Li/Mn)", xlab = "Type", add = "jitter" #!!!Change element in this line!!!
)
#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Li_Mn ~ Type) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp_FwM_Li_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

################################# Boxplots with stats for Oxides vs. Carbonates #####################################

  #boxplot
bxp_OC_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Mn",
  ylab = "log(Mn)", xlab = "Mn facies", add = "jitter"
)
  #add stat test
  #stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Mg_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Mg_Mn",
  ylab = "log(Mg/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Mg_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Mg_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Fe_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Fe_Mn",
  ylab = "log(Fe/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Fe_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Fe_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Ba_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Ba_Mn",
  ylab = "log(Ba/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Ba_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Ba_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Co_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Co_Mn",
  ylab = "log(Co/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Co_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Co_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Cu_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Cu_Mn",
  ylab = "log(Cu/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Cu_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Cu_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Mo_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Mo_Mn",
  ylab = "log(Mo/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Mo_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Mo_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Ni_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Ni_Mn",
  ylab = "log(Ni/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Ni_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Ni_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Sr_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Sr_Mn",
  ylab = "log(Sr/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Sr_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Sr_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Zn_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Zn_Mn",
  ylab = "log(Zn/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Zn_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Zn_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#boxplot
bxp_OC_Li_Mn<-ggboxplot(
  Oxides_Carbonates, x = "Mn_Species", y = "log_Li_Mn",
  ylab = "log(Li/Mn)", xlab = "Mn facies", add = "jitter"
)
#add stat test
#stat test
stat.test <- Oxides_Carbonates %>% 
  t_test(log_Li_Mn ~ Mn_Species) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Mn_Species")
bxp_OC_Li_Mn + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

################### BOXPLOT WITH STATS terrestrial by formation #############

Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Co_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Co_Mn",
  ylab = "log(Co/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Cu_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Cu_Mn",
  ylab = "log(Cu/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Mo_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Mo_Mn",
  ylab = "log(Mo/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Ni_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Ni_Mn",
  ylab = "log(Ni/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Zn_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Zn_Mn",
  ylab = "log(Zn/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Li_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Li_Mn",
  ylab = "log(Li/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

##########################CREATING BOXPLOT  using Gale Crater Data ##########

Gale<-filter(Full_Data,Type=="Marine"|Type=="Freshwater"|Type=="Gale Crater"|Type=="Carbonates")
Gale$Formation <- factor(Gale$Formation , levels=c("Hydrogenetic", "Mixed", "Diagenetic", "Freshwater","Carbonates","Gale Crater"))

Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Mg_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Mg_Mn",
  ylab = "log(Mg/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp


Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Mn",
  ylab = "log(Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp


Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Fe_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Fe_Mn",
  ylab = "log(Fe/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp


Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Ba_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Ba_Mn",
  ylab = "log(Ba/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp


Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Sr_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Sr_Mn",
  ylab = "log(Sr/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

Gale%>%
  group_by(Formation) %>%
  get_summary_stats(log_Li_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Gale, x = "Formation", y = "log_Li_Mn",
  ylab = "log(Li/Mn)", xlab = FALSE, add = "jitter") +
  font("xy.text", size = 10)
bxp

############################### not using #######################

# not using this code

#pulling stats
Oxides_Carbonates%>%
  group_by(Formation) %>%
  get_summary_stats(log_Mn, type="mean_sd") #pulling stats
#boxplot
bxp<-ggboxplot(
  Oxides_Carbonates, x = "Formation", y = "log_Mn",
  ylab = "log(Mn)", xlab = "Mn-Species", add = "jitter"
)
bxp

#add stat test
#stat test
stat.test <- Freshwater_Marine %>% 
  t_test(log_Mn ~ Type) %>%
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

#add stat test
#stat test
stat.test <- Freshwater %>% 
  t_test(log_Zn ~ Formation) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Type")
bxp + 
  stat_ +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))
