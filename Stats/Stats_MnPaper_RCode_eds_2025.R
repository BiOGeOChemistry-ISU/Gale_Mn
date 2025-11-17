########################################## Load and install packages ################################

# List of required packages
packages <- c(
  "devtools", "ggplot2", "plotly", "rstatix", "readr",
  "dplyr", "tidyr", "readxl", "ggpubr", "purrr", "ggsignif"
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
setwd("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Stats")

#Bringing in datasheet
Full_Data <- read_csv("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn/Compiled_Lit_chemistry_v2_withcategoricals.csv")


########################################### unit conversions #########################################

#convert to numeric
wtpct <- c("Fe",
           "FeO",
           "Fe2O3",
           "MnO")

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


#define molar masses
molar_masses <- c(
  Zn = 65.38,
  Mn = 54.938,
  MgO = 40.304,
  Fe = 55.845,
  Co = 58.933,
  Cu = 63.646,
  Mo = 95.95,
  Ni = 58.693,
  Sr = 87.62,
  Ba = 137.33,
  Li = 6.941
)


# Convert to numeric and calculate molar units
Full_Data <- Full_Data %>%
  mutate(across(all_of(names(molar_masses)), ~ as.numeric(.x))) %>%
  mutate(across(all_of(names(molar_masses)), 
                ~ .x / (molar_masses[cur_column()] * 1000000), 
                .names = "{.col}_M"))

# Special case for Wt% data (divide by 100 after conversion)
Full_Data <- Full_Data %>%
  mutate(across(c(MgO_M, Mn_M, Fe_M), ~ .x * 10000))


elements <- c("Fe_M", "Zn_M", "Ba_M", "Cu_M", "Co_M", "Mo_M", "Ni_M", "Sr_M", "MgO_M", "Li_M")

Full_Data <- Full_Data %>%
  mutate(log_Mn_M = log10(Mn_M)) %>%
  mutate(across(all_of(elements), 
                ~ log10(.x / Mn_M), 
                .names = "log_{.col}_Mn"))


#################################### Filtering dataset #############################################

#CARBONATES
Carbonates<-filter(Full_Data,Mn_species=="carbonate" & Pathway=="Carbonates") #filtering

Carbonates <- Carbonates %>%
  mutate(
    log_Zn_M_Mn = ifelse(is.infinite(log_Zn_M_Mn) & log_Zn_M_Mn < 0, NA, log_Zn_M_Mn))
   
#FRESHWATER & MARINE OXIDES
Oxides <- Full_Data %>%
  filter(Mn_species == "oxide" & Formation %in% c("Freshwater", "Marine"))

#remove NA and inf
Oxides <- Oxides %>%
  filter(!is.na(log_Mn_M) & is.finite(log_Mn_M))


Oxides <- Oxides %>%
  mutate(
    log_Fe_M_Mn = ifelse(is.infinite(log_Fe_M_Mn) & log_Fe_M_Mn < 0, NA, log_Fe_M_Mn),
    log_Ba_M_Mn = ifelse(is.infinite(log_Ba_M_Mn) & log_Ba_M_Mn < 0, NA, log_Ba_M_Mn),
    log_Mo_M_Mn = ifelse(is.infinite(log_Mo_M_Mn) & log_Mo_M_Mn < 0, NA, log_Mo_M_Mn)
  )

#GALE CRATER
Gale <- Full_Data %>%
  filter(Pathway == "Gale crater")

#OXIDES AND CARBONATES
Oxides_Carbonates <-bind_rows(Carbonates, Oxides)

#ALL DATA
All_Data<-bind_rows(Carbonates, Oxides, Gale)

########################## OXIDES normality plots ###############################

# Open PDF device with 8.5 x 11 inches (portrait)
pdf("Oxides_Histograms.pdf", width = 8.5, height = 11)

# Set up a 4x3 plotting grid (4 rows, 3 columns)
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))  # Standard margins

# Variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # 11 variables

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Oxides[[var]])
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 2.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles\n",
       "per kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn\n",
       "in Mn-(oxyhydr)oxides.\n",
       "W values above 0.9 are\n",
       "indicative of a log normal\n",
       "distribution and p-values less\n",
       "than 0.05 indicate the null\n",
       "hypothesis can be rejected.",
       sep = ""
     ))

# Close the PDF device
dev.off()

# Open JPEG device with 8.5 x 11 inches (portrait)
jpeg("Oxides_Histograms.jpeg", width = 8.5, height = 11, units = "in", res = 300)

# Set up a 4x3 plotting grid (4 rows, 3 columns)
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))  # Standard margins

# Variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # 11 variables

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Oxides[[var]])
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 2.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles\n",
       "per kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn\n",
       "in Mn-(oxyhydr)oxides.\n",
       "W values above 0.9 are\n",
       "indicative of a log normal\n",
       "distribution and p-values less\n",
       "than 0.05 indicate the null\n",
       "hypothesis can be rejected.",
       sep = ""
     ))

# Close the JPEG device
dev.off()

################ CARBONATES normality plots ##################################

# Open PDF device with 8.5 x 11 inches (portrait)
pdf("Carbonates_Histograms.pdf", width = 8.5, height = 11)

# Set up a 4x3 plotting grid
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))  # Standard margins

# Alphabetized list of variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # 11 variables

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Carbonates[[var]])  # Ensure numeric
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 3.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles per\n",
       "kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn in\n",
       "carbonates. W values\n",
       "above 0.9 are indicative of a\n",
       "log normal distribution and\n",
       "p-values less than 0.05 indicate\n",
       "the null hypothesis can\n",
       "be rejected.",
       sep = ""
     ))

# Close the PDF device
dev.off()


# Open JPEG device with 8.5 x 11 inches (portrait)
jpeg("Carbonates_Histograms.jpeg", width = 8.5, height = 11, units = "in", res = 300)

# Set up a 4x3 plotting grid
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))  # Standard margins

# Alphabetized list of variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # 11 variables

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Carbonates[[var]])  # Ensure numeric
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 3.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles per\n",
       "kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn in\n",
       "carbonates. W values\n",
       "above 0.9 are indicative of a\n",
       "log normal distribution and\n",
       "p-values less than 0.05 indicate\n",
       "the null hypothesis can\n",
       "be rejected.",
       sep = ""
     ))

# Close the JPEG device
dev.off()

##################### FRESHWATER normality plots ###############

# Filter the Oxides dataframe for Freshwater only
Oxides_Freshwater <- Oxides %>%
  filter(Formation == "Freshwater")

# Open PDF device
pdf("Oxides_Freshwater_Histograms.pdf", width = 8.5, height = 11)

# Set up a 4x3 plotting grid
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))

# Alphabetized list of variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # Replace with your 12th variable

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")  # Match labels

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Oxides_Freshwater[[var]])  # Ensure numeric
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 4.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles per\n",
       "kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn in\n",
       "freshwater Mn-(oxyhydr)oxides.\n",
       "W values above 0.9 are indicative\n",
       "of a log normal distribution and\n",
       "p-values less than 0.05 indicate\n",
       "the null hypothesis can\n",
       "be rejected.",
       sep = ""
     ))

# Close the PDF device
dev.off()

# Open JPEG device
jpeg("Oxides_Freshwater_Histograms.jpeg", width = 8.5, height = 11, units = "in", res = 300)

# Set up a 4x3 plotting grid
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))

# Alphabetized list of variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # Replace with your 12th variable

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")  # Match labels

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Oxides_Freshwater[[var]])  # Ensure numeric
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 4.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles per\n",
       "kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn in\n",
       "freshwater Mn-(oxyhydr)oxides.\n",
       "W values above 0.9 are indicative\n",
       "of a log normal distribution and\n",
       "p-values less than 0.05 indicate\n",
       "the null hypothesis can\n",
       "be rejected.",
       sep = ""
     ))

# Close the JPEG device
dev.off()

##################### MARINE normality plots ################################

# Filter the Oxides dataframe for Marine only
Oxides_Marine <- Oxides %>%
  filter(Formation == "Marine")

# Open PDF device
pdf("Oxides_Marine_Histograms.pdf", width = 8.5, height = 11)

# Set up a 4x3 plotting grid
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))

# Alphabetized list of variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # Replace with your 12th variable

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")  # Match labels

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Oxides_Freshwater[[var]])  # Ensure numeric
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 5.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles per\n",
       "kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn in\n",
       "marine Mn-(oxyhydr)oxides.\n",
       "W values above 0.9 are indicative\n",
       "of a log normal distribution and\n",
       "p-values less than 0.05 indicate\n",
       "the null hypothesis can\n",
       "be rejected.",
       sep = ""
     ))

# Close the PDF device
dev.off()

# Open JPEG device
jpeg("Oxides_Marine_Histograms.jpeg", width = 8.5, height = 11, units = "in", res = 300)

# Set up a 4x3 plotting grid
par(mfrow = c(4, 3), mar = c(4, 4, 4, 2))

# Alphabetized list of variables and labels
vars <- c("log_Ba_M_Mn", "log_Co_M_Mn", "log_Cu_M_Mn", "log_Fe_M_Mn",
          "log_Li_M_Mn", "log_MgO_M_Mn", "log_Mn_M", "log_Mo_M_Mn",
          "log_Ni_M_Mn", "log_Sr_M_Mn", "log_Zn_M_Mn")  # Replace with your 12th variable

labels <- c("log Ba/Mn", "log Co/Mn", "log Cu/Mn", "log Fe/Mn",
            "log Li/Mn", "log Mg/Mn", "log Mn", "log Mo/Mn",
            "log Ni/Mn", "log Sr/Mn", "log Zn/Mn")  # Match labels

# Loop through each variable
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- labels[i]
  data <- as.numeric(Oxides_Freshwater[[var]])  # Ensure numeric
  
  # Run Shapiro-Wilk test
  test <- shapiro.test(data)
  W <- round(test$statistic, 4)
  p <- format.pval(test$p.value, digits = 4, eps = .Machine$double.eps)
  
  # Plot histogram
  hist(data,
       xlab = label,
       main = paste0("\nW = ", W, ", p = ", p))
}

# Use the 12th panel for the caption
plot.new()
text(x = 0.05, y = 0.95, adj = c(0, 1), cex = 1.0, family = "sans",
     labels = paste(
       "Supplementary Figure 5.\n",
       "Shapiro-Wilk test for the log\n",
       "normal distribution of moles per\n",
       "kg Mn and trace elements\n",
       "(Ba, Co, Cu, Fe, Li, Mg, Mo,\n",
       "Ni, Sr, Zn) normalized to Mn in\n",
       "marine Mn-(oxyhydr)oxides.\n",
       "W values above 0.9 are indicative\n",
       "of a log normal distribution and\n",
       "p-values less than 0.05 indicate\n",
       "the null hypothesis can\n",
       "be rejected.",
       sep = ""
     ))

# Close the JPEG device
dev.off()

################################# Boxplots with stats for Terrestrial #####################################

# First is the code to make by individual element

# Create boxplot
bxp_FwM_Co <- ggboxplot(
  Oxides_Carbonates, x = "Pathway", y = "log_Co_M_Mn",
  ylab = "log Co/Mn", xlab = NULL,
  add = "jitter"
)

# Run Kruskal-Wallis test
kruskal_result <- Oxides_Carbonates %>%
  kruskal_test(log_Co_M_Mn ~ Pathway)

# If Kruskal-Wallis is significant, run pairwise Wilcoxon post-hoc test
if (kruskal_result$p < 0.05) {
  posthoc <- Oxides_Carbonates %>%
    pairwise_wilcox_test(log_Co_M_Mn ~ Pathway, p.adjust.method = "BH") %>%
    add_significance() %>%
    add_xy_position(x = "Pathway")
  
  # Add significance annotations to the plot
  bxp_FwM_Co <- bxp_FwM_Co +
    stat_pvalue_manual(posthoc, tip.length = 0) +
    labs(subtitle = NULL)
}

# Display the plot
print(bxp_FwM_Co)


## Function to perform Kruskal-Wallis tests on all terrestrial-only datasets and make plot(s)

plot_kruskal_boxplot <- function(data, y_var, y_label) {
  library(ggpubr)
  library(rstatix)
  library(dplyr)
  
  
  # Define the desired order of x-axis categories
  desired_order <- c("Carbonates", "Freshwater", "Hydrogenetic", "Diagenetic", "Mixed") # Customize this!
  
  # Filter and set factor levels
  data <- data %>%
    filter(!is.na(.data[[y_var]])) %>%
    mutate(Pathway = factor(Pathway, levels = desired_order))
  
  
  # Check for sufficient data
  if (nrow(data) < 5 || length(unique(data$Pathway)) < 2) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = paste("Insufficient data for", y_label)) +
             theme_void())
  }
  
  # Plot
  p <- ggboxplot(
    data, x = "Pathway", y = y_var,
    ylab = y_label, xlab = NULL,
    add = "jitter"
  )
  
  kruskal_result <- data %>%
    kruskal_test(as.formula(paste(y_var, "~ Pathway")))
  
  if (kruskal_result$p < 0.05) {
    posthoc <- data %>%
      pairwise_wilcox_test(as.formula(paste(y_var, "~ Pathway")), p.adjust.method = "BH") %>%
      add_significance() %>%
      add_xy_position(x = "Pathway")
    
    p <- p +
      stat_pvalue_manual(posthoc, tip.length = 0) +
      labs(subtitle = NULL)
    
    
    print(kruskal_result)
    
  }
  
  return(p)
}

#Call the function and assign the result to the variable
bxp_FwM_Ba <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Ba_M_Mn", "log Ba/Mn")
bxp_FwM_Co <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Co_M_Mn", "log Co/Mn")
bxp_FwM_Cu <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Cu_M_Mn", "log Cu/Mn")
bxp_FwM_Fe <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Fe_M_Mn", "log Fe/Mn")
bxp_FwM_Li <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Li_M_Mn", "log Li/Mn")
bxp_FwM_Mg <- plot_kruskal_boxplot(Oxides_Carbonates, "log_MgO_M_Mn", "log Mg/Mn")
bxp_FwM_Mn <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Mo_M_Mn", "log Mn")
bxp_FwM_Mo <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Mo_M_Mn", "log Mo/Mn")
bxp_FwM_Ni <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Ni_M_Mn", "log Ni/Mn")
bxp_FwM_Sr <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Sr_M_Mn", "log Sr/Mn")
bxp_FwM_Zn <- plot_kruskal_boxplot(Oxides_Carbonates, "log_Zn_M_Mn", "log Zn/Mn")



# Remove x-axis text, ticks, and label from the top 3 plots
plots <- list(
  bxp_FwM_Ba + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Co + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Cu + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Fe + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Li + labs(x = NULL) + theme(axis.text.x = element_text(size = 9)),
  bxp_FwM_Mg + labs(x = NULL) + theme(axis.text.x = element_text(size = 9)),
  bxp_FwM_Mn + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Mo + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Ni + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Sr + labs(x = NULL) + theme(axis.text.x = element_text(size = 9)),
  bxp_FwM_Zn + labs(x = NULL) + theme(axis.text.x = element_text(size = 9))
)



# Arrange and save to PDF
pdf("Terrestrial_Boxplots.pdf", width = 8.5, height = 11)
ggarrange(plotlist = plots, ncol = 2, nrow = 3, align = "v")
dev.off()

jpeg("Terrestrial_Boxplots.jpeg", width = 8.5, height = 11, units = "in", res = 300)
ggarrange(plotlist = plots, ncol = 2, nrow = 3, align = "v")
dev.off()

################################# Boxplots with stats for all #####################################

# Define the reusable function
plot_kruskal_boxplot <- function(data, y_var, y_label) {
  library(ggpubr)
  library(rstatix)
  library(dplyr)
  

  # Filter out rows with NA in the y_var column
  data <- data %>% filter(!is.na(.data[[y_var]]))
  
  # Define the desired order of x-axis categories
  desired_order <- c("Carbonates", "Freshwater", "Marine", "Gale crater") # Customize this!
  
  # Filter and set factor levels
  data <- data %>%
    filter(!is.na(.data[[y_var]])) %>%
    mutate(Formation = factor(Formation, levels = desired_order))
  
  
  # Check for sufficient data
  if (nrow(data) < 5 || length(unique(data$Formation)) < 2) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = paste("Insufficient data for", y_label)) +
             theme_void())
  }
  
  # Create the boxplot
  p <- ggboxplot(
    data, x = "Formation", y = y_var,
    ylab = y_label, xlab = NULL,
    add = "jitter"
  ) #+
   # scale_y_log10() # Apply log10 scale to y-axis
  
  
  kruskal_result <- data %>%
    kruskal_test(as.formula(paste(y_var, "~ Formation")))
  
  if (kruskal_result$p < 0.05) {
    posthoc <- data %>%
      pairwise_wilcox_test(as.formula(paste(y_var, "~ Formation")), p.adjust.method = "BH") %>%
      add_significance() %>%
      add_xy_position(x = "Formation")
    
    p <- p +
      stat_pvalue_manual(posthoc, tip.length = 0) +
      labs(subtitle = NULL)
  }
  
  return(p)
}


# Generate plots
bxp_FwM_Ba  <- plot_kruskal_boxplot(All_Data, "log_Ba_M_Mn", "log Ba/Mn")
bxp_FwM_Fe  <- plot_kruskal_boxplot(All_Data, "log_Fe_M_Mn", "log Fe/Mn")
bxp_FwM_Li  <- plot_kruskal_boxplot(All_Data, "log_Li_M_Mn", "log Li/Mn")
bxp_FwM_Mg  <- plot_kruskal_boxplot(All_Data, "log_MgO_M_Mn", "log Mg/Mn")
bxp_FwM_Mn  <- plot_kruskal_boxplot(All_Data, "log_Mn_M", "log Mn")
bxp_FwM_Sr  <- plot_kruskal_boxplot(All_Data, "log_Sr_M_Mn", "log Sr/Mn")

# Format plots for layout
plots <- list(
  bxp_FwM_Ba + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Fe + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Li + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Mg + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + labs(x = NULL),
  bxp_FwM_Mn + labs(x = NULL) + theme(axis.text.x = element_text(size = 8)),
  bxp_FwM_Sr + labs(x = NULL) + theme(axis.text.x = element_text(size = 8))
)


# Save to PDF
pdf("All_Boxplots.pdf", width = 8.5, height = 11)
ggarrange(plotlist = plots, ncol = 2, nrow = 3, align = "v")
dev.off()

jpeg("All_Boxplots.jpeg", width = 8.5, height = 11, units = "in", res = 300)
ggarrange(plotlist = plots, ncol = 2, nrow = 3, align = "v")
dev.off()


