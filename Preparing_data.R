

library(readr)
library(dplyr)

#set directory to source file location
setwd("~/Documents/R/BiOGeOChemistry-ISU/Gale_Mn")


############################################# Importing and merging v2 csv and categoricals #############################
## Because the version 1 database with different encoding and headers caused some downstream data loss

# Step 1: Read the first file (converted to UTF-8 earlier)
data1 <- read_csv("Compiled_Lit_chemistry_v2.csv")

# Step 2: Read the second file (categoricals)
data2 <- read_csv("Compiled_Lit_Chemistry_v2_categoricals.csv")

# Step 3: Merge using dplyr
# remove empty columns and remove additions to column names
merged_data <- left_join(data1, data2, by = "Number") %>%
  select(where(~ !all(is.na(.))))              # Remove empty columns
  

write.csv(merged_data, "Compiled_Lit_chemistry_v2_withcategoricals.csv", row.names = FALSE)
