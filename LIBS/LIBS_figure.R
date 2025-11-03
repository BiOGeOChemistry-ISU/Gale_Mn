

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(scales)

##################################### Plot LIBS data ######################################

# load terrestrial LIBS data
df <- read_excel("LIBS/Terrestrial_CSV_LIBS_Averages.xlsx")


# Define categories
MnO_cols <- c("LV_FM", "LW_Mass", "LW_Nod")
MnC_cols <- setdiff(names(df), c(MnO_cols, "Wavelength"))


# Define ranges and names
ranges <- list(
  Ba_614 = c(612, 618),
  Ba_455 = c(452, 458),
  Sr_421 = c(419, 425),
  Sr_407 = c(405, 411),
  Zn_255 = c(252, 258)
)

# Create a list to store filtered data
filtered_data <- list()


# Loop through ranges
for (name in names(ranges)) {
  range_vals <- ranges[[name]]
  
  # Filter and reshape
  df_filtered <- df %>%
    filter(Wavelength >= range_vals[1], Wavelength <= range_vals[2]) %>%
    pivot_longer(cols = c(MnO_cols, MnC_cols), names_to = "Variable", values_to = "Value") %>%
    mutate(Category = ifelse(Variable %in% MnO_cols,
                             "Terrestrial Mn (oxyhydr)oxides",
                             "Terrestrial Mn carbonates"))
  
  
  # Store in list
  filtered_data[[name]] <- df_filtered
}


# create theme
theme_black_box <- theme_minimal() +
  theme(
    panel.grid = element_blank(),        # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add black box
    panel.background = element_rect(fill = "white"), # White background
    plot.background = element_rect(fill = "white", color = NA), # White outer background
    text = element_text(family = "sans", size = 8)
  )

#define style for log scale
scale_y_log(
  limits = c(0, 0.0005),
  labels = function(x) {
    sapply(x, function(val) {
      exp <- floor(log10(val))
      base <- val / 10^exp
      bquote(.(base) %*% 10^.(exp))
    })
  }
)


# Plot Ba_614
Ba_614 <- ggplot(filtered_data$Ba_614, aes(x = Wavelength, y = Value, color = Category, group = Variable)) +
  geom_line(size = 1) +
  labs(x = "Wavelength (nm)", y = "Intensity") +
  scale_color_manual(values = c(
    "Terrestrial Mn (oxyhydr)oxides" = "black",
    "Terrestrial Mn carbonates" = "grey"
  )) +
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) +
  geom_vline(xintercept = 614.34, color = "black", linetype = "dashed", size = 1) +
  ylim(0, 0.006)   # <-- sets y-axis from 0 to 0.005
Ba_614


# Plot Ba_455
Ba_455 <- ggplot(filtered_data$Ba_455, aes(x = Wavelength, y = Value, color = Category, group = Variable)) +
  geom_line(size = 1) +
  labs(x = "Wavelength (nm)", y = "Intensity") +
  scale_color_manual(values = c(
    "Terrestrial Mn (oxyhydr)oxides" = "black",
    "Terrestrial Mn carbonates" = "grey"
  )) +
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) +
  geom_vline(xintercept = 455.53, color = "black", linetype = "dashed", size = 1) +
  ylim(0, 0.006)   # <-- sets y-axis from 0 to 0.005
Ba_455

# Plot Sr_407
Sr_407 <- ggplot(filtered_data$Sr_407, aes(x = Wavelength, y = Value, color = Category, group = Variable)) +
  geom_line(size = 1) +
  labs(x = "Wavelength (nm)", y = "Intensity") +
  scale_color_manual(values = c(
    "Terrestrial Mn (oxyhydr)oxides" = "black",
    "Terrestrial Mn carbonates" = "grey"
  )) +
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) +
  geom_vline(xintercept = 407.87, color = "black", linetype = "dashed", size = 1) +
  ylim(0, 0.006)   # <-- sets y-axis from 0 to 0.005
Sr_407

# Plot Sr_421
Sr_421 <- ggplot(filtered_data$Sr_421, aes(x = Wavelength, y = Value, color = Category, group = Variable)) +
  geom_line(size = 1) +
  labs(x = "Wavelength (nm)", y = "Intensity") +
  scale_color_manual(values = c(
    "Terrestrial Mn (oxyhydr)oxides" = "black",
    "Terrestrial Mn carbonates" = "grey"
  )) +
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) +
  geom_vline(xintercept = 421.67, color = "black", linetype = "dashed", size = 1) +
  ylim(0, 0.006)   # <-- sets y-axis from 0 to 0.005
Sr_421

# Plot Zn_255
Zn_255 <- ggplot(filtered_data$Zn_255, aes(x = Wavelength, y = Value, color = Category, group = Variable)) +
  geom_line(size = 1) +
  labs(x = "Wavelength (nm)", y = "Intensity") +
  scale_color_manual(values = c(
    "Terrestrial Mn (oxyhydr)oxides" = "black",
    "Terrestrial Mn carbonates" = "grey"
  )) +
  scale_y_continuous(
    limits = c(0, 0.0005),
    labels = function(x) {
      parse(text = sapply(x, function(val) {
        if (val == 0) {
          "0"
        } else {
          exp <- floor(log10(val))
          base <- signif(val / 10^exp, digits = 2)
          paste0(base, " %*% 10^", exp)
        }
      }))
    }
  ) +
  theme_black_box +
  theme(
    text = element_text(size = 8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.title = element_text(size = 8)
  ) +
  geom_vline(xintercept = 255.87, color = "black", linetype = "dashed", size = 1)
Zn_255

################################## Plot ICP-MS data ##################################

df2 <- read_excel("Compiled_Lit_chemistry_v2_plotting.xlsx")

df2_filtered <- df2 %>%
  filter(Ref == 62, Formation != "", !is.na(Formation))


df2_filtered <- df2_filtered %>%
  mutate(scale = seq(6.1, 6.8, by = 0.1))


df2_filtered <- df2_filtered %>%
  mutate(across(c(Sr, Zn, Ba), as.numeric))

Sr <- 
    ggplot(df2_filtered, aes(x = scale, y = Sr, color = Formation)) +
  geom_point(size = 3) +
  labs(x = NULL,
       y = "Sr (ppm)",
       color = "Formation") +
  scale_color_manual(values = c(
    "Carbonates" = "grey",
    "Freshwater" = "black"
  )) +
  scale_x_continuous(limits = c(6, 7), labels = NULL) +
  scale_y_log10() +
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) 
Sr

Zn <- 
  ggplot(df2_filtered, aes(x = scale, y = Zn, color = Formation)) +
  geom_point(size = 3) +
  labs(x = NULL,
       y = "Zn (ppm)",
       color = "Formation") +
  scale_color_manual(values = c(
    "Carbonates" = "grey",
    "Freshwater" = "black"
  )) +
  scale_x_continuous(limits = c(6,7), labels = NULL) +
  scale_y_log10()+
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) 
Zn

Ba <- 
  ggplot(df2_filtered, aes(x = scale, y = Ba, color = Formation)) +
  geom_point(size = 3) +
  labs(x = NULL,
       y = "Ba (ppm)",
       color = "Formation") +
  scale_color_manual(values = c(
    "Carbonates" = "grey",
    "Freshwater" = "black"
  )) +
  scale_x_continuous(limits = c(6, 7), labels = NULL) +
  scale_y_log10() +
  theme_black_box +
  theme(
    text = element_text(size = 8),          # All text elements
    axis.title = element_text(size = 8),    # Axis titles
    axis.text = element_text(size = 8),     # Axis tick labels
    legend.text = element_text(size = 8),   # Legend text
    legend.title = element_text(size = 8),  # Legend title
    plot.title = element_text(size = 8)     # Plot title
  ) 
Ba

################################ big plot ##########################

# Arrange the plots
plots <- ggarrange(
  Ba, Sr, Zn, Ba_455, Sr_407, Zn_255, Ba_614, Sr_421, 
  labels = c("", "", "", "Ba(II) 455.53 nm", "Sr(II) 407.87 nm", "Zn(II) 255.87 nm",
             "Ba(II) 614.34 nm", "Sr(II) 421.67 nm"),
  ncol = 3, nrow = 3,
  heights = c(0.8, 1.2, 1.2),  # Top row shorter, bottom rows taller
  legend = FALSE,
  font.label = list(size = 8, family = "sans"), 
  label.x = 0.25,  # Center horizontally
  label.y = 1.02  # Slightly above the plot
  
)

  
# Combine plots and legend at bottom-right
big_plot <- ggdraw() +
  draw_plot(plots, 0, 0, 1, 1) +
  draw_plot(legend, 0.7, 0.02, 0.3, 0.15) +  # Adjust position and size
  draw_label("Terrestrial Mn (oxyhdr)oxides", 
             x = 0.75, y = 0.15, hjust = 0, vjust = 0, 
             size = 8, fontfamily = "sans", color = "black") +
  draw_label("Terrestrial Mn carbonates", 
             x = 0.75, y = 0.1, hjust = 0, vjust = 0, 
             size = 8, fontfamily = "sans", color = "grey40")

big_plot

# Save to PDF
ggsave("LIBS.pdf", big_plot, width = 8.5, height = 11, units = "in")

# Save as TIFF
ggsave("LIBS.tiff", big_plot, width = 8.5, height = 11, units = "in", dpi = 300)

# Save as JPEG
ggsave("LIBS.jpeg", big_plot, width = 8.5, height = 11, units = "in", dpi = 300)
