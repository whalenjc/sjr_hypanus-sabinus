#### readme ####

# Created by: John Whalen
# Last Updated by: John Whalen
# Last Updated: 6/26/24

# ## ID
# SJR14: 2014
# SJR15: 2015
# SJR16: 2016
# AEB: 2002-2005
# 
# ## Maturity
# Mature: 1
# Immature: 0
# 
# ## Sex
# Male: 1
# Female: 0
# 
# ## Site
# GA: 1
# SMR: 2
# NR: 3
# LSJR: 4
# LG: 5
# LM: 6
# LJ: 7


#### INITIALIZE ####

# for John
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### PACKAGES ####
packages_used <- 
  c("rfishbase",
    "ggforce",
    "vegan",
    "stats",
    "ggrepel",
    "ggfortify",
    "dplyr",
    "lubridate",
    "readxl",
    "ggplot2")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

#### IMPORT DATA ####

# Uses the relative path from the sjr_hypanus-sabinus/scripts/ directory where this R file is located. 
sjr<- read_excel("../data/SJR_Sabina_All_Data.xlsx")
#! this data file does not contain weight. Find that file to calculate 

# Replace numbers with site IDs
site_map <- setNames(c("GA", "MR", "NR", "JR", "LG", "LM", "LJ"), 1:7)
sjr <- sjr %>%
  mutate(Site = factor(Site, levels = 1:7, labels = c("GA", "MR", "NR", "JR", "LG", "LM", "LJ")))

# Extract year from Date column
sjr <- sjr %>%
  mutate(Year = if_else(is.na(Date), 2002, year(Date)))

# Create site_id column combining site code and year
sjr <- sjr %>%
  mutate(site_id = paste(Site, Year, sep = "-"))

# View the updated dataset
head(sjr)


# Continue from the previous transformation
sjr <- sjr %>%
  mutate(site_id_new_old = case_when(
    Year == 2002 ~ paste(Site, "old", sep = "_"),
    Year >= 2014 & Year <= 2016 ~ paste(Site, "new", sep = "_"),
    TRUE ~ paste(Site, "other", sep = "_")  # You can specify what to do with other years or leave it out
  ))

# View the updated dataset
head(sjr)



#### visualize ####

### PCA
# First, let's create a clean dataset including the site_id_new_old for PCA
sjr_clean <- sjr %>%
  select(CYP, GST, UGT, NPH, PYR, BaP, LPO, site_id_new_old) %>%
  na.omit()  # This now also omits any rows with NA in 'site_id_new_old'

# Now perform PCA on just the biomarker data
pca_result <- prcomp(sjr_clean[,1:7], scale. = TRUE)  # Select only the biomarker columns, now that site_id_new_old is column 8

# Prepare a data frame for plotting, using the PCA scores and site_id_new_old which have the same number of rows now
pca_data <- data.frame(Score1 = pca_result$x[,1], Score2 = pca_result$x[,2], site_id_new_old = sjr_clean$site_id_new_old)

# Plotting the PCA results
ggplot(pca_data, aes(x = Score1, y = Score2, color = site_id_new_old)) +
  geom_point(alpha = 0.8, size = 3) +
  labs(title = "PCA of Biomarkers", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Adjust color palette as needed

# Display the plot
print(ggplot_object)

# Assuming all previous steps and data preparations are correct
# Adjusted plotting code
ggplot_object <- ggplot(pca_data, aes(x = Score1, y = Score2, color = site_id_new_old)) +
  geom_point(alpha = 0.8, size = 3) +
  stat_ellipse(type = "t", linetype = 2, level = 0.95) +  # Draws ellipses
  labs(title = "PCA of Biomarkers", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Adjust color palette as needed

# Maximum scores for proper scaling
max_score1 <- max(abs(pca_data$Score1))
max_score2 <- max(abs(pca_data$Score2))


# Add eigenvectors as arrows
for (i in 1:nrow(loadings)) {
  ggplot_object <- ggplot_object +
    geom_segment(aes(x = 0, y = 0), 
                 xend = loadings[i, "PC1"] * max_score1, 
                 yend = loadings[i, "PC2"] * max_score2, 
                 arrow = arrow(length = unit(0.5, "cm")), 
                 color = "black", alpha = 0.75) +
    geom_text_repel(aes(x = loadings[i, "PC1"] * max_score1 * 1.1, 
                        y = loadings[i, "PC2"] * max_score2 * 1.1, 
                        label = rownames(loadings)[i]),
                    size = 4, color = "black", nudge_x = 0.05, nudge_y = 0.05)
}

# Display the plot
print(ggplot_object)


### MDS & nMDS
# Assuming mds_df and nmds_df are already created with 'Group' column
# Plot MDS with ellipses
mds_plot <- ggplot(mds_df, aes(x = X1, y = X2, color = Group)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_mark_ellipse(aes(color = Group), show.legend = FALSE, alpha = 0.2) +
  ggtitle("MDS Plot with Ellipses") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Adjust color palette as needed

# Plot nMDS with ellipses
nmds_plot <- ggplot(nmds_df, aes(x = X1, y = X2, color = Group)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_mark_ellipse(aes(color = Group), show.legend = FALSE, alpha = 0.2) +
  ggtitle("nMDS Plot with Ellipses") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Adjust color palette as needed

# Display the plots
print(mds_plot)
print(nmds_plot)
