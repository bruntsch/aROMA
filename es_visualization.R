#### effect size plotting ####
library(ggplot2)
library(viridis)
library(gsheet)
library(forestplot)
library(patchwork)
library(stringr)
library(dplyr)
library(gridExtra)
library(grid)


#load metadata from google spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
es_data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 530932371), stringsAsFactors=FALSE, na = c("", "NA", "not reported"), check.names = FALSE)

### Adding ID variable
es_data$ID <- NA
for (row in 1:nrow(es_data)){
  es_data$ID[row] <- paste0(es_data$first_author[row]," ",es_data$year[row])
}

es_data$ma_es_value <- as.numeric(es_data$ma_es_value)
es_data$ma_ci_low    <- as.numeric(es_data$ma_ci_low)
es_data$ma_ci_high    <- as.numeric(es_data$ma_ci_high)

# forestplots ####
# Sample function creating "forest plot"-style plot with ggplot
# chose between "manipulations", "Paradigm specifications", 
# "patient vs. control", "Correlations with third variables"
focus_to_plot <- "patient vs. control"
plot_data <- filter(es_data, focus == focus_to_plot)

# Basic ggplot for forest-like plot
p <- ggplot(plot_data, aes(y = reorder(ID, ma_es_value), x = ma_es_value)) +
  geom_point(color = "#22A884FF", size=2) +
  geom_errorbarh(aes(xmin = ma_ci_low, xmax = ma_ci_high), height = 0.3, color = "#14614D") +
  facet_grid(ma_phase ~ ma_dv, scales = "free_y", space = "free_y") +
  labs(x = "Effect Size", y = "ID",
       title = paste("Forest plots for focus:", focus_to_plot)) +
  theme_classic() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    panel.spacing = unit(1, "lines")
  )

# Optionally add number of studies/effects as text outside plot panels with patchwork, 

print(p)
ggsave(filename = paste0("plots/fp_grid_",gsub("[^[:alnum:]_]", "_", focus_to_plot),".png"), plot = p, width = 12, height = 10, dpi = 300)

# reducing to CS+/CS-/CS diff and acquisition/extinction ####
# chose between "manipulations", "Paradigm specifications", 
# "patient vs. control", "Correlations with third variables"

focus_to_plot <- "manipulations"
plot_data <- filter(es_data, focus == focus_to_plot)
# Define categories for coloring
outcome_phys <- c("EKG","startle","HR","SCR","PD")
outcome_rats <- plot_data$ma_outcome[str_detect(plot_data$ma_outcome, "rating")]
outcome_behav <- c("freezing", "avoidance")

colors <- c("Physiological" = "#440154FF", "Ratings" = "#2A788EFF", 
            "Behavioral" = "#22A884FF", "Multiple outcomes" = "#7AD151FF", "Other" = "#FDE725FF")

data_filtered <- plot_data %>%
  filter(ma_dv %in% c("CS+", "CS-", "CS discrimination"),
         ma_phase %in% c("acquisition", "extinction")) %>%
  mutate(outcome_type = case_when(
    str_detect(ma_outcome, ",") ~ "Multiple outcomes",  # catch commas first
    ma_outcome %in% outcome_phys ~ "Physiological",
    ma_outcome %in% outcome_rats ~ "Ratings",
    ma_outcome %in% outcome_behav ~ "Behavioral",
    TRUE ~ "Other"
  ))

#filter for table
data_filtered <- data_filtered %>%
  mutate(y_row = interaction(ID, ma_outcome)) # creates unique y for plotting

p <- ggplot(data_filtered, aes(x = ma_es_value, y = y_row, color = outcome_type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ma_ci_low, xmax = ma_ci_high), height = 0.2) +
  facet_grid(ma_phase ~ ma_dv, scales = "free_y", space = "free") +
  scale_color_manual(values = colors) +
  labs(x = "Effect Size", y = "Study ID", color = "Outcome Type") +
  theme_classic() +
  theme(strip.background = element_rect(fill = "gray90"),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom")

p + scale_y_discrete(
  labels = setNames(data_filtered$ID, data_filtered$y_row))

ggsave(filename = paste0("plots/fp_grid_",gsub("[^[:alnum:]_]", "_", focus_to_plot),".png"), plot = p, width = 12, height = 10, dpi = 300)

# Assuming your plot is stored in p
# Create info table
info_table <- data_filtered %>%
  select(ID, ma_outcome, ma_additional, ma_n, ma_k) # select desired columns

table_grob <- tableGrob(info_table, rows = NULL, theme = ttheme_minimal())

# Combine plot and table
grid.arrange(p, table_grob, ncol = 2, widths = c(3, 1))  # side-by-side
