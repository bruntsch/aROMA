# Load libraries
library(ggplot2)
library(viridis)
library(readxl)
library(tidyr)
library(gridExtra)
library(gsheet)
library(stringr)
library(dplyr)
library(cowplot)
library(forcats)

# access the created plotting functions
source("plot_functions.R")

# load metadata from google spreadsheet ####
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 1468738086), stringsAsFactors=FALSE, na = c("", "NA"), check.names = FALSE)

# Adding an ID variable containing author & year ####
data$ID <- NA
for (row in 1:nrow(data)){
  data$ID[row] <- paste0(data$first_author[row]," ",data$year[row])
}

# plotting counts based on simple_pie function ####
# grey literature
greyL_pie <- plot_simple_pie(data, "grey_literature", "Grey Literature Included", "", "Number of MAs")
ggsave("plots/pie_grey-literature.png", plot = greyL_pie, width=5, height=5, dpi=300)

# number of screeners - abstract & title 
# only differentiate between not reported, unclear, 1 and more than 1
for (i in 1:nrow(data)){
  if (! data$n_screeners_AT[i] %in%  c("1","not reported", "unclear")){
    data$screener_AT_group[i] <-  ">1"
  }
  else {
    data$screener_AT_group[i] <-  data$n_screeners_AT[i]
  }
}
screener_AT_pie <- plot_simple_pie(data, "screener_AT_group", "Title & Abstract Screening", "", "Number of Screeners")
ggsave("plots/pie_screener_AT.png", plot = screener_AT_pie, width=5, height=5, dpi=300)
# number of screeners - full-text 
# only differentiate between not reported, unclear, 1 and more than 1
for (i in 1:nrow(data)){
  if (! data$n_screeners_FT[i] %in%  c("1","not reported", "unclear")){
    data$screener_FT_group[i] <-  ">1"
  }
  else {
    data$screener_FT_group[i] <-  data$n_screeners_FT[i]
  }
}
screener_FT_pie <- plot_simple_pie(data, "screener_FT_group", "Full-Text Screening", "", "Number of Screeners")
ggsave("plots/pie_screener_FT.png", plot = screener_FT_pie, width=5, height=5, dpi=300)
# MA model
model_pie <- plot_simple_pie(data, "ma_model", "Applied MA Model", "", "Number of MAs")
ggsave("plots/pie_model.png", plot = model_pie, width=7, height=5, dpi=300)
# preregistration
prereg_pie <- plot_simple_pie(data, "pre_registration", "Preregistered\nMeta-Analyses", "", "Number of MAs")
ggsave("plots/pie_preregistration.png", plot = prereg_pie, width=5, height=5, dpi=300)
# quality assessment 
qa_pie <- plot_simple_pie(data, "qa", "Quality Assessement\nConducted Based on", "", "Number of MAs")
ggsave("plots/pie_qa.png", plot = qa_pie, width=5, height=5, dpi=300)
# outlier & influential analysis 
oa_pie <- plot_simple_pie(data, "outlier_analysis", "Assessment for Outlier\nand Influential Studies", "", "Number of MAs")
ggsave("plots/pie_oa.png", plot = oa_pie, width=5, height=5, dpi=300)

# plotting counts based on nested_count function ####
# used guidelines
guideline_plot <- plot_nested_count(data, "guidelines_search", "Guidelines Used", "", "Number of MAs")
ggsave("plots/guidelines_search.png", plot = guideline_plot, width=7, height=5, dpi=300)
# bias detection methods
bias_plot <- plot_nested_count(data, "publication_bias", "Bias Detection Tools", "", "Number of MAs")
ggsave("plots/bias.png", plot = bias_plot, width=7, height=5, dpi=300)
# open material
open_plot <- plot_nested_count(data, "open_material", "Openly Shared Materials", "", "Number of MAs")
ggsave("plots/open_material.png", plot = open_plot, width=7, height=5, dpi=300)

# plotting numbers - number studies, ES & age, N ####

# number of age and N participants
age_plot <- plot_numbers(data, "age_mean", "Mean Age of Sample", "Study", "mean age")
ggsave("plots/mage.png", plot = age_plot, width=7, height=5, dpi=300)
n_participant_plot <- plot_numbers(data, "n_participants", "Number of Participants", "Study", "Number")
ggsave("plots/n_participants.png", plot = n_participant_plot, width=7, height=7, dpi=300)

data_plot <- data %>%
  mutate(
    n_studies_human_char = as.character(n_studies_human),
    n_studies_human_num = suppressWarnings(as.numeric(n_studies_human_char)),
    n_studies_human_num = ifelse(is.na(n_studies_human_num), 0, n_studies_human_num),
    n_es_human_char = as.character(n_es_human),
    n_es_human_num = suppressWarnings(as.numeric(n_es_human_char)),
    n_es_human_num = ifelse(is.na(n_es_human_num), 0, n_es_human_num),
  ) %>%
  
  pivot_longer(
    cols = c(n_studies_human_num, n_es_human_num, n_studies_human_char, n_es_human_char),
    names_to = c("variable", ".value"),
    names_pattern = "(n_studies_human|n_es_human)_(num|char)")

study_plot <- ggplot(data_plot, aes(x = reorder(ID, n_studies_human), y = num, fill = variable)) +
  geom_col(position = "dodge", width = 1) +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", begin = 0, end = 0.5, labels = c("ES", "Studies"), name = "") +
  geom_text(aes(label = char,
                hjust = ifelse(char == "not reported", 0, -0.1)),
            position = position_dodge(width = 0.9),
            size = 6)+ 
  labs(
    x = "Study",
    y = "Number",
    fill = "Variable",
    title = "Number of Studies and ES"
  ) +
  theme_classic() +
  plot_theme_bar +
  theme(legend.text=element_text(size=20), 
        legend.position = "inside",
        legend.position.inside = c(.8, .45),
        legend.background = element_rect(fill = "transparent", colour = NA))

ggsave("plots/study_es.png", plot = study_plot, width=12, height=11, dpi=300)

# combined plots -> grid ###
grid_plot_study_participant <- plot_grid(study_plot, n_participant_plot, 
                             ncol = 2, nrow = 1, align = "h",
                             labels = "auto")
ggsave("plots/study_participant_orderN.jpg", plot = grid_plot_study_participant, width=28, height=16, dpi=300)

# plot outcomes and phases ####
# outcomes ###
outcome_phys <- c("EKG","FPS","HR","SCR","PD")
outcome_all <- unique(unlist(strsplit(data$ma_outcomes, ", ")))
outcome_rats <- c("arousal", "valence", "risk", "expectancy", "fear", 
                  "mood", "disgust", "un/pleasantness", "anxiety", 
                  "other rating")
#outcome_behav <- c("freezing","avoidance")

# Prepare the data for 'ma_outcomes'
counts <- data %>%
  mutate(Answer = strsplit(.data[["ma_outcomes"]], ",\\s*")) %>%
  unnest(Answer) %>%
  count(Answer, name = "Count") %>%
  mutate(outcome_type = case_when(
    Answer %in% outcome_phys ~ "Physiological",
    Answer %in% outcome_rats ~ "Rating",
    #Answer %in% outcome_behav ~ "Behavior",
    TRUE ~ "Other"
  )) 

# move "other" at the end
counts$outcome_type <- factor(counts$outcome_type,
                              levels = c("Other", "Rating", "Physiological"))

counts <- counts %>%
    group_by(outcome_type) %>%
    mutate(Answer_ordered = fct_reorder2(Answer, outcome_type, Count, .desc = FALSE)) %>%
    ungroup()

# Plot with viridis discrete palette for outcome_type
outcome_plot <- ggplot(counts, aes(x = Answer_ordered, y = Count, fill = outcome_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_d(option = "viridis", begin = 1, end = 0) +
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.2, size = 8) +
  labs(x = "", y = "Number of MAs", 
       title = "Outcome Measures Analyzed",
       fill = "Measurement Type") +
  theme_classic() +
  plot_theme_bar +
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text=element_text(size=20), 
        legend.position = "inside",
        legend.position.inside = c(.8, .45),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  guides(fill = guide_legend(reverse=T))
ggsave("plots/ma_outcomes.png", plot = outcome_plot, width=10, height=7, dpi=300)

# phases ###
# Prepare the data for 'ma_phases'
rof_list <- c("recall", "extinction recall", "spontaneous recovery", 
              "return of fear", "retention","renewal","reinstatement")

phase_data <- data %>%
  mutate(phase_cat = sapply(strsplit(ma_phases, ","), function(words) {
    words <- trimws(words)  # delete spaces
    # words in the list
    if (any(words %in% rof_list)) {
      # Ersetze alle Listen-Wörter durch "ROF", behalte andere
      words <- ifelse(words %in% rof_list, "ROF", words)
      # Wenn mehrere ROFs drin sind -> nur einen behalten
      words <- unique(words)
      # Falls mehrmals ROF vorkommt, nur einen lassen
      if (sum(words == "ROF") > 1) {
        words <- c("ROF", words[words != "ROF"])
      }
    }
    paste(words, collapse = ",")
  }))  

counts <- phase_data %>%
  mutate(Answer = strsplit(.data[["phase_cat"]], ",\\s*")) %>%
  unnest(Answer) %>%
  count(Answer, name = "Count")

#counts$Answer <- factor(counts$Answer, levels = rev(phase_order))

# Plot with viridis discrete palette for outcome_type
phase_plot <- ggplot(counts, aes(x = reorder(Answer,Count, decreasing = FALSE), y = Count, fill = Answer)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_d(option = "viridis", begin = 0.5, end = 0.5) +           # <- key change here
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.2, size = 8) +
  labs(x = "", y = "Number of MAs", 
       title = "Phases Analyzed") +
  theme_classic() +
  plot_theme_bar 
ggsave("plots/ma_phases.png", plot = phase_plot, width=9, height=7, dpi=300)

# plot for moderator ####
data_mod <- data %>%
  mutate(non_sign_mod = moderator_num - sign_moderator_num) %>%
  filter(!is.na(moderator_num) & moderator_num != 0) %>%  # Exclude NA and 0
  pivot_longer(
  cols = c("sign_moderator_num", "non_sign_mod"),
  names_to = "Type",
  values_to = "Value")

mod_plot <- ggplot(data_mod, aes(x = reorder(ID,Value, decreasing = FALSE), y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0, end = 0.5, labels = c("Non-Significant", "Significant")) +
  coord_flip() +
  labs(title = "Number of Tested Moderators", x = "Study", y = "Moderators Tested") +
  theme_classic()+ 
  plot_theme_bar +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=20), 
        legend.position = "inside",
        legend.position.inside = c(.8, .45),
        legend.background = element_rect(fill = "transparent", colour = NA))
# add the total number 
mod_plot <- mod_plot + geom_text(data = data_mod, aes(x = reorder(ID, year), y = moderator_num, label = moderator_num),
                     hjust = -0.2, size = 8)
ggsave("plots/moderators.png", plot = mod_plot, width=12, height=9, dpi=300)

# combine count plots -> grid ####
# Literature Review grid plot 
grid_pie_general <- plot_grid(greyL_pie, screener_AT_pie, screener_FT_pie,
                              ncol = 3, nrow = 1, align = "v",
                              labels = c("b","c","d"))
guideline_plot_fixed <- guideline_plot + theme(aspect.ratio = 5/12)
grid_general <- plot_grid(guideline_plot_fixed, grid_pie_general,
                          ncol = 1, nrow = 2,
                          rel_heights = c(1/3),
                          labels = c("a",""),
                          scale= 0.9)

ggsave("plots/pie_general.jpg", plot = grid_general, width=15, height=10, dpi=300)
# Open science  & Quality grid plot
grid_plot_quality_part1 <- plot_grid(prereg_pie,qa_pie, oa_pie, 
                                     ncol = 3, 
                                     nrow = 1, 
                                     align = "v",
                                     labels = c("a","b","c"))
grid_plot_quality_part2 <- plot_grid(bias_plot,open_plot, 
                                     ncol = 2, 
                                     nrow = 1,
                                     align = "v",
                                     labels = c("d","e"))
grid_plot_quality <- plot_grid(grid_plot_quality_part1, grid_plot_quality_part2, 
                               ncol = 1, 
                               nrow = 2)
ggsave("plots/quality.jpg", plot = grid_plot_quality, width=15, height=10, dpi=300)

grid_ma <- plot_grid(model_pie, phase_plot, outcome_plot, mod_plot,
                     ncol = 2, 
                     nrow = 2,
                     align = "v",
                     rel_heights = c(1,1.25),
                     labels = c("a","b","c","d"))

ggsave("plots/grid_ma.jpg", plot = grid_ma, width=25, height=20, dpi=300)

# missed stats ####
#calculating the proportion of missed articles #
data <- data %>%
 mutate(
   n_studies = as.numeric(n_studies),
   n_studies_miss_stats = as.numeric(n_studies_miss_stats),
   missed_percentage = ifelse(!is.na(n_studies) & !is.na(n_studies_miss_stats) & n_studies != 0,
                              100 * n_studies_miss_stats / n_studies, NA))
data$missed_percentage
