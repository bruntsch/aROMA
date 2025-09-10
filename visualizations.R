# Libraries
library(ggplot2)
library(viridis)
library(readxl)
library(tidyr)
library(gridExtra)
library(gsheet)
library(stringr)
library(dplyr)
library(cowplot)

source("plot_functions.R")

# load metadata from google spreadsheet ####
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 1468738086), stringsAsFactors=FALSE, na = c("", "NA"), check.names = FALSE)

# Adding ID variable ####
data$ID <- NA
for (row in 1:nrow(data)){
  data$ID[row] <- paste0(data$first_author[row]," ",data$year[row])
}
# data$ID

# calculating the proportion of missed articles ####
data <- data %>%
  mutate(
    n_studies = as.numeric(n_studies),
    n_studies_miss_stats = as.numeric(n_studies_miss_stats),
    missed_percentage = ifelse(!is.na(n_studies) & !is.na(n_studies_miss_stats) & n_studies != 0,
                               100 * n_studies_miss_stats / n_studies, NA))
data$missed_percentage

# plotting counts - prereg, qa, open data, guidelines ####
# generating prereg plot based function
prereg_plot <- plot_simple_count(data, "pre_registration", "Number of preregistered MAs", "Answer", "Count")
ggsave("plots/preregistration.png", plot = prereg_plot, width=7, height=5, dpi=300)
# generating qa plot based function
qa_plot <- plot_simple_count(data, "qa", "Number of MAs with Quality Assessement", "Answer", "Count")
ggsave("plots/qa.png", plot = qa_plot, width=7, height=5, dpi=300)
# generating grey literature plot
greyL_plot <- plot_simple_count(data, "grey_literature", "Including Grey Literature", "Answer", "Count")
ggsave("plots/grey_literature.png", plot = greyL_plot, width=7, height=5, dpi=300)

open_plot <- plot_nested_count(data, "open_material", "Openly shared materials across all MAs", "Answer", "Count")
ggsave("plots/open_material.png", plot = open_plot, width=7, height=5, dpi=300)
guideline_plot <- plot_nested_count(data, "guidelines_search", "Guidelines used for all MAs", "Answer", "Count")
ggsave("plots/guidelines_search.png", plot = guideline_plot, width=7, height=5, dpi=300)

# combine count plots -> grid ####
grid_plot_os_qa <- plot_grid(guideline_plot,qa_plot,prereg_plot,open_plot, 
          ncol = 2, nrow = 2, align = "vh",
          labels = "auto")
ggsave("plots/os_qa.png", plot = grid_plot_os_qa, width=15, height=7, dpi=300)

# plotting numbers - number studies, ES & age, N####
# number of age and N participants
age_plot <- plot_numbers(data, "age_mean", "Mean Age of Sample", "Study", "mean age")
ggsave("plots/mage.png", plot = age_plot, width=7, height=5, dpi=300)
n_participant_plot <- plot_numbers(data, "n_participants", "Number of Participants", "Study", "Number")
ggsave("plots/n_participants.png", plot = n_participant_plot, width=7, height=5, dpi=300)

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

study_plot <- ggplot(data_plot, aes(x = reorder(ID, year), y = num, fill = variable)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", begin = 0, end = 0.5, labels = c("ES", "Studies"), name = "") +
  geom_text(aes(label = char,
                hjust = ifelse(char == "not reported", 0, -0.1)),
            position = position_dodge(width = 0.9),
            size = 3)+ 
  labs(
    x = "Study",
    y = "Number",
    fill = "Variable",
    title = "Number of Studies and ES"
  ) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .45))

ggsave("plots/study_es.png", plot = study_plot, width=7, height=7, dpi=300)

# combined plots -> grid ####
grid_plot_study_participant <- plot_grid(study_plot, n_participant_plot, age_plot, 
                             ncol = 2, nrow = 2, align = "h",
                             labels = "auto")
ggsave("plots/study_participant.png", plot = grid_plot_study_participant, width=24, height=7, dpi=300)

# outcomes ####
outcome_phys <- c("EKG","startle response","heart rate","skin conductance response","pupil dilation")
outcome_all <- unique(unlist(strsplit(data$ma_outcomes, ", ")))
outcome_rats <- c(outcome_all[str_detect(outcome_all,"rating")])
outcome_behav <- c("freezing","avoidance")

# plot outcomes and phases ####
# Prepare the data for 'ma_outcomes'
counts <- data %>%
  mutate(Answer = strsplit(.data[["ma_outcomes"]], ",\\s*")) %>%
  unnest(Answer) %>%
  count(Answer, name = "Count") %>%
  mutate(outcome_type = case_when(
    Answer %in% outcome_phys ~ "Physiological",
    Answer %in% outcome_rats ~ "Rating",
    Answer %in% outcome_behav ~ "Behavior",
    TRUE ~ "Other"
  )) 

# move "other" at the end
counts$outcome_type <- factor(counts$outcome_type,
                              levels = c("Physiological", "Rating", "Behavior", "Other"))

# Plot with viridis discrete palette for outcome_type
outcome_plot <- ggplot(counts, aes(x = reorder(Answer, Count), y = Count, fill = outcome_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_d(option = "viridis") +           # <- key change here
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.2, size = 5) +
  labs(x = "Measures", y = "Count", 
       title = "Outcome measures analyzed",
       fill = "Measure Type") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .45),)

ggsave("plots/ma_outcomes.png", plot = outcome_plot, width=8, height=5, dpi=300)

# phases ####
# Prepare the data for 'ma_phases'
rof_list <- c("recall", "spontaneous recovery", "return of fear", "retention","renewal","reinstatement")

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

phase_order <- c("habituation", "acquisition", 
                 "extinction", "generalization", "ROF",
                 "reacquisition","post-retrieval extinction", "memory")

counts$Answer <- factor(counts$Answer, levels = rev(phase_order))

# Plot with viridis discrete palette for outcome_type
phase_plot <- ggplot(counts, aes(x = Answer, y = Count, fill = Answer)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_d(option = "viridis", begin = 0.5, end = 0.5) +           # <- key change here
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.2, size = 5) +
  labs(x = "Phases", y = "Count", 
       title = "Phases analyzed") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("plots/ma_phases.png", plot = phase_plot, width=8, height=5, dpi=300)

# combine the two plots 
grid_plot_outcome_phase <- grid.arrange(outcome_plot, phase_plot, 
                                            nrow = 1, 
                                            ncol = 2)
grid_plot_outcome_phase <- plot_grid(outcome_plot, phase_plot, 
                                         ncol = 2, nrow = 1,
                                         labels = "auto")
ggsave("plots/outcome_phase.png", plot = grid_plot_outcome_phase, width=15, height=7, dpi=300)


