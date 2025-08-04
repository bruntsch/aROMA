# Libraries
library(ggplot2)
library(viridis)
library(readxl)
library(dplyr)
library(tidyr)
library(gridExtra)
library(gsheet)
library(stringr)

# load metadata from google spreadsheet ####
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 1468738086), stringsAsFactors=FALSE, na = c("", "NA"), check.names = FALSE)

### Adding ID variable ###
data$ID <- NA
for (row in 1:nrow(data)){
  data$ID[row] <- paste0(data$first_author[row]," ",data$year[row])
}

data$ID

# plot simple answers - QA, pre-reg, open data, guidelines ####
plot_counts <- list()
for (var in c("pre_registration","qa")){
    counts <- data %>%
      count(Answer = .data[[var]], name = "Count")
    
    #sort by count but set no/not reported last 
    special_vals <- c("not reported","no")
    existing_specials <- intersect(special_vals, counts$Answer)
    
    # Order other answers by descending count
    ordered_others <- counts$Answer[!counts$Answer %in% existing_specials]
    ordered_others <- ordered_others[order(counts$Count[!counts$Answer %in% existing_specials], decreasing = FALSE)]
    
    # Combine order: others first by count, then specials last
    final_order <- c(existing_specials, ordered_others)
    
    # Set factor levels in that order
    counts$Answer <- factor(counts$Answer, levels = final_order)
    
    if (var == "pre_registration"){
      title_text <- "Preregistered MA"
    }
    else {
      title_text <- "Quality Assessment of the MA"
    }
    
    plot_counts[[var]] <- ggplot(counts, aes(x = Answer, y = Count, fill = Answer)) +
      geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
      scale_fill_viridis_d(option = "viridis", begin = 0.5, end = 0.5, na.value = "grey") +
      coord_flip()+
      geom_text(aes(label = Count), vjust = -0.3, size = 5) +
      labs(x = "Answers", y = "Count", title = paste0(title_text)) +
      theme_classic() +
      theme(legend.position = "none")
    print(plot_counts[[var]])
}

ggsave("plots/preregistration.png", plot = plot_counts[["pre_registration"]], width=5, height=5, dpi=300)
ggsave("plots/qa.png", plot = plot_counts[["qa"]], width=5, height=5, dpi=300)

plot_list_counts <- list()
for (var in c("open_material","guidelines_search")){
  counts <- data %>%
    # Split answers into lists
    mutate(Answer = strsplit(.data[[var]], ",\\s*")) %>%  # split by comma
    unnest(Answer) %>%    # unnest so each entry is one row
    count(Answer, name = "Count")   # count occurrences for each answer
  
  #sort by count but set no/not reported last 
  special_vals <- c("not reported", "no")
  existing_specials <- intersect(special_vals, counts$Answer)
  
  # Order other answers by descending count
  ordered_others <- counts$Answer[!counts$Answer %in% existing_specials]
  ordered_others <- ordered_others[order(counts$Count[!counts$Answer %in% existing_specials], decreasing = FALSE)]
  
  # Combine order: others first by count, then specials last
  final_order <- c(existing_specials, ordered_others)
  
  # Set factor levels in that order
  counts$Answer <- factor(counts$Answer, levels = final_order)
  
  if (var == "open_material"){
    title_text <- "Publicibly available Materials"
  }
  else {
    title_text <- "Guideline used for the MA"
  }
    
  plot_list_counts[[var]] <- ggplot(counts, 
                                    aes(x = Answer, 
                                        y = Count, 
                                        fill = Count)) +
    geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
    scale_fill_viridis_c(option = "viridis", begin=0.5, end=0.5, na.value = "grey") +
    coord_flip()+
    geom_text(aes(label = Count),hjust = -0.2, size = 5) +
    labs(x = "Answers", y = "Count", title = paste(title_text," (N =28)")) +
    theme_classic() +
    theme(legend.position = "none")
  print(plot_list_counts[[var]])
}

ggsave("plots/open_material.png", plot = plot_list_counts[["open_material"]], width=7, height=5, dpi=300)
ggsave("plots/guidelines_search.png", plot = plot_list_counts[["guidelines_search"]], width=7, height=5, dpi=300)

# combined plots -> grid ####
grid_plot_os_qa <- grid.arrange(plot_list_counts[["guidelines_search"]], plot_counts[["qa"]], plot_counts[["pre_registration"]], plot_list_counts[["open_material"]],
                                widths=c(2,3), 
                                nrow = 2, 
                                ncol = 2)
ggsave("plots/os_qa.png", plot = grid_plot_os_qa, width=21, height=10, dpi=300)

# age, N ####
# number of studies
plot_numbers <- list()
for (var in c("age_mean","n_participants")){
  
  data_plot <- data %>%
    mutate(
      var_char = as.character(.data[[var]]),
      var_num = suppressWarnings(as.numeric(var_char)),
      var_num = ifelse(is.na(var_num), 0, var_num)
    )
  
  plot_numbers[[var]] <- ggplot(data_plot, aes(x = reorder(ID, year),
                                               y = var_num, 
                                               fill = var_num)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_viridis_c(option = "viridis", begin =0.5, end=0.5) +
    geom_text(aes(label = .data[[var]]), 
              hjust = -0.1, 
              size = 3) +
    labs(title = var,
         x = "Study",
         y = "Number") +
    theme_classic() + # or minimal
    theme(panel.grid = element_blank(), 
          legend.position = "none")
  print(plot_numbers[[var]])
}

ggsave("plots/n_participants.png", plot = plot_numbers[["n_participants"]], width=7, height=5, dpi=300)

#ggsave("plots/n_mage.png", plot = plot_numbers[["age_mean"]], width=7, height=5, dpi=300)
#grid_plot <- grid.arrange(plot_numbers[["n_participants"]], plot_numbers[["age_mean"]], nrow = 1)
#ggsave("plots/summary_N_age.png", plot = grid_plot, width=21, height=5, dpi=300)

# outcomes ####
outcome_phys <- c("EKG","startle","HR","SCR","PD")
outcome_rats <- c(counts$Answer[str_detect(counts$Answer,"rating")])
outcome_behav <- c("freezing","avoidance")

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
       title = "Outcome measures analyzed (N = 28)",
       fill = "Measure Type") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .45),)

ggsave("plots/ma_outcomes.png", plot = outcome_plot, width=8, height=5, dpi=300)

# number studies & ES ####
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
    
sum_num_plot <- ggplot(data_plot, aes(x = reorder(ID, year), y = num, fill = variable)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", begin = 0, end = 0.5) +
  geom_text(aes(label = char), position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  labs(
    x = "Study",
    y = "Count",
    fill = "Variable",
    title = "Number of Studies and ES for each MA"
  ) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .45))

ggsave("plots/summary_n_k.png", plot = sum_num_plot, width=7, height=7, dpi=300)




