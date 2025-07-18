# Libraries
library(ggplot2)
library(viridis)
library(readxl)
library(gsheet)
library(dplyr)
library(tidyryr)

#load metadata from google spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 1468738086), stringsAsFactors=FALSE, na = c("", "NA", "not reported"), check.names = FALSE)

#### Rearranging the sheet ####
data$ID <- NA
for (row in 1:length(data)){
  data$ID[row] <- paste0(data$first_author[row]," ",data$year[row])
}

data$ID

####Ploting####
## plot simple answers
# preregistration
plot_counts <- list()
for (var in c("pre_registration","qa")){
    counts <- data %>%
      count(Answer = .data[[var]], name = "Count")
    
    plot_counts[[var]] <- ggplot(counts, aes(x = Answer, y = Count, fill = Answer)) +
      geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
      scale_fill_viridis_d(option = "viridis", na.value = "grey") +
      geom_text(aes(label = Count), vjust = -0.3, size = 5) +
      labs(x = "Answers", y = "Count", title = paste0(var," done?")) +
      theme_classic() +
      theme(legend.position = "none")
      # Flip the plot if necessary
    if (var == "qa") {
      plot_counts[[var]] <- plot_counts[[var]] + coord_flip()
    }
    print(plot_counts[[var]])
  }
ggsave("plots/preregistration.png", plot = plot_counts[["pre_registration"]], width=5, height=5, dpi=300)
ggsave("plots/qa.png", plot = plot_counts[["qa"]], width=5, height=5, dpi=300)

## plot single entries in a list of entries 
plot_list_counts <- list()
for (var in c("open_material","guidelines_search","ma_outcomes")){
  counts <- data %>%
    # Split answers into lists
    mutate(Answer = strsplit(.data[[var]], ",\\s*")) %>%  # split by comma
    unnest(Answer) %>%    # unnest so each entry is one row
    count(Answer, name = "Count")   # count occurrences for each answer
  
  plot_list_counts[[var]] <- ggplot(counts, aes(x = reorder(Answer, Count), y = Count, fill = Count)) +
    geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
    scale_fill_viridis_c(option = "viridis", na.value = "grey") +
    coord_flip()+
    geom_text(aes(label = Count),hjust = -0.2, size = 5) +
    labs(x = "Answers", y = "Count", title = paste(var,"(N =28)")) +
    theme_classic() +
    theme(legend.position = "none")
  print(plot_list_counts[[var]])
}
ggsave("plots/open_material.png", plot = plot_list_counts[["open_material"]], width=7, height=5, dpi=300)
ggsave("plots/guidelines_search.png", plot = plot_list_counts[["guidelines_search"]], width=7, height=5, dpi=300)
ggsave("plots/ma_outcomes.png", plot = plot_list_counts[["ma_outcomes"]], width=7, height=5, dpi=300)

##plot the actual numbers 
# number of studies
plot_numbers <- list()
for (var in c("n_studies_human","n_es_human","age_mean","n_participants")){
  plot_numbers[[var]] <- ggplot(data, aes(x = reorder(ID, year), 
                   y = .data[[var]], 
                   fill = .data[[var]])) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_viridis_c(option = "viridis") +
    geom_text(aes(label = .data[[var]]), 
              hjust = -0.1, 
              size = 3) +
    labs(title = var,
         x = "Meta-Analysis",
         y = "Number") +
    theme_classic() + # or minimal
    theme(panel.grid = element_blank(), 
          legend.position = "none")
  print(plot_numbers[[var]])
  }
ggsave("plots/n_studies_human.png", plot = plot_numbers[["n_studies_human"]], width=7, height=5, dpi=300)
ggsave("plots/n_es_human.png", plot = plot_numbers[["n_es_human"]], width=7, height=5, dpi=300)
ggsave("plots/n_mage.png", plot = plot_numbers[["age_mean"]], width=7, height=5, dpi=300)
ggsave("plots/n_participants.png", plot = plot_numbers[["n_participants"]], width=7, height=5, dpi=300)

## specific cases
# operationalization_dv, exclusion reason, separate_ma, 

