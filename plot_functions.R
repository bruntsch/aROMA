library(ggplot2)
library(viridis)
library(tidyr)
library(stringr)
library(dplyr)

plot_simple_count <- function(data, column, title_text, x_axis_name, y_axis_name){
  counts <- data %>%
    count(Answer = .data[[column]], name = "Count")
  
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
  
  p <- ggplot(counts, aes(x = Answer, y = Count, fill = Answer)) +
    geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
    scale_fill_viridis_d(option = "viridis", begin = 0.5, end = 0.5, na.value = "grey") +
    coord_flip()+
    geom_text(aes(label = Count), hjust = -0.2, size = 5) +
    labs(x = x_axis_name, y = y_axis_name, title = paste(title_text)) +
    theme_classic() +
    theme(legend.position = "none")
  return(p)
}

plot_nested_count <- function(data, column, title_text, x_axis_name, y_axis_name){
  counts <- data %>%
    # Split answers into lists
    mutate(Answer = strsplit(.data[[column]], ",\\s*")) %>%  # split by comma
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
  
  p <- ggplot(counts, aes(x = Answer,
                          y = Count, 
                          fill = Count)) +
    geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
    scale_fill_viridis_c(option = "viridis", begin=0.5, end=0.5, na.value = "grey") +
    coord_flip()+
    geom_text(aes(label = Count),hjust = -0.2, size = 5) +
    labs(x = x_axis_name, y = y_axis_name, title = paste0(title_text)) +
    theme_classic() +
    theme(legend.position = "none")
  return(p)
}

plot_numbers <- function(data, column, title_text, x_axis_name, y_axis_name){
  data_plot <- data %>%
    mutate(
      var_char = as.character(.data[[column]]),
      var_num = suppressWarnings(as.numeric(var_char)),
      var_num = ifelse(is.na(var_num), 0, var_num)
    )
  
  p <- ggplot(data_plot, aes(x = reorder(ID, year),
                             y = var_num, 
                             fill = var_num)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_viridis_c(option = "viridis", begin =0.5, end=0.5) +
    geom_text(aes(label = .data[[column]], 
                  hjust = ifelse(.data[[column]] == "not reported", 0, -0.1)), 
              size = 3) +
    labs(title = title_text,
         x = x_axis_name,
         y = y_axis_name) +
    theme_classic() + # or minimal
    theme(panel.grid = element_blank(), 
          legend.position = "none")
  return(p)
}