library(ggplot2)
library(viridis)
library(tidyr)
library(stringr)
library(dplyr)

# bar plot where every study has one answer 
plot_simple_pie <- function(data, column, title_text, x_axis_name, y_axis_name){
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
  
  p <- ggplot(counts, aes(x = "", y = Count, fill = Answer)) +
    geom_col(width = 1, color = "black") +
    coord_polar(theta = "y") +
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 5, color = "white") +
    #geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
    #coord_flip()+
    scale_fill_viridis_d(option = "viridis", begin = 0, end = 0.5, na.value = "grey") +
    labs(x = x_axis_name, y = y_axis_name, title = paste(title_text)) +
    theme_void() +
    theme(plot.title = element_text(size=15, hjust = 0),
          legend.position = "bottom", 
          legend.title = element_blank(), 
          legend.text=element_text(size=10))
  return(p)
}

# bar plot where every study has one answer 
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
    coord_flip()+
    scale_fill_viridis_d(option = "viridis", begin = 0.5, end = 0.5, na.value = "grey") +
    geom_text(aes(label = Count), hjust = -0.2, size = 5) +
    labs(x = x_axis_name, y = y_axis_name, title = paste(title_text)) +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(size=15, hjust = 0),
          plot.title = element_text(hjust = 0),          # make the background transparent
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  return(p)
}

# bar plot where every study has several answers 
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
    coord_flip()+
    scale_fill_viridis_c(option = "viridis", begin=0.5, end=0.5, na.value = "grey") +
    geom_text(aes(label = Count),hjust = -0.2, size = 5) +
    labs(x = x_axis_name, y = y_axis_name, title = paste0(title_text)) +
    theme_classic() +
    theme(legend.position = "none", 
          plot.title = element_text(size=15, hjust = 0), 
          # make the background transparent 
          panel.background = element_rect(fill = "transparent", colour = NA), 
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  return(p)
}

# bar plot with plain numbers as answers
plot_numbers <- function(data, column, title_text, x_axis_name, y_axis_name){
  data_plot <- data %>%
    mutate(
      var_char = as.character(.data[[column]]),
      var_num = suppressWarnings(as.numeric(var_char)),
      var_num = ifelse(is.na(var_num), 0, var_num)
    )
  
  p <- ggplot(data_plot, aes(x = reorder(ID, var_num),
                             y = var_num, 
                             fill = var_num)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_viridis_c(option = "viridis", begin =0.5, end=0.5) +
    geom_text(aes(label = .data[[column]], 
                  hjust = ifelse(is.character(.data[[column]]), 0, -0.1)), 
              size = 3) +
    labs(title = title_text,
         x = x_axis_name,
         y = y_axis_name) +
    theme_classic() + # or minimal
    theme(panel.grid = element_blank(), 
          legend.position = "none", 
          plot.title = element_text(size = 15, hjust = 0),
          # make the background transparent
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  return(p)
}