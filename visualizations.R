# Libraries
library(ggplot2)
library(viridis)
library(readxl)
library(gsheet)
library(dplyr)

#load metadata from google spreadsheet
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 1468738086), stringsAsFactors=FALSE, na = c("", "NA"), check.names = FALSE)

#### Rearranging the sheet ####
data$ID <- NA
for (row in 1:length(data)){
  data$ID[row] <- paste0(data$"First Author"[row]," ",data$"Year"[row])
}

data$ID

####Ploting####
## preregistration
# prepare data 
prereg_counts <- data %>%
  count(`pre-registration`) %>%
  rename(Answer = `pre-registration`, Count = n)

ggplot(prereg_counts, aes(x = Answer, y = Count, fill = Answer)) +
  geom_bar(stat = "identity", width = 0.7) +     # width for aesthetics
  scale_fill_viridis_d(option = "viridis") +
  geom_text(aes(label = Count), vjust = -0.3, size = 5) +
  labs(x = "", y = "Answer", title = "Preregistration done?") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("plots/preregistration.png", width=5, height=5, dpi=300)

## number of studies
ggplot(data, aes(x = reorder(ID, `Year`), 
                 y = `N studies for MA`, 
                 fill = `N studies for MA`)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  geom_text(aes(label = `N studies for MA`), 
            hjust = -0.1, 
            size = 3) +
  labs(title = "Number of human studies in an MA",
       x = "Meta-Analysis",
       y = "Num of Studies") +
  theme_classic() + # or minimal
  theme(panel.grid = element_blank(), 
        legend.position = "none")

ggsave("plots/Nstudies.png", width=5, height=5, dpi=300)

