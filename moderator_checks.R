# load metadata from google spreadsheet ####
url <- "https://docs.google.com/spreadsheets/d/1UtuHWBGqRnDnIB22Qs8lBElyr8jwv6UPu9rxcffiqLA/edit?usp=sharing"
data <- read.csv(text=gsheet2text(url, format='csv', sheetid = 1468738086), stringsAsFactors=FALSE, na = c("", "NA"), check.names = FALSE)

counts_mod <- data %>%
  # Split answers into lists
  mutate(Answer = strsplit(.data[["moderator_type"]], "\\s*[,]\\s*"))%>%  # split by comma
  unnest(Answer) %>%    # unnest so each entry is one row
  count(Answer, name = "Count")
write.csv(counts_mod, file = "moderator_types.csv")

data$moderator_num_check <- str_count(data$moderator_type, ",") + 1

data$moderator_num
data$moderator_num_check
