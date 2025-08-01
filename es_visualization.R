#### effect size plotting ####
library(forestploter)
library(ggplot2)
library(viridis)
library(gsheet)

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

### forestplot
library("forestplot")

for (foc in unique(es_data$focus)){
 
  plot_data <- subset(es_data, focus == foc)
  
  tabletext <- rbind(
    c("ID", "Phase", "Outcome", "DV"),  # Header row
    cbind(
      plot_data$ID,
      plot_data$ma_phase,
      plot_data$ma_outcome,
      plot_data$ma_dv
    )
  )
  
  mean  <- c(NA, plot_data$ma_es_value)
  lower <- c(NA, plot_data$ma_ci_low)
  upper <- c(NA, plot_data$ma_ci_high)
  
  filename <- paste0("plots/", gsub("[^[:alnum:]_]", "_", foc), "_forestplot.png")
  png(filename, width = 4800, height = 2400, res = 150)
  
  forestplot(
    labeltext = tabletext,
    mean = mean,
    lower = lower,
    upper = upper,
    xlab = "Effect Size",
    title = paste("Forest plot for focus:", foc),
    zero = 0, # line at 0
    boxsize = 0.2, # adjust as needed
    lineheight = unit(1, "cm"),
    col = fpColors(box="royalblue", lines="darkblue", summary="royalblue")
  )
  dev.off()
}

