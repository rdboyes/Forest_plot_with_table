library(gridExtra)
library(tidyverse)
library(patchwork)

data <- readxl::read_excel(here::here("figure_data.xlsx"))

# format a copy of the data as character for the tables
tdata <- data %>% mutate_all(as.character)

tdata[is.na(tdata)] <- " "

tdata$`Estimate (95% CI)` <- ifelse(tdata$Placebo == " ", " ", 
                         paste0(tdata$Estimate, 
                         " (", tdata$`CI low`, 
                         " to ", tdata$`CI high`, ")"))

tdata$Subgroup <- ifelse(tdata$Placebo == " ", 
                         tdata$Subgroup,
                         paste0("  ", tdata$Subgroup))

# add a blank row at the top of the graph data
gdata <- add_row(data, .before = 1)
gdata$row_num <- nrow(gdata):1


center <- ggplot(data = gdata, aes(y = row_num, x = Estimate)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(y = row_num, xmin = `CI low`, xmax = `CI high`), height = .35) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed")
  
# custom theme to prevent centering of text

theme_1 <- ttheme_minimal(core = list(fg_params = list(hjust = 0, 
                                                       x = 0.1)),
                          colhead = list(fg_params = list(hjust = 0,
                                                          x = 0.1)))


#combine the two

final <- wrap_elements(tableGrob(tdata[,1:3], theme = theme_1, rows = NULL)) +
  center + 
  wrap_elements(tableGrob(tdata[, 7], theme = theme_1, rows = NULL))

final
