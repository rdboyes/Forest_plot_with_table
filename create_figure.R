# packages needed

library(gridExtra)
library(tidyverse)
library(patchwork)

# this table uses Fira Sans font - this will need to be installed 
# https://fonts.google.com/specimen/Fira+Sans

library(extrafont)
loadfonts(device = "win")

windowsFonts("Fira Sans" = windowsFont("Fira Sans"))

################ read data scraped from original table #########################

data <- readxl::read_excel(here::here("figure_data.xlsx"))

######## format a copy of the data as character for the tables #################

# format the numbers with either no decimals (left) or one decimal (right)
tdata <- data %>% mutate_at(4:6, ~sprintf(., fmt = '%#.1f')) %>% 
  mutate_at(2:3, as.character)

# hide NA values using a space
tdata[is.na(tdata)] <- " "

# pretty formatting for confidence intervals to match the table
tdata$`Estimate (95% CI)` <- ifelse(tdata$Placebo == " ", " ", 
                         paste0(tdata$Estimate, 
                         " (", tdata$`CI low`, 
                         " to ", tdata$`CI high`, ")"))

# indent the subgroup if there is a number in the placebo column
tdata$Subgroup <- ifelse(tdata$Placebo == " ", 
                         tdata$Subgroup,
                         paste0("   ", tdata$Subgroup))

# remove indent of the first row
tdata$Subgroup[1] <- "All Patients"

# insert a blank column so we can put the ggplot object on top
tdata$` ` <- "                                                           "

# correctly order columns
tdata_print <- select(tdata, 
                      Subgroup, 
                      Inclisiran, 
                      Placebo, 
                      ` `, 
                      `Estimate (95% CI)`)

#################### add row numbers for graph data ############################

gdata <- data
gdata$row_num <- (nrow(gdata) - 1):0

### make some small data frames to help place the arrows and sub-axis labels ###

# this df has the text labels
xlab_df <- data.frame(text = c("Inclisiran Better", "Placebo Better"),
                      x = c(-55, 10),
                      y = c(0, 0))

# this df has the arrows
arrow_df <- data.frame(id = c(1,2), 
                       xstart = c(-13, -9),
                       xend = c(-95, 12),
                       y = c(1, 1)) 

########## the main figure - this will be overlaid on the table ################

center <- ggplot(data = gdata, aes(y = row_num, x = Estimate)) +
  geom_point(size = 3.25) + # the point estimates, with big dots
  geom_errorbarh(aes(y = row_num, 
                     xmin = `CI low`, 
                     xmax = `CI high`), 
                     height = .25) + # the CIs, with short ends
  theme_classic() + # base theme 
  scale_y_continuous(expand = c(0,0), limits = c(-.65, 30.7)) + # remove padding
  theme(axis.title.y = element_blank(), # remove axis, make bg transparent
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.length.x = unit(.1, "in"),
        text = element_text(family = "Fira Sans", size = 14),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")) +
  geom_vline(xintercept = 0, linetype = "dashed") + # add the null line
  scale_x_continuous(breaks = c(-100, -75, -50, -25, 0, 25), 
                     limits = c(-100, 25),
                     labels = scales::number_format(accuracy = 0.1), 
                     expand = c(0,0)) +
  xlab("")

############## the ggplot object for the sub-axis labels #######################

arrows <- ggplot() + 
  geom_segment(data = arrow_df, aes(x = xstart, xend = xend, y = y, yend = y),
      arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "npc"))) +
  geom_text(data = xlab_df, aes(x = x, y = y, label = text),
      family = "Fira Sans", size = 4) +
  scale_y_continuous(expand = c(0,0), limits = c(-3, 7)) +
  scale_x_continuous(expand = c(0,0), limits = c(-100, 70)) + 
  theme(panel.background = element_rect(fill = "transparent"), 
      plot.background = element_rect(fill = "transparent", color = NA), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      panel.border = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank())

###### custom theme to prevent centering of text, change font, add bands #######

t1 <- ttheme_minimal(core=list(
  fg_params = list(hjust = 0, x = 0.05, fontfamily = "Fira Sans"),
  bg_params = list(fill=c(rep(c("#eff3f2", "white"), length.out=4)))
  ),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, 
                      fontfamily = "Fira Sans"),
                 bg_params = list(fill = "white"))
  )


# defining the layout
# we want the table to take up the whole space, then overlay the ggplot on top
# in the right spot
# under both object, adds the arrows and labels

layout <- c(area(t = 1, b = 20, l = 1, r = 15),
            area(t = 1, b = 20, l = 8, r = 11), 
            area(t = 20, b = 21, l = 8, r = 13))


#combine the pieces

final <- wrap_elements(tableGrob(tdata_print, theme = t1, rows = NULL)) +
  center + 
  arrows +
  plot_layout(design = layout)

# save the table as a png

ggsave(dpi = 600, height = 10.2, 
       width = 11, units = "in", 
       filename = "final_plot.png")


