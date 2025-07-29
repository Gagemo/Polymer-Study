################################################################################
################################################################################
#########################     Hydrogel Study     ##############################
#########################   Field - Weather Data   ##############################
#########################   University of Florida ##############################
#########################      Gage LaPierre       ##############################
#########################      2022 - 2025         ##############################
################################################################################
################################################################################

######################### Clears Environment & History   ########################
rm(list=ls(all=TRUE))
cat("\014")

#########################       Installs Packages    ##############################
list.of.packages <- c("tidyverse", "vegan", "agricolae", "lubridate", "ggpubr", "ggtext")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################      Loads Packages      ##############################
library(tidyverse)
library(vegan)
library(agricolae)
library(lubridate)
library(ggpubr)
library(ggtext) # Load ggtext

##########################      Read in  Data      ##############################
data = read.csv("Data/Polymer Study - Weather.csv")
data$Date <- mdy(data$Date)

data$Month <- months(as.Date(data$Date))
data$Year <- as.numeric(format(data$Date,'%Y'))

rain =
  ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Precip*25.4), color = "black", size = 1.25) +
  scale_x_date(date_breaks = "months", date_labels = "%b",
               labels = function(x) {
                 month_abbr <- format(x, "%b")
                 # Use <b> tag for bolding. Color is set by element_markdown below.
                 if (month_abbr %in% c("May", "Oct", "Apr")) {
                   return(paste0("<b>", month_abbr, "</b>"))
                 } else {
                   return(month_abbr) # Return plain month abbreviation for others
                 }
               }) +
  theme_classic() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        axis.title.x = element_text( size=20, face="bold", colour = "black"),
        axis.title.y = element_text(size=20, face="bold", colour = "black"),
        axis.text.x=ggtext::element_markdown(angle=90, hjust=1, # Use element_markdown
                                             size=20, color = "black"), # Set default color for all axis text
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Total Precipitation (mm)", title = "")
rain
ggsave("Figures/Field/precip.png",
       width = 10, height = 7)

temp =
  ggplot(data, aes(x = Date)) +
  geom_line(aes(y = (Temp.High-(32))*(5/9)), color = "red", size = 1.25) +
  geom_line(aes(y = (Temp.Ave-(32))*(5/9)), color = "black", size = 1.25) +
  geom_line(aes(y = (Temp.Low-(32))*(5/9)), color = "lightblue", size = 1.25) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  theme_classic() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        axis.title.x = element_text( size=20, face="bold", colour = "black"),
        axis.title.y = element_text(size=20, face="bold", colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.text.x = element_text(size = 20, colour = "black", face = "bold"),
        axis.line.x = element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Temperature C", title = "")
temp

ggsave("Figures/Field/temp.png",
       width = 14, height = 7)

################## Save Figures Above using ggarrange ##########################
ggarrange(temp, rain, ncol = 1, nrow = 2)
ggsave("Figures/Field/rainTemp.png",
       width = 14, height = 12)