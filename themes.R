#Data Setup Document
#Matt Barger
rm(list = ls())
#Load in Required Libraries
library(tidyverse)
library(worldfootballR)
library(itscalledsoccer)
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(janitor)
library(ggforce)

  ### Load color schemes
black_hues <- c('#f2e5da','#e6d9ce', '#ccc1b7','#b3a9a0', '#999189','#807973','#66605c','#4d4845','#33302e','#1a1817')
blue_hues <- c('#1a8cff','#177ee6','#1470cc','#1262b3','#0f5499','#0d4680','#0a3866','#082a4d')
claret_hues <- c('#ff1a66','#e6175c','#cc1452','#b31247','#990f3d','#800d33','#660a29','#4d081f')
teal_hues <- c('#1aecff','#17d4e6','#14bdcc','#12a5b3','#0f8e99','#0d7680','#0a5e66','#08474d','#052f33')
plot_colors <- c('#990f3d', '#0d7680','#0f5499','#262a33','#ff8833','#00a0dd','#cc0000','#006f9b','#ff7faa','#00994d','#593380',"#ffec1a")
stoplight <- c('#0d7680','#ff8833','#990f3d')


#Load in Fonts
sysfonts::font_add_google(name = 'Inter', family = 'Inter', db_cache = F)
sysfonts::font_add_google(name = 'Fira Mono', family = 'Fira Mono', db_cache = F)
showtext::showtext_auto()

#Load in Custom Theme
theme_matt <- theme_minimal() +
  theme(
    text = element_text(family = "Inter"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(face = "italic", size = 12),
    plot.caption = element_text(hjust = 0, size = 11),
    axis.title = element_text(family = "Fira Mono",hjust = 0, size = 12, face = "bold"),
    axis.text = element_text(family = "Fira Mono", size = 10),
    strip.text = element_text(family = "Fira Mono",  face = "bold", hjust = 0, size = 12),
    strip.background = element_rect(fill = black_hues[2], color = NA),
    panel.grid.major = element_line(color = black_hues[1]),
    panel.grid.minor = element_blank(),
    #panel.ontop = T,
    plot.background = element_rect(fill = "floralwhite", color = "floralwhite")
  )




