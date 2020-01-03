# packages
library(tidyverse)
library(gganimate)
library(RColorBrewer)

# data
load("data/2_processed/causa_muerte.Rdata")
# logo 
img <- png::readPNG("resources/logo.png")
img <- grid::rasterGrob(img, interpolate = T)
# Paleta de colores
num_color <- 20
mycolors <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(num_color)

causa_muerte %>% 
  group_by(años) %>% 
  arrange(-años, -muertes) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank <= 10) %>% 
  mutate(Value_rel = muertes/muertes[rank == 1],
         Value_lbl = paste0(" ", muertes)) %>%
  group_by(causa1) %>%  
  ggplot(aes(rank, group = causa1, fill = causa1))+
  geom_tile(aes(y = muertes/2,
                height = muertes,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(causa1, " ")), vjust = 0.2, hjust = 1, size = 5) + # causas label
  geom_text(aes(y = muertes,label = Value_lbl, hjust = 0),size = 8 ) +  #muertes label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .1, color = "gray55" ), # lines
        panel.grid.minor.x = element_line(size = .1, color = "gray25" ), # lines
        plot.title = element_text(size = 28, face = "bold", colour = "black", hjust = 2.4), # title
        plot.subtitle = element_text(size = 25, hjust = 0.08, face = "italic", color = "lightskyblue4"), # año label
        plot.caption = element_text(size = 18, face = "bold", color = "white", hjust = 1.15), # caption
        plot.background = element_rect(fill = "gray75"), # background
        plot.margin = margin(1,3, 1, 16, "cm")) + # margenes
  scale_fill_manual(values = mycolors) + # palette 
  transition_states(años, transition_length = 4, state_length = 1) +
  ease_aes('sine-in-out') +
  labs(title = 'Principales causas de muerte en Ecuador',
       subtitle = ". . . . . . . . .\nAño: {closest_state}", 
       caption = "Estadística para No Estadísticos") +
  annotation_custom(img, xmin = -20, xmax = 7, ymin = 15, ymax = Inf) -> anim

  animate(anim, nframes = 900, fps = 30, width = 1000, height = 600, # nframes = 1100
        renderer = gifski_renderer("figures/causas_muerte.gif"))
