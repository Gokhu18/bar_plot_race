# packages
library(tidyverse)
library(gganimate)

# data
load("data/2_processed/causa_muerte.Rdata")

causa_muerte %>% 
  group_by(años) %>% 
  arrange(-años, -muertes) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank <= 10) %>% 
  mutate(Value_rel = muertes/muertes[rank == 1],
         Value_lbl = paste0(" ", muertes)) %>%
  group_by(causa1) %>%  
  ggplot(aes(rank, group = causa1))+
  geom_tile(aes(y = muertes/2,
                height = muertes,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(causa1, " ")), vjust = 0.2, hjust = 1, size = 5) + # causas label
  geom_text(aes(y = muertes,label = Value_lbl, hjust=0),size = 8 ) +  #muertes label
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
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title = element_text(size = 25, face = "bold", colour = "black", hjust = 1.4, vjust = -5), # title
        plot.subtitle = element_text(size = 45, hjust = .9, face = "italic", color = "red", vjust = -30), # año label
        plot.caption = element_text(size = 18, face = "bold", color = "gray", hjust = 1, vjust = -1), # caption
        plot.background = element_blank(),
        plot.margin = margin(1,3, 1, 16, "cm")) +
  transition_states(años, transition_length = 4, state_length = 1) +
  ease_aes('sine-in-out') +
  labs(title = 'Número de muertes en Ecuador y sus respectivas causas',
       subtitle = "Año\n{closest_state}", 
       caption = "Estadística para No Estadísticos") -> anim

  animate(anim, nframes = 1100, fps = 30, width = 1000, height = 600, 
        renderer = gifski_renderer("figures/causas_muerte.gif"))