library(tidyverse)
library(tidytuesdayR)
library(ggpubr)
library(RColorBrewer)


tt_data <- tidytuesdayR::tt_load(2024, week=43) #load in tidytuesday data

cia_original <- tt_data$cia_factbook #extract

cia_internet <- cia_original %>% mutate(internet_percent = 100*(internet_users/population)) %>% 
  na.omit()

label_countries <- cia_internet %>% 
  filter(country %in% c("Japan", "Singapore", "Canada", "Iceland"))

  ggplot(data = cia_internet, aes(x = internet_percent, y = life_exp_at_birth)) + 
  geom_point(alpha = .5, color = "#00FF00") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_line(linetype = 3, color = "#3b3b40"),
          panel.grid.minor = element_line(linetype = 5, color = "#3b3b40"),
          plot.background = element_rect(fill = "#000004"),
          plot.margin = margin(r = 30),
          panel.background = element_rect(fill = "#000004"),
          axis.title = element_text(color = "gray", family = "mono"),
          axis.title.x = element_text(margin = margin(b = 20)),
          axis.title.y = element_text(margin = margin(l = 20)),
          axis.text = element_text(color = "gray"),
          plot.title = element_text(color = "#00FF00", size = 22, face = "bold", family = "mono", hjust= .5, margin = margin(b = 10, t = 20)),
          plot.subtitle = element_text(color = "gray", hjust = .5, family = "mono", size = 11, margin = margin(b = 30))) + 
    xlab("\nPercent of population that uses the internent (%)") +
    ylab("Average lifespan (years)\n ") +
    labs(title= "Using the Internet Makes You Live Longer",
         subtitle = "Source: CIA Factbook") +
    geom_text(data = label_countries, aes(label = country), 
              color = "#00FF00", family = "mono", vjust = -9.5, size = 4) +  # Adjust vjust and size as neede
    geom_segment(data = label_countries, 
                 aes(xend = internet_percent, yend = life_exp_at_birth + 5),  # Adjust +5 to control line length
                 color = "#00FF00", linetype = "dotted")

  ggsave(filename = "internet_lifespan.png", width = 1000, height = 800, unit = "px", dpi = 100, type = "cairo")
  