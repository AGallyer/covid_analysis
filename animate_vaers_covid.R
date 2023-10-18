library(tidyverse)
library(here)
library(readxl)
library(gganimate)

#VAERS numbers pulled from CDC wonder on July 12, 2023
#COVID deaths pulled from John Hopkins on 7/12/2023. JHU stopped updating on 3/10/2023

vaers_idaho_data <- data.frame('event_type' = c('vaers_all', 
                                                'vaers_serious', 
                                                'vaers_death', 
                                                'vaccine_given', 
                                                'covid_deaths'
                                                ), 
           'number_reports' = c(4487, 318, 30, 2871087, 5416))


order <- c("vaers_death", "vaers_serious", "vaers_all", "covid_deaths", "vaccine_given")

# Create a new column to indicate the order
vaers_idaho_data$order <- factor(vaers_idaho_data$event_type, levels = order)

# Create the initial plot
p <- vaers_idaho_data %>%
  ggplot(aes(x = order, y = number_reports, fill = order, group = order)) +
  geom_col() +
  scale_fill_manual(values = c("vaers_all" = "#9a4e9e",
                               "vaers_serious" = "#fdd404",
                               "vaers_death" = "#00447c",
                               "covid_deaths" = "#cf4727",
                               "vaccine_given" = "#32bfc5")) +
  ggtitle('COVID-19 and COVID Vaccine Idaho data') + 
  scale_x_discrete(labels = c('Vax. deaths reported',
                              'Serious vax. side effects', 
                              'All vax. side effects', 
                              'COVID-19 deaths', 
                              'COVID vaccines administered'
    
  )) + 
  labs(x = "", y = "", fill = '') + 
  theme_classic() + 
  theme(text = element_text(size = 26.9), legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 21), axis.title.x = element_blank()) + 
  transition_states(order, transition_length = c(5, 5, 5, 10), state_length = 1) +
  view_follow(fixed_x = TRUE) + 
  enter_grow() + 
  shadow_mark()


# Animate the plot
animate(p, fps = 50, duration = 29, width = 10, height = 12, units = 'in', 
        res = 150, renderer = gifski_renderer(loop = FALSE))

anim_save('vaers_covid.gif')
