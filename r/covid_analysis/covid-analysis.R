# Installing packages
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
# Data: https://open.canada.ca/data/en/dataset/261c32ab-4cfd-4f81-9dea-7b64065690dc
cov <- read_csv("./covid19-canada.csv")

#Creating plots
voc_plot <-
cov %>%  
  as_tibble() %>% 
  select(prov = prname, date, cases = avgcases_last7) %>% 
  filter(prov != 'Canada') %>% 
  filter(prov == "Ontario" | prov == "Alberta" | prov == "British Columbia") %>% 
  filter(date >= '2021-01-01' & date <= '2021-12-31') %>% 
  ggplot(aes(x = date, y = cases, color = prov)) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  labs(x = NULL, 
       y = 'Infections (7-day rolling average)',
       color = NULL,
       title = 'Infections and prevalent variants in Canada in 2021') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top", 
        axis.title.y = element_text(size = 10, 
                                    margin = margin(r = 10))) + 
  scale_color_brewer(palette="Set1") + 
  scale_x_date(breaks = "2 month", date_labels = "%b",
               minor_breaks = '1 month',
               expand = c(0, 0))

timeline_plot <-
tibble(period = c('n', 'ag', 'd', 'o'), 
       labels = c('Non-VoCs', 'Alpha & Gamma', 'Omicron', 'Delta'),
       start = as.Date(c('2021-01-01', '2021-03-15', '2021-11-02', '2021-05-01')),
       end = as.Date(c('2021-04-20', '2021-06-15', '2021-12-31', '2021-12-15')),
       height = c(400, 300, 100, 200),
       date_lab = c('Jan - Apr', 'Mar - Jun', 'Nov - Dec', 'May - Dec'),
       max = as.Date(c('2021-04-20', '2021-06-15', '2021-09-25', '2021-03-25'))
) %>% 
  mutate(period  = factor(period, levels = c('n', 'ag', 'd','o'))) %>%  
  pivot_longer(cols = start:end, 
               names_to = 'time', 
               values_to = 'date') %>% 
  group_by(period) %>% 
  mutate(mean = mean(date)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = height, color = period)) + 
  geom_line(size = 6) + 
  geom_text(aes( x = mean, y = height + 7, label = labels), color = 'white', size = 3) + 
  geom_text(aes( x = max + 2, y = height + 7, label = date_lab), color = 'darkgrey', size = 3, hjust = 0) + 
  scale_color_manual(labels =  c('Non-VOC vs. Alpha/Gamma', 'Alpha vs. Gamma', 'Delta vs. Omicron'),
                     values = c("#D16103", "#52854C", "#4E84C4", "darkblue")) +
  ylim(50, 450) + 
  labs(x = NULL, y = "Prevalent variants", color = NULL) + 
  theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.y= element_text(color = 'black', size = 10, margin = margin(r  = 10)),
        axis.text.y = element_text(color = 'white'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y =element_blank()) +
  scale_x_date(breaks = "2 month", date_labels = "%b",
               minor_breaks = '1 month',
               expand = c(0, 0))

# Grouping plots
ggarrange(voc_plot, "", timeline_plot, heights = c(2.5, 0.25, 1), 
          ncol = 1, nrow = 3, 
          labels = c("A", "B"))

# Saving final plot
ggsave( "../../images/epi-plot.png", width = 6.8, height = 5, , bg = "white")
