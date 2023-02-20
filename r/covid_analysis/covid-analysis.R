# Installing packages
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(mapcan)
# Data: https://open.canada.ca/data/en/dataset/261c32ab-4cfd-4f81-9dea-7b64065690dc
cov <- read_csv("./data/covid19-canada.csv")
vax <- read_csv('./data/vaccination.csv')

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
       y = 'Infections',
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





### 4-way plot ####

voc_plot <-
  cov %>%  
  as_tibble() %>% 
  select(prov = prname, date, cases = avgcases_last7) %>% 
  filter(prov != 'Canada') %>% 
  mutate(prov = case_when(prov == 'Alberta' ~ 'Alberta', 
                          prov == 'Ontario' ~ 'Ontario', 
                          prov == 'British Columbia' ~ 'British Columbia', 
                          prov == 'Quebec' ~ 'Quebec', 
                          TRUE ~ 'Rest of Canada')) %>% 
  group_by(date, prov) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup() %>% 
  # filter(prov == "Ontario" | prov == "Alberta" | prov == "British Columbia") %>% 
  filter(date >= '2021-01-01' & date <= '2021-12-31') %>% 
  ggplot(aes(x = date, y = cases, color = prov)) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  labs(x = NULL, 
       y = 'Infections',
       color = NULL,
       title = NULL) + 
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
         max = as.Date(c('2021-04-20', '2021-06-15', '2021-09-25', '2021-03-20'))
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




pop_2017 <- mapcan::province_pop_annual %>%
  filter(year == 2017)

pr_geographic <- mapcan(boundaries = province,
                        type = standard)


pr_geographic <- inner_join(pr_geographic, 
                            pop_2017, 
                            by = c("pr_english" = "province"))

rates <-
  cov %>% 
  select(pr_english = prname, date, rate = ratedeaths_last14) %>% 
  filter(pr_english !='Canada') %>% 
  filter(pr_english != 'Repatriated travellers') %>% 
  droplevels() %>% 
  group_by(pr_english) %>% 
  summarise(rate = mean(rate, na.rm=T)) 


rates_join <- left_join(pr_geographic, rates, by = 'pr_english')


map <-
rates_join %>%
  ggplot(aes(x = long, y = lat, group = group, fill = rate)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() + 
  scale_fill_viridis_c(name = "Mortality rate / 100K") +
  # ggtitle("Canadian Population by Province") + 
  guides(fill = guide_colourbar(title.position = "top")) + 
  theme(legend.position = c(0.7, 0.75),
        legend.direction="horizontal",
        legend.title = element_text(size = 9))
  #geom_text(aes(x = long, y = lat, label = prov), data = centroids)

centroids <- 
read_csv('prov,  long, lat
Newfoundland and Labrador,	53.000000,	-60.000000
Saskatchewan, 55.000000,	-106.000000
Prince Edward Island,	46.250000,	-63.000000
Ontario,	50.000000,	-85.000000
Nova Scotia,	45.000000,	-63.000000
Alberta,	55.000000,	-115.000000
British Columbia,	53.726669,	-127.647621
Manitoba,	56.415211,	-98.739075
Newfoundland and Labrador,	53.135509,	-57.660435
New Brunswick,	46.498390	,-66.159668
Quebec Province,	53.000000,	-70.000000') 

vax_plot <- 
vax %>% 
  select(prov = prename, partial = proptotal_partially, 
         full = proptotal_fully, date = week_end) %>% 
  filter(prov ==  'Canada') %>% 
  mutate(full = as.numeric(ifelse(full == '<0.1', 0, full))) %>% 
  #add_row(prov = 'Canada', full = 0, partial = 0, date =as.Date('2021-01-01')) %>% 
  #add_row(prov = 'Canada', full = max(.$full), partial = max(.$partial), date =as.Date('2021-12-31')) %>% 
  mutate(full = as.numeric(ifelse(full == '<0.1', 0, full))) %>% 
  filter(date >= '2021-01-01' & date <= '2021-12-31')  %>% 
pivot_longer(c(partial, full), names_to = 'vax', values_to = 'prop') %>% 
ggplot(aes(x=date, y = prop, fill = vax)) +
  geom_area(alpha = 0.5) + 
  theme_minimal() + 
  labs(x = NULL, 
       y = '% Vaccinated',
       color = NULL,
       title = NULL) + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.title.y= element_text(color = 'black', size = 10, margin = margin(r  = 10)),
        axis.text.y = element_text(color = 'white'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y =element_blank()) +
  scale_x_date(breaks = "2 month", date_labels = "%b",
               minor_breaks = '1 month',
               expand = c(0, 0))
  
library(patchwork)

( (voc_plot / vax_plot / timeline_plot) | map ) + 
  plot_annotation(title = 'The COVID-19 pandemic at a glance', 
                  subtitle = "Canada, 2021", 
                  tag_levels = 'A')  & 
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.tag = element_text(size = 8))


ggsave( "../../images/epi-plot-full.png", width = 13, height = 6.5, , bg = "white")



ggarrange(voc_plot, "", timeline_plot, vax_plot, map,  heights = c(2.5, 0.25, 1, 1, 4), 
          ncol = 1, nrow = 5, 
          labels = c("A", "B", 'C', "D"))


