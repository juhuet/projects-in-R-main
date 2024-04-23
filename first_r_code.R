library(readr)
library(tidyverse)
setwd("C:/Users/Public/projects-in-R-main/data")
df <- insurance_with_date

df$region <- factor(df$region)
df$sex <- factor(df$sex)
df$more_than_2_kids_and_smokes <- ifelse(df$children > 2 & df$smoker == "yes", 1, 0)

#tidyverse
df <- df %>%
  mutate(more_than_2_kids_and_smokes = if_else(children > 2 & smoker == "yes", 1, 0))

df <- df %>%
  mutate(added_6 = date %m+% months(6))

library(dplyr)
library(lubridate)

covid <- read_csv("COVID19Cases_geoRegion.csv")

# filter data frame covid: 
# only keep confirmed cases in the cantons of Zurich, Bern and Vaud 
# in the first half of the year 2020
covid_cantons_2020 <- covid %>% filter(datum <= ymd("2020-06-30") 
                                       & (geoRegion == "ZH" | geoRegion == "BE" | geoRegion == "VD"))

# write data frame covid_cantons_2020 to a csv file
write_csv(x = covid_cantons_2020, file = "processed/covid_cantons_2020_06.csv")

library(ggplot2)

plot_covid_point_v0 <- ggplot(data = covid_cantons_2020, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_point()

ebola2 <- ebola %>% filter(Date <= ymd("2021-03-31") 
                                       & (Country == "Guinea" | Country == "Nigeria" | Country == "Sierra Leone"))

#cumulative cases
data_ebola_cum_cases <- ebola %>% 
  select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) %>% 
  filter(date <= ymd("2015-03-31") & 
           (country == "Guinea" | country ==  "Liberia" | country == "Sierra Leone"))

#point plot
plot_ebola <- ggplot(data = data_ebola_cum_cases, 
                     mapping = aes(x = date, y = cum_conf_cases, color = country)) + 
  geom_point() +
  scale_color_manual(values = c("Guinea" = "red", "Liberia" = "blue", "Sierra Leone" = "green"))

print(plot_ebola)

#line plot
plot_ebola <- ggplot(data = data_ebola_cum_cases, 
                     mapping = aes(x = date, y = cum_conf_cases, color = country)) + 
  geom_line() +
  scale_color_manual(values = c("Guinea" = "red", "Liberia" = "blue", "Sierra Leone" = "green"))

print(plot_ebola)


#column plot

plot_ebola <- ggplot(data = data_ebola_cum_cases, 
                     mapping = aes(x = date, y = cum_conf_cases, fill = country)) + 
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set2")

print(plot_ebola)

#labels
plot_ebola <- ggplot(data = data_ebola_cum_cases, 
                     mapping = aes(x = date, y = cum_conf_cases, fill = country)) + 
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle(label = "Confirmed covid cases in 3 countries") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")

print(plot_ebola)

plot_ebola_point_v4 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.9, shape = 16, size = 1.5, stroke = 2) +
  scale_fill_brewer(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    palette = "Set2",
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_brewer(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      palette = "Set2",
                      labels = c("GIN", "LBR", "SLE")) +
  
  ggtitle(label = "Confirmed Ebola") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")

print(plot_ebola_point_v4)




plot_ebola_stacked_v4 <- ggplot(data = data_ebola_cum_cases, 
                                mapping = aes(x = date, y = cum_conf_cases, fill = country)) + 
  geom_col(alpha = 0.9, position = "stack", width = 2) +
  scale_fill_brewer(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    palette = "Reds",
                    labels = c("GIN", "LBR", "SLE")) +
  ggtitle(label = "Confirmed Ebola") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom")

print(plot_ebola_stacked_v4)


plot_ebola_point_v4 <- ggplot(data = data_ebola_cum_cases, 
                              aes(x = date, y = cum_conf_cases, color = country, shape = country)) + 
  geom_point(alpha = 0.7, size = 3, stroke = 0.5) +
  scale_color_brewer(name = "Country",
                     breaks = c("Guinea", "Liberia", "Sierra Leone"),
                     palette = "Set2",
                     labels = c("Guinea (GIN)", "Liberia (LBR)", "Sierra Leone (SLE)")) +
  scale_shape_manual(name = "Country",
                     breaks = c("Guinea", "Liberia", "Sierra Leone"),
                     values = c(16, 16, 16),
                     labels = c("Guinea (GIN)", "Liberia (LBR)", "Sierra Leone (SLE)")) +
  ggtitle("Confirmed Ebola Cases Over Time") +
  xlab("Date") +
  ylab("Cumulative Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_ebola_point_v4)


# Adjust theme
theme_set(theme_minimal())

# Plot with improved aesthetics
plot_ebola_column_v4 <- ggplot(data = data_ebola_cum_cases, 
                               aes(x = date, y = cum_conf_cases, fill = country)) + 
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_fill_brewer(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    palette = "Set2",
                    labels = c("Guinea (GIN)", "Liberia (LBR)", "Sierra Leone (SLE)")) +
  ggtitle("Confirmed Ebola Cases Over Time") +
  xlab("Date") +
  ylab("Cumulative Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_ebola_column_v4)



plot_ebola_stacked_column_v4 <- ggplot(data = data_ebola_cum_cases, 
                                       aes(x = date, y = cum_conf_cases, fill = country)) + 
  geom_col(color = "black", size = 0.5, width = 2) + # Adjusted size here
  scale_fill_brewer(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    palette = "Set2",
                    labels = c("Guinea (GIN)", "Liberia (LBR)", "Sierra Leone (SLE)")) +
  ggtitle("Confirmed Ebola Cases Over Time") +
  xlab("Date") +
  ylab("Cumulative Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_ebola_stacked_column_v4)
