library(tidyverse)
library(janitor)
library(here)
library(RColorBrewer)

#------
# Poll: Govt Spending
#------
poll <- read.csv("poll.csv") %>% 
  janitor::clean_names()%>%
  rename("Too Much" = too_much,
         "Too Little" = too_little,
         "About Right" = about_right,
         Unsure = unsure)

poll_long <- poll %>%
  pivot_longer(cols = 4:7,
               names_to = "position",
               values_to = "percent")

years <- unique(poll$year)

ggplot(data = poll_long) +
  geom_area(aes(x = year,
                y = (100*percent),
                fill = position),
            alpha = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(1992, 1995, 2000, 2005, 2010, 2015, 2018),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Public Opinion on:",
       subtitle = "Do you think the U.S. government is doing too much, too little,\nor about the right amount in terms of protecting the environment?",
       x = "Year",
       y = "Percent",
       caption = "Gallup Poll\nLast Conducted: March 1-8, 2018. N=1,041 adults nationwide. Margin of error ± 4\nPoll Years: 1992, 2000, 2003, 2004, 2005, 2006, 2010, 2011, 2012, 2013, 2018\n\nESM 243: Spring 2021\nKeene Morrow") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))



#------
# Poll: Carbon Tax
#------

carbon <- read.csv("carbon_long.csv") %>% 
  janitor::clean_names()

ggplot(data = carbon) +
  geom_col(aes(x = position,
               y = percent,
               fill = position),
           show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Public Opinion on:",
       subtitle = "Passing a carbon tax to encourage reductions in carbon dioxide emissions",
       x = "Position",
       y = "Percent",
       caption = "Gallup Poll\nConducted: March 1-8, 2018. N=1,041 adults nationwide. Margin of error ± 4\n\nESM 243: Spring 2021\nKeene Morrow") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))
