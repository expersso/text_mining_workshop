library(tidyverse)
library(lubridate)
library(scales)
library(tidytext)
library(cbcoms)

fed_count <- fed_speeches %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date, id) %>%
  mutate(length = n()) %>%
  ungroup()

fed_summary <- fed_count %>%
  filter(word %in% c("financial crisis")) %>%
  group_by(date, id, word) %>%
  summarise(n = n() / unique(length)) %>%
  ungroup()

p_fed_fin_crisis <-
  ggplot(fed_summary, aes(x = date, y = n * 1000)) +
  geom_col(fill = "firebrick") +
  geom_vline(xintercept = as.Date("2008-09-15"), size = 0.1) +
  scale_y_continuous(expand = expand_scale(c(0, 0.1))) +
  scale_x_date(breaks = pretty_breaks(10)) +
  theme_light(4) +
  labs(x = NULL, y = NULL,
       title = "Frequency of expression 'financial crisis' in Fed speeches",
       subtitle = "Per 1,000 words, aggregated quarterly",
       caption = "Note: Vertical line denotes Lehman collapse.")

ggsave("images/p_fed_fin_crisis.png", p_fed_fin_crisis, width = 2.5, height = 1)
