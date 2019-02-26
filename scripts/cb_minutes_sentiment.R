library(tidyverse)
library(lubridate)
library(scales)
library(ecbutils)
library(glue)
library(tidytext)
library(cbcoms)

cb_sentiments <- cb_minutes %>%
  filter(type %in% c("Minutes", "Record of Policy Actions")) %>%
  select(date, text, bank) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("loughran"))

df_plot <- cb_sentiments %>%
  mutate(date = floor_date(date, "quarter")) %>%
  group_by(date, bank) %>%
  summarise(d = sum(sentiment == "positive") / sum(sentiment == "negative")) %>%
  filter(year(date) >= 1999,
         bank != "ecb")

ggplot(df_plot, aes(x = date, y = d, color = bank)) +
  geom_point() +
  geom_smooth(span = 0.3, se = FALSE)
