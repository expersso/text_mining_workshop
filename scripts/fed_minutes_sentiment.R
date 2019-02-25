library(tidyverse)
library(scales)
library(lubridate)
library(glue)
library(cbcoms)
library(tidytext)
library(SnowballC)
library(fredr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))
rec_dates <- fredr("JHDUSRGDPBR") %>% filter(value == 1)

fed <- cb_minutes %>%
  filter(bank == "fed",
         type %in% c("Minutes", "Record of Policy Actions")) %>%
  select(date, text) %>%
  unnest_tokens(word, text, "words") %>%
  anti_join(stop_words) %>%
  left_join(filter(sentiments, lexicon == "loughran")) %>%
  filter(!is.na(sentiment)) %>%
  group_by(date) %>%
  summarise(share = (sum(sentiment == "positive") - sum(sentiment == "negative")) / n())

df_plot <- fed %>% filter(year(date) >= 1960)

p_fed_sentiment <-
  ggplot(df_plot) +
  geom_vline(aes(xintercept = as.numeric(date)), rec_dates, color = "lightgreen",
             alpha = 0.25) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = date, y = scale(share)), alpha = 0.5, color = "red",
             size = 0.5) +
  geom_smooth(aes(x = date, y = scale(share)), span = 0.1, size = 0.1) +
  scale_x_date(breaks = pretty_breaks(10), expand = c(0, 0)) +
  theme_light(4) +
  labs(
    x = NULL, y = NULL,
    title = "Sentiment in Fed policy meeting minutes",
    subtitle = "Difference in positive and negative sentiment.",
    caption = "Source: Federal Reserve. Note: Loughran-McDonald sentiment lexicon.
    Vertical bars indicate quarters of US recessions."
  )

ggsave("images/p_fed_sentiment.png", p_fed_sentiment, width = 3, height = 2)
