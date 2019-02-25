library(tidyverse)
library(lubridate)
library(tidytext)
library(topicmodels)
library(SnowballC)
library(cbcoms) # personal package

ecb_tok <- ecb_speeches %>%
  filter(!str_detect(title, "[Ss]lides"),
         str_detect(url, "\\.en\\.")) %>%
  select(date, text) %>%
  unnest_tokens(word, text)

ecb_ngrams <- ecb_tok %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "\\d")) %>%
  mutate(word_stem = wordStem(word)) %>%
  group_by(date) %>%
  summarise(text = paste(word_stem, collapse = " ")) %>%
  unnest_tokens(word, text, "ngrams", n = 2)

ecb_counts <- ecb_ngrams %>%
  count(date, word) %>%
  bind_tf_idf(word, date, n)

dtm <- ecb_counts %>%
  filter(n > 2) %>%
  filter(tf_idf > quantile(tf_idf, .40)) %>%
  cast_dtm(date, word, n)

lda <- LDA(dtm, k = 20, control = list(seed = 1, verbose = 10))

topic_lbls <- tidy(lda, "beta") %>%
  arrange(topic, desc(beta)) %>%
  group_by(topic) %>%
  slice(1:3) %>%
  summarise(topic_lbl = paste(term, collapse = "\n")) %>%
  ungroup() %>%
  mutate(topic = factor(topic))

gammas <- tidy(lda, "gamma") %>%
  mutate(date = as.Date(document),
         topic = factor(topic))

gammas_q <- gammas %>%
  group_by(q_date = floor_date(date, "quarter")) %>%
  mutate(total_gamma = sum(gamma)) %>%
  group_by(topic, add = TRUE) %>%
  summarise(gamma = sum(gamma) / unique(total_gamma))

df_plot <- gammas_q %>% left_join(topic_lbls)

p_ecb_topics <-
  ggplot(df_plot, aes(x = q_date, y = gamma, color = topic_lbl)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(span = 0.3) +
  facet_wrap(~topic_lbl, scales = "free_y") +
  theme_light(9) +
  theme(legend.position = "none",
        strip.text = element_text(hjust = 0)) +
  labs(x = NULL, y = NULL,
       title = "Topics in ECB speeches",
       subtitle = "Share of speeches devoted to topic, by quarter")

ggsave("images/ecb_topics.png", p_ecb_topics, width = 8, height = 6)
