library(tidyverse)
library(topicmodels)
library(cbcoms)

ecb <- ecb_speeches %>%
  filter(str_detect(url, "\\.en\\.")) %>%
  filter(!str_detect(title, "[Ss]lides")) %>%
  unnest_tokens(word, text, "bigram") %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "\\d")) %>%
  select(date, id, speaker, word)

ecb_small <- ecb %>%
  count(date, word) %>%
  bind_tf_idf(word, date, n) %>%
  filter(tf_idf > quantile(tf_idf, .3))

dtm <- ecb_small %>% cast_dtm(date, word, n)

lda <- LDA(dtm, 5)

df <- tidy(lda, "gamma") %>%
  mutate(date = as.Date(document),
         topic = factor(topic))

df2 <- df %>%
  mutate(date_q = floor_date(date, "year")) %>%
  group_by(date_q) %>%
  mutate(total_gamma = sum(gamma)) %>%
  group_by(topic, add = TRUE) %>%
  summarise(gamma = sum(gamma) / unique(total_gamma))

ggplot(df2, aes(x = date_q, y = gamma, color = topic)) +
  geom_area(aes(fill = topic))
geom_point() +
  geom_smooth(span = 0.1)

tidy(lda, "beta") %>%
  filter(topic == 1) %>%
  arrange(desc(beta)) %>%
  print(n = 30)
