library(tidyverse)
library(gtools)
library(topicmodels)
library(tidytext)

rcat <- function(n, x, p) {
  sample(x, size = n, replace = TRUE, prob = p)
}

generate_document <- function(z, k, w, beta) {
  w <- map(seq_len(k), ~rcat(n = z[.], x = w, beta[[.]]))
  flatten_chr(w)
}

generate_corpus <- function(M, k, lambda, alpha, w, beta) {
  tibble(
    d = seq_len(M),
    N = rpois(M, lambda),
    t = map(d,     ~rdirichlet(1, alpha)[1, ]),
    z = map2(N, t, ~rmultinom(1, .x, .y)[, 1]),
    w = map(z,     ~generate_document(., k, w, beta))
  )
}

ps <- list(
  M = 50,
  k = 3,
  lambda = 300,
  alpha = c(1, 1, 1),
  beta = tribble(
    ~w,       ~z1, ~z2, ~z3,
    "1_HICP", .9,  .0,  .0,
    "2_GDP",  .1,  .9,  .0,
    "3_NPLs", .0,  .1,  .9,
    "4_bank", .0,  .0,  .1
  )
)

set.seed(2)
df <- generate_corpus(ps$M, ps$k, ps$lambda, ps$alpha, ps$beta$w, ps$beta[, -1])

dtm <- df %>%
  unnest(w) %>%
  count(d, w) %>%
  cast_dtm(d, w, n)

k <- 3
lda <- LDA(dtm, k, control = list(seed = 1))

# Word distributions ("betas") successfully recovered
tidy(lda, "beta") %>%
  spread(topic, beta) %>%
  rename(z1 = `1`, z2 = `3`, z3 = `2`) %>%
  select(term, z1, z2, z3) %>%
  mutate_if(is.numeric, ~round(., 1))

lda_gamma_wide <- tidy(lda, matrix = "gamma") %>%
  spread(topic, gamma) %>%
  rename(d = document, z1 = `1`, z2 = `3`, z3 = `2`) %>%
  mutate(d = as.numeric(d)) %>%
  arrange(d) %>%
  select(d, z1, z2, z3)

# Distribution of topics balanced across documents
# (corresponding to specified alpha parameter)
lda_gamma_wide %>%
  gather(topic, gamma, -d) %>%
  group_by(topic) %>%
  summarise(m = mean(gamma))

df_gamma <- df %>%
  mutate(topic = map(d, ~c("z1", "z2", "z3"))) %>%
  unnest(t, topic) %>%
  rename(gamma = t)

df_compare <- inner_join(
  df_gamma,
  gather(lda_gamma_wide, topic, gamma, -d),
  by = c("d" = "d", "topic" = "topic")
)

# Topic distributions ("gammas") successfully recovered
p_lda_comparison <-
  ggplot(df_compare, aes(gamma.x, gamma.y)) +
  geom_abline(slope = 1, intercept = 0, color = "red", size = 0.1) +
  geom_text(aes(label = d), size = 0.75, alpha = 0.75, color = "blue") +
  facet_wrap(~topic) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_equal() +
  theme_light(4) +
  labs(x = "Actual topic share", y = "Predicted topic share",
       title = "LDA successfully recovering simulated topic shares",
       subtitle = "Numbers denote document IDs in corpus")

ggsave("images/p_lda_comparison.png", p_lda_comparison, width = 2, height = 1)
