library(here)
library(tidyverse)
library(tidytext)
library(textdata)

newhope <-
  read_delim(
    here("data/SW_EpisodeIV.txt"),
    delim = " ",
    trim_ws = TRUE,
    skip = 1,
    col_names = c("line", "actor", "dialogue")
  )

empire <-
  read_delim(
    here("data/SW_EpisodeV.txt"),
    delim = " ",
    trim_ws = TRUE,
    skip = 1,
    col_names = c("line", "actor", "dialogue")
  )

jedi <-
  read_delim(
    here("data/SW_EpisodeVI.txt"),
    delim = " ",
    trim_ws = TRUE,
    skip = 1,
    col_names = c("line", "actor", "dialogue")
  )

sw <- bind_rows(
  `New Hope` = newhope,
  `Empire` = empire,
  `ROTJ` = jedi,
  .id = "movie"
)

# split sentences
sw <- sw |>
  unnest_sentences("sentences", "dialogue", drop = FALSE) |>
  nest(sentences = sentences)

# number of lines per character
sw |>
  mutate(movie = factor(movie, levels = c("New Hope", "Empire", "ROTJ"))) |>
  unnest(sentences) |>
  group_by(actor, movie) |>
  count() |>
  filter(n > 10) |>
  ggplot(aes(y = reorder(actor, n), x = n)) +
  geom_col(width = 0.5) +
  facet_wrap(vars(movie)) +
  theme(axis.title = element_blank())

# tokenization
sw <- sw |>
  unnest(sentences) |>
  unnest_tokens("words", "sentences", drop = FALSE) |>
  nest(sentences = sentences, words = words)

# remove character names from tokens
# data("starwars")
#
# sw_names <- starwars |>
#   mutate(name = str_to_lower(name)) |>
#   separate(name, into = c("first", "last"), sep = " ")
#
# sw <- sw |>
#   mutate(words = map(words, ~ filter(
#     .x, !words %in% c(sw_names$first, sw_names$last)
#   )))

# tf-idf
sw_words <- sw |>
  unnest(words) |>
  count(movie, words, sort = TRUE)

sw_total <- sw_words |>
  group_by(movie) |>
  summarize(total = n())

sw_words <- left_join(sw_words, sw_total) |>
  group_by(movie) |>
  mutate(tf = n / total, rank = row_number()) |>
  ungroup()

ggplot(sw_words, aes(tf, fill = movie)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(vars(movie), ncol = 2, scales = "free_y")

ggplot(sw_words, aes(rank, tf, colour = movie)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope = -1, linetype = 3)

sw_tf_idf <- sw_words |>
  bind_tf_idf(words, movie, n)

sw_tf_idf |>
  arrange(desc(tf_idf))

sw_tf_idf %>%
  group_by(movie) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = movie)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(movie), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# most common words by character (excluding stopwords)
sw |>
  mutate(words = map(words, ~ filter(.x, !words %in% stop_words$word))) |>
  unnest(words) |>
  group_by(movie, actor) |>
  count(words) |>
  ungroup() |>
  filter(actor == "LUKE", n > 3) |>
  arrange(desc(n)) |>
  ggplot(aes(y = words, x = n, fill = actor)) +
  geom_col() +
  facet_wrap(vars(movie), scales = "free")

# length of sentences per character
sw <- sw |>
  mutate(length = map_dbl(words, nrow))

sw |>
  group_by(movie, actor) |>
  group_modify(~ mutate(.x, n = n()) |> filter(n > 10)) |>
  ggplot(aes(y = actor, x = length)) +
  geom_col() +
  facet_wrap(vars(movie), scales = "free_y")

# pre-defined sentiments for single-words
sw <- sw |>
  mutate(sentiments = map(
    words,
    inner_join,
    get_sentiments(lexicon = "nrc"),
    by = c("words" = "word")
  ))

sentiment_order <-
  c("anger", "disgust", "negative", "fear", "sadness", "surprise", "positive",
    "anticipation", "joy", "trust")

df <- sw |>
  select(-words) |>
  unnest(sentiments) |>
  group_by(actor, movie) |>
  count(sentiment) |>
  mutate(sentiment = factor(sentiment, levels = sentiment_order)) |>
  group_by(actor, movie) |>
  mutate(prop = n / sum(n))

positive <- df |>
  filter(sentiment %in% c("surprise", "positive", "anticipation", "joy", "trust")) |>
  group_by(actor, movie, sentiment) |>
  summarize(positive = sum(n)) |>
  summarize(positive = sum(positive))

left_join(df, positive) |>
  filter(movie == "New Hope") |>
  mutate(positive = positive / sum(n)) |>
  group_modify(~ filter(.x, sum(n) > 15)) |>
  ggplot(aes(y = reorder(actor, positive), x = prop, fill = sentiment)) +
  geom_col() +
  scale_fill_brewer(palette = "RdBu")

left_join(df, positive) |>
  filter(movie == "Empire") |>
  mutate(positive = positive / sum(n)) |>
  group_modify(~ filter(.x, sum(n) > 15)) |>
  ggplot(aes(y = reorder(actor, positive), x = prop, fill = sentiment)) +
  geom_col() +
  scale_fill_brewer(palette = "RdBu")

left_join(df, positive) |>
  filter(movie == "ROTJ") |>
  mutate(positive = positive / sum(n)) |>
  group_modify(~ filter(.x, sum(n) > 15)) |>
  ggplot(aes(y = reorder(actor, positive), x = prop, fill = sentiment)) +
  geom_col() +
  scale_fill_brewer(palette = "RdBu")

# sentiment analysis (bigrams)
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

sw <- sw |>
  unnest(sentences) |>
  unnest_tokens(
    output = "bigrams",
    input = "sentences",
    token = "ngrams",
    n = 2,
    drop = FALSE
  ) |>
  nest(bigrams = bigrams)

sw |>
  unnest(bigrams) |>
  group_by(movie, actor) |>
  count(bigrams) |>
  filter(movie == "Empire") |>
  arrange(desc(n), .by_group = TRUE) |>
  slice(1:10) |>
  drop_na(bigrams) |>
  group_modify(~ mutate(.x, total = sum(n)) |> filter(total > 200)) |>
  ggplot(aes(y = bigrams, x = n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(actor), scales = "free")
