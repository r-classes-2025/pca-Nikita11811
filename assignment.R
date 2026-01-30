library(friends)
library(tidyverse) 
library(tidytext)
library(factoextra)

top_speakers <- count(friends, speaker, sort = TRUE) |> 
  slice_head(n = 6) |> 
  pull(speaker)

friends_tokens <- friends |>
  filter(speaker %in% top_speakers) |>
  unnest_tokens(word, text) |>
  mutate(word = str_remove_all(word, "\\d+")) |>
  filter(word != "") |>
  select(speaker, word)

friends_tf <- friends_tokens |>
  count(speaker, word) |>
  group_by(speaker) |>
  mutate(tf = n / sum(n)) |>
  arrange(-n) |> 
  slice_head(n = 500) |>
  ungroup() |>
  select(speaker, word, tf)

friends_tf_wide <- friends_tf |>
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>
  column_to_rownames("speaker")

set.seed(118)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

q <- fviz_pca_biplot(
  pca_fit,
  geom = c("text"),
  habillage = as.factor(km.out$cluster),
  select.var = list(cos2 = 20),
  repel = FALSE,
  ggtheme = theme_minimal()
)