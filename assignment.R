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
  filter(!str_detect(word, "\\d")) |> 
  select(speaker, word)

friends_tf <- friends_tokens |>
  count(speaker, word) |>
  group_by(speaker) |>
  mutate(tf = n / sum(n)) |>
  arrange(desc(n), .by_group = TRUE) |>
  slice_head(n = 500) |> 
  ungroup() |>
  select(speaker, word, tf)

friends_tf_wide <- friends_tf |>
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>
  column_to_rownames("speaker")

friends_scaled <- scale(friends_tf_wide)

set.seed(118)
km.out <- kmeans(friends_scaled, centers = 3, nstart = 20)

pca_fit <- prcomp(friends_tf_wide, center = TRUE, scale. = TRUE)

q <- fviz_pca_biplot(
  pca_fit,
  geom = c("text"),
  habillage = as.factor(km.out$cluster),
  select.var = list(cos2 = 20),
  repel = TRUE,
  ggtheme = theme_minimal()
) 


