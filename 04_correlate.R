library(tidyverse)

topic_shares    <- read_csv("data/fed_topic_shares.csv", show_col_types = FALSE)
industry_shares <- read_csv("data/fed_industry_shares.csv", show_col_types = FALSE)

sectors <- c("Housing","Energy","Agriculture","Technology","Finance","Trade","Manufacturing","Labor")

topic_wide <- topic_shares |> select(fed, all_of(sectors))
ind_wide   <- industry_shares |>
  select(fed, sector, emp_share) |>
  pivot_wider(names_from = sector, values_from = emp_share)

joined <- topic_wide |>
  left_join(ind_wide, by = "fed", suffix = c("_topic", "_ind"))

# Matched-pair correlations (diagonal)
pair_cors <- tibble(sector = sectors) |>
  mutate(
    r = map_dbl(sector, ~ cor(joined[[paste0(.x,"_topic")]], joined[[paste0(.x,"_ind")]])),
    p = map_dbl(sector, ~ cor.test(joined[[paste0(.x,"_topic")]], joined[[paste0(.x,"_ind")]])$p.value)
  ) |>
  arrange(desc(r))

message("Matched-pair correlations:")
print(pair_cors |> mutate(across(c(r,p), ~ round(.x, 3))))

# Full cross-correlation matrix
cross_cor <- expand_grid(topic = sectors, sector = sectors) |>
  mutate(
    r = map2_dbl(topic, sector, ~ cor(joined[[paste0(.x,"_topic")]], joined[[paste0(.y,"_ind")]]))
  )

message("\nCross-correlation matrix (topic rows, industry cols):")
print(
  cross_cor |>
    pivot_wider(names_from = sector, values_from = r) |>
    mutate(across(where(is.numeric), ~ round(.x, 2)))
)

# Per-Fed alignment: correlation between Fed's topic vector and its industry vector
alignment <- joined |>
  mutate(
    alignment_score = map_dbl(row_number(), function(i) {
      topic_vec <- as.numeric(joined[i, paste0(sectors, "_topic")])
      ind_vec   <- as.numeric(joined[i, paste0(sectors, "_ind")])
      cor(topic_vec, ind_vec)
    })
  ) |>
  select(fed, alignment_score) |>
  arrange(desc(alignment_score))

message("\nPer-Fed alignment scores:")
print(alignment)

write_csv(pair_cors,  "data/pair_correlations.csv")
write_csv(cross_cor,  "data/cross_correlation_matrix.csv")
write_csv(alignment,  "data/fed_alignment_scores.csv")
write_csv(joined,     "data/joined_wide.csv")
message("Done.")
