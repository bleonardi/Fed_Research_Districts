library(tidyverse)
library(tidytext)
library(keyATM)
library(quanteda)

papers <- read_rds("data/fed_papers_raw.rds")

# Combine title + abstract as the document text
# Weight title more by repeating it 3x (titles are dense signal)
papers <- papers |>
  mutate(
    text = case_when(
      !is.na(abstract) ~ paste(title, title, title, abstract),
      TRUE             ~ paste(title, title, title)
    ),
    doc_id = row_number()
  )

message("Documents with abstracts: ", sum(!is.na(papers$abstract)), " / ", nrow(papers))

# --- Build corpus and DFM ---

corp <- corpus(papers, docid_field = "doc_id", text_field = "text")

# Standard econ stopwords + generic academic words
econ_stopwords <- c(
  "paper", "study", "find", "findings", "using", "use", "used", "data",
  "model", "models", "result", "results", "show", "shows", "effect",
  "effects", "estimate", "estimates", "estimated", "analysis", "evidence",
  "empirical", "approach", "based", "new", "two", "also", "well", "may",
  "can", "one", "us", "u.s", "federal", "reserve", "bank", "working",
  "suggest", "suggests", "identify", "identifies", "provide", "provides",
  "examine", "examines", "investigate", "investigates", "journal", "review"
)

toks <- corp |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_remove(c(stopwords("en"), econ_stopwords)) |>
  tokens_wordstem()

dfm <- dfm(toks) |>
  dfm_trim(min_termfreq = 10, min_docfreq = 5)

message("Vocabulary size: ", ncol(dfm))
message("Documents: ", nrow(dfm))

# --- keyATM keyword seeds ---
# Each topic anchored to a district's likely specialty
# Keywords are pre-stemmed to match the DFM

keywords <- list(
  Housing       = c("hous", "mortgag", "rent", "foreclosur", "homeown", "real_estat", "propert", "construct"),
  Energy        = c("energi", "oil", "gas", "petroleum", "electr", "fossil", "carbon", "pipelin"),
  Agriculture   = c("agricultur", "farm", "crop", "food", "rural", "grain", "livestock", "commodit"),
  Technology    = c("innov", "technolog", "patent", "startup", "digit", "fintech", "softwar", "platform"),
  Labor         = c("labor", "emploi", "wage", "worker", "job", "unemploy", "workforce", "earns"),
  Finance       = c("bank", "credit", "lend", "financ", "loan", "capit", "equiti", "asset"),
  Trade         = c("trade", "export", "import", "tariff", "exchang", "global", "open", "foreign"),
  Manufacturing = c("manufactur", "industri", "factori", "product", "suppli", "output", "plant", "firm"),
  MonetaryPolicy = c("inflat", "interest", "rate", "monetari", "feder", "polici", "gdp", "macroeconom"),
  Inequality    = c("inequ", "incom", "poverti", "racial", "gap", "dispar", "distribut", "wealth")
)

# keyATM requires keywords to exist in the vocab — filter to what's present
vocab <- colnames(dfm)
keywords_filtered <- map(keywords, ~ intersect(.x, vocab))

# Report coverage
walk2(keywords_filtered, names(keywords_filtered), function(kws, nm) {
  message(nm, ": ", length(kws), " / ", length(keywords[[nm]]), " keywords in vocab")
})

# --- Fit keyATM ---

keyatm_docs <- keyATM_read(texts = dfm)

set.seed(42)
fit <- keyATM(
  docs       = keyatm_docs,
  no_keyword_topics = 3,       # 3 extra free topics to catch residual content
  keywords   = keywords_filtered,
  model      = "base",
  options    = list(seed = 42, iterations = 1500, verbose = FALSE)
)

message("Model fit complete")

# --- Extract document-topic distributions ---

theta <- as.data.frame(fit$theta)
topic_names <- c(names(keywords), paste0("Other_", 1:3))
colnames(theta) <- topic_names

theta <- theta |>
  mutate(doc_id = as.integer(rownames(theta))) |>
  left_join(papers |> select(doc_id, fed, year), by = "doc_id")

# Per-Fed average topic share
fed_topic_shares <- theta |>
  group_by(fed) |>
  summarise(across(all_of(topic_names), mean), .groups = "drop")

dir.create("data", showWarnings = FALSE)
write_rds(fit,             "data/keyatm_fit.rds")
write_rds(theta,           "data/doc_topic_theta.rds")
write_csv(fed_topic_shares,"data/fed_topic_shares.csv")

message("\nFed topic shares saved to data/fed_topic_shares.csv")
print(fed_topic_shares |> arrange(fed))
