library(tidyverse)
library(httr2)
library(jsonlite)

# OpenAlex institution IDs for each Federal Reserve Bank
# Found via https://api.openalex.org/institutions?search=federal+reserve
fed_institutions <- tribble(
  ~fed,           ~city,            ~openalex_id,
  "Boston",       "Boston",         "I46510805",
  "New York",     "New York",       "I43638361",
  "Philadelphia", "Philadelphia",   "I57502794",
  "Cleveland",    "Cleveland",      "I4210109900",
  "Richmond",     "Richmond",       "I4210158875",
  "Atlanta",      "Atlanta",        "I119878761",
  "Chicago",      "Chicago",        "I77697173",
  "St. Louis",    "St. Louis",      "I77793887",
  "Minneapolis",  "Minneapolis",    "I1341575764",
  "Kansas City",  "Kansas City",    "I84318336",
  "Dallas",       "Dallas",         "I68415526",
  "San Francisco","San Francisco",  "I19990318"
)

fetch_papers_for_fed <- function(institution_id, fed_name, from_year = 2010, to_year = 2024) {
  message("Fetching papers for ", fed_name, "...")

  all_results <- list()
  cursor <- "*"
  page <- 1

  repeat {
    resp <- request("https://api.openalex.org/works") |>
      req_url_query(
        filter = paste0(
          "institutions.id:", institution_id,
          ",publication_year:", from_year, "-", to_year
        ),
        select = "id,title,abstract_inverted_index,publication_year,concepts,keywords",
        per_page = 200,
        cursor = cursor,
        mailto = "research@example.com"
      ) |>
      req_throttle(rate = 10) |>
      req_perform()

    data <- resp_body_json(resp, simplifyVector = FALSE)

    results <- data$results
    if (length(results) == 0) break

    all_results <- c(all_results, results)
    message("  Page ", page, ": ", length(all_results), " papers so far")

    next_cursor <- data$meta$next_cursor
    if (is.null(next_cursor)) break
    cursor <- next_cursor
    page <- page + 1

    Sys.sleep(0.1)
  }

  all_results
}

# Convert OpenAlex inverted index abstract back to plain text
reconstruct_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) return(NA_character_)

  positions <- unlist(lapply(names(inverted_index), function(word) {
    pos <- inverted_index[[word]]
    setNames(rep(word, length(pos)), pos)
  }))

  paste(positions[order(as.integer(names(positions)))], collapse = " ")
}

parse_papers <- function(raw_results, fed_name) {
  map_dfr(raw_results, function(work) {
    tibble(
      fed          = fed_name,
      openalex_id  = work$id %||% NA_character_,
      title        = work$title %||% NA_character_,
      year         = work$publication_year %||% NA_integer_,
      abstract     = reconstruct_abstract(work$abstract_inverted_index),
      concepts     = list(map_chr(work$concepts %||% list(), ~ .x$display_name %||% NA_character_)),
      keywords     = list(map_chr(work$keywords %||% list(), ~ .x$keyword %||% NA_character_))
    )
  })
}

# Pull all Feds
papers_raw <- fed_institutions |>
  pmap(function(fed, city, openalex_id) {
    raw <- fetch_papers_for_fed(openalex_id, fed)
    parse_papers(raw, fed)
  })

papers <- bind_rows(papers_raw)

message("\nTotal papers pulled: ", nrow(papers))
message("Papers per Fed:")
print(count(papers, fed, sort = TRUE))

dir.create("data", showWarnings = FALSE)
write_rds(papers, "data/fed_papers_raw.rds")
write_csv(papers |> select(-concepts, -keywords), "data/fed_papers_flat.csv")

message("Saved to data/fed_papers_raw.rds and data/fed_papers_flat.csv")
