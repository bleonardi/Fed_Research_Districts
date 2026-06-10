# Do Federal Reserve Banks Research What Their Districts Produce?

## Overview
A topic-modeling analysis of ~18,300 Federal Reserve working papers (2010–2024) to test whether each Fed's research output mirrors its district's economic specialization. Using keyATM with 10 seeded topics and BLS QCEW industry employment data, the project constructs per-Fed "alignment scores" — Pearson correlations between a Fed's research topic vector and its district's industry mix.

## Key Findings
*   **Midwest Feds** (St. Louis, Kansas City, Minneapolis) are most aligned — their research portfolios match their manufacturing, agriculture, and trade emphasis.
*   **Finance** has the strongest positive matched-pair correlation (r ≈ 0.6): New York and Philadelphia publish proportionally more finance research.
*   **Energy** is near zero — Dallas leads on energy employment but San Francisco's "energy" topics capture climate/grid research rather than oil & gas.
*   **Housing** is *negative*: Feds in high-construction districts are not the ones publishing the most housing research.

## Key Data Science Skills
*   **Topic Modeling:** keyATM (keyword-assisted topic model) with 10 seeded topics on 18k+ paper abstracts.
*   **Large-Scale API Scraping:** OpenAlex API for working paper metadata at scale.
*   **Geospatial Aggregation:** Mapping BLS QCEW county-level employment to Federal Reserve district boundaries.
*   **Correlation Analysis:** Matched-pair and cross-correlation matrices between research topics and industry sectors.

## Tech Stack
*   **R (keyATM, tidyverse, ggrepel, patchwork):** Topic modeling and visualization.
*   **Quarto:** Reproducible research document.

## Data Sources
*   **OpenAlex API:** Federal Reserve working papers, 2010–2024.
*   **BLS QCEW:** 2022 annual county-level employment data.
*   **Federal Reserve District Maps:** Official county-to-district crosswalks.
