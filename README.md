# Data Science Projects — AIUB (IDS Course)

Two end-to-end data science projects built in R, covering the full pipeline from raw data to insights.

---

## 📊 Mid-Term: Titanic Survival Analysis

**Goal:** Predict and understand survival patterns from the Titanic dataset.

**Pipeline:**
- Data cleaning: missing value imputation (median-by-group for Age, class-based median for Fare), outlier winsorization via IQR
- Feature engineering: `AgeGroup`, `Title` (extracted from names), `IsAlone`, `FamilySize`, `FarePerPerson`
- Encoding: one-hot encoding for Sex and Embarked, z-score normalization for continuous features
- Visualizations: survival heatmaps by Pclass × AgeGroup, density plots, correlation matrix, pie charts

**Dataset:** Titanic (train + test merged, ~1300 records)  
**Tools:** R, tidyverse, ggplot2, ggcorrplot, GGally, fastDummies

---

## 📰 Final: BBC News NLP Pipeline

**Goal:** Scrape, clean, and analyze BBC News articles across 5 categories using NLP.

**Pipeline:**
- Web scraping via RSS feeds (Business, Technology, Science, Health, Politics — 50 articles each)
- Text preprocessing: HTML stripping, lowercasing, stopword removal, stemming (SnowballC)
- Analysis: word frequency analysis, sentiment scoring (syuzhet/NRC), topic modeling (LDA via topicmodels)
- Visualizations: word clouds, category-level TF-IDF bar charts, sentiment breakdown

**Dataset:** 250 BBC News articles (`bbc_news_final.csv`)  
**Tools:** R, tidytext, tm, topicmodels, syuzhet, wordcloud, ggplot2, rvest

---

## 🗂 Structure
├── Mid/
│   ├── G01-mid-project.r         # Full analysis script
│   ├── titanic.csv               # Combined dataset
│   └── G01-mid-project-report.pdf
├── Final/
│   ├── G01 final-project(Text Mining).R    # BBC scraper
│   ├── `G01 final-project (NLP Tecniq).R   # NLP analysis
│   ├── bbc_news_final.csv                  # Scraped dataset
│   └── G01 final_project report 1.pdf
**Course:** Introduction to Data Science | AIUB | 2025
