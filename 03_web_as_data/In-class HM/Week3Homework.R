###############################################################################
# Web Scraping + Google Scholar Tutorial: R (Penn State Faculty Example)
# Author: Jared Edgerton
#Question 1
#### Two scientific risks: sampling bias, and missing data. 
##Researchers should first mention the platforms they assess the data, and 
###based on some external sources, such as the core audience for this platform to clarity this bias. Useful website, using the Comscore platform to check the platform information.
##Second, for the missing data, researcher should save the raw data for further check in and replicate for other people. Also， when treating missing values, clearify how you cope with these missing values, treat it as 0, or replace it as N/A.

# install.packages(c("rvest", "dplyr", "ggplot2", "scholar", "stringr", "tibble"))
library(rvest)
library(dplyr)
library(ggplot2)
library(scholar)
library(stringr)
library(tibble)

# -----------------------------------------------------------------------------
# Part 1: Web Scraping (Wikipedia Warm-up + Penn State Faculty Pages)
# -----------------------------------------------------------------------------
# We will do web scraping in two stages:
#
# Note:
# - Department websites do not always have the same structure.
# - Some pages may have "Areas of Interest" while others have "Research Interests".


# Part 1B: Hard-code four Penn State faculty (social sciences broadly)
# -----------------------------------------------------------------------------
# These are the four faculty members we will use throughout the script.
# (We will repeat the same scraping steps for each person.)
##1
shen_name <- "Fuyuan Shen"
cha_name <- "Jiyoung Cha"
#3
hgz_name <- "Homero"
##4
heo_name <- "Yujin Heo"
##5
mbo_name <- "Mary Beth Oliver"
#6
holly_name <- "Holly Overton"
#7
hes_name <- "Heather Shoenbergerr"
#8
sss_name <- "Shyam Sundar"
#9
chi_name <- "Chris Skurka"
#10
liao_name <- "liao Liao"



# -----------------------------------------------------------------------------
# Part 2: Pulling Google Scholar Data (Citations Over Time)
# -----------------------------------------------------------------------------
# Goal:
# - For each professor, we will:
#   (1) Define the Google Scholar ID
#   (2) Pull a profile summary
#   (3) Pull publications (and view the first 5)
#   (4) Pull citation history by year
#   (5) Combine all citation histories into one table and plot them

# -----------------------------------------------------------------------------
# Step 1: Hard-code Google Scholar IDs
# -----------------------------------------------------------------------------
shen_scholar_id   <- "b5gwU-wAAAAJ"
cha_scholar_id  <- "Rzo4xm4AAAAJ"
hgz_scholar_id   <- "T3VspYkAAAAJ"
heo_scholar_id  <- "HEcTx_wAAAAJ"
mbo_scholar_id   <- "MGIJ8gMAAAAJ"
holly_scholar_id  <- "eKTly7IAAAAJ"
hes_scholar_id   <- "fLD0R7QAAAAJ"
sss_scholar_id  <- "KP-DwH0AAAAJ"
chi_scholar_id   <- "65Cg6DMAAAAJ"
liao_scholar_id  <- "LXZaIJ4AAAAJ"
# -----------------------------------------------------------------------------
# Step 2: Pull Google Scholar profiles (sequentially)
# -----------------------------------------------------------------------------
shen_profile <- get_profile(shen_scholar_id)
cha_profile <- get_profile(cha_scholar_id)
hgz_profile <- get_profile(hgz_scholar_id)
heo_profile <- get_profile(heo_scholar_id)
mbo_profile <- get_profile(mbo_scholar_id)
holly_profile <- get_profile(holly_scholar_id)
hes_profile <- get_profile(hes_scholar_id)
sss_profile <- get_profile(sss_scholar_id)
chi_profile <- get_profile(chi_scholar_id)
liao_profile <- get_profile(liao_scholar_id)
# --- build Google Scholar profile URLs ---
shen_url  <- paste0("https://scholar.google.com/citations?user=", shen_scholar_id, "&hl=en")
cha_url   <- paste0("https://scholar.google.com/citations?user=", cha_scholar_id, "&hl=en")
hgz_url   <- paste0("https://scholar.google.com/citations?user=", hgz_scholar_id, "&hl=en")
heo_url   <- paste0("https://scholar.google.com/citations?user=", heo_scholar_id, "&hl=en")
mbo_url   <- paste0("https://scholar.google.com/citations?user=", mbo_scholar_id, "&hl=en")
holly_url <- paste0("https://scholar.google.com/citations?user=", holly_scholar_id, "&hl=en")
hes_url   <- paste0("https://scholar.google.com/citations?user=", hes_scholar_id, "&hl=en")
sss_url   <- paste0("https://scholar.google.com/citations?user=", sss_scholar_id, "&hl=en")
chi_url   <- paste0("https://scholar.google.com/citations?user=", chi_scholar_id, "&hl=en")
liao_url  <- paste0("https://scholar.google.com/citations?user=", liao_scholar_id, "&hl=en")

# --- read HTML pages ---
shen_page  <- read_html(shen_url)
cha_page   <- read_html(cha_url)
hgz_page   <- read_html(hgz_url)
heo_page   <- read_html(heo_url)
mbo_page   <- read_html(mbo_url)
holly_page <- read_html(holly_url)
hes_page   <- read_html(hes_url)
sss_page   <- read_html(sss_url)
chi_page   <- read_html(chi_url)
liao_page  <- read_html(liao_url)

cat("\n------------------------------\n")
cat("Google Scholar Profile Summaries\n")
cat("------------------------------\n")

cat("\n", shen_name, "\n", sep = "")
print(shen_profile)
cat("\n", cha_name, "\n", sep = "")
print(cha_profile)
cat("\n", holly_name, "\n", sep = "")
print(holly_profile)
cat("\n", hgz_name, "\n", sep = "")
print(hgz_profile)
cat("\n", heo_name, "\n", sep = "")
print(heo_profile)
cat("\n", mbo_name, "\n", sep = "")
print(mbo_profile)
cat("\n", hes_name, "\n", sep = "")
print(hes_profile)
cat("\n", sss_name, "\n", sep = "")
print(sss_profile)
cat("\n", chi_name, "\n", sep = "")
print(chi_profile)
cat("\n", liao_name, "\n", sep = "")
print(liao_profile)



###interests
shen_keywords <- shen_page %>% html_elements("a.gsc_prf_inta") %>% html_text2()
cha_keywords  <- cha_page  %>% html_elements("a.gsc_prf_inta") %>% html_text2()
hgz_keywords <- hgz_page %>% html_elements("a.gsc_prf_inta") %>% html_text2()
mbo_keywords  <- mbo_page  %>% html_elements("a.gsc_prf_inta") %>% html_text2()

heo_keywords <- heo_page %>% html_elements("a.gsc_prf_inta") %>% html_text2()
sss_keywords  <- sss_page  %>% html_elements("a.gsc_prf_inta") %>% html_text2()
chi_keywords <- chi_page %>% html_elements("a.gsc_prf_inta") %>% html_text2()
hes_keywords  <- hes_page  %>% html_elements("a.gsc_prf_inta") %>% html_text2()

liao_keywords <- liao_page %>% html_elements("a.gsc_prf_inta") %>% html_text2()
holly_keywords  <- holly_page  %>% html_elements("a.gsc_prf_inta") %>% html_text2()

shen_keywords
cha_keywords
liao_keywords
sss_keywords
hes_keywords
hgz_keywords
holly_keywords
chi_keywords
heo_keywords
mbo_keywords

length(shen_keywords)
length(cha_keywords)
length(mbo_keywords)
length(chi_keywords)
length(hgz_keywords)
length(holly_keywords)
length(sss_keywords)
length(liao_keywords)
length(hes_keywords)
length(heo_keywords)

# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Step 4: Pull citation history (citations by year) and combine
# -----------------------------------------------------------------------------
shen_ct <- get_citation_history(shen_scholar_id) %>% mutate(name = shen_name)
cha_ct <- get_citation_history(cha_scholar_id) %>% mutate(name = cha_name)
hgz_ct <- get_citation_history(hgz_scholar_id) %>% mutate(name = hgz_name)
mbo_ct <- get_citation_history(mbo_scholar_id) %>% mutate(name = mbo_name)
hes_ct <- get_citation_history(hes_scholar_id) %>% mutate(name = hes_name)
chi_ct <- get_citation_history(chi_scholar_id) %>% mutate(name = chi_name)
holly_ct <- get_citation_history(holly_scholar_id) %>% mutate(name = holly_name)
heo_ct <- get_citation_history(heo_scholar_id) %>% mutate(name = heo_name)
liao_ct <- get_citation_history(liao_scholar_id) %>% mutate(name = liao_name)
sss_ct <- get_citation_history(sss_scholar_id) %>% mutate(name = sss_name)

citation_df <- bind_rows(shen_ct, cha_ct, cha_ct, hgz_ct, mbo_ct, hes_ct, chi_ct, holly_ct, heo_ct, liao_ct, sss_ct)

# Print the combined citation data
print(head(citation_df, 10))

# -----------------------------------------------------------------------------
# Step 5: Plot citations over time for each professor
# -----------------------------------------------------------------------------
ggplot(citation_df, aes(x = year, y = cites)) +
  geom_line(color = "black") +
  geom_point(size = 1.5) +
  facet_wrap(~ name, scales = "free_y") +
  labs(
    title = "Citation History by Faculty Member",
    x = "Year",
    y = "Citations"
  ) +
  theme_minimal()


# overlap & unique keywords
all_keywords <- c(
  shen_keywords,
  cha_keywords,
  liao_keywords,
  sss_keywords,
  hes_keywords,
  hgz_keywords,
  holly_keywords,
  chi_keywords,
  heo_keywords,
  mbo_keywords
)
keyword_freq <- sort(table(all_keywords), decreasing = TRUE)
top_common <- head(keyword_freq, 10)

common_df <- data.frame(
  keyword = names(top_common),
  freq = as.numeric(top_common)
)
ggplot(common_df, aes(x = reorder(keyword, freq), y = freq)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Common Overlapping Research Interests (Top 10)",
    x = "",
    y = "Number of Faculty Mentioning"
  )
##3 The most overlapped research interested are in media effects, health communication, communication technology and advertising. 
#Median citations per year for each professor
median_cites <- citation_df %>%
  group_by(name) %>%
  summarize(median_cites = median(cites, na.rm = TRUE), .groups = "drop")

print(median_cites)
## This is counted based on the observed years. The histogram on google profile showed there is no years gained 0 citation. Second, the code did not set any missing values as 0. 