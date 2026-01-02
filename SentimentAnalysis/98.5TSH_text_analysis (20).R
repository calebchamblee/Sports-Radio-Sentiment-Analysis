Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jre-8.0.452.9-hotspot")
install.packages("rJava")
library(rJava)
install.packages("openNLP")
library(openNLP)
options(timeout = 10000)
install.packages(
  "http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
  repos = NULL,
  type = "source"
)
library(openNLPmodels.en)
install.packages("remotes")
library(remotes)
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
remotes::install_github("trinker/entity")
library(entity)
install.packages("quanteda")
install.packages("readtext")
install.packages("text2vec")
library(tidyverse)
library(text2vec)
library(Matrix)
library(quanteda)
install.packages("quanteda.textstats")
library(quanteda.textstats)
install.packages("sentimentr")
install.packages("glmnet")
install.packages("tidymodels")
library(sentimentr)
library(glmnet)
library(tidymodels)
library(readtext)
install.packages("textclean")
library(textclean)
install.packages("text2map")
install.packages("udpipe")
install.packages("tidytext")
library(text2map)
install.packages("caret")
install.packages("quanteda.textmodels")
library(caret)
library(quanteda.textmodels)
library(udpipe)
library(tidytext)
remotes::install_gitlab("culturalcartography/text2map.pretrained")
library(text2map.pretrained)
install.packages("factoextra")
library(factoextra)
install.packages("topicmodels")
library(topicmodels)
install.packages("stm")
library(stm)
install.packages("topicdoc")
library(topicdoc)
install.packages("broom")
install.packages("marginaleffects")
library(broom)
library(marginaleffects)
install.packages("ggrepel")
library(ggrepel)
install.packages("irlba")
library(irlba)


# Function to process one transcript file
process_transcript <- function(path) {
  ep_date <- sub(".*-(\\d{1,2}-\\d{1,2}-\\d{2})\\.txt$", "\\1", path)
  show <- sub("^(.*?/)*([^/-]+).*", "\\2", path)
  
  txt <- readtext(path)
  txt <- paste(txt, collapse = "\n")
  
  # Split on ## labels (speakers of interest) or "Speaker X" (others)
  parts <- unlist(strsplit(txt, "(?m)(?=^##|^Speaker )", perl = TRUE))
  parts <- parts[parts != ""]
  
  # Extract raw speaker label
  raw_speaker <- sub(":.*", "", parts)
  # Clean out leading ##
  clean_speaker <- trimws(gsub("^#+", "", raw_speaker))
  
  # Valid host list (adjust as needed)
  hosts <- c("Hardy", "Toucher", "Wallach", "Felger", "Mazz", "Big Jim", "Bedard")

  # Assign "other" if not a host (if it starts with "Speaker")
  speaker <- ifelse(
    clean_speaker %in% hosts, 
    clean_speaker,
    "other"
  )
  
  # Extract text
  texts <- gsub("^([^:]+):\\s*", "", parts)
  
  df <- data.frame(
    speaker = speaker,
    text = texts,
    show = show,
    stringsAsFactors = FALSE
  ) |>
    group_by(speaker) |>
    summarise(all_words = paste(text, collapse = " "), 
              show = first(show), .groups = "drop") |>
    mutate(speaker_date = paste0(speaker, "_", ep_date))
  
  df <- df |> filter(speaker != "other")
  return(df)
}

process_utterances <- function(path) {
  ep_date <- sub(".*-(\\d{1,2}-\\d{1,2}-\\d{2})\\.txt$", "\\1", path)
  show <- sub("^(.*?/)*([^/-]+).*", "\\2", path)
  
  txt <- readtext(path)
  txt <- paste(txt, collapse = "\n")
  
  # Split on ## or Speaker
  parts <- unlist(strsplit(txt, "(?m)(?=^##|^Speaker )", perl = TRUE))
  parts <- parts[parts != ""]
  
  # Extract raw speaker label
  raw_speaker <- sub(":.*", "", parts)
  # Clean out leading ##
  clean_speaker <- trimws(gsub("^#+", "", raw_speaker))
    
  # Valid host list (adjust as needed)
  hosts <- c("Hardy", "Toucher", "Wallach", "Felger", "Mazz", "Big Jim", "Bedard")
  
  # Assign "other" if not a host (if it starts with "Speaker")
  speaker <- ifelse(
    clean_speaker %in% hosts, 
    clean_speaker,
    "other"
  )
  
  # Extract text
  utterance <- gsub("^([^:]+):\\s*", "", parts)
  
  df <- data.frame(
    speaker = speaker,
    utterance = utterance,
    show = show,
    stringsAsFactors = FALSE
  ) |> mutate(speaker_date = paste0(speaker, "_", ep_date))
  
  return(df)
}


# Keep adding for every new file
files <- c(
  "C:/Users/cchamb03/Box/F&M-1-19-21.txt",
  "C:/Users/cchamb03/Box/F&M-9-13-21.txt",
  "C:/Users/cchamb03/Box/F&M-6-17-22.txt",
  "C:/Users/cchamb03/Box/F&M-5-22-23.txt",
  "C:/Users/cchamb03/Box/F&M-5-30-23.txt",
  "C:/Users/cchamb03/Box/F&M-7-26-23.txt",
  "C:/Users/cchamb03/Box/F&M-12-13-23.txt",
  "C:/Users/cchamb03/Box/F&M-4-22-24.txt",
  "C:/Users/cchamb03/Box/F&M-6-18-24.txt",
  "C:/Users/cchamb03/Box/F&M-10-24-24.txt",
  "C:/Users/cchamb03/Box/F&M-1-30-25.txt",
  "C:/Users/cchamb03/Box/F&M-2-10-25.txt",
  "C:/Users/cchamb03/Box/F&M-3-5-25.txt",
  "C:/Users/cchamb03/Box/F&M-5-13-25.txt",
  "C:/Users/cchamb03/Box/F&M-8-7-25.txt",
  "C:/Users/cchamb03/Box/F&M-9-23-25.txt",
  "C:/Users/cchamb03/Box/F&M-10-13-25.txt",
  "C:/Users/cchamb03/Box/F&M-11-18-25.txt",
  "C:/Users/cchamb03/Box/T&H-5-16-24.txt",
  "C:/Users/cchamb03/Box/T&H-7-16-24.txt",
  "C:/Users/cchamb03/Box/T&H-9-16-24.txt",
  "C:/Users/cchamb03/Box/T&H-10-21-24.txt",
  "C:/Users/cchamb03/Box/T&H-11-18-24.txt",
  "C:/Users/cchamb03/Box/T&H-12-5-24.txt",
  "C:/Users/cchamb03/Box/T&H-1-14-25.txt",
  "C:/Users/cchamb03/Box/T&H-2-10-25.txt",
  "C:/Users/cchamb03/Box/T&H-3-10-25.txt",
  "C:/Users/cchamb03/Box/T&H-6-2-25.txt",
  "C:/Users/cchamb03/Box/T&H-8-18-25.txt",
  "C:/Users/cchamb03/Box/T&H-9-8-25.txt",
  "C:/Users/cchamb03/Box/T&H-10-6-25.txt",
  "C:/Users/cchamb03/Box/T&H-11-3-25.txt"
)

all_eps_full <- lapply(files, process_transcript)
all_eps_utter <- lapply(files, process_utterances)
# Combine each episode's df into one
df <- bind_rows(all_eps_full)
df_utter <- bind_rows(all_eps_utter)
df <- df |> mutate(speaker = sub("_.*", "", speaker_date),
                   date = sub(".*_", "", speaker_date)) |>
  select(speaker_date, speaker, date, show, all_words)

df_utter <- df_utter |> mutate(speaker = sub("_.*", "", speaker_date),
                               date = sub(".*_", "", speaker_date),
                               utter_id = row_number()) |>
  select(utter_id, speaker, date, show, utterance) |> filter(speaker != "other")

# First transliterate names with NER on uncleaned text
people <- person_entity(df_utter$utterance)
persons <- unlist(people)
persons <- tolower(persons) |>
  gsub("[[:punct:]]", "", x = _) |>
  gsub("\\s+", " ", x = _) |>
  trimws()
name_freq <- table(persons) |> sort(decreasing = TRUE) |> as.data.frame()
colnames(name_freq) <- c("variant", "freq")

orgs <- organization_entity(df_utter$utterance)
all_orgs <- unlist(orgs)
all_orgs <- tolower(all_orgs) |>
  gsub("[[:punct:]]", "", x = _) |>
  gsub("\\s+", " ", x = _) |>
  trimws()
org_freq <- table(all_orgs) |> sort(decreasing = TRUE) |> as.data.frame()
colnames(org_freq) <- c("variant", "freq")
# Use these files to create dictionaries
write.csv(name_freq, "entity_variants_people.csv", row.names = FALSE)
write.csv(org_freq, "entity_variants_orgs.csv", row.names = FALSE)
# Get the dictionaries
source("C:/Users/cchamb03/Box/name_dict.R")
source("C:/Users/cchamb03/Box/org_dict.R")
source("C:/Users/cchamb03/Box/team_dict.R")

# Basic text cleaning
df <- df |> mutate(all_words = gsub("\n", " ", all_words),
                   all_words = gsub("[^A-Za-z0-9' ]", " ", all_words),
                   all_words = gsub("(?<![A-Za-z])'|'(?![A-Za-z])", " ", all_words, perl = TRUE),
                   all_words = gsub("\\s+", " ", all_words),
                   all_words = replace_number(all_words),
                   all_words = tolower(all_words))

df_utter <- df_utter |> mutate( utterance = gsub("\n", " ", utterance),
    utterance = gsub("[^A-Za-z0-9' ]", " ", utterance),
    utterance = gsub("(?<![A-Za-z])'|'(?![A-Za-z])", " ", utterance, perl = TRUE),
    utterance = gsub("\\s+", " ", utterance),
    utterance = replace_number(utterance),
    utterance = tolower(utterance))

#tokens gets back ALL words, not just unique
tokens <- quanteda::tokens(df$all_words)

# Annotate each term and lemmatize, make df with each term and its count
eng <- udpipe_download_model(language = "english")
ud_eng <- udpipe_load_model(eng$file_model)
df_udpipe <- udpipe_annotate(x = df$all_words, object = ud_eng, doc_id = df$speaker_date) |>
  as_tibble()
df_tagged <- df_udpipe |> filter(!upos %in% c("PUNCT", "SYM")) |>
  mutate(lemma_pos = paste0(lemma, "_", upos)) |>
  select(doc_id, lemma_pos) |>
  count(doc_id, lemma_pos)

# Make dtm, remove stopwords
dtm <- df_tagged |> cast_dfm(document = doc_id, term = lemma_pos, value = n)
colnames(dtm) <- gsub("_.*", "", colnames(dtm))
dtm <- dfm_remove(dtm, pattern = get_stoplist("snowball2014"))
# continue to add stop words as they come up, earlier
remove <- c("yeah", "like", "just", "dont", "e", "draftkings", "thats", "know", "one", "going", "think")
dtm <- dtm |> dtm_stopper(stop_list = remove)
dtm <- as.dfm(dtm)

# tfidf (basic word importance analysis)

# By Speaker-Date
dtmtfidf <- dfm_tfidf(dtm)
terms <- colnames(dtmtfidf)
top_words_speaker_date <- lapply(1:nrow(dtmtfidf), function(i) {
  row <- dtmtfidf[i, ]
  scores <- as.numeric(row)
  
  tibble(
    speaker_date = df$speaker_date[i],
    speaker = df$speaker[i],
    date = df$date[i],
    term = terms,
    tfidf = scores
  ) |>
    arrange(desc(tfidf)) |> slice_max(tfidf, n = 20)
})

top_words_speaker_date <- bind_rows(top_words_speaker_date)
top_words_speaker_date |>
  ggplot(aes(tfidf)) +
  geom_histogram(bins = 50) +
  theme_minimal()

# By Speaker
dtm_speaker <- dtm |>
  dfm_group(groups = df$speaker)
dtm_speaker_tfidf <- dfm_tfidf(dtm_speaker)
terms <- colnames(dtm_speaker_tfidf)

top_words_speaker <- lapply(
  1:nrow(dtm_speaker_tfidf),
  function(i) {
    scores <- as.numeric(dtm_speaker_tfidf[i, ])
    
    tibble(
      speaker = rownames(dtm_speaker_tfidf)[i],
      term    = terms,
      tfidf   = scores
    ) |>
      arrange(desc(tfidf)) |>
      slice_max(tfidf, n = 20)
  }
)

top_words_speaker <- bind_rows(top_words_speaker)
top_words_speaker |>
  ggplot(aes(tfidf)) +
  geom_histogram(bins = 50) +
  theme_minimal()

sim_speaker <- quanteda.textstats::textstat_simil(
  dtm_speaker_tfidf,
  margin = "documents",
  method = "cosine"
)

sim_matrix <- as.matrix(sim_speaker)


heatmap_df <- as.data.frame(as.table(sim_matrix))

ggplot(heatmap_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Speaker", y = "Speaker", fill = "Cosine\nSimilarity")


# By Date
dtm_date <- dtm |>
  dfm_group(groups = df$date)
dtm_date_tfidf <- dfm_tfidf(dtm_date)
terms <- colnames(dtm_date_tfidf)

top_words_date <- lapply(
  1:nrow(dtm_date_tfidf),
  function(i) {
    scores <- as.numeric(dtm_date_tfidf[i, ])
    
    tibble(
      date = rownames(dtm_date_tfidf)[i],
      term    = terms,
      tfidf   = scores
    ) |>
      arrange(desc(tfidf)) |>
      slice_max(tfidf, n = 20)
  }
)

top_words_date <- bind_rows(top_words_date)
top_words_date |>
  ggplot(aes(tfidf)) +
  geom_histogram(bins = 50) +
  theme_minimal()

# By Show
dtm_show <- dtm |>
  dfm_group(groups = df$show)
dtm_show_tfidf <- dfm_tfidf(dtm_show)
terms <- colnames(dtm_show_tfidf)

top_words_show <- lapply(
  1:nrow(dtm_show_tfidf),
  function(i) {
    scores <- as.numeric(dtm_show_tfidf[i, ])
    
    tibble(
      show  = rownames(dtm_show_tfidf)[i],
      term  = terms,
      tfidf = scores
    ) |>
      arrange(desc(tfidf)) |>
      slice_max(tfidf, n = 10)
  }
)

top_words_show <- bind_rows(top_words_show)

tcm <- fcm(tokens, context = "window", window = 10L, tri = FALSE)

# Clustering-- Hierarchical
# By speaker
dist_mat <- 1 - sim_matrix
hc <- hclust(as.dist(dist_mat), method = "average")
plot(hc, main = "Host Clustering via TF-IDF Cosine Similarity")

cluster1 <- c("Felger", "Big Jim", "Mazz")
cluster2 <- c("Toucher", "Hardy", "Wallach")
docvars(dtm_speaker, "speaker") <- docnames(dtm_speaker)
docvars(dtm_speaker, "cluster") <- ifelse(
  docvars(dtm_speaker, "speaker") %in% cluster1, "cluster1",
  ifelse(docvars(dtm_speaker, "speaker") %in% cluster2, "cluster2", NA)
)
dfm_clusters <- dtm_speaker[!is.na(docvars(dtm_speaker, "cluster")), ]
dfm_cluster_grouped <- dfm_group(dfm_clusters, 
                                 groups = docvars(dfm_clusters, "cluster"))
result_cluster1 <- textstat_keyness(dfm_cluster_grouped, target = "cluster1")
result_cluster2 <- textstat_keyness(dfm_cluster_grouped, target = "cluster2")
head(result_cluster1, n = 20)
head(result_cluster2, n = 20)

# By date
sim_date <- textstat_simil(
  dtm_date_tfidf,
  margin = "documents",
  method = "cosine"
)
sim_date_matrix <- as.matrix(sim_date)
dist_date <- as.dist(1 - sim_date_matrix)
hc_date <- hclust(dist_date, method = "average")
plot(
  hc_date,
  main = "Date Clustering via TF-IDF Cosine Similarity",
  xlab = "Date",
  sub = ""
)

# Get cluster keywords
cos_dist <- dist2(dtm, method = "cosine")
hier_cluster <- hcut(as.dist(cos_dist), k = 10, hc_method = "ward.D2")
# By speaker-date (bad)
fviz_dend(hier_cluster, cex = .5, k = 2, horiz = TRUE)

get_clust_words <- function(dtm, clusters, n_words) {
  prop <- colSums(dtm) / sum(dtm)
  k <- unique(clusters)
  words <- lapply(k, function(x) {
    sub <- dtm[clusters == x, , drop = FALSE]
    res <- colSums(sub) / sum(sub) - prop
    res <- names(sort(res, decreasing = TRUE))
    res <- paste(res[seq.int(n_words)], collapse = ", ")
    return(res)
  })
  return(data.frame(cluster = k, size = tabulate(clusters), top_words = unlist(words)))
}

get_clust_words(dtm, hier_cluster$cluster, n_words = 10)

# Sentiment analysis

df_utter <- df_utter |>
  mutate(
    sentiment = sentimentr::sentiment(utterance)$sentiment,
    date = date |>
      str_squish() |>
      str_replace_all("[–—−]", "-") |>
      as.Date(format = "%m-%d-%y")
  )

# General sentiment summaries

sent_by_speaker <- df_utter |>
  group_by(speaker) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    median_sent = median(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

sent_by_date <- df_utter |>
  group_by(date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

sent_by_show <- df_utter |>
  filter(show != "Other") |>
  group_by(show, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

# Org-level sentiment (using org_dict)

org_variants_dict <- tibble(
  canonical_org = names(org_dict),
  variants = org_dict
) |>
  unnest(variants) |>
  rename(variant = variants) |>
  mutate(
    canonical_org = str_squish(str_to_lower(canonical_org)),
    variant = str_squish(str_to_lower(variant))
  )

all_org_variants <- org_variants_dict$variant

find_orgs <- function(text) {
  text_low <- str_to_lower(text)
  matches <- all_org_variants[
    str_detect(text_low, paste0("\\b", all_org_variants, "\\b"))
  ]
  if (length(matches) == 0) character(0) else unique(matches)
}

df_org_mentions <- df_utter |>
  mutate(orgs = map(utterance, find_orgs)) |>
  unnest(orgs, keep_empty = FALSE) |>
  rename(variant = orgs) |>
  mutate(variant = str_squish(str_to_lower(variant))) |>
  left_join(org_variants_dict, by = "variant") |>
  filter(!is.na(canonical_org)) |>
  select(utter_id, speaker, show, date, sentiment, canonical_org)

org_sentiment <- df_org_mentions |>
  group_by(canonical_org) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    median_sent = median(sentiment, na.rm = TRUE),
    n_mentions = n(),
    n_utterances = n_distinct(utter_id),
    .groups = "drop"
  ) |>
  arrange(desc(mean_sent))

local_orgs <- c("patriots", "celtics", "red sox", "bruins")

org_sentiment_speaker <- df_org_mentions |>
  filter(canonical_org %in% local_orgs) |>
  group_by(speaker, canonical_org) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

org_sentiment_date <- df_org_mentions |>
  filter(canonical_org %in% local_orgs) |>
  group_by(date, canonical_org) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

# Player-level sentiment (using name_dict)

name_variants_dict <- tibble(
  canonical_person = names(name_dict),
  variants = name_dict
) |>
  unnest(variants) |>
  rename(variant = variants) |>
  mutate(
    canonical_person = str_squish(str_to_lower(canonical_person)),
    variant = str_squish(str_to_lower(variant))
  )

all_person_variants <- name_variants_dict$variant

find_people <- function(text) {
  text_low <- str_to_lower(text)
  matches <- all_person_variants[
    str_detect(text_low, paste0("\\b", all_person_variants, "\\b"))
  ]
  if (length(matches) == 0) character(0) else unique(matches)
}

df_people_mentions <- df_utter |>
  mutate(people = map(utterance, find_people)) |>
  unnest(people, keep_empty = FALSE) |>
  rename(variant = people) |>
  mutate(variant = str_squish(str_to_lower(variant))) |>
  left_join(name_variants_dict, by = "variant") |>
  filter(!is.na(canonical_person)) |>
  select(utter_id, speaker, show, date, sentiment, canonical_person)

player_sentiment <- df_people_mentions |>
  group_by(canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    n_utterances = n_distinct(utter_id),
    .groups = "drop"
  ) |>
  arrange(desc(mean_sent))

speaker_person_sent <- df_people_mentions |>
  group_by(speaker, canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

people_by_show <- df_people_mentions |>
  group_by(show, canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

# Team mapping (using team_dict to team_category)

team_lookup <- map2_df(
  team_dict,
  names(team_dict),
  ~ tibble(
    canonical_person = .x,
    team_category = .y
  )
) |>
  mutate(
    canonical_person = str_squish(str_to_lower(canonical_person)),
    team_category = str_squish(str_to_lower(team_category))
  )

df_people_mentions <- df_people_mentions |>
  left_join(team_lookup, by = "canonical_person")

people_team_sent <- df_people_mentions |>
  filter(!is.na(team_category)) |>
  group_by(team_category, canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

team_category_sentiment <- df_people_mentions |>
  filter(!is.na(team_category)) |>
  group_by(team_category) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    n_people = n_distinct(canonical_person),
    .groups = "drop"
  ) |>
  arrange(desc(mean_sent))

# Person-level sentiment over time (for top people)
top_people <- df_people_mentions |>
  count(canonical_person, sort = TRUE) |>
  filter(n >= 100) |>
  pull(canonical_person)

person_daily <- df_people_mentions |>
  filter(canonical_person %in% top_people) |>
  group_by(canonical_person, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

# Team sentiment by category

org_to_team <- tibble(
  canonical_org = names(org_dict),
  team_category = case_when(
    canonical_org == "patriots" ~ "patriots",
    canonical_org == "celtics" ~ "celtics",
    canonical_org == "red sox" ~ "redsox",
    canonical_org == "bruins" ~ "bruins",
    TRUE ~ NA_character_
  )
)

df_org_team <- df_org_mentions |>
  left_join(org_to_team, by = "canonical_org") |>
  filter(!is.na(team_category))

team_lookup <- map2_df(
  team_dict,
  names(team_dict),
  ~ tibble(
    canonical_person = .x,
    team_category = .y
  )
) |>
  mutate(
    canonical_person = str_squish(str_to_lower(canonical_person)),
    team_category = str_squish(str_to_lower(team_category))
  )

df_people_team <- df_people_mentions |>
  left_join(team_lookup, by = "canonical_person") |>
  filter(!is.na(team_category.y)) |>
  rename(team_category = team_category.y)

df_team_sent_all <- bind_rows(
  df_org_team |> select(speaker, date, sentiment, team_category),
  df_people_team |> select(speaker, date, sentiment, team_category)
)

main_cats <- c(
  "patriots","patriots_opponent",
  "celtics","celtics_opponents",
  "redsox","redsox_opponents",
  "bruins","bruins_opponenets"
)

df_team_sent_all <- df_team_sent_all |>
  filter(team_category %in% main_cats)

speaker_team_sent <- df_team_sent_all |>
  group_by(speaker, team_category) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    sd_sent = sd(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

team_daily_sent <- df_team_sent_all |>
  group_by(team_category, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(speaker_team_sent,
       aes(x = team_category, y = speaker, fill = mean_sent)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "gray90", midpoint = 0) +
  theme_minimal(base_size = 14)

ggplot(team_daily_sent,
       aes(x = date, y = mean_sent, color = team_category)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  facet_wrap(~ team_category, scales = "free_y") +
  theme_minimal(base_size = 14)

# Combine Speaker/Orgs

org_lookup_patch <- tribble(
  ~canonical_org, ~team_category,
  
  # Local teams
  "red sox",        "redsox",
  "bruins",         "bruins",
  
  # Patriots opponents
  "chiefs",         "patriots_opponents",
  "eagles",         "patriots_opponents",
  "bears",          "patriots_opponents",
  "raiders",        "patriots_opponents",
  "steelers",       "patriots_opponents",
  "ravens",         "patriots_opponents",
  "vikings",        "patriots_opponents",
  "falcons",        "patriots_opponents",
  "broncos",        "patriots_opponents",
  "commanders",     "patriots_opponents",
  "colts",          "patriots_opponents",
  "cowboys",        "patriots_opponents",
  "jets",           "patriots_opponents",
  "saints",         "patriots_opponents",
  
  # Celtics opponents
  "warriors",       "celtics_opponents",
  "heat",           "celtics_opponents",
  "lakers",         "celtics_opponents",
  "knicks",         "celtics_opponents",
  "bucks",          "celtics_opponents",
  "clippers",       "celtics_opponents",
  "pacers",         "celtics_opponents",
  "suns",           "celtics_opponents",
  "bulls",          "celtics_opponents",
  
  # Red Sox opponents
  "yankees",        "redsox_opponents",
  "dodgers",        "redsox_opponents",
  "mets",           "redsox_opponents",
  "marlins",        "redsox_opponents",
  "cardinals",      "redsox_opponents"
)

# Append patch to org_lookup
org_lookup <- org_lookup_patch


allowed_team_categories <- c(
  "patriots", "celtics", "redsox", "bruins",
  "patriots_opponents", "celtics_opponents",
  "redsox_opponents", "bruins_opponents",
  "other"
)

df_org_mentions_filtered <- df_org_mentions |>
  left_join(org_lookup, by = "canonical_org") |>
  filter(team_category %in% allowed_team_categories)

df_people_team_filtered <- df_people_team |>
  filter(team_category %in% allowed_team_categories)

team_org_sent <- df_org_mentions_filtered |>
  group_by(team_category) |>
  summarise(
    org_mean_sent = mean(sentiment, na.rm = TRUE),
    org_n_mentions = n(),
    .groups = "drop"
  )

team_player_sent <- df_people_team_filtered |>
  group_by(team_category) |>
  summarise(
    player_mean_sent = mean(sentiment, na.rm = TRUE),
    player_n_mentions = n(),
    .groups = "drop"
  )

team_sentiment_overall <- team_org_sent |>
  full_join(team_player_sent, by = "team_category") |>
  mutate(
    total_mentions = org_n_mentions + player_n_mentions,
    overall_sentiment = (org_mean_sent + player_mean_sent) / 2
  ) |>
  filter(team_category %in% allowed_team_categories)


team_sentiment_overall |>
  ggplot(aes(x = reorder(team_category, overall_sentiment),
             y = overall_sentiment,
             fill = overall_sentiment)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0
  ) +
  labs(
    title = "Overall Sentiment Toward Teams (Equal Org + Player Weight)",
    x = "Team Category",
    y = "Sentiment"
  ) +
  theme_minimal(base_size = 14)


# Sentiment change over time

team_org_daily <- df_org_mentions_filtered |>
  group_by(date, team_category) |>
  summarise(
    org_daily_mentions = n(),
    org_daily_sentiment = mean(sentiment, na.rm = TRUE),
    .groups = "drop"
  )

team_player_daily <- df_people_team_filtered |>
  group_by(date, team_category) |>
  summarise(
    player_daily_mentions = n(),
    player_daily_sentiment = mean(sentiment, na.rm = TRUE),
    .groups = "drop"
  )

team_daily_sentiment <- full_join(team_org_daily, team_player_daily,
                                  by = c("date", "team_category")) |>
  mutate(
    total_daily_mentions = org_daily_mentions + player_daily_mentions,
    daily_sentiment = (org_daily_sentiment + player_daily_sentiment) / 2
  )

team_daily_sentiment |>
  filter(total_daily_mentions > 0) |>
  ggplot(aes(x = date, y = daily_sentiment, color = team_category)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Team Sentiment Over Time (Daily)",
    y = "Sentiment",
    x = "Date"
  ) +
  theme_minimal()


team_daily_sentiment |>
  filter(total_daily_mentions > 0) |>
  ggplot(aes(x = date,
             y = daily_sentiment,
             size = total_daily_mentions,
             color = team_category)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Daily Team Mentions and Sentiment (Bubble Plot)",
    x = "Date",
    y = "Sentiment",
    size = "Mentions"
  ) +
  theme_minimal()


speaker_org_sent <- df_org_mentions_filtered |>
  group_by(speaker, team_category) |>
  summarise(
    org_mean_sent   = mean(sentiment, na.rm = TRUE),
    org_n_mentions  = n(),
    .groups = "drop"
  )

speaker_player_sent <- df_people_team_filtered |>
  group_by(speaker, team_category) |>
  summarise(
    player_mean_sent   = mean(sentiment, na.rm = TRUE),
    player_n_mentions  = n(),
    .groups = "drop"
  )

speaker_team_sentiment <- full_join(
  speaker_org_sent,
  speaker_player_sent,
  by = c("speaker", "team_category")
) |>
  mutate(
    org_n_mentions    = coalesce(org_n_mentions, 0L),
    player_n_mentions = coalesce(player_n_mentions, 0L),
    total_mentions    = org_n_mentions + player_n_mentions,
    # average over available sources (org, player)
    sources_used      = as.numeric(!is.na(org_mean_sent)) +
      as.numeric(!is.na(player_mean_sent)),
    combined_sentiment = (coalesce(org_mean_sent, 0) +
                            coalesce(player_mean_sent, 0)) /
      if_else(sources_used > 0, sources_used, NA_real_)
  )

speaker_bias_rankings <- speaker_team_sentiment |>
  filter(total_mentions >= 10) |>
  group_by(team_category) |>
  summarise(
    most_positive_speaker = speaker[which.max(combined_sentiment)],
    most_negative_speaker = speaker[which.min(combined_sentiment)],
    .groups = "drop"
  )

speaker_bias_rankings

speaker_team_sentiment |>
  filter(total_mentions >= 10) |>
  ggplot(aes(x = team_category,
             y = speaker,
             fill = combined_sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0
  ) +
  labs(
    title = "Speaker × Team Sentiment (Combined Org + Player)",
    x = "Team Category",
    y = "Speaker",
    fill = "Sentiment"
  ) +
  theme_minimal(base_size = 14)

speaker_team_sentiment |>
  filter(total_mentions >= 10) |>
  ggplot(aes(x = team_category,
             y = speaker,
             fill = total_mentions)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low  = "white",
    high = "blue"
  ) +
  labs(
    title = "Speaker × Team Mention Volume",
    x = "Team Category",
    y = "Speaker",
    fill = "Mentions"
  ) +
  theme_minimal(base_size = 14)

team_daily_sentiment <- full_join(
  team_org_daily,
  team_player_daily,
  by = c("date", "team_category")
) |>
  mutate(
    org_daily_mentions    = coalesce(org_daily_mentions, 0L),
    player_daily_mentions = coalesce(player_daily_mentions, 0L),
    total_daily_mentions  = org_daily_mentions + player_daily_mentions,
    daily_sentiment = case_when(
      org_daily_mentions > 0 & player_daily_mentions > 0 ~ 
        (org_daily_sentiment + player_daily_sentiment) / 2,
      org_daily_mentions > 0 ~ org_daily_sentiment,
      player_daily_mentions > 0 ~ player_daily_sentiment,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(daily_sentiment))


ggplot(team_daily_sentiment,
       aes(x = date, y = daily_sentiment, color = team_category)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal()

ggplot(team_daily_sentiment,
       aes(x = date,
           y = daily_sentiment,
           size = total_daily_mentions,
           color = team_category)) +
  geom_point(alpha = 0.7) +
  theme_minimal()

person_sentiment_base <- df_people_mentions |>
  group_by(canonical_person) |>
  summarise(
    mean_sent      = mean(sentiment, na.rm = TRUE),
    sd_sent        = sd(sentiment, na.rm = TRUE),
    range_sent     = max(sentiment, na.rm = TRUE) - min(sentiment, na.rm = TRUE),
    pos_count      = sum(sentiment > 0, na.rm = TRUE),
    neg_count      = sum(sentiment < 0, na.rm = TRUE),
    n_mentions     = n(),
    .groups = "drop"
  ) |>
  mutate(
    polarity_ratio = abs(pos_count - neg_count) / n_mentions,
    polarity_spread = (pos_count + neg_count) / n_mentions,
    controversial_index = sd_sent + range_sent
  ) |>
  filter(n_mentions >= 15, canonical_person != "felger", canonical_person != "murray")

top_controversial_people <- person_sentiment_base |>
  arrange(desc(controversial_index)) |>
  slice_head(n = 20)

speaker_person_variance <- df_people_mentions |>
  group_by(speaker, canonical_person) |>
  summarise(
    sd_sent = sd(sentiment, na.rm = TRUE),
    range_sent = max(sentiment, na.rm = TRUE) - min(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  mutate(
    controversial_index = sd_sent + range_sent
  ) |>
  filter(n_mentions >= 10)

speaker_most_controversial <- speaker_person_variance |>
  group_by(speaker) |>
  slice_max(order_by = controversial_index, n = 10) |>
  ungroup()


ggplot(top_controversial_people,
       aes(x = reorder(canonical_person, controversial_index),
           y = controversial_index)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal()

ggplot(person_sentiment_base,
       aes(x = sd_sent)) +
  geom_density(fill = "purple", alpha = 0.4) +
  theme_minimal()

# Day of Week

weekday_sentiment <- df_utter |>
  mutate(weekday = weekdays(date)) |>
  group_by(weekday) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

weekday_speaker_sentiment <- df_utter |>
  mutate(weekday = weekdays(date)) |>
  group_by(weekday, speaker) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

weekday_team_org <- df_org_mentions_filtered |>
  mutate(weekday = weekdays(date)) |>
  group_by(weekday, team_category) |>
  summarise(
    org_mean_sent = mean(sentiment, na.rm = TRUE),
    org_mentions = n(),
    .groups = "drop"
  )

weekday_team_player <- df_people_team_filtered |>
  mutate(weekday = weekdays(date)) |>
  group_by(weekday, team_category) |>
  summarise(
    player_mean_sent = mean(sentiment, na.rm = TRUE),
    player_mentions = n(),
    .groups = "drop"
  )

weekday_team_sentiment <- full_join(
  weekday_team_org,
  weekday_team_player,
  by = c("weekday", "team_category")
) |>
  mutate(
    org_mentions = coalesce(org_mentions, 0L),
    player_mentions = coalesce(player_mentions, 0L),
    total_mentions = org_mentions + player_mentions,
    combined_sentiment = case_when(
      org_mentions > 0 & player_mentions > 0 ~ 
        (org_mean_sent + player_mean_sent) / 2,
      org_mentions > 0 ~ org_mean_sent,
      player_mentions > 0 ~ player_mean_sent,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(combined_sentiment))

ggplot(weekday_sentiment,
       aes(x = reorder(weekday, mean_sent),
           y = mean_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal()

ggplot(weekday_team_sentiment,
       aes(x = weekday,
           y = combined_sentiment,
           color = team_category,
           group = team_category)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal()

speaker_weekday_sentiment <- df_utter |>
  mutate(weekday = weekdays(date)) |>
  group_by(speaker, weekday) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

ggplot(speaker_weekday_sentiment,
       aes(x = weekday, y = speaker, fill = mean_sent)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0
  ) +
  theme_minimal(base_size = 14)

raw_paths_filtered <- files[grepl("-(9|10|11|12)-\\d{1,2}-\\d{2}\\.txt$", files)]

team_results <- tibble(
  raw = raw_paths_filtered,
  date_str = sub(".*-(\\d{1,2}-\\d{1,2}-\\d{2})\\.txt$", "\\1", raw_paths_filtered),
  date = as.Date(date_str, format = "%m-%d-%y"),
  team_category = "patriots",
  outcome = NA_character_
) |>
  select(date, team_category, outcome)

team_results <- team_results |>
  mutate(
    outcome = case_when(
      date == as.Date("2021-09-13") ~ "loss",
      date == as.Date("2023-12-13") ~ "win",
      date == as.Date("2024-10-24") ~ "loss",
      date == as.Date("2025-09-23") ~ "loss",
      date == as.Date("2025-10-13") ~ "win",
      date == as.Date("2025-11-18") ~ "win",
      date == as.Date("2024-12-05") ~ "loss",
      date == as.Date("2024-09-16") ~ "loss",
      date == as.Date("2024-10-21") ~ "loss",
      date == as.Date("2024-11-18") ~ "loss",
      date == as.Date("2025-09-08") ~ "loss",
      date == as.Date("2025-10-06") ~ "win",
      date == as.Date("2025-11-03") ~ "win",
      TRUE ~ outcome
    )
  )

team_results <- team_results |>
  mutate(
    outcome_binary = if_else(outcome == "win", 1, 0)
  )

sentiment_outcomes <- team_daily_sentiment |>
  left_join(team_results, by = c("date", "team_category")) |>
  filter(!is.na(outcome_binary))

sentiment_vs_outcome_cor <- cor.test(
  sentiment_outcomes$daily_sentiment,
  sentiment_outcomes$outcome_binary
)

sentiment_outcome_summary <- sentiment_outcomes |>
  group_by(outcome) |>
  summarise(
    mean_sent = mean(daily_sentiment, na.rm = TRUE),
    n_days    = n(),
    .groups = "drop"
  )

sentiment_outcome_summary

sentiment_outcome_reg <- lm(
  daily_sentiment ~ outcome_binary,
  data = sentiment_outcomes
)

sentiment_outcome_reg

ggplot(sentiment_outcomes,
       aes(x = outcome,
           y = daily_sentiment,
           fill = outcome)) +
  geom_boxplot() +
  theme_minimal()

ggplot(sentiment_outcomes,
       aes(x = date,
           y = daily_sentiment,
           color = outcome)) +
  geom_point(size = 3) +
  geom_line() +
  theme_minimal()

patriots_daily_sent <- bind_rows(
  df_org_mentions |> 
    filter(canonical_org == "patriots") |> 
    group_by(date) |> 
    summarise(sent = mean(sentiment, na.rm = TRUE), .groups = "drop"),
  
  df_people_team |> 
    filter(team_category == "patriots") |> 
    group_by(date) |> 
    summarise(sent = mean(sentiment, na.rm = TRUE), .groups = "drop")
) |>
  group_by(date) |>
  summarise(patriots_sent = mean(sent, na.rm = TRUE), .groups = "drop")

patriots_sent_vs_outcome <- patriots_daily_sent |>
  inner_join(team_results, by = "date") |>
  mutate(
    outcome_binary = ifelse(outcome == "win", 1,
                            ifelse(outcome == "loss", 0, NA))
  )

patriots_outcome_cor <- cor.test(
  patriots_sent_vs_outcome$patriots_sent,
  patriots_sent_vs_outcome$outcome_binary
)

ggplot(patriots_sent_vs_outcome,
       aes(x = outcome,
           y = patriots_sent,
           fill = outcome)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    x = "Game Outcome",
    y = "Patriots-Only Daily Sentiment",
    title = "Patriots Sentiment on Win vs Loss Days"
  )


ggplot(patriots_sent_vs_outcome,
       aes(x = patriots_sent, y = outcome_binary)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_continuous(breaks = c(0,1), labels = c("Loss", "Win")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Patriots-Only Sentiment",
    y = "Game Outcome",
    title = "Patriots Sentiment vs Win/Loss"
  )

# Emotional volatility

speaker_daily_sentiment <- df_utter |>
  group_by(speaker, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

speaker_volatility_over_time <- speaker_daily_sentiment |>
  group_by(speaker) |>
  summarise(
    mean_daily_sent   = mean(mean_sent, na.rm = TRUE),
    sd_daily_sent     = sd(mean_sent, na.rm = TRUE),
    var_daily_sent    = var(mean_sent, na.rm = TRUE),
    range_daily_sent  = max(mean_sent, na.rm = TRUE) - min(mean_sent, na.rm = TRUE),
    n_days            = n(),
    .groups = "drop"
  )

show_volatility_over_time <- sent_by_show |>
  group_by(show) |>
  summarise(
    mean_daily_sent   = mean(mean_sent, na.rm = TRUE),
    sd_daily_sent     = sd(mean_sent, na.rm = TRUE),
    var_daily_sent    = var(mean_sent, na.rm = TRUE),
    range_daily_sent  = max(mean_sent, na.rm = TRUE) - min(mean_sent, na.rm = TRUE),
    n_days            = n(),
    .groups = "drop"
  )

daily_sentiment_volatility <- df_utter |>
  group_by(date) |>
  summarise(
    mean_sent     = mean(sentiment, na.rm = TRUE),
    sd_sent       = sd(sentiment, na.rm = TRUE),
    n_utterances  = n(),
    .groups = "drop"
  )

player_volatility_index <- person_sentiment_base |>
  select(
    canonical_person,
    mean_sent,
    sd_sent,
    range_sent,
    controversial_index,
    n_mentions
  )

team_volatility_index <- team_daily_sentiment |>
  group_by(team_category) |>
  summarise(
    mean_daily_sent   = mean(daily_sentiment, na.rm = TRUE),
    sd_daily_sent     = sd(daily_sentiment, na.rm = TRUE),
    range_daily_sent  = max(daily_sentiment, na.rm = TRUE) - min(daily_sentiment, na.rm = TRUE),
    n_days            = n(),
    .groups = "drop"
  ) |>
  mutate(
    volatility_index = sd_daily_sent + range_daily_sent
  )


overall_rolling_volatility <- daily_sentiment_volatility |>
  arrange(date) |>
  mutate(
    rolling_sd_7 = slide_dbl(
      mean_sent,
      ~ sd(.x, na.rm = TRUE),
      .before = 6,
      .complete = TRUE
    )
  )

team_rolling_volatility <- team_daily_sentiment |>
  arrange(team_category, date) |>
  group_by(team_category) |>
  mutate(
    rolling_sd_7 = slide_dbl(
      daily_sentiment,
      ~ sd(.x, na.rm = TRUE),
      .before = 6,
      .complete = TRUE
    ),
    rolling_mean_7 = slide_dbl(
      daily_sentiment,
      ~ mean(.x, na.rm = TRUE),
      .before = 6,
      .complete = TRUE)) |>
  ungroup()


team_sentiment_spikes <- team_daily_sentiment |>
  arrange(team_category, date) |>
  group_by(team_category) |>
  mutate(
    team_mean_sent = mean(daily_sentiment, na.rm = TRUE),
    team_sd_sent   = sd(daily_sentiment, na.rm = TRUE),
    z_sent         = (daily_sentiment - team_mean_sent) / team_sd_sent,
    lag_sent       = lag(daily_sentiment),
    diff_sent      = daily_sentiment - lag_sent,
    spike_z        = if_else(!is.na(z_sent) & abs(z_sent) >= 2, TRUE, FALSE),
    spike_diff     = if_else(!is.na(diff_sent) & abs(diff_sent) >= 0.5, TRUE, FALSE),
    any_spike      = spike_z | spike_diff
  ) |>
  ungroup()

ggplot(speaker_volatility_over_time,
       aes(x = reorder(speaker, sd_daily_sent),
           y = sd_daily_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Speaker Emotional Volatility (Daily SD)",
    x = "Speaker",
    y = "Daily Sentiment SD"
  )

ggplot(show_volatility_over_time,
       aes(x = reorder(show, sd_daily_sent),
           y = sd_daily_sent)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Show-Level Emotional Volatility (Daily SD)",
    x = "Show",
    y = "Daily Sentiment SD"
  )


ggplot(daily_sentiment_volatility,
       aes(x = date, y = sd_sent)) +
  geom_line(color = "purple", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Overall Daily Sentiment Volatility",
    y = "Sentiment SD",
    x = "Date"
  )

ggplot(team_volatility_index,
       aes(x = reorder(team_category, volatility_index),
           y = volatility_index,
           fill = volatility_index)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Team Emotional Volatility Index",
    x = "Team",
    y = "Volatility (SD + Range)"
  )

ggplot(overall_rolling_volatility,
       aes(x = date, y = rolling_sd_7)) +
  geom_line(color = "steelblue", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Overall Rolling 7-Day Sentiment Volatility",
    y = "Rolling SD (7-Day)",
    x = "Date"
  )

ggplot(team_rolling_volatility,
       aes(x = date, y = rolling_sd_7, color = team_category)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ team_category, scales = "free_y") +
  theme_minimal(base_size = 14) +
  guides(color = "none") +
  labs(
    title = "Rolling 7-Day Volatility by Team",
    y = "Rolling SD",
    x = "Date"
  )





# Significance Testing
mod <- glm(date ~ sentiment, family = binomial, data = df_utter)
tidy(mod)
df_mod <- df_mod |> mutate(emotionality = abs(sentiment))
mod <- glm(aug ~ emotionality, family = binomial, data = df_mod)
tidy(mod)

df_mod |>
  group_by(speaker) |>
  summarise(
    avg_sentiment = mean(sentiment),
    n = n()
  )

# Plot most commonly mentioned names
people_df <- tibble(name = unlist(people)) |> filter(!is.na(name)) |> count(name, sort = TRUE) |> filter(n >= 50)
people_df |> ggplot(aes(x = name, y = n)) +
  geom_col() +
  labs(x = "people", y = "count") +
  coord_flip()

# LDA Topic Modeling
opts <- list(seed = 61761)
# Try with different k vals
lda_corpus <- LDA(dtm, k = 9, control = opts)
terms(lda_corpus, 5)
df_terms <- tidy(lda_corpus, "beta")
df_top <- df_terms |> group_by(topic) |> slice_max(n = 10, beta) |>
  ungroup() |> arrange(topic, beta) |> mutate(order = row_number())

df_top |> filter(topic <= 4) |>
  ggplot(aes(order, beta)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylim(0, .013) +
  labs(x = NULL, y = "Probability of Term in Topic") +
  facet_wrap(~topic, scale = "free", ncol = 4) +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks = df_top$order, labels = df_top$term)

k_topic <- seq(4, 16, by = 2)
opts <- list(seed = 10001)

multi_lda <- function(k, dtm, opts) {
  print(k)
  return(LDA(dtm, k, control = opts))
}

lda_mods <- lapply(k_topic, multi_lda, dtm, opts)
diag_list <- lapply(lda_mods, topic_diagnostics, dtm, 10)
names(diag_list) <- k_topic
df_diag <- bind_rows(diag_list, .id = "K")
df_diag |> group_by(K) |> summarize(coherence = mean(topic_coherence),
                                    exclusivity = mean(topic_exclusivity)) |>
  ggplot(aes(coherence, exclusivity)) +
  geom_text(aes(label = K)) +
  labs(x = "Coherence", y = "Exclusivity")

# Highest exclusivity/coherence at 8
lda_corpus <- lda_mods[[3]]

#Get best and use to make new lda_corp
df_terms <- tidy(lda_corpus, "beta")
df_top <- df_terms |> group_by(topic) |> slice_max(n = 12, beta) |>
  ungroup() |> arrange(topic, beta) |> mutate(order = row_number())
# Show 4 topics
df_top |> filter(topic <= 4) |>
  ggplot(aes(order, beta)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylim(0, .013) +
  labs(x = NULL, y = "Probability of Term in Topic") +
  facet_wrap(~topic, scale = "free", ncol = 4) +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks = df_top$order, labels = df_top$term)

df_doc_topics <- tidy(lda_corpus, matrix = "gamma", document_names = rownames(dtm))

df_doc_topics |> mutate(topic = as.factor(topic)) |>
  ggplot(aes(topic, document, fill = gamma, label = round(gamma, 2))) +
  geom_tile(show.legend = FALSE, color = "white", alpha = .55) +
  geom_text() +
  labs(x = "Topics", y = "Speaker/Date")

# Sentiment X Topic Modeling
df_docs <- df_utter |> 
  mutate(doc_id = paste(speaker, date, sep = "_")) |>
  group_by(doc_id) |>
  summarise(text = paste(utterance, collapse = " "), .groups = "drop")

tokens_docs <- df_docs |> 
  mutate(text = tolower(text)) |>
  mutate(text = gsub("[^a-z\\s]", " ", text)) |>
  mutate(text = gsub("\\s+", " ", text))

corp_docs <- quanteda::corpus(tokens_docs, text_field = "text")
tok_docs <- quanteda::tokens(corp_docs, remove_punct = TRUE, remove_numbers = TRUE)
tok_docs <- quanteda::tokens_remove(tok_docs, stopwords("en"))
dtm_docs <- quanteda::dfm(tok_docs)
dtm_docs <- quanteda::dfm_trim(dtm_docs, min_termfreq = 5)

opts2 <- list(seed = 61761)
lda_long <- topicmodels::LDA(dtm_docs, k = 8, control = opts2)

df_long_terms <- tidy(lda_long, "beta")
df_long_top <- df_long_terms |> 
  group_by(topic) |> 
  slice_max(n = 12, beta) |> 
  ungroup() |> 
  arrange(topic, desc(beta))

df_long_top |>
  mutate(order = row_number()) |>
  ggplot(aes(x = reorder(term, beta), y = beta)) +
  geom_col(fill = "steelblue", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    x = NULL,
    y = "Probability (beta)",
    title = "Top Words per Topic (Long-Document LDA)"
  )


# Supervised Learning: INCLUDE ALL SPEAKERS
df_sentences <- df_utter |> 
  select(speaker, utterance) |> 
  mutate(
    sentence = tolower(utterance),
    sentence = gsub("[[:punct:]]+", " ", sentence),
    sentence = gsub("[[:digit:]]+", " ", sentence),
    sentence = str_squish(sentence),
    doc_id   = row_number(),
    felger   = ifelse(speaker == "Felger", 1, 0)
  ) |> 
  filter(sentence != "")

init <- initial_split(df_sentences, prop = 0.75, strata = speaker)
df_trn <- training(init)
df_test <- testing(init)

dtm_trn <- df_trn |> 
  dtm_builder(sentence, doc_id = doc_id) |> 
  dtm_stopper(stop_list = get_stoplist("snowball2014"))

dtm_trn <- dtm_trn[, sort(colnames(dtm_trn))]

dtm_test <- df_test |> 
  dtm_builder(sentence, doc_id = doc_id, vocab = colnames(dtm_trn))

dtm_trn <- normalize(dtm_trn)
dtm_test <- normalize(dtm_test)

fit <- cv.glmnet(
  x = dtm_trn,
  y = df_trn$felger,
  family = "binomial",
  intercept = FALSE
)

confusion.glmnet(
  fit,
  newx = dtm_test,
  newy = df_test$felger,
  s   = "lambda.min"
)

coef_tbl <- coef(fit, s = "lambda.min") |> 
  as.matrix() |> 
  as.data.frame() |> 
  rownames_to_column("term") |> 
  rename(weight = 2) |> 
  arrange(desc(abs(weight)))

head(coef_tbl, 20)


# Logistic regression

lr_fit <- textmodel_lr(
  x = as.dfm(dtm_trn),
  y = as.factor(df_trn$speaker)
)

lr_pred <- predict(
  lr_fit,
  newdata = as.dfm(dtm_test),
  type = "class"
)

confusionMatrix(lr_pred, as.factor(df_test$speaker))

lr_prob <- predict(
  lr_fit,
  newdata = as.dfm(dtm_test),
  type = "probability"
) |> 
  as.data.frame() |> 
  mutate(speaker = df_test$speaker)

lr_prob |> 
  pivot_longer(c(Felger, Mazz), names_to = "predicted") |> 
  ggplot(aes(value, fill = speaker)) +
  geom_density(alpha = 0.6) +
  labs(title = "Predicted Speaker Probability Distributions",
       x = "Predicted Probability")

# concept movers distance
dtm_cmd <- df_utter |> 
  mutate(doc_id = as.character(utter_id)) |> 
  dtm_builder(utterance, doc_id = doc_id)


terms_smart <- cbind(
  c("smart", "intelligent", "bright"),
  c("dumb", "stupid", "idiot")
)

dir_smart <- get_direction(terms_smart, wv)

terms_smart <- cbind(
  c("smart", "intelligent", "bright"),
  c("dumb", "stupid", "idiot")
)

dir_smart <- get_direction(terms_smart, wv)

cmd_out <- CMDist(
  dtm = dtm_cmd,
  cv  = dir_smart,
  wv  = wv,
  sens_interval = TRUE,
  n_iters = 50L
)

df_cmd <- df_utter |> 
  mutate(doc_id = as.character(utter_id)) |> 
  left_join(cmd_out, by = "doc_id")

df_cmd |> 
  ggplot(aes(x = sentiment, y = smart_pole, color = speaker)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  labs(x = "Sentiment", y = "Smartness Axis")

vocab <- intersect(rownames(wv), rownames(tcm))
tcm2  <- tcm[vocab, vocab]
wv2   <- wv[vocab, ]

retrofitter <- function(x, ref) {
  x <- find_transformation(x, method = "norm")
  ref <- find_transformation(ref, method = "norm")
  ref <- rsvd(ref, k = ncol(x))$v
  ref <- find_transformation(ref, x, method = "align")
  ref <- x + ref
  ref <- find_transformation(ref, method = "norm")
}

ret_wv <- retrofitter(wv2, tcm2)

terms_love <- cbind(c("love","like"), c("hate","despise"))
dir_love <- get_direction(terms_love, ret_wv)

cmd_love <- CMDist(dtm = dtm_cmd, cv = dir_love, wv = ret_wv)

df_love <- df_utter |> 
  mutate(doc_id = as.character(utter_id)) |> 
  left_join(cmd_love, by = "doc_id")

df_love |> 
  ggplot(aes(utter_id, love_pole, color = speaker)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE)



# COMBINE WITH TOPIC MODELING
# linear regression with date predicting sentiment, controlling by speaker
df_mod$date_num <- as.Date(df_mod$date, format = "%m-%d-%y") |> as.numeric()
lm(sentiment ~ date_num + speaker, data = df_mod) |> tidy()

df_md <- lm(sentiment ~ date_num + speaker, data = df_mod)
df_prd <- predictions(model = df_md, newdata = datagrid(date_num = 
                                                          seq(min(df_mod$date_num), max(df_mod$date_num), length.out = 100),
                                                        speaker = unique(df_mod$speaker)))

# Sentiment tends to increase over time for everyone
df_prd |> ggplot(aes(x = date_num, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  geom_line() +
  facet_wrap(~speaker) +
  labs(x = "Date as Number", y = "Average Sentiment") +
  scale_x_continuous(breaks = seq(min(df_mod$date_num), max(df_mod$date_num), by = 50))

# More topic modeling
# Can also do for other covariaties
df <- df |>
  mutate(date = as.Date(date, format = "%m-%d-%y"))

avg_sent <- df_utter |> 
  group_by(speaker, date) |> 
  summarise(avg_sentiment = mean(sentiment, na.rm = TRUE), .groups = "drop")

df_stm_meta <- df |> 
  left_join(avg_sent, by = c("speaker", "date")) |> 
  mutate(
    date_num = as.numeric(date),
    speaker = factor(speaker),
    show = factor(show),
    avg_sentiment = avg_sentiment
  )

custom_stoplist <- c(
  "go","get","got","getting","gets",
  "say","says","said","saying",
  "guy","guys","dude","man",
  "can","cant","cannot",
  "thing","things","stuff",
  "look","looked","looking",
  "mean","means","meant",
  "now","well","oh","yeah","yep","nope",
  "want","wanted","wants",
  "two","three","one","first","last",
  "year","years","day","days","game","games",
  
  "like","just","really","actually","literally",
  "right","left","back","front",
  "uh","um","uhh","umm","ok","okay",
  "think","thought","know","maybe","sure",
  
  "play","plays","played","playing",
  "team","teams",
  "good","great","bad",
  "make","makes","made","making",
  "time","times"
)

dtm <- dtm |> dtm_stopper(stop_list = custom_stoplist)

stm_model <- stm(
  documents = dtm,
  K = 10,
  data = df_stm_meta,
  prevalence = ~ speaker + show + date_num + avg_sentiment,
  verbose = FALSE
)

effect_sent <- estimateEffect(
  1:10 ~ avg_sentiment,
  stm_model,
  metadata = df_stm_meta,
  uncertainty = "Global"
)

plot(
  effect_sent,
  covariate = "avg_sentiment",
  topics = 1:10,
  method = "continuous",
  model = stm_model,
  xlab = "Sentiment",
  main = "Topic Prevalence as a Function of Sentiment"
)

effect_date <- estimateEffect(
  1:10 ~ date_num,
  stm_model,
  metadata = df_stm_meta,
  uncertainty = "Global"
)

plot(
  effect_date,
  covariate = "date_num",
  topics = 1:10,
  method = "continuous",
  model = stm_model,
  xlab = "Date (numeric)",
  main = "Topic Prevalence as a Function of Date"
)







# Top terms per topic
stm_terms <- tidy(stm_model, matrix = "beta")

stm_top <- stm_terms |>
  group_by(topic) |>
  slice_max(beta, n = 10, with_ties = FALSE) |>
  ungroup()

# Faceted topic chart
ggplot(stm_top,
       aes(x = reorder(term, beta), y = beta)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  theme_minimal(base_size = 14) +
  labs(
    x = NULL,
    y = "Probability (beta)",
    title = "Top Words per Topic (STM with Speaker/Show/Date/Sentiment)"
  )

topic_gamma <- tidy(
  stm_model,
  matrix = "gamma",
  document_names = rownames(dtm)
)

topic_gamma_meta <- topic_gamma |>
  left_join(df_stm_meta, by = c("document" = "speaker_date"))

topic_speaker <- topic_gamma_meta |>
  group_by(speaker, topic) |>
  summarise(mean_gamma = mean(gamma), .groups = "drop")

ggplot(topic_speaker,
       aes(x = factor(topic), y = speaker, fill = mean_gamma)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Topic × Speaker Heatmap",
    x = "Topic",
    y = "Speaker",
    fill = "Prevalence"
  ) +
  theme_minimal()

# bigrams

df_tidy <- df %>%
  select(speaker_date, all_words)


df_bigrams <- df_tidy %>%
  unnest_tokens(bigram, all_words, token = "ngrams", n = 2)

bigrams_separated <- df_bigrams %>%
  separate(bigram, c("w1", "w2"), sep = " ")

# remove any bigram containing a stopword you already removed
bigrams_filtered <- bigrams_separated %>%
  filter(!w1 %in% stoplist, !w2 %in% stoplist) %>%
  unite(bigram, w1, w2, sep = " ")

df_tokens <- df %>%
  unnest_tokens(bigram, all_words, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

dtm_mix <- df_tokens %>%
  count(speaker_date, bigram) %>%
  cast_dtm(speaker_date, bigram, n)

mat <- dtm_mix
vocab <- colnames(mat)

doclist <- apply(mat, 1, function(row) {
  idx <- which(row > 0)
  rbind(idx, row[idx])     # 1-based STM format
})

stm_bigram <- stm(
  documents = doclist,
  vocab = vocab,
  data = df_stm_meta,
  K = 10,
  prevalence = ~ speaker + show + date_num + avg_sentiment,
  verbose = FALSE
)

eff_sent <- estimateEffect(
  1:10 ~ avg_sentiment,
  stm_bigram,
  metadata = df_stm_meta,
  uncertainty = "Global"
)

plot(
  eff_sent,
  "avg_sentiment",
  method = "continuous",
  model = stm_bigram,
  printlegend = TRUE,
  xlab = "Sentiment",
  ylab = "Expected Topic Proportion",
  main = "Topic Prevalence as a Function of Sentiment (Bigram STM)"
)


# Word Embeddings

tokens_embed <- df_utter$utterance |>
  tolower() |>
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) |>
  tokens_select(stopwords("en"), selection = "remove")

tok_char <- as.list(tokens_embed)

it <- itoken(tok_char)

vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5)

vectorizer <- vocab_vectorizer(vocab)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

keep <- which(colSums(tcm) > 30)
tcm <- tcm[keep, keep]

weight_ppmi <- function(m) {
  total <- sum(m)
  p_ij  <- m / total
  
  p_i   <- rowSums(m) / total
  p_j   <- colSums(m) / total
  
  PMI <- log((p_ij + 1e-9) / ((p_i %*% t(p_j)) + 1e-12))
  PMI[PMI < 0] <- 0  # PPMI
  
  as(Matrix(PMI, sparse = TRUE), "dgTMatrix")
}

tcm_ppmi <- weight_ppmi(tcm)

sv <- irlba::irlba(tcm_ppmi, nv = 100)
wv <- sv$v
rownames(wv) <- rownames(tcm_ppmi)

cos_sim <- function(x, y) sim2(x, y, method = "cosine")

words_test <- intersect(c("celtic", "patriot", "sox", "player"), rownames(wv))

wv[words_test, , drop = FALSE]

# Pairwise sims
if ("player" %in% rownames(wv) && "celtic" %in% rownames(wv))
  print(cos_sim(wv["player", , drop=FALSE], wv["celtic", , drop=FALSE]))

if ("player" %in% rownames(wv) && "patriot" %in% rownames(wv))
  print(cos_sim(wv["player", , drop=FALSE], wv["patriot", , drop=FALSE]))

if (all(c("celtic","patriot","sox") %in% rownames(wv))) {
  
  analogy <- wv["celtic",] - wv["patriot",] + wv["sox",]
  sims    <- sim2(wv, matrix(analogy, ncol=1), method="cosine")[,1]
  
  print(sort(sims, decreasing=TRUE)[1:10])
}

focal <- intersect(c("celtic","patriot"), rownames(wv))

if (length(focal) == 2) {
  
  sims <- sim2(wv[focal, ], wv, method = "cosine")
  
  df_sims <- tibble(
    term  = rownames(wv),
    celt  = sims[1, ],
    pat   = sims[2, ],
    bias  = celt - pat,
    team  = if_else(bias > 0, "celtics", "patriots"),
    abs_bias = abs(bias)
  )
  
  df_sims |>
    group_by(team) |>
    slice_max(abs_bias, n = 30) |>
    mutate(term = fct_reorder(term, abs_bias)) |>
    ggplot(aes(term, abs_bias, fill = team)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~team, scales = "free_y") +
    theme_minimal(base_size = 14) +
    labs(title = "Embedding-Based Vocabulary Bias: Celtics vs Patriots",
         x = "Terms", y = "Bias Magnitude")
}


# PCA on GloVe matrix
pca <- prcomp(X, scale. = TRUE)

# Full coordinates
coords <- as.data.frame(pca$x[, 1:2])
names(coords)[1:2] <- c("PC1", "PC2")
coords$term <- glove_df$term

# Drop any rows with NA coords
coords <- coords[complete.cases(coords$PC1, coords$PC2), ]

# Choose a subset of words to label: the 50 furthest from origin
coords$dist2 <- coords$PC1^2 + coords$PC2^2
coords_labels <- coords |>
  dplyr::arrange(dplyr::desc(dist2)) |>
  dplyr::slice_head(n = 50)

# Plot: all points, labels only for coords_labels
ggplot(coords, aes(PC1, PC2)) +
  geom_point(alpha = 0.15) +
  ggrepel::geom_text_repel(
    data = coords_labels,
    aes(label = term),
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "GloVe Word Embeddings (PCA 2D Projection)",
    x = "PC1",
    y = "PC2"
  )

compare <- function(w1, w2) {
  sim2(
    glove_wv[w1, , drop = FALSE],
    glove_wv[w2, , drop = FALSE],
    method = "cosine"
  )[1, 1]
}

compare("patriots", "celtics")
compare("mac", "bill")
compare("tatum", "brown")
compare("belichick", "brady")

p_sim <- function(player) {
  sims <- sim2(
    glove_wv,
    glove_wv[player, , drop = FALSE],
    method = "cosine"
  )[,1]
  
  sort(sims, decreasing = TRUE)[1:15]
}

p_sim("tatum")
p_sim("brown")
p_sim("bill")
p_sim("mac")
p_sim("drake")

analogy <- function(a, b, c, wv = glove_wv, top = 10) {
  # Build analogy vector
  vec <- wv[a, ] - wv[b, ] + wv[c, ]

  # vec is a numeric vector. Make it 1 x D (one row, many cols)
  vec_mat <- matrix(vec, nrow = 1)

  # Compute cosine similarity
  sims <- sim2(
    wv,
    vec_mat,
    method = "cosine"
  )[,1]

  sort(sims, decreasing = TRUE)[1:top]
}
analogy("tatum", "celtics", "patriots")
analogy("brady", "patriots", "celtics")
analogy("mac", "patriots", "nba")
analogy("belichick", "coach", "nba")

closest_words <- function(term, n = 25) {
  sims <- sim2(glove_wv, glove_wv[term, , drop = FALSE], method = "cosine")[,1]
  sort(sims, decreasing = TRUE)[1:n]
}

closest_words("celtics", 25)
closest_words("patriots", 25)
closest_words("bruins", 25)
closest_words("sox", 25)


terms <- intersect(rownames(glove_wv), c("patriots","celtics"))

vecs <- glove_wv[terms, ]

sims <- sim2(vecs, glove_wv, method = "cosine")

df <- tibble(
  term = rownames(glove_wv),
  patriots_sim = sims["patriots",],
  celtics_sim  = sims["celtics",],
  bias = celtics_sim - patriots_sim
)

df |>
  filter(!term %in% stopwords("en")) |>
  slice_max(abs(bias), n = 30) |>
  mutate(term = fct_reorder(term, bias)) |>
  ggplot(aes(term, bias, fill = bias > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal(base_size = 14) +
  labs(title = "Embedding-Based Language Bias: Celtics vs Patriots",
       x = "Word", y = "Bias")


# Pre-trained
data("vecs_fasttext300_commoncrawl", package = "text2map.pretrained")
wv <- vecs_fasttext300_commoncrawl
vectors <- wv[rownames(wv) %in% colnames(dtm), ]

anchors <- data.frame(
  add = c("win", "good", "great", "winner", "smart", "smart"),
  sub = c("lose", "bad", "terrible", "loser", "stupid", "dumb")
)

win_loss <- get_direction(anchors = anchors, wv = vectors)
sim_dir <- sim2(win_loss, vectors, method = "cosine")
df_dir <- data.frame(win_pole = sim_dir["win_pole", ],
                     terms = colnames(sim_dir)) |>
  mutate(win_loss_label = ifelse(win_pole >= 0,
                                 "Positive", "Negative"),
         win_loss = abs(win_pole))

df_dir |> group_by(win_loss_label) |> slice_max(win_loss, n = 30) |>
  mutate(term = fct_reorder(terms, win_loss)) |>
  ggplot(aes(term, win_loss, fill = win_loss_label, label = terms)) +
  geom_col() +
  guides(fill = "none") +
  labs(x = NULL, y = "Cosine Similarity to Pole") +
  coord_flip() +
  facet_wrap(~win_loss_label, scale = "free")

win_cen <- get_centroid(anchors = anchors$add, wv = vectors)
sims <- sim2(vectors, win_cen, method = "cosine")
names(head(sort(sims[,1], decreasing = TRUE)))
