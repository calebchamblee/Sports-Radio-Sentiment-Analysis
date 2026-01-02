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
    summarise(all_words = paste(text, collapse = " "), .groups = "drop") |>
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
# ADD SEPARATE COLS FOR SPEAKER AND DATE
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
  mutate(sentiment = sentiment(utterance)$sentiment)
# Map people to teams
source("C:/Users/cchamb03/Box/team_dict.R")

# General Trends

# By Speaker
sent_by_speaker <- df_utter |>
  group_by(speaker) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    median_sent = median(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

# By Date
df_utter <- df_utter |>
  mutate(date = as.Date(date, format = "%m-%d-%y"))


sent_by_date <- df_utter |>
  group_by(date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )

speaker_name <- "Felger"

df_speaker <- df_utter |>
  filter(speaker == speaker_name)


speaker_daily <- df_speaker |>
  group_by(date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_utterances = n(),
    .groups = "drop"
  )


ggplot(speaker_daily, aes(x = date, y = mean_sent)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  labs(
    title = paste0(speaker_name, " — Sentiment Over Time"),
    x = "Date",
    y = "Mean Sentiment (per day)"
  )



speaker_daily <- df_utter |>
  group_by(speaker, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(speaker_daily, aes(x = date, y = mean_sent, color = speaker)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1.1) +
  labs(
    title = "Sentiment Over Time by Speaker",
    x = "Date",
    y = "Mean Sentiment (per day)",
    color = "Speaker"
  )


# By Show
show_daily <- df_utter |>
  group_by(show, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    .groups = "drop"
  ) |> filter(show != "Other")

ggplot(show_daily, aes(x = date, y = mean_sent, color = show)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  labs(
    title = "Show-Level Sentiment Over Time",
    x = "Date",
    y = "Mean Sentiment"
  )

# Sentiment towards Orgs (using NER)
org_variants_dict <- tibble(
  canonical_org = names(org_dict),
  variants = org_dict
) |>
  unnest(variants) |>
  rename(variant = variants)
all_org_variants <- org_variants_dict$variant

# Function to find all org variants in an utterance
find_orgs <- function(text) {
  matches <- all_org_variants[str_detect(
    str_to_lower(text),
    paste0("\\b", all_org_variants, "\\b")
  )]
  if (length(matches) == 0) return(NA_character_)
  return(matches)
}

df_utter <- df_utter |>
  mutate(orgs_found = map_chr(utterance, ~ {
    out <- find_orgs(.x)
    if (all(is.na(out))) "" else paste(out, collapse = ", ")
  }))

df_org_mentions <- df_utter |>
  filter(orgs_found != "") |>
  select(utter_id, sentiment, orgs_found) |>
  mutate(orgs_found = str_split(orgs_found, ",\\s*")) |>
  unnest(orgs_found) |>
  rename(variant = orgs_found) |>
  left_join(org_variants_dict, by = "variant")
df_utter <- df_utter |> select(-orgs_found)

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

org_sentiment_filt <- org_sentiment |>
  filter(n_utterances >= 50)
ggplot(org_sentiment_filt,
       aes(x = reorder(canonical_org, mean_sent),
           y = mean_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Direct Organization-Level Sentiment",
    x = "Organization",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)


df_utter <- df_utter |>
  mutate(orgs_found = map_chr(utterance, ~ {
    out <- find_orgs(.x)
    if (all(is.na(out))) "" else paste(out, collapse = ", ")
  }))

df_org_mentions <- df_utter |>
  filter(orgs_found != "") |>
  mutate(orgs_found = str_split(orgs_found, ",\\s*")) |>
  unnest(orgs_found) |>
  rename(variant = orgs_found) |>
  left_join(org_variants_dict, by = "variant") |>
  filter(!is.na(canonical_org))

df_utter <- df_utter |> select(-orgs_found)

local <- c("patriots", "celtics", "red sox", "bruins")

org_sentiment_speaker <- df_org_mentions |>
  group_by(speaker, canonical_org) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  filter(canonical_org %in% local)

ggplot(org_sentiment_speaker,
       aes(x = reorder(canonical_org, mean_sent),
           y = mean_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ speaker, scales = "free_y") +
  labs(
    title = "Direct Organization-Level Sentiment by Speaker",
    x = "Organization",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)


org_sentiment_date <- df_org_mentions |>
  group_by(date, canonical_org) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  filter(canonical_org %in% local)

ggplot(org_sentiment_date,
       aes(x = date, y = mean_sent)) +
  geom_line(alpha = 0.5, color = "gray40") +
  geom_smooth(se = FALSE, linewidth = 1.2, color = "steelblue") +
  facet_wrap(~ canonical_org, scales = "free_y") +
  labs(
    title = "Direct Org-Level Sentiment Over Time (Boston Teams)",
    x = "Date",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)


# Sentiment Towards Players (NER)
name_variants_dict <- tibble(
  canonical_person = names(name_dict),
  variants = name_dict
) |>
  unnest(variants) |>
  rename(variant = variants) |>
  mutate(
    canonical_person = str_squish(tolower(canonical_person)),
    variant = str_squish(tolower(variant))
  )

all_person_variants <- name_variants_dict$variant

find_people <- function(text) {
  text_low <- str_to_lower(text)
  matches <- all_person_variants[
    str_detect(text_low, paste0("\\b", all_person_variants, "\\b"))
  ]
  if (length(matches) == 0) return(NA_character_)
  return(matches)
}

df_utter <- df_utter |>
  mutate(persons_raw = map_chr(utterance, ~ {
    out <- find_people(.x)
    if (all(is.na(out))) "" else paste(unique(out), collapse = ", ")
  }))

df_people_mentions <- df_utter |>
  filter(persons_raw != "") |>
  mutate(persons_raw = str_split(persons_raw, ",\\s*")) |>
  unnest(persons_raw) |>
  rename(variant = persons_raw) |>
  mutate(variant = str_squish(tolower(variant))) |>
  left_join(name_variants_dict, by = "variant") |>
  filter(!is.na(canonical_person))

df_utter <- df_utter |> select(-persons_raw)

player_sentiment <- df_people_mentions |>
  group_by(canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    n_utterances = n_distinct(utter_id),
    .groups = "drop"
  ) |>
  arrange(desc(mean_sent)) |>
  filter(n_utterances > 15)

ggplot(player_sentiment,
       aes(x = reorder(canonical_person, mean_sent),
           y = mean_sent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Sentiment Toward Players",
    x = "Player",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)

top_positive <- player_sentiment |>
  arrange(desc(mean_sent)) |>
  slice_head(n = 20)

top_negative <- player_sentiment |>
  arrange(mean_sent) |>
  slice_head(n = 20)

player_daily_speaker <- df_people_mentions |>
  group_by(speaker, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  )

ggplot(player_daily_speaker,
       aes(x = date, y = mean_sent, color = speaker)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1.1) +
  labs(
    title = "Sentiment Toward Players Over Time by Speaker",
    x = "Date",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)


speaker_person_sent <- df_people_mentions |>
  group_by(speaker, canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  filter(n_mentions >= 10)

top_per_speaker <- speaker_person_sent |>
  group_by(speaker) |>
  slice_max(mean_sent, n = 10)

bottom_per_speaker <- speaker_person_sent |>
  group_by(speaker) |>
  slice_min(mean_sent, n = 10)

ggplot(top_per_speaker,
       aes(x = reorder(canonical_person, mean_sent),
           y = mean_sent,
           fill = speaker)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ speaker, scales = "free_y") +
  labs(
    title = "Top People by Sentiment (Per Speaker)",
    x = "Person",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)


bottom_per_speaker <- speaker_person_sent |>
  group_by(speaker) |>
  slice_min(mean_sent, n = 10)

ggplot(bottom_per_speaker,
       aes(x = mean_sent,
           y = reorder_within(canonical_person, mean_sent, speaker),
           fill = speaker)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~ speaker, scales = "free_y") +
  labs(
    title = "Bottom People by Sentiment (Per Speaker)",
    x = "Mean Sentiment",
    y = "Person"
  ) +
  theme_minimal(base_size = 14)



people_by_show <- df_people_mentions |>
  group_by(show, canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  filter(n_mentions >= 25) 

people_by_show <- df_people_mentions |>
  group_by(show, canonical_person) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  filter(n_mentions >= 5)

top_per_show <- people_by_show |>
  group_by(show) |>
  slice_max(mean_sent, n = 10)

ggplot(top_per_show,
       aes(x = mean_sent,
           y = reorder_within(canonical_person, mean_sent, show),
           fill = show)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~ show, scales = "free_y") +
  labs(
    title = "Top People by Sentiment (Per Show)",
    x = "Mean Sentiment",
    y = "Person"
  ) +
  theme_minimal(base_size = 14)

ggplot(bottom_per_show,
       aes(x = mean_sent,
           y = reorder_within(canonical_person, mean_sent, show),
           fill = show)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~ show, scales = "free_y") +
  labs(
    title = "Bottom People by Sentiment (Per Show)",
    x = "Mean Sentiment",
    y = "Person"
  ) +
  theme_minimal(base_size = 14)

# Filter for people assoicated with a team
team_dict_df <- map2_df(
  team_dict,
  names(team_dict),
  ~ tibble(
    canonical_person = .x,
    team = .y
  )
)

people_by_show <- df_people_mentions |>
  left_join(team_dict_df, by = "canonical_person") |>
  filter(!is.na(team), team != "other") |>
  group_by(show, canonical_person, team) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop") |>
  filter(n_mentions >= 25)

top_per_show <- people_by_show |>
  group_by(show) |>
  slice_max(mean_sent, n = 10)

bottom_per_show <- people_by_show |>
  group_by(show) |>
  slice_min(mean_sent, n = 10)

p1 <- ggplot(top_per_show,
             aes(x = mean_sent,
                 y = reorder_within(canonical_person, mean_sent, show),
                 fill = show)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~ show, scales = "free_y") +
  labs(
    title = "Top Team-Mapped People by Sentiment (Per Show)",
    x = "Mean Sentiment", y = "Person") +
  theme_minimal(base_size = 14)

p2 <- ggplot(bottom_per_show,
             aes(x = mean_sent,
                 y = reorder_within(canonical_person, mean_sent, show),
                 fill = show)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~ show, scales = "free_y") +
  labs(
    title = "Bottom Team-Mapped People by Sentiment (Per Show)",
    x = "Mean Sentiment", y = "Person") +
  theme_minimal(base_size = 14)

bottom_per_show <- people_by_show |>
  group_by(show) |>
  slice_min(mean_sent, n = 10)

# Compare players on different teams
team_dict_df <- map2_df(
  team_dict,
  names(team_dict),
  ~ tibble(
    canonical_person = .x,
    team_category = .y
  )
)

people_team_sent <- df_people_mentions |>
  left_join(team_dict_df, by = "canonical_person") |>
  filter(!is.na(team_category))

team_category_sentiment <- people_team_sent |>
  group_by(team_category) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    n_people = n_distinct(canonical_person),
    .groups = "drop"
  ) |>
  arrange(desc(mean_sent))

team_category_sentiment

# By date
team_lookup <- tibble(
  canonical_person = unlist(team_dict),
  team_category = rep(names(team_dict), lengths(team_dict)))
df_people_mentions <- df_people_mentions |>
  left_join(team_lookup, by = "canonical_person")

daily_person_sent <- df_people_mentions |>
  filter(team_category != "other") |>
  group_by(canonical_person, date) |>
  summarise(
    mean_sent = mean(sentiment, na.rm = TRUE),
    n_mentions = n(),
    .groups = "drop"
  ) |>
  filter(n_mentions >= 10)

top_daily <- daily_person_sent |>
  group_by(date) |>
  slice_max(mean_sent, n = 1, with_ties = FALSE) |>
  mutate(type = "Top") |>
  ungroup()

bottom_daily <- daily_person_sent |>
  group_by(date) |>
  slice_min(mean_sent, n = 1, with_ties = FALSE) |>
  mutate(type = "Bottom") |>
  ungroup()

leaderboard <- bind_rows(top_daily, bottom_daily)

ggplot(leaderboard,
       aes(x = date, y = mean_sent, color = type)) +
  geom_point(size = 2.5) +
  geom_text(
    aes(label = canonical_person),
    vjust = -0.5,
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  labs(
    title = "Daily Sentiment Leaderboard",
    subtitle = "Top and Bottom Individual Each Day (mean sentiment per day)",
    x = "Date",
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 14)









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

# NEED A LOT MORE HERE

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

# Highest exclusivity/coherence at 12
lda_corpus <- lda_mods[[5]]

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

# Combine with other things

# Supervised Learning: INCLUDE ALL SPEAKERS
# Split by sentence
df_felger <- df_unclean |> filter(speaker == "Felger")
felger_sent <- df_felger$all_words |> get_sentences() |> unlist()
df_mazz <- df_unclean |> filter(speaker == "Mazz")
mazz_sent <- df_mazz$all_words |> get_sentences() |> unlist()

# Turn into data frames with speaker label
df_felger_sent <- data.frame(
  sentence = felger_sent,
  speaker = "Felger",
  stringsAsFactors = FALSE
)

df_mazz_sent <- data.frame(
  sentence = mazz_sent,
  speaker = "Mazz",
  stringsAsFactors = FALSE
)

# Combine into one data frame
df_sentences <- bind_rows(df_felger_sent, df_mazz_sent) |> mutate(sentence = tolower(sentence), 
                                                                  sentence = gsub("[[:punct:]]+", " ", sentence),
                                                                  sentence = gsub("[[:digit:]]+", " ", sentence),
                                                                  sentence = str_squish(sentence)) |>
  mutate(doc_id = row_number(),
         felger = ifelse(speaker == "Felger", TRUE, FALSE))

init <- initial_split(df_sentences, prop = 3/4, strata = "speaker")
df_trn <- training(init)
df_test <- testing(init)

dtm_trn <- df_trn |> dtm_builder(sentence, doc_id = doc_id) |> dtm_stopper(stop_list = get_stoplist("snowball2014"))
dtm_trn <- dtm_trn[, sort(colnames(dtm_trn))]
dtm_test <- df_test |> dtm_builder(sentence, doc_id = doc_id, vocab = colnames(dtm_trn))

dtm_trn <- normalize(dtm_trn)
dtm_test <- normalize(dtm_test)

fit <- cv.glmnet(x = dtm_trn, y = df_trn$felger, family = "binomial", intercept = FALSE)
confusion.glmnet(fit, newx = dtm_test,
                 newy = df_test$felger,
                 s = "lambda.min")

data("hash_sentiment_jockers_rinker", package = "lexicon")
dict <- data.frame(term = rownames(coef(fit)), weight = coef(fit)[, 1])
dict <- inner_join(dict, hash_sentiment_jockers_rinker, by = c("term" = "x"))
cor(dict$weight, dict$y)

# EXTENDED DEDUCTIVE

lr_fit <- textmodel_lr(x = as.dfm(dtm_trn), y = as.factor(df_trn$speaker))
lr_pred <- predict(lr_fit, newdata = as.dfm(dtm_test),type = "class")
confusionMatrix(lr_pred, as.factor(df_test$speaker))

feat_weights <- function(model, n) {
  return(coef(model) |> as.matrix() |>
           as.data.frame() |> rename(value = 1) |>
           rownames_to_column("term") |>
           select(term, value) |>
           slice_max(value, n = n, with_ties = FALSE))
}
# Nothing to remove
feat_weights(lr_fit, n = 5)

# Gradationally
lr_pred <- predict(lr_fit, newdata = as.dfm(dtm_test),type = "probability") |>
  as.data.frame()
lr_pred$speaker <- df_test$speaker
lr_pred |> select(speaker, Felger, Mazz) |>
  pivot_longer(!speaker) |> ggplot(aes(value, fill = speaker)) +
  geom_density() +
  labs(fill="Predicted Probability of Speaker") +
  theme(legend.title = element_text())

# Concept-Mover's Distance
download_pretrained("vecs_fasttext300_commoncrawl")
data("vecs_fasttext300_commoncrawl", package = "text2map.pretrained")
wv <- vecs_fasttext300_commoncrawl

dtm_2 <- df |> dtm_builder(all_words, doc_id = speaker_date)
terms = cbind(c("smart", "intelligent"), c("dumb", "stupid"))
dir_smart <- get_direction(terms, wv)
cmd <- CMDist(dtm = dtm_2, cv = dir_smart, wv = wv, sens_interval = TRUE, n_iters = 100L)

df_cmd <- df |> mutate(doc_id = as.character(speaker_date)) |>
  left_join(cmd, by = "doc_id")

df_cmd |> ggplot(aes(x = avg_sentiment, y = smart_pole)) +
  geom_errorbar(aes(x = avg_sentiment, y = smart_pole,
                    ymin = smart_pole_lower, ymax = smart_pole_upper),
                alpha = 0.5) +
  geom_point(aes(color = speaker), size = 2) +
  labs(x = "Average Sentiment", y = "Interaction with Smart")

vocab <- intersect(rownames(wv), rownames(tcm))
tcm <- tcm[vocab, vocab]
pre_wv <- wv[vocab, ]

retrofitter <- function(x, ref) {
  x <- find_transformation(x, method = "norm")
  ref <- find_transformation(ref, method = "norm")
  ref <- rsvd(ref, k = ncol(x))$v
  ref <- find_transformation(ref, x, method = "align")
  ref <- x + ref
  ref <- find_transformation(ref, method = "norm")
}

ret_wv <- retrofitter(pre_wv, tcm)

terms <- cbind(c("love", "like"), c("hate", "despise"))
dir_love <- get_direction(terms, ret_wv)

cmd <- CMDist(dtm = dtm_2, cv = dir_love, wv = ret_wv)
df_cmd <- left_join(df, cmd)

df_cmd <- df_cmd |> mutate(position = row_number())
df_cmd |> mutate(fill = case_when(love_pole > 0 ~ "To Love",
                                  love_pole < 0 ~ "From Love")) |>
  ggplot(aes(position, love_pole)) +
  geom_col(aes(fill = fill), width = 8) +
  geom_smooth(se = FALSE, alpha = 0.5) +
  facet_wrap(~speaker) +
  scale_fill_manual(values = c("black", "darkgrey"))


df_parse <- df |> select(speaker_date, all_words) |>
  rename(doc_id = speaker_date, text = all_words) |>
  mutate(
    doc_id = as.character(doc_id),
    text   = as.character(text)
  )

df_udpipe <- df_udpipe |> select(doc_id, sentence_id, token_id, token, lemma,
                               upos, dep_rel, head_token_id)
df_udpipe <- df_udpipe |> rename(pos = upos)
df$doc_id <- df$speaker_date

motifs <- extract_motifs(df_udpipe, entities = c("*"))
# CHANGE THIS UP
motifs <- motifs[c("actions", "characterizations")]
df_motifs <- bind_rows(motifs) |> pivot_longer(action:characterization) |>
  filter(Entity %in% c("I", "you")) |>
  left_join(df) |> left_join(df_cmd) |> na.omit()

df_motifs |> mutate(smart = smart_pole - min(smart_pole)) |>
  ggplot(aes(x = smart_pole, fill = Entity)) +
  geom_density(position = "fill", alpha = 0.7) +
  facet_grid(name ~ speaker) +
  labs(x = "Toward Smart")

# By speaker
cmd <- cmdscale(as.dist(1 - sim_matrix), k = 2)
plot(cmd, type = "n")
text(cmd, labels = rownames(cmd))

# By date
cmd_date <- cmdscale(dist_date, k = 2)
plot(
  cmd_date,
  type = "n",
  main = "CMD Map of Dates",
  xlab = "Dimension 1",
  ylab = "Dimension 2"
)
text(cmd_date, labels = rownames(cmd_date), cex = 0.7)
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
stm <- stm(documents = dtm, K = 10, data = df,
           prevalence = ~avg_sentiment, verbose = FALSE)
stm_beta <- tidy(stm, matrix = "beta") |> group_by(topic) |>
  slice_max(beta, n = 5, with_ties = FALSE) |>
  summarize(terms = paste0(term, collapse = ", "))
print(stm_beta, n = 10)

df_effs <- estimateEffect(c(3, 7) ~ avg_sentiment, stm, metadata = df)
df_effs <- get_effects(df_effs, variable = "avg_sentiment", type = "continuous") |>
  mutate(Topics = case_when(
    topic == 3 ~ "Patriots",
    topic == 7 ~ "Celtics"
  ))

df_effs |> ggplot(aes(x = value, y = proportion, fill = Topics)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(linetype = Topics)) +
  labs(x = "Sentiment")

# Will have to wait until shows from each month: now results are meaningless
#FIX
df <- df |> mutate(month = as.numeric(str_extract(date, "^[0-9]+")))
df <- df |> mutate(rank = case_when(
  month == 1 ~ "Jan",
  month == 2 ~ "Feb",
  month == 3 ~ "Mar",
  month == 4 ~ "Apr",
  month == 5 ~ "May",
  month == 6 ~ "June",
  month == 7 ~ "July",
  month == 8 ~ "Aug",
  month == 9 ~ "Sept",
  month == 10 ~ "Oct",
  month == 11 ~ "Nov",
  month == 12 ~ "Dec"
))
stm <- stm(documents = dtm, K = 10, data = df,
           prevalence = ~avg_sentiment, content = ~rank, verbose = FALSE)

# FIX
stm_beta <- tidy(stm, matrix = "beta") |> group_by(topic, term) |>
  slice_max(beta, n = 5, with_ties = FALSE) |>
  summarize(beta = mean(beta)) |> slice_max(beta, n = 5, with_ties = FALSE) |>
  summarize(terms = paste0(term, collapse = ", "))

stm_gamma <- tidy(stm, matrix = "gamma", document_names = rownames(dtm))
df_prev <- stm_gamma |> group_by(topic) |>
  summarize(prevalence = sum(gamma, na.rm = TRUE) * 50) |>
  arrange(desc(prevalence)) |> left_join(stm_beta, by = "topic") |>
  mutate(topic = paste0("Topic", str_pad(topic, 2, pad = "0")),
         topic = reorder(topic, prevalence))
df_prev |> ggplot(aes(topic, prevalence, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.5, size = 3) +
  coord_flip() +
  ylim(0, 110)

df_effs <- estimateEffect(c(3, 7) ~ rank, stm, metadata = df)
df_effs <- get_effects(estimates = df_effs,
                       variable = "rank", type = "pointestimate") |>
  mutate(Topics = case_when(
    topic == 3 ~ "Patriots",
    topic == 7 ~ "Celtics"),
    rank = factor(value, levels = c("Jan", "Feb", "Mar", "Apr", "May", "June",
                                    "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
  )

df_effs |> ggplot(aes(x = rank, y = proportion)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
  geom_point(size = 3) +
  facet_grid(~Topics) +
  coord_flip() +
  labs(x = "Rank", y = "Topic Proportion")

# Word Embeddings

tcm <- tcm[colSums(tcm) > 30, colSums(tcm) > 30]

weight_ppmi <- function(tcm) {
  diag(tcm) <- diag(tcm) + 1
  tcm <- tcm %*% diag(1 / diag(tcm))
  tcm <- log(tcm)
  tcm[tcm < 0] <- 0
  return(Matrix(tcm, sparse = TRUE))
}

tcm_ppmi <- weight_ppmi(tcm)

vec1 <- tcm_ppmi["player", , drop = FALSE]
vec2 <- tcm_ppmi["celtic", , drop = FALSE]
vec3 <- tcm_ppmi["patriot", , drop = FALSE]

sim2(vec1, vec2, method = "cosine")
sim2(vec1, vec3, method = "cosine")

svd <- svd(tcm_ppmi)
wv <- svd$v[, 1:100]
rownames(wv) <- rownames(tcm_ppmi)

vec1 <- wv["celtic", , drop = FALSE]
vec2 <- wv["patriot", , drop = FALSE]
vec3 <- wv["sox", , drop = FALSE]
vec_syn <- vec1 - vec2 + vec3
sim <- sim2(wv, vec_syn, method = "cosine")
sim[, 1] |> sort(decreasing = TRUE) |> head(n = 5)

focal <- c("celtic", "patriot")
vecs <- wv[focal, ]
sims <- sim2(vecs, wv, method  ="cosine")

df_sims <- data.frame(word1 = sims[1, ],
                      word2 = sims[2, ],
                      terms = colnames(sims))

df_plot <- df_sims |> mutate(team_bias = word1 - word2,
                             team = ifelse(team_bias > 0, "celtics", "patriots"),
                             team_bias = abs(team_bias))

df_plot |> group_by(team) |> slice_max(team_bias, n = 30) |>
  mutate(term = fct_reorder(terms, team_bias)) |>
  ggplot(aes(term, team_bias, fill = team, label = term)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~team, scale = "free")

# GloVe

tokens <- space_tokenizer(df$all_words)
iterate <- itoken(tokens)
voc <- create_vocabulary(iterate)
vocab_vec <- vocab_vectorizer(voc)
tcm <- create_tcm(iterate, vocab_vec, skip_grams_window = 5L)

g_model <- GlobalVectors$new(rank = 100, x_max = 20)


vectors <- g_model$fit_transform(tcm, n_iter = 50)
vectors <- vectors + t(g_model$components)
vectors_2d <- vectors |> as.data.frame() |>
  mutate(name = rownames(vectors)) |>
  filter(!name %in% get_stoplist("snowball2014")) |>
  select(!name)

wv_coords  <- svd(vectors_2d)$u
wv_coords <- wv_coords |> as.data.frame() |> select(V1, V2) |>
  mutate(name = rownames(vectors_2d))
wv_coords |> mutate(col = case_when(abs(V1) >= 0.03 ~ TRUE,
                                    abs(V2) >= 0.03 ~ TRUE,
                                    TRUE ~ FALSE),
                    lab = case_when(abs(V1) >= 0.03 ~ name,
                                    abs(V2) >= 0.03 ~ name,
                                    TRUE ~ NA_character_)) |>
  ggplot(aes(V1, V2)) +
  geom_point(aes(color = col), alpha = 0.1, show.legend = FALSE) +
  geom_text_repel(aes(label = lab), max.overlaps = Inf, show.legend = FALSE) +
  labs(x = "Dimension 1", y = "Dimension 2")

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
