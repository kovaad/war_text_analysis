##############################
##   Quantitext analysis   ##
##   Kovács Ádám József   ##
###########################


# preparatory steps -------------------------------------------------------

#clear environment
rm(list = ls())

#load packages
devtools::install_github("poltextlab/HunMineR")

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr,tidyverse, quanteda, quanteda.textstats, ggrepel,text2vec, topicmodels,ggfortify, kableExtra,ggwordcloud, lubridate, tidytext )

#check out custom theme
source("theme_adam.R")

#read in data
df <- read_csv("data/mn_df_all_final_version.csv")


# initial cleaning steps --------------------------------------------------

#replace na value in tabs column with "HáborúUkrajnában"
df <- df %>%
  mutate(tabs = ifelse(is.na(tabs), "HáborúUkrajnában", tabs))

#check for any possible na values in tibble 
to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

df |> filter(is.na(body))

#replace na value in body with ""
df <- df %>%
  mutate(body = ifelse(is.na(body), "", body))

#check out data
glimpse(df)

#create target variable of before and after the elections
#rename body for text
#remove any unnecessary whitespces
df <- df |> 
  mutate(
    label = ifelse(dates < ymd("2022-4-3"), "before", "after") 
  ) |> 
  rename(text = body) |> 
  mutate(
    text = stringr::str_trim(text),
    text = stringr::str_squish(text)
  )


# basic descriptives using quanteda ---------------------------------------

#turn to corpus
corpus_df <- corpus(df)

#add number of tokens to metadata
docvars(corpus_df, "tokens") <- summary(corpus_df, n =  nrow(df))$Tokens

#look at summary of length of articles
summary(docvars(corpus_df, "tokens"))

#drop articles with less than 10 words
#corpus_df <- corpus_subset(corpus_df, !(tokens <= 10))

#look at summary of length of articles again
summary(docvars(corpus_df, "tokens"))

summary(corpus_df) |> summarise( 
  min_wordc = min(Tokens))

#create subcorpuses
corpus_df_before <- quanteda::corpus_subset(corpus_df, label == "before")

corpus_df_after <- quanteda::corpus_subset(corpus_df, label == "after")

#create summary statistics based on subcorpuses on length of articles
sb <- summary(corpus_df_before) |> summarise( 
  mean_wordcount = mean(Tokens), 
  std_dev = sd(Tokens), 
  min_wordc = min(Tokens), 
  wordc_10 = quantile(Tokens,.1), 
  wordc_50 = quantile(Tokens,.5), 
  wordc_90 = quantile(Tokens,.9), 
  max_wordc = max(Tokens) 
)

sa <- summary(corpus_df_after) |> summarise( 
  mean_wordcount = mean(Tokens), 
  std_dev = sd(Tokens), 
  min_wordc = min(Tokens), 
  wordc_10 = quantile(Tokens,.1), 
  wordc_50 = quantile(Tokens,.5), 
  wordc_90 = quantile(Tokens,.9), 
  max_wordc = max(Tokens)
)

wordcount <- bind_rows(sb,sa)

rownames(wordcount) <- c("before", "after")

#create table
kable(wordcount,format="latex", digits = 3, col.names = c("Avg",
                                                          "Std",
                                                          "Min",
                                                          "10%", 
                                                          "Median", 
                                                          "90%", 
                                                          "Max"), caption = 'Number of words before and after') |> 
  kable_styling(latex_options = c("hold_position","striped"), font_size = 8)

#create dataframe from metadata
df_meta <- data.frame(docvars(corpus_df))

#get number of articles and average length of articles by date
df_meta <- df_meta %>%
  group_by(dates) %>% 
  summarise( 
    n_articles = n(),
    avg_tokens = mean(tokens)
  )

#plot of evolution of length of articles
arrows <- 
  tibble(
    x1 = c(ymd("2022-3-15"), ymd("2022-4-20")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(200, 460), 
    y2 = c(250, 500)
  )

ggplot(df_meta, aes(dates, avg_tokens)) +
  geom_line() +
  labs(
    y = "Average length of articles",
    x = NULL
  ) + 
  geom_smooth(method = loess, se = F) + 
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-3-12"), y = 170, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-5-1"), y = 450, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = 0.3) +
  scale_y_continuous(breaks = seq(0, 800, 100))
#theme_adam()

#plot number of articles in dataset over time
arrows <- 
  tibble(
    x1 = c(ymd("2022-3-13"), ymd("2022-4-23")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(55, 45), 
    y2 = c(65, 35)
  )

ggplot(df_meta, aes(dates, n_articles)) +
  geom_line() +
  labs(
    y = "Number of articles",
    x = NULL
  ) + 
  geom_smooth(method = loess, se = F) + 
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-3-18"), y = 50, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-5-7"), y = 45, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = 0.3) #+
  #theme_adam()



# data preprocessing ------------------------------------------------------

#set pattern hashtag to null so as not to preserve them
quanteda_options("pattern_hashtag" = NULL)

#load stopwords
custom_stopwords <- HunMineR::data_stopwords_extra

#data cleaning and creating dfm
dfm <- corpus_df %>% 
  tokens( 
    remove_punct = TRUE, 
    remove_symbols = TRUE, 
    remove_numbers = TRUE 
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  #tokens_wordstem(language = "hungarian") %>% 
  #filter out those that have less than 3 characters?
  dfm()

#data cleaning and creating grouped dfm
dfm_grouped <- corpus_df %>% 
  tokens( 
    remove_punct = TRUE, 
    remove_symbols = TRUE, 
    remove_numbers = TRUE 
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_remove(pattern = stopwords('english')) |> 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  #tokens_wordstem(language = "hungarian") %>% 
  dfm() |> 
  quanteda::dfm_group(label)

#data cleaning and creating dfm of texts before elections
dfm_before <- corpus_df_before %>% 
  tokens( 
    remove_punct = TRUE, 
    remove_symbols = TRUE, 
    remove_numbers = TRUE 
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_remove(pattern = stopwords('english')) |> 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  #tokens_wordstem(language = "hungarian") %>% 
  dfm()


#data cleaning and creating dfm of texts after elections
dfm_after <- corpus_df_after %>% 
  tokens( 
    remove_punct = TRUE, 
    remove_symbols = TRUE, 
    remove_numbers = TRUE 
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_remove(pattern = stopwords('english')) |> 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  #tokens_wordstem(language = "hungarian") %>% 
  dfm()

# word frequencies --------------------------------------------------------

#get words by group
freq <- dfm %>% 
  quanteda.textstats::textstat_frequency(
    n = 10,
    groups = docvars(dfm, 'label') 
  )

#before after word frequency
freq |> 
  mutate(group = factor(group, levels = c("before", "after"))) |> 
  ggplot(aes(x = tidytext::reorder_within(x=feature, 
                                          by=frequency, 
                                          within=group), 
             y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL,
       y = "Frequency") +
  facet_wrap(~group, scales = "free") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(breaks = seq(0, 4000, 500)) #+
  #theme_adam()

#wordcloud
textstat_frequency(dfm, 60, groups = label) %>%
  arrange(-frequency) %>%
  ggplot(aes(label = feature, size = frequency, colour = group)) +
  scale_size_area(max_size = 7) +
  geom_text_wordcloud(shape = "square", eccentricity = 0.4, 
                      nudge_x = 4.2, nudge_y = 0.5) +
  theme_minimal()

#get most frequent important words by group
result_keyness <- dfm_grouped %>% 
  quanteda.textstats::textstat_keyness(target = "before")

#plot
result_keyness %>% 
  quanteda.textplots::textplot_keyness(color = c("#00BFC4", "#F8766D"), n=8) +
  xlim(c(-130, 130)) +
  theme(legend.position = c(0.9,0.1)) 

# keyword in context ------------------------------------------------------

#NOT USED IN THE END

#menekült in context before
menekult_before <- corpus_df_before %>% 
  tokens() %>% 
  quanteda::kwic(
    pattern = "menekült", 
    valuetype = "glob",
    window = 3, 
    case_insensitive = TRUE
  )

menekult_before_pre_dfm <- corpus(menekult_before$pre) |> 
  tokens( 
    remove_punct = TRUE, 
    remove_symbols = TRUE #do not remove numbers here
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  dfm()

menekult_before_pre_dfm %>% 
  quanteda.textstats::textstat_frequency(
    n = 10
  )

#menekült in context after
menekult_after <- corpus_df_after %>% 
  tokens() %>% 
  quanteda::kwic(
    pattern = "menekült", 
    valuetype = "glob",
    window = 3, 
    case_insensitive = TRUE
  )

menekult_after_pre_dfm <- corpus(menekult_after$pre) |> 
  tokens( 
    remove_punct = TRUE, 
    remove_symbols = TRUE #do not remove numbers here
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  dfm()

menekult_after_pre_dfm %>% 
  quanteda.textstats::textstat_frequency(
    n = 10
  )


# sentiment analysis ------------------------------------------------------

#load sentiment dictionary
poltext_szotar <- HunMineR::dictionary_poltext

#look up words from dictionary in the dfm
szentiment <- quanteda::dfm_lookup(dfm, dictionary = poltext_szotar)

#add to metadata count of positive and negative words
docvars(corpus_df, "pos") <- as.numeric(szentiment[, 1])
docvars(corpus_df, "neg") <- as.numeric(szentiment[, 2])

#convert metadata to dataframe
df_sent <- quanteda::convert(corpus_df, to = "data.frame")

#calculate by date the sentiment score (normalized net daily sentiment)
df_sent <- df_sent %>%
  group_by(dates) %>% 
  summarise( 
    daily_pos = sum(pos),
    daily_neg = sum(neg),
    net_daily = daily_pos - daily_neg,
    net_daily_normalized = net_daily / sum(tokens)
  )

#helper tibble for arrows
arrows <- 
  tibble(
    x1 = c(ymd("2022-2-25"), ymd("2022-4-23")),
    x2 =  c(ymd("2022-3-15"),ymd("2022-4-3")),
    y1 = c(0.02, 0.017), 
    y2 = c(0.016, 0.017)
  )

#plot
ggplot(df_sent, aes(dates, net_daily_normalized)) +
  geom_line() +
  labs(
    y = "Sentiment score",
    x = NULL
  ) + 
  geom_smooth(method = loess, se = T) +
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-3-15"), y = 0.014, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-4-23"), y = 0.02, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3) #+
  #theme_adam()


# word embeddings ---------------------------------------------------------

#create tokens from texts only before 
before_tokens <- corpus_df_before |> 
  tokens(
    remove_numbers = TRUE, 
    remove_punct = TRUE, 
    remove_separators = TRUE
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_select(pattern = custom_stopwords, selection = "remove")

#trim dataset to include only terms that appeared at least 50 times
features_before <- dfm(before_tokens) %>%
  dfm_trim(min_termfreq = 50) %>%
  quanteda::featnames()

#get tokens that made the cut
before_tokens <- tokens_select(before_tokens, features_before, padding = TRUE)

#create feature co-occurence matrix
before_fcm <- quanteda::fcm(before_tokens, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)

#set parameters of glove
glove <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = 0.1)

#fit and transform glove on fcm
before_main <- glove$fit_transform(before_fcm, n_iter = 10, convergence_tol = 0.1)
#get components of resulting object
before_context <- glove$components

#add up the two resulting documents to get the final word vectors
before_word_vectors <- before_main + t(before_context)

#look at top features
topfeatures(before_fcm, 20)

#conduct principal component analysis
before_pca <- prcomp(before_word_vectors, center = TRUE,scale. = TRUE)

#create dataframe
before_embedding_df <- as.data.frame(before_pca$x[,c(1,2)]) %>% 
  tibble::rownames_to_column(var = "words")

#conduct the very same steps on the articles after the elections
after_tokens <- corpus_df_after |> 
  tokens(
    remove_numbers = TRUE, 
    remove_punct = TRUE, 
    remove_separators = TRUE
  ) %>% 
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("hungarian")) %>% 
  tokens_select(pattern = custom_stopwords, selection = "remove")

features_after <- dfm(after_tokens) %>%
  dfm_trim(min_termfreq = 3) %>%
  quanteda::featnames()

after_tokens <- tokens_select(after_tokens, features_after, padding = TRUE)

after_fcm <- quanteda::fcm(after_tokens, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)

glove <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = 0.1)

after_main <- glove$fit_transform(after_fcm, n_iter = 10, convergence_tol = 0.1)

after_context <- glove$components

after_word_vectors <- after_main + t(after_context)

topfeatures(after_fcm, 20)

after_pca <- prcomp(after_word_vectors, center = TRUE,scale. = TRUE)

after_embedding_df <- as.data.frame(after_pca$x[,c(1,2)]) %>% 
  tibble::rownames_to_column(var = "words")

#create a function that plots the word embeddings of selected keywords
embedding_plot <- function(data, keywords, breaks_x, breaks_y) {
  data %>% 
    filter(words %in% keywords) %>% 
    ggplot(aes(PC1, PC2, label = words)) +
    labs(
      x = "First dimension",
      y = "Second dimension"
    ) +
    geom_text()+ 
    scale_x_continuous(expand = expansion(add = 1), 
                       breaks = breaks_x) +
    scale_y_continuous(expand = expansion(add = 1), 
                       breaks = breaks_y)
}

#create character vector of selected words
words_selected <- c("menekült", 'humanitárius', 'háború', 'zelenszkij','orbán', 'katonai', 'márki-zay', 'putyin') 

#create embedding plot from before elections articles
embedding_plot(data = before_embedding_df, keywords = words_selected, breaks_x = seq(-8, 5, 2), breaks_y = seq(-8, 5, 2)) #+ theme_adam()

#create embedding plot from after elections articles
embedding_plot(data = after_embedding_df, keywords = words_selected, breaks_x = seq(-2, 8, 1), breaks_y = seq(-1, 1, 1))# + theme_adam()

# topic modelling ---------------------------------------------------------

#conduct LDA using Gibbs method with 9 topics on articles before elections
gibbs_before <- LDA(dfm_before, k = 9, method = "Gibbs", control = list(seed = 1234))

#conduct LDA using Gibbs method with 9 topics  on articles after elections
gibbs_after <- LDA(dfm_after, k = 9, method = "Gibbs", control = list(seed = 1234))

#get topics before elecitons
topics_before <- tidy(gibbs_before, matrix = "beta") %>%
  mutate(label = "előtte")

#get topics after elections
topics_after <- tidy(gibbs_after, matrix = "beta") %>%
  mutate(label = "utána")

#put it into one dataframe
lda_gibbs <- bind_rows(topics_before, topics_after)

#get top 5 terms by topics
top_terms_gibbs <- lda_gibbs %>%
  group_by(label, topic) %>%
  top_n(10, beta) %>%
  top_n(10, term) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_gibbs$topic <- as.factor(top_terms_gibbs$topic)

levels(top_terms_gibbs$topic) <- c("sg1", "menekült ", "sg3", "menekült","USA ","USA","sg5", "belpolitika", "gazdaság"  )

#plot them for articles before the elections
top_terms_gibbs %>%
  #filter(label == "előtte" & topic %in% c(4,6,8,9)) %>%
  filter(label == "előtte" & topic %in% c("menekült","USA","belpolitika","gazdaság")) %>%
  ggplot(aes(reorder_within(term, beta, topic), beta)) +
  geom_col(show.legend = FALSE) +
  theme(panel.spacing = unit(4, "lines")) +
  coord_flip() +
  labs(
    title = ,
    x = NULL,
    y = NULL
  ) +
  tidytext::scale_x_reordered() +
  facet_wrap(~topic, scales = "free") + theme_adam()

levels(top_terms_gibbs$topic) <- c("sg1", "menekült", "sg2", "sg3","USA","belpolitika","gazdaság", "EU", "sg4")

#plot them for articles after the elections
top_terms_gibbs %>%
  #filter(label == "utána" & topic %in% c(2,5,6,8)) %>%
  filter(label == "utána"  & topic %in% c("menekült","USA","belpolitika","gazdaság")) %>%
  ggplot(aes(reorder_within(term, beta, topic), beta)) +
  geom_col(show.legend = FALSE) +
  theme(panel.spacing = unit(4, "lines")) +
  coord_flip() +
  labs(
    title = ,
    x = NULL,
    y = NULL
  ) +
  tidytext::scale_x_reordered() +
  facet_wrap(~topic, scales = "free") + theme_adam()

#plot them for articles after the elections
top_terms_gibbs %>%
  #filter(label == "utána" & topic %in% c(2,5,6,8)) %>%
  filter(label == "utána"  & topic %in% c("EU")) %>%
  ggplot(aes(reorder_within(term, beta, topic), beta)) +
  geom_col(show.legend = FALSE) +
  theme(panel.spacing = unit(4, "lines")) +
  coord_flip() +
  labs(
    title = ,
    x = NULL,
    y = NULL
  ) +
  tidytext::scale_x_reordered() +
  facet_wrap(~topic, scales = "free") + theme_adam()


#other useful
df |> group_by(label) |> summarise(count = n())





