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
pacman::p_load(dplyr,tidyverse, stringr, lubridate, tidytext, HunMineR, quanteda,quanteda.textstats,quanteda.textplots, topicmodels, ggwordcloud, widyr, igraph, ggraph)


#check out custom theme
source("theme_adam.R")

#read in data
df <- read_csv("data/mn_df_all_final_version.csv")

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

#remove remainder of tweet contents
#df2 <- df |> 
#       unnest_tokens(sentence, body, token = "sentences") |> 
       #filter(links == "http://magyarnemzet.hu//kulfold/2022/03/a-haboru-sem-akadalyozta-meg-a-hazassagukat") |> 
       #filter(str_detect(sentence, "pic.twitter.com")) |> 
#       mutate(sentence = ifelse(str_detect(sentence, "pic.twitter.com"), gsub("^.*pic.twitter.com[^ ]*", "", sentence), sentence)) |> 
#       group_by(titles, dates, links, tabs, name, label ) |> 
#       summarize(body = str_c(sentence, collapse = " ")) |> 
#       ungroup()

#df2 |> filter(links == "http://magyarnemzet.hu//kulfold/2022/03/a-haboru-sem-akadalyozta-meg-a-hazassagukat")

#I have two almost identical tibbles in R, how to filter on only the rows that are present in one, but not in the other

#anti_join(df |> select(-body), df2|> select(-body))

#df |> filter(links == "http://magyarnemzet.hu//kulfold/2022/06/videon-a-bevasarlokozpont-a-bombazas-utan")

#rename body for text
#remove any unnecessary whitespces
df <- df |> 
  rename(text = body) |> 
  mutate(
    text = stringr::str_trim(text),
    text = stringr::str_squish(text)
  )

# basic descriptives using tidytext ------------------------------------------------------

#create tidy tokens dataframe using tidytext
tokens <- df |> 
  unnest_tokens(word, text)

#count tokens by article
tok_count <- tokens |> 
  group_by(dates,links) |> 
  summarise( 
    sum_tokens = n()
  )

#add to dataframe this information
df <- left_join(df, tok_count[, names(tok_count) != "dates"], by = "links")


df |> filter(is.na(sum_tokens))

#write R code that takes the NA values in a tibble colun "sum_tokens" and replaces them with 0 using tidyverse
df <- df %>%
  mutate(sum_tokens = ifelse(is.na(sum_tokens), 0, sum_tokens))

#look at summary of tokens
summary(df$sum_tokens)

#get number of articles and average length of articles by date
overtime <- tok_count |>
  group_by(dates) |> 
  summarise( 
    n_articles = n(),
    avg_tokens = mean(sum_tokens)
  )


#plot number of articles in dataset over time
arrows <- 
  tibble(
    x1 = c(ymd("2022-3-15"), ymd("2022-4-19")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(60, 45), 
    y2 = c(65, 48)
  )

ggplot(overtime, aes(dates, n_articles)) +
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
  ggplot2::annotate("text", x = ymd("2022-3-18"), y = 56, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-4-19"), y = 41, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = 0.3) +
  scale_y_continuous(breaks = seq(0, 60, 10))#+
  #theme_adam()

#plot of evolution of length of articles
arrows <- 
  tibble(
    x1 = c(ymd("2022-3-15"), ymd("2022-4-19")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(650, 220), 
    y2 = c(700, 250)
  )

ggplot(overtime, aes(dates, avg_tokens)) +
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
  ggplot2::annotate("text", x = ymd("2022-3-18"), y = 620, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-4-19"), y = 190, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = 0.3) +
  scale_y_continuous(breaks = seq(0, 500, 100))#+
 # theme_adam()

# data preprocessing ------------------------------------------------------

#load stopwords
custom_stopwords <- HunMineR::data_stopwords_extra

tidy_stops <- get_stopwords('hu')[,1]$word

#create cleaner function
cleaner <- function(text) {
  
  #remove punctuations, numbers, make it lower case, remove unnecessary white spaces
  text <- stringr::str_remove_all(string = text, pattern = "[:punct:]") 
  text <- stringr::str_remove_all(string = text, pattern = "[:digit:]") 
  text <- stringr::str_to_lower(text)
  text <- stringr::str_trim(text) 
  text <- stringr::str_squish(text)
  
  # tokenize, filter out stopwords, drop those with less than 3 characters
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[!(tokens %in% tidy_stops)]
  tokens <- tokens[!(tokens %in% custom_stopwords)]
  tokens <- tokens[length(tokens) >= 3]
  
  # get back processed text
  clean_text <- paste(tokens, collapse = " ")
  
  return(clean_text)
}

#apply function
df$clean_text <- pblapply(df$text, cleaner)

# word frequencies --------------------------------------------------------

#create tidy tokens dataframe using tidytext from tokens before designated date
tokens_before <- df |> 
  filter(label == "before") |> 
  unnest_tokens(word, clean_text)

#count tokens by article
tok_count_before <- tokens_before |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  mutate(group = "before")

#create tidy tokens dataframe using tidytext
tokens_after <- df |> 
  filter(label == "after") |> 
  unnest_tokens(word, clean_text)

#count tokens by article
tok_count_after <- tokens_after |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  mutate(group = "after")

freq <- bind_rows(tok_count_before,tok_count_after)

#before after word frequency
freq |> 
  mutate(group = factor(group, levels = c("before", "after"))) |> 
  ggplot(aes(x = tidytext::reorder_within(x=word, 
                                          by=n, 
                                          within=group), 
             y = n)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL,
       y = "Frequency") +
  facet_wrap(~group,scales = "free") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(breaks = seq(0, 4000, 500))
  #theme_adam()


# wordcloud ---------------------------------------------------------------

#count tokens by article 2
tok_count_before2 <- tokens_before |> 
  count(word, sort = TRUE) |> 
  top_n(40) |> 
  mutate(group = "before")

#count tokens by article 2
tok_count_after2 <- tokens_after |> 
  count(word, sort = TRUE) |> 
  top_n(40) |> 
  mutate(group = "after")

freq2 <- bind_rows(tok_count_before2,tok_count_after2)

#freq2 <- freq2 %>%
#  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

#wordcloud
wc <- freq2 |> 
  ggplot(aes(label = word, size = n, colour = group)) +
  scale_size_area(max_size = 7) +
  geom_text_wordcloud(shape = "square", eccentricity = 0.4, 
                      nudge_x = 4.2, nudge_y = 0.5) +
  theme_minimal()
  #geom_text_wordcloud_area(show.legend = TRUE, 
  #                         mask = png::readPNG(system.file("extdata/hearth.png",
  #                                                         package = "ggwordcloud", mustWork = TRUE
  #                         ))#,
                           #rm_outside = TRUE
  #) +
wc

#create tf_idf doc
df_tf_idf <- tokens  |> 
  count(label, word) |>
  filter(!str_detect(word, "\\d+")) |>
  bind_tf_idf(word, label, n) |>
  arrange(-tf_idf)

#visualize
df_tf_idf |>    
  arrange(desc(tf_idf)) |>
  mutate(word = factor(word, levels = rev(unique(word)))) |> 
  group_by(label) |> 
  top_n(10) #|> 
  #ungroup #|>
  #ggplot(aes(word, tf_idf, fill = label)) +
  #geom_col(show.legend = FALSE) +
  #labs(x = NULL, y = "tf-idf") +
  #facet_wrap(~label, ncol = 2, scales = "free") +
  #coord_flip()


# keyness -----------------------------------------------------------------

#data cleaning and creating grouped quanteda dfm
dfm_grouped <- corpus(df) |> 
  tokens( 
    remove_punct = TRUE, 
    remove_numbers = TRUE 
  ) |> 
  tokens_tolower() |>  
  tokens_select(pattern = tidy_stops,selection = "remove" ) |> 
  tokens_select(pattern = custom_stopwords, selection = "remove") |> 
  dfm() |> 
  quanteda::dfm_group(label)

#get most frequent important words by group
result_keyness <- dfm_grouped |>  
  quanteda.textstats::textstat_keyness(target = "before")

#plot
result_keyness |> 
  quanteda.textplots::textplot_keyness(color = c("#00BFC4", "#F8766D")) +
  xlim(c(-200, 200)) +
  theme(legend.position = c(0.9,0.1)) 


# sentiment analysis ------------------------------------------------------

#sentiment contribution

#load and create sentiment dictionaries
positive_words <- read_csv("../data/PrecoSenti/PrecoPos.csv") |>
  mutate(sentiment=1)

negative_words <- read_csv("../data/PrecoSenti/PrecoNeg.csv") |>
  mutate(sentiment=-1)

hungarian_sentiment <- rbind(positive_words, negative_words)

sent_tokens <- tokens |> 
  inner_join(hungarian_sentiment)

sent_tokens |>
  count(word, sentiment, sort = TRUE) |>
  ungroup()|> 
  mutate(word = reorder(word, n)) |>
  top_n(15) |> 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip()

#sentiment over time
df_sent_time <- sent_tokens |> 
  group_by(dates) |> 
  summarise( 
    score = sum(sentiment)
  )

#helper tibble for arrows
arrows <- 
  tibble(
    x1 = c(ymd("2022-2-3"), ymd("2022-4-23")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(-50, -100), 
    y2 = c(-50, -100)
  )

#plot
ggplot(df_sent_time, aes(dates, score)) +
  geom_line() +
  labs(
    y = "Sentiment score",
    x = NULL
  ) + 
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-2-1"), y = -60, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-4-23"), y = -90, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3) +
  theme_adam()

#Hungarian version
ggplot(df_sent_time, aes(dates, score)) +
  geom_line() +
  labs(
    y = "Szentiment",
    x = NULL
  ) + 
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-2-1"), y = -60, label = "Oroszország megtámadja \n Ukrajnát") +
  ggplot2::annotate("text", x = ymd("2022-4-23"), y = -90, label = "Választások/\nBucsai mészárlás") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3) +
  theme_adam()

# Topic modelling ---------------------------------------------------------

#create wordcount table from tokens before elections
word_count_before <- tokens_before %>%
  count(links,word, sort = TRUE) %>%
  ungroup()

#create dtm
dtm_before <- word_count_before %>%
  cast_dtm(links, word, n)

#create wordcount table from tokens after elections
word_count_after <- tokens_after %>%
  count(links,word, sort = TRUE) %>%
  ungroup()

#create dtm
dtm_after <- word_count_after %>%
  cast_dtm(links, word, n)

#conduct LDA using Gibbs method with 9 topics on articles before elections
gibbs_before <- LDA(dtm_before, k = 9, method = "Gibbs", control = list(seed = 1234))

#conduct LDA using Gibbs method with 9 topics  on articles after elections
gibbs_after <- LDA(dtm_after, k = 9, method = "Gibbs", control = list(seed = 1234))

#get topics before elecitons
topics_before <- tidy(gibbs_before, matrix = "beta") %>%
  mutate(label = "before")

#get topics after elections
topics_after <- tidy(gibbs_after, matrix = "beta") %>%
  mutate(label = "after")

#put it into one dataframe
lda_gibbs <- bind_rows(topics_before, topics_after)

#get top 5 terms by topics
top_terms_gibbs <- lda_gibbs %>%
  group_by(label, topic) %>%
  top_n(5, beta) %>%
  top_n(5, term) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot them for articles before the elections
top_terms_gibbs %>%
  filter(label == "before") %>%
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
  filter(label == "after") %>%
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


# Co-occurences -----------------------------------------------------------

#apply function
df$clean_titles <- pblapply(df$titles, cleaner)

title_tokens <- df %>% 
  unnest_tokens(word, clean_titles)

title_word_pairs <- title_tokens %>% 
  pairwise_count(word, links, sort = TRUE, upper = FALSE)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


