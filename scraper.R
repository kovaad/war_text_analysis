#############################
##   Quantitext scraper   ##
##   Kovács Ádám József  ##
##########################

#clear environment
rm(list = ls())

# load packages -----------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(rvest, data.table, xml2,kableExtra, tidyverse, glue, lubridate, pbapply, stringr, flextable)

#set up chatgpt within R
require(devtools)
install_github("MichelNivard/gptstudio")

# add key
#Sys.setenv(OPENAI_API_KEY = "")


# magyar nemzet -----------------------------------------------------------

get_one_page_mn <- function(t_url) {
  
  t <- read_html(t_url)
  
  rel_links <- t %>% html_nodes(".article-left .article-link") |>  html_attr('href')
  
  t_link <- paste0('http://magyarnemzet.hu/', rel_links)
  
  t_title <-  t %>% html_nodes(".article-right .article-title") %>% html_text()
  
  t_date <- t %>% html_nodes(".article-right .article-date") %>% html_text()
  
  df <- data.frame('titles' = t_title, 'dates' = t_date, 'links' = t_link)
  
  return(df)
}

links <- paste0('http://magyarnemzet.hu/cimke/haboru-ukrajnaban/?page=', 141:349)

list_of_dfs <- pblapply(links, get_one_page_mn)

final_df <- rbindlist(list_of_dfs)

#check for any possible na values in tibble 
to_filter <- sapply(final_df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

write.csv(final_df,"data/final_df_all.csv", row.names = FALSE)

#CHECKPOINT
final_df <- read_csv("data/final_df_all.csv")


get_article_mn <- function(t_url) {
  
  t <- read_html(t_url)
  
  #source <- t %>% html_nodes(xpath = "//*[contains(concat( ' ',@class, ' ' ), concat( ' ','source', ' ' ))]") %>% html_text() %>% ifelse(is.na(.), "Nincs forrás", .)
  
  text <- t %>% html_nodes("p") 
  
  toremove <- text  %>% 
    xml_find_all("//p[@dir='ltr']")
  
  xml_remove(toremove)
  
  text <- t %>% html_nodes("p") |>  html_text()
  
  body <- paste0(text, sep=" ", collapse="") 
  
  tabs_raw <- str_remove(t %>% html_nodes(".tag-list-item") %>% html_text(), " ")
  
  tabs <- paste0(tabs_raw, sep=" ", collapse="") 
  
  df <- data.frame('url' = t_url, 'body' = body, 'tabs' = tabs)
  
  print(t_url)
  
  return(df)
}

pblapply("https://magyarnemzet.hu/kulfold/2022/05/elkepeszto-videon-ahogy-oroszok-tengeralattjarorol-lonek-cirkaloraketakat-ukrajna-fele"
, get_article_mn)

links <- final_df$links

list_of_texts <- pblapply(links, get_article_mn)

final_texts_mn <- rbindlist(list_of_texts)

write.csv(final_texts_mn,"data/final_texts_mn_all.csv", row.names = FALSE)

# CHECKPOINT
final_texts_mn <- read_csv("data/final_texts_mn_all.csv")

# Create dataframe --------------------------------------------------------

mn_df <- left_join(final_df, final_texts_mn, by = c("links" = "url"))

#check for any possible na values in tibble 
to_filter <- sapply(mn_df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# show the row with the missing value in R tibble
filter(mn_df, is.na(tabs))

#replace the NA value with "" in tibble
mn_df[is.na(mn_df)] <- ""

# create dates in desired format, throw out all text in the end, which no longer part of article
mn_df <- mn_df |> 
  mutate(name = "magyar nemzet",
         dates  = ymd(str_remove(dates, "[.][^.]+$")), 
         body = ifelse(str_detect(body, "Borítókép"), stringr::str_extract(body, "^.*(?=(Borítókép))"), body))

# throw out advertisements within text
mn_df <- mn_df |> 
  mutate(body = str_replace(body, "Ajánló.*?\\.", ""))

#check for any possible na values in tibble 
to_filter <- sapply(mn_df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# show the row with the missing value in R tibble
#View(mn_df |> filter(is.na(body)) |> select("links"))

#mn_df <- left_join(final_df, final_texts_mn, by = c("links" = "url"))

#flextable::flextable(mn_df |> 
#                       filter(titles == "A szankció visszaüt"), cwidth = c(0.1,0.1,0.1,0.6,0.1))

#flextable::flextable(mn_df |> 
#                       filter(titles == "Az EU elfogadta az Oroszországot sújtó célzott szankciókat"),
#                     cwidth = c(0.1,0.1,0.1,0.6,0.1))

#drop NAs
mn_df <- mn_df |> filter(!is.na(body))

#save dataframe
write.csv(mn_df,"data/mn_df_full.csv", row.names = FALSE)

# CHECKPOINT
mn_df <- read_csv("data/mn_df_full.csv")

#filter so that we have the same number of articles before and after 2022-04-3

mn_df <- mn_df |> 
  mutate(
    label = ifelse(dates < ymd("2022-4-3"), "előtte", "utána") 
  )

nrow( mn_df |> 
          filter(label == "előtte"))
#there are 1035 articles before 4th April, so we need 2070 in total
mn_df <- mn_df %>%
  tail(2070)

nrow( mn_df |> 
        filter(label == "utána"))

write.csv(mn_df,"data/mn_df_all_final.csv", row.names = FALSE)

# CHECKPOINT
mn_df_final <- read_csv("data/mn_df_all_final.csv")

# mandiner ----------------------------------------------------------------

get_one_page_mr <- function(t_url) {
  
  t <- read_html(t_url)
  
  rel_links <- t %>% html_nodes(".nodelist .news_list_row .title a") |>  html_attr('href')
  
  t_link <- ifelse(grepl('^/', rel_links), paste0('http://mandiner.hu',rel_links), rel_links)
  
  t_title <-  trimws(t %>% html_nodes(".nodelist .title") %>% html_text())
  
  t_date <- t %>% html_nodes(".nodelist .info") %>% html_text()
  
  df <- data.frame('titles' = t_title, 'dates' = t_date, 'links' = t_link)
  
  return(df)
}

df <- get_one_page_mr('http://mandiner.hu/tag/orosz_ukran_haboru')

offset <- seq(0,612,18)
links <- glue('http://mandiner.hu/tag/orosz_ukran_haboru/?offset={offset}&limit=18')

list_of_dfs2 <- pblapply(links, get_one_page_mr)

final_df_mr <- rbindlist(list_of_dfs2)

t <- read_html("https://mandiner.hu/cikk/20230223_europai_tanacs_europai_unio_tagallam_oroszorszag_orosz_ukran_haboru_kulfold")

#using rvest write R code that gets all paragraphs and divs with the class "keretes" using one pipe

t %>% html_nodes('.text p, .keretes') %>% html_text()

text <- t %>% html_nodes(".text p") %>% html_text()

body <- paste0(text, sep=" ", collapse="") 

str_remove(t %>% html_nodes(".taglist a") %>% html_text(), " ")

t <- read_html("https://mandiner.hu/cikk/20230222_ukrajna_kreminna_haboru_orosz_ukran_haboru_harckocsi_video")

text <- t %>% html_nodes(".text p, .keretes") 

toremove <- text  %>% 
  xml_find_all("//p[@dir='ltr']")

xml_remove(toremove)

text <- t %>% html_nodes(".text p") |>  html_text()

t <- read_html("https://mandiner.hu/cikk/20230212_jens_stoltenberg_nato_orosz_ukran_haboru_kulfold")
text <-  t %>% html_nodes(".text p")
toremove <- text  %>% 
  xml_find_all("//p[@dir='ltr']")
xml_remove(toremove)
text <- t %>% html_nodes(".text p, .keretes") |>  html_text()
body <- paste0(text, sep=" ", collapse="") 
ifelse(str_detect(body, "Nyitókép"), stringr::str_extract(body, "^.*(?=(Nyitókép))"), body)

get_article_mr <- function(t_url) {
  
  t <- read_html(t_url)
  
  text <- t %>% html_nodes('.text p, .keretes') 
  
  toremove <- text  %>% 
    xml_find_all("//p[@dir='ltr']")
  
  xml_remove(toremove)
  
  text <- t %>% html_nodes(".text p, .keretes") |>  html_text()
  
  body <- paste0(text, sep=" ", collapse="") 
  
  tabs_raw <- str_remove(t %>% html_nodes(".taglist a") %>% html_text(), " ")
  
  tabs <- paste0(tabs_raw, sep=" ", collapse="") 
  
  df <- data.frame('url' = t_url, 'body' = body)
  
  return(df)
}

links <- final_df_mr$links

list_of_texts_mr <- pblapply(links, get_article_mr)

final_texts_mr <- rbindlist(list_of_texts_mr)

write.csv(final_texts_mr,"data/final_texts_mr_all.csv", row.names = FALSE)

#CHECKPOINT
final_texts_mr <- read_csv("data/final_texts_mr_all.csv")

mr_df <- left_join(final_df_mr, final_texts_mr, by = c("links" = "url"))

#filter out pic description, but careful not to lose too much data either

nrow(mr_df |> filter(str_detect(body, "Nyitókép") == T))

nrow(mr_df |> filter(str_detect(body, "A nyitóképen") == T))

nrow(mr_df |> filter(str_detect(body, "A nyitókép illusztráció") == T))

nrow(mr_df |> filter(str_detect(body, "Fotó") == T))

nrow(mr_df |> filter(str_detect(body, "Címlapfotó") == T))

nrow(mr_df |> filter(str_detect(body, "Borítókép") == T))

nrow(mr_df |> filter(str_detect(body, "Nyitófotó") == T))

nrow(mr_df |> filter(str_detect(body, "(MTI)") == T))

mr_df <- mr_df |> 
  mutate(name = "mandiner",
    dates = parse_date(gsub('\\.','',str_remove(dates, "[.][^.]+$")), "%Y %B %d", locale = locale("hu")),
    body = ifelse(str_detect(body, "Nyitókép"), stringr::str_extract(body, "^.*(?=(Nyitókép))"), body))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "A nyitóképen"), stringr::str_extract(body, "^.*(?=(A nyitóképen))"), body))

#check for any possible na values in tibble 
to_filter <- sapply(mr_df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

View(mr_df |> filter(is.na(body)) |> select("links"))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "Fotó"), stringr::str_extract(body, "^.*(?=(Fotó))"), body))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "Címlapfotó"), stringr::str_extract(body, "^.*(?=(Címlapfotó))"), body))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "Borítókép"), stringr::str_extract(body, "^.*(?=(Borítókép))"), body))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "(MTI)"), stringr::str_extract(body, "^.*(?=((MTI)))"), body))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "Nyitófotó"), stringr::str_extract(body, "^.*(?=(Nyitófotó))"), body))

mr_df <- mr_df |> 
  mutate(body = ifelse(str_detect(body, "A nyitókép illusztráció"), stringr::str_extract(body, "^.*(?=(A nyitókép illusztráció))"), body))




write.csv(mr_df,"data/mr_df.csv", row.names = FALSE)

# szolnok -----------------------------------------------------------------

get_one_page_sz <- function(t_url) {
  
  t <- read_html(t_url)
  
  rel_links <- t %>% html_nodes(".right .article-link") |>  html_attr('href')
  
  t_link <- paste0('http://www.szoljon.hu',rel_links)
  
  t_title <-  t %>% html_nodes(".right .article-title") |>  html_text()
  
  t_date <- t %>% html_nodes(".right .article-date") |>  html_text()
  
  df <- data.frame('titles' = t_title, 'dates' = t_date, 'links' = t_link)
  
  return(df)
}

links <- paste0('http://www.szoljon.hu/cimke/ukrajna?sortDirection=asc&page=', 1:14)

list_of_dfs3 <- pblapply(links, get_one_page_sz)

final_df_sz <- rbindlist(list_of_dfs3)

get_article_sz <- function(t_url) {
  
  t <- read_html(t_url)
  
  text <- t %>% html_nodes(".block-content p") %>% html_text()
  
  body <- paste0(text, sep=" ", collapse="") 
  
  df <- data.frame('url' = t_url, 'body' = body)
  
  return(df)
}

links <- final_df_sz$links

list_of_texts_sz <- pblapply(links, get_article_sz)

final_texts_sz <- rbindlist(list_of_texts_sz)

write.csv(final_texts_sz,"data/final_texts_sz.csv", row.names = FALSE)

sz_df <- left_join(final_df_sz, final_texts_sz, by = c("links" = "url"))

sz_df <- sz_df |> 
  mutate(name = "szoljon",
         dates  = ymd(str_remove(dates, "[.][^.]+$")))

write.csv(sz_df,"data/sz_df.csv", row.names = FALSE)


# pesti sracok ------------------------------------------------------------


get_one_page_ps <- function(t_url) {
  
  t <- read_html(t_url)
  
  t_link <- t %>% html_nodes(".infinite-post .widget-full-list-text a") |>  html_attr('href')
  
  t_title <-  t %>% html_nodes(".infinite-post .widget-full-list-text a") |>  html_text()
  
  t_date <- t %>% html_nodes(".infinite-post .widget-post-info .widget-post-date") |>  html_text()
  
  df <- data.frame('titles' = t_title, 'dates' = t_date, 'links' = t_link)
  
  return(df)
}

links <- paste0('http://pestisracok.hu/tag/ukrajna/page/', 1:3)

list_of_dfs4 <- pblapply(links, get_one_page_ps)

final_df_ps <- rbindlist(list_of_dfs4)
#some articles turned out to be from a different site just also put on pesti sracok so they were removed
final_df_ps <- final_df_ps[-c(4,8,35,54,74,88),]

t <- read_html("https://pestisracok.hu/a-pofatlansag-csucsa-zelenszkij-a-romanoknak-igerget-kisebbsegi-jogokat/")

t %>% html_nodes(".wprt-container h5") %>% html_text()

paste0(t %>% html_nodes(".wprt-container p") %>% html_text(), sep=" ", collapse="")

get_article_ps <- function(t_url) {
  
  t <- read_html(t_url)
  
  text1 <- t %>% html_nodes(".wprt-container h5") %>% html_text()
  
  text2 <- t %>% html_nodes(".wprt-container p") %>% html_text()
  
  body <- paste0(text2, sep=" ", collapse="") 
  
  full_body <- paste0(text1, body, sep=" ", collapse="") 
  
  df <- data.frame('url' = t_url, 'body' = full_body)
  
  return(df)
}

links <- final_df_ps$links

list_of_texts_ps <- pblapply(links, get_article_ps)

final_texts_ps <- rbindlist(list_of_texts_ps)

write.csv(final_texts_ps,"data/final_texts_ps.csv", row.names = FALSE)

ps_df <- left_join(final_df_ps, final_texts_ps, by = c("links" = "url"))

ps_df <- ps_df |> 
  mutate(name = "pesti sracok", 
         dates = ymd(dates))

write.csv(ps_df,"data/ps_df.csv", row.names = FALSE)


# combine -----------------------------------------------------------------

full_df <- bind_rows(mn_df,mr_df,sz_df,ps_df)

write.csv(full_df,"data/full_df_correct.csv", row.names = FALSE)



