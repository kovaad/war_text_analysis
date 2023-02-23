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
pacman::p_load(rvest, data.table, xml2,kableExtra, tidyverse, glue, lubridate, pbapply, stringr)

#set up chatgpt within R
require(devtools)
install_github("MichelNivard/gptstudio")

Sys.setenv(OPENAI_API_KEY = "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")


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

links <- paste0('http://magyarnemzet.hu/cimke/haboru-ukrajnaban/?page=', 1:204)

list_of_dfs <- pblapply(links, get_one_page_mn)

final_df <- rbindlist(list_of_dfs)

write.csv(final_df,"data/final_df2_v2.csv", row.names = FALSE)

#CHECKPOINT

final_df <- read_csv("data/final_df2.csv")

final_df_2 <- read_csv("data/final_df2_v2.csv")

t <- read_html(links[1])

t %>% html_nodes("p") %>% html_text()

get_article_mn <- function(t_url) {
  
  t <- read_html(t_url)
  
  text <- t %>% html_nodes("p") %>% html_text()
  
  body <- paste0(text, sep=" ", collapse="") 
  
  df <- data.frame('url' = t_url, 'body' = body)
  
  return(df)
}

pblapply(links[1], get_article_mn)

links <- final_df_2$links

list_of_texts <- pblapply(links, get_article_mn)

final_texts_mn <- rbindlist(list_of_texts)

write.csv(final_texts_mn,"data/final_texts_mn_v2.csv", row.names = FALSE)

# CHECKPOINT

final_texts_mn <- read_csv("data/final_texts_mn.csv")

final_texts_mn_2 <- read_csv("data/final_texts_mn_v2.csv")


# Create dataframe --------------------------------------------------------

mn_df <- left_join(final_df, final_texts_mn, by = c("links" = "url"))

# create dates in desired format, throw out all text in the end, which no longer part of article
mn_df <- mn_df |> 
  mutate(name = "magyar nemzet",
    dates  = ymd(str_remove(dates, "[.][^.]+$")), 
    body = stringr::str_extract(body, "^.*(?=(Borítókép))"))

# throw out advertisements within text
mn_df <- mn_df |> 
  mutate(body = str_replace(body, "Ajánló.*?\\.", ""))

#save dataframe
write.csv(mn_df,"data/mn_df.csv", row.names = FALSE)

#get newly scraped data in a dataframe
mn_df_2 <- left_join(final_df_2, final_texts_mn_2, by = c("links" = "url"))

# create dates in desired format, throw out all text in the end, which no longer part of article
mn_df_2 <- mn_df_2 |> 
mutate(name = "magyar nemzet",
       dates  = ymd(str_remove(dates, "[.][^.]+$")), 
       body = ifelse(str_detect(body, "Borítókép"), stringr::str_extract(body, "^.*(?=(Borítókép))"), body))

# throw out advertisements within text
mn_df_2 <- mn_df_2 |> 
  mutate(body = str_replace(body, "Ajánló.*?\\.", ""))

#save dataframe
write.csv(mn_df_2,"data/mn_df_2_test.csv", row.names = FALSE)


# dealing with missing data -----------------------------------------------

tocorrect_links <- mn_df_2[is.na(mn_df_2$body),]$links

tocorrect_list_of_texts <- pblapply(tocorrect_links, get_article_mn)

tocorrect_final_texts_mn <- rbindlist(tocorrect_list_of_texts)

temp <- inner_join(mn_df_2, tocorrect_final_texts_mn, by = c("links" = "url"))


mn_df_2$body <- ifelse(mn_df_2$links %in% temp$links, temp$body.y, mn_df_2$body)

mn_df_2 <- mn_df_2 |> 
  mutate(name = "magyar nemzet",
         dates  = ymd(str_remove(dates, "[.][^.]+$")), 
         body = ifelse(str_detect(body, "Borítókép"), stringr::str_extract(body, "^.*(?=(Borítókép))"), body))

View(mn_df_2[is.na(mn_df_2$body),])


mn_df_2 |> filter(titles == "Belga liberális politikus: Vissza kell utasítanunk Zelenszkijt") |>
  mutate(body = as.character(body)) |> 
  select(body) #|> 
  #str_detect( "Borítókép")
  #stringr::str_extract("^.*(?=Borítókép)")

mn_df_2 |> filter(titles == "Már több mint 650 ezer menekült érkezett Magyarországra") |>
  #mutate(body = as.character(body)) |> 
  select(body) #|> 
#str_detect( "Borítókép")
#stringr::str_extract("^.*(?=Borítókép)")

mn_df_2 |> filter(titles == "Már több mint 650 ezer menekült érkezett Magyarországra") |>
  #mutate(body = as.character(body)) |> 
  select(body)



write.csv(mn_df_2,"data/mn_df_2_test.csv", row.names = FALSE)


# drop the last two rows, already present in mn_df
mn_df_2 <- mn_df_2[-c(nrow(mn_df_2)-1, nrow(mn_df_2)),]

# filter on those rows of the mn_df_2 dataframe where the body column has NA in R

View(mn_df_2[is.na(mn_df_2$body),])

write.csv(mn_df_2,"data/mn_df_2.csv", row.names = FALSE)

# CHECKPOINT

mn_df <- read_csv("data/mn_df.csv")

mn_df_2 <- read_csv("data/mn_df_2.csv")

# merge the two tibbles
mn_df_all <- rbind(mn_df_2, mn_df)

write.csv(mn_df_all,"data/mn_df_all.csv", row.names = FALSE)

# CHECKPOINT

mn_df_all <- read_csv("data/mn_df_all.csv")

#filter so that we have the same number of articles before and after 2022-04-3

mn_df_final <- mn_df_all %>%
  tail(2070)

write.csv(mn_df_final,"data/mn_df_final.csv", row.names = FALSE)


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

offset <- seq(0,522,18)
links <- glue('http://mandiner.hu/tag/orosz_ukran_haboru/?offset={offset}&limit=18')

list_of_dfs2 <- pblapply(links, get_one_page_mr)

final_df_mr <- rbindlist(list_of_dfs2)

get_article_mr <- function(t_url) {
  
  t <- read_html(t_url)
  
  text <- t %>% html_nodes(".text p") %>% html_text()
  
  body <- paste0(text, sep=" ", collapse="") 
  
  df <- data.frame('url' = t_url, 'body' = body)
  
  return(df)
}

links <- final_df_mr$links

list_of_texts_mr <- pblapply(links, get_article_mr)

final_texts_mr <- rbindlist(list_of_texts_mr)

write.csv(final_texts_mr,"data/final_texts_mr.csv", row.names = FALSE)

mr_df <- left_join(final_df_mr, final_texts_mr, by = c("links" = "url"))

mr_df <- mr_df |> 
  mutate(name = "mandiner",
    dates = parse_date(gsub('\\.','',str_remove(dates, "[.][^.]+$")), "%Y %B %d", locale = locale("hu")))

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



