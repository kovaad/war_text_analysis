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
Sys.setenv(OPENAI_API_KEY = "")


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

