library(rvest)
library(tidyverse)

#these functions are not stand alone and many of the pages had different list formatting, so had to be added manually.
# I Recommend using the backlisted_reference_reading.csv instead.

#initiate session
session <- html_session("https://www.backlisted.fm/index")

#collect links for all episodes from the index page:

raw_html <- read_html(session)
node <- raw_html %>% html_nodes(css = "li p a")
link <- node %>% html_attr("href")
desc <- node %>% html_text()
link_df <- tibble(desc, link)
link_df[[1]][[1]] <- "A Month in the Country - J.L. Carr (with Lissa Evans)"
link_df <- link_df[-2,]
link_df

#get author names from the url

link_df <- link_df %>% 
  mutate(link2name = str_remove_all(link, pattern = paste0("https://www.backlisted.fm/episodes/","\\(?[0-9,.]+", collapse = "|"))) %>% 
  separate(link2name, into = paste("word", 1:15), sep = "-") %>% 
  unite("name_temp", `word 2`:`word 3` , remove=FALSE) %>% 
  select(everything(), -contains("word")) %>% 
  separate(desc, into = c("titles", "episode"), sep = "-") %>% 
  mutate(episode = case_when(
    is.na(episode) ~ as.character(name_temp), 
    TRUE ~ as.character(episode)),
    episode = str_replace(episode, pattern = "_", replacement = " "),
    episode = snakecase::to_any_case(episode, case = "title")
  ) %>% 
  select(everything(), -name_temp)

#extract across multiple pages

recs_extract <- function(df){
  
pages <- df %>% map(read_html, url = link)
  
pages_text <- pages %>% 
  map_dfr(. %>% 
          html_nodes(css = "ul li") %>% 
            html_text() %>% 
            tibble(text = .) %>% 
            slice(12:n()-2) %>% 
            separate(col = text,
                     into = c("author", "titles"),
                     sep = "-" ) %>% 
            separate(titles, 
                     into = c(paste("book", 1:15)),
                     sep = ",", 
                     extra = "drop") %>% 
            mutate(across(where(is.character), str_trim)) %>% 
            janitor::remove_empty(which = "cols") %>% 
            pivot_longer(cols = contains("book"),
                         names_to = NULL, 
                         values_to = "Title", 
                         values_drop_na = TRUE)
          )
  
}

nested_df1 <- link_df %>%
  filter(episode != "Randall Jarrell" & episode != "Patrick Hamilton") %>% #episodes are problematic not sure why
  slice(1:21,24,27:64) %>% 
  mutate(data = map(link, recs_extract))

#after 60 the formatting changes and all the text is in the same p tag :/
#use the first nested_df to build app until fixed


recs_extract2 <- function(df){
  
  pages <- df %>% map(read_html, url = link)
  
  pages_text <- pages %>% 
    map_dfr(. %>% 
              html_nodes(xpath = "//div[@class='sqs-block-content']/descendant::p[contains(., 'Books mentioned:') or contains(., 'Books Mentioned:')]/following-sibling::*/descendant::a/parent::*") %>%
              html_text(trim = F) %>% #this helps separate the book recommendations from the other links
              tibble(text = .) %>% 
              slice(1) 
            
            
    )
  
}

#between 65 and 86 (penelope fitz) can use recs_extract2 but need to clean text col still
nested_df2 <- 
  link_df %>% 
  filter(episode != "Randall Jarrell" & episode != "Patrick Hamilton") %>% #episodes are problematic not sure why
  slice(65:85) %>%
  mutate(data = map(link, recs_extract2)) 

