#                                                                               
#                          Analyzing The Office Dialogue                     
#                                                                               
# Purpose: Analyzing The Office dialogue to see sentiment and polarity interactions
# Created by: Geoff Koch                                                        
# Created on: 11/20/2019                                                 
#################################################################################

if (!require("pacman")) install.packages("pacman")
p_load(dplyr,magrittr,tidyr,stringr,tidytext,data.table)

tokenize <- function(df, colname, sentiment.type = "afinn", sentimentremoved = "stopwords.sentimentremoved"){
  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,magrittr,tidytext,SnowballC)
  if(exists(sentimentremoved)) 
    sentimentremoved
  else stopwords.sentimentremoved<- anti_join(stop_words,get_sentiments(sentiment.type)) #give me all stop words that are not related to sentiment
  df <- df %>%
    unnest_tokens_("word",colname) %>%
    mutate(Question = colname) %>%
    anti_join(stopwords.sentimentremoved, by = "word") %>% #give me all words that are not stop words
    filter(!str_detect(word,"[0-9]")) %>%
    filter(!str_detect(word,"[[:punct:]]")) %>%
    mutate(word_stem = wordStem(word))
}

#get the good good dialogue
mydf <- schrute::theoffice 

mydf <- mydf %>%
  mutate(prevchar = case_when(season == lag(season) & episode == lag(episode) ~ lag(character),
                              TRUE ~ "N/A"),
         forwardchar = case_when(season == lead(season) & episode == lead(episode) ~ lead(character),
                                 TRUE ~ "N/A"),
         rownum = as.numeric(row.names(mydf)))

#retrieving running episode numbers
epnums <- mydf %>%
  group_by(season,episode) %>%
  summarize(mindex = min(index)) %>%
  ungroup() 

epnums <- epnums %>%
  mutate(epnums = row.names(epnums))

##remove action descriptors from text - only want dialogue
mydf.noaction <- mydf %>%
  mutate(text = stringr::str_remove(text,"\\[(.*?)\\]")) %>%
  dplyr::select(-text_w_direction)

#tokenize using custom function then score sentiment and polarity
mydf.final <- left_join(mydf.noaction,
                        dplyr::select(tokenize(mydf.noaction,"text"),rownum,word,word_stem), by = "rownum") %>%
  left_join(lexicon::nrc_emotions, by = c("word" = "term")) %>%
  left_join(get_sentiments("afinn"), by = "word") %>%
  rename("polarity" = value) %>%
  left_join(epnums, by = c("season","episode"))
 # dplyr::select(-Question) #legacy item from custom function meant to ETL surveys 
  
fwrite(mydf.final, file = "YOURPATH")
