#                                                                              
#                          Harry Potter Data     
#                                                                               
# Purpose: Analyzing the text in Harry Potter books for PYP Dec 2022
# Created by: Geoff Koch                                                        
# Created on: 11/22/2022                                     
# Validated by:                                             
# Validated on:


# if (packageVersion("devtools") < 1.6) {
#   install.packages("devtools")
# }
# 
# devtools::install_github("bradleyboehmke/harrypotter")

#Load Packages and Data ----
source("INTERNAL FUNCTIONS")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,janitor,data.table,udpipe,future.apply,stringi,harrypotter,topicmodels)

hp1 <- harrypotter::philosophers_stone
hp2 <- harrypotter::chamber_of_secrets
hp3 <- harrypotter::prisoner_of_azkaban
hp4 <- harrypotter::goblet_of_fire
hp5 <- harrypotter::order_of_the_phoenix
hp6 <- harrypotter::half_blood_prince
hp7 <- harrypotter::deathly_hallows

hpdf <- rbind(as.data.frame(hp1) %>% dplyr::rename(text = hp1) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'philosophers_stone',
                                                                                 book = 1),
              as.data.frame(hp2) %>% dplyr::rename(text = hp2) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'chamber_of_secrets',
                                                                                 book = 2),
              as.data.frame(hp3) %>% dplyr::rename(text = hp3) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'prisoner_of_azkaban',
                                                                                 book = 3),
              as.data.frame(hp4) %>% dplyr::rename(text = hp4) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'goblet_of_fire',
                                                                                 book = 4),
              as.data.frame(hp5) %>% dplyr::rename(text = hp5) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'order_of_the_phoenix',
                                                                                 book = 5),
              as.data.frame(hp6) %>% dplyr::rename(text = hp6) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'halfblood_prince',
                                                                                 book = 6),
              as.data.frame(hp7) %>% dplyr::rename(text = hp7) %>% dplyr::mutate(chpt = dplyr::row_number(),
                                                                                 bookname = 'deathly_hallows',
                                                                                 book = 7)) %>%
  dplyr::mutate(doc_id = paste(book,chpt,
                               sep = "-"))
  
##Sentiment and Tokenizing ----
hp_sent <- hpdf %>%
  sentimentr::get_sentences(hpdf) %>%
  IRSA_sentiment(colname = "text",
                 by = c("chpt","book")) %>%
  dplyr::mutate(doc_id = paste(book,chpt,
                               sep = "-"))

#old tokenization function. using pos tagging instead
# hp_token <- hpdf %>%
#   IRSA_tokenize(colname = "text") %>%
#   dplyr::select(-text) %>%
#   left_join(hp_sent %>% dplyr::select(word_count,ave_sentiment,doc_id),
#             by = "doc_id")


##Annotate ----

udmodel_eng <- udpipe::udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe") #load the model for part-of-speech tagging


###Prepping data----
hpdf.text <- hpdf %>%
  dplyr::select(doc_id,
                text)


hpdf.text$text <- stringi::stri_enc_toascii(hpdf.text$text) #converting characters. Some of these things can really mess up your function

#slightly slower than next version
# annotation <- udpipe(comments.unique, udmodel_eng, parallel.cores = 4, parallel.chunksize = 100)


###Tagging (parallel)----

note_that("anno_begin")
#setting the plan for future.apply
future::plan(multisession, workers = 4L) #specify how many workers (cores if using multisession strategy)

#split the data into a large list with documents of length = by
anno <- split(hpdf.text, seq(1, nrow(hpdf.text), by = 50))

#apply your plan from earlier to a custom function of udpipe
anno <- future.apply::future_lapply(anno,
                                    FUN = function(hpdf.text, ...) udpipe(hpdf.text, "english-ewt", parallel.chunksize = 100, ...), 
                                    model_dir = getwd(),
                                    future.seed = TRUE)

#data.table's rbindlist is super fast to gather the list of data and convert to a usable dataframe
anno <- data.table::rbindlist(anno)
note_that("anno_end")


#join the sentiment to the annotated text. Also, indentify stopwords
anno.df <- left_join(
  anno,
  hp_sent %>% dplyr::select(word_count,ave_sentiment,doc_id),
  by = "doc_id") %>%
  dplyr::mutate(is_stopword = dplyr::case_when(token %in% tidytext::stop_words$word ~ 1,
                                               TRUE ~ 0),
                row_id = dplyr::row_number())

##Verbs of He/She ----
#He walked, she ran (find out actions that immediately followed he and she)
anno.df <- anno.df %>%
  dplyr::mutate(next_word = dplyr::case_when(dplyr::lead(sentence_id) == sentence_id ~ dplyr::lead(token),
                                             TRUE ~ "END_SENTENCE"),
                is_shehe = dplyr::case_when(tolower(token) %in% c("he","she") ~ 1,
                                            TRUE ~ 0)
  ) %>%
  dplyr::mutate(is_shehe_verb = dplyr::case_when(is_shehe == 1 & dplyr::lead(upos == "VERB") ~ 1,
                                                 TRUE ~ 0))
fwrite(anno.df, 
       file = "INTERNAL PATH")

###clean up----
#remove dataframes we no longer need
rm(anno,hpdf.text,hp_sent,hp_token,hp1,hp2,hp3,hp4,hp5,hp6,hp7)
gc()

##TF-IDF Scores by Book-----
###Attribution ----
# these next two parts are, essentially, copy-pasted from the wonderful TidyText Mining book:
# https://www.tidytextmining.com/index.html. Please support these folks!


book_words <- hpdf  %>%
  tidytext::unnest_tokens(word, text) %>%
  dplyr::count(book, word, sort = TRUE)

total_words <- book_words %>% 
  dplyr::group_by(book) %>% 
  dplyr::summarize(total = sum(n))

book_words <- dplyr::left_join(book_words, total_words)

freq_by_rank <- book_words %>% 
  dplyr::group_by(book) %>% 
  dplyr::mutate(rank = dplyr::row_number(), 
         `term frequency` = n/total) %>%
  dplyr::ungroup()

book_tf_idf <- book_words %>%
  tidytext::bind_tf_idf(word, book, n) %>%
  janitor::clean_names() %>%
  dplyr::arrange(book, tf_idf) %>%
  dplyr::group_by(book) %>%
  dplyr::mutate(tfidf_rank = dense_rank(-tf_idf))

data.table::fwrite(book_tf_idf, 
       file = "INTERNAL PATH")

rm(book_words,book_tf_idf)

##LDA Topic Modelling ----
#helper function for DRY
my_LDA <- function(df, book = "book", book_choice = 1, word = "word", n = "n") {
  
  df.dtm <- df %>%
    dplyr::filter(book == book_choice,
                  !word %in% tidytext::stop_words$word) %>% #remove the pesky stop words
    tidytext::cast_dtm(book,word, n) 
  
  df_lda <- topicmodels::LDA(df.dtm, k = 6, control = list(seed = 1234))
  df_topics <- tidytext::tidy(df_lda, matrix = "beta")
  return(df_topics)

}


n = 7
booklist = list()
# pre-allocate for slightly more efficiency
booklist = vector("list", length = n)

test <- for (i in 1:n) {
  mydf <- my_LDA(book_words,
         book_choice = i)
  mydf$book <- i
  booklist[[i]] <- mydf
}

hp_lda <- data.table::rbindlist(booklist) #let's get a dataframe


hp_lda_top_terms <- hp_lda %>%
  group_by(topic,book) %>%
  slice_max(beta, n = 50) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  dplyr::group_by(book,topic) %>%
  dplyr::mutate(beta_rank = dense_rank(-beta))

data.table::fwrite(hp_lda_top_terms, 
                   file = "INTERNAL PATH")
