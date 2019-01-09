get_keyword_glm_sets <- function(keywords) {
  with_competition_enabled <- keywords %>%
    mutate(match_type = replace(match_type, str_detect(Keyword, "[+]"), "BMM")) %>%
    mutate(Keyword = str_replace(Keyword, "^[+]", "")) %>%
    mutate(Keyword = str_replace_all(Keyword, "[+]", " ")) %>%
    mutate(Keyword = str_replace_all(Keyword, "[:punct:]", "")) %>%
    filter(roi < 2000) %>%
    mutate(Competition= replace(Competition, is.na(Competition), "xlow")) %>%
    filter(!is.na(Competition)) %>%
    filter(!is.na(top_of_page_bid_high) ) %>%
    filter(!is.na(top_of_page_bid_low) ) %>%
    mutate( have_click = factor(have_click)) %>%
    mutate( traffic = factor(traffic)) %>%
    mutate( Competition = factor(Competition)) %>%
    filter(`Keyword status` == 'Enabled')
  
  set.seed(123)
  
  split_data <- sample.split(with_competition_enabled$have_click, SplitRatio = 0.7)
  
  train_set <- with_competition_enabled[split_data,]
  test_set <- with_competition_enabled[!split_data,]
  list("train_set" = train_set, "test_set" = test_set)
}

train_glm <- function(train_set){
  model <- glm(have_click ~ max_cpc + `Included Insurer` + 
               `Included Product` + match_type + Product +
               top_of_page_bid_high +
               top_of_page_bid_low + `No. of words`, data = train_set, family=binomial)
}
