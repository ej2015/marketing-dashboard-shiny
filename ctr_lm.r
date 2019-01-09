train_ctr_lm <- function(keywords) {
  keywords <- keywords %>%
    mutate(match_type = replace(match_type, str_detect(Keyword, "[+]"), "BMM")) %>%
    mutate(Keyword = str_replace(Keyword, "^[+]", "")) %>%
    mutate(Keyword = str_replace_all(Keyword, "[+]", " ")) %>%
    mutate(Keyword = str_replace_all(Keyword, "[:punct:]", "")) %>%
    filter(roi < 2000) %>%
    filter(!is.na(ctr)) %>%
    filter(!is.na(avg_cpc)) %>%
    filter(ctr < 10)
  ctr_lm_model <- lm(ctr ~ `Included Insurer` +
                       `Included Product` + match_type + `Length of Keywords`
                     + `No. of words`, data = keywords )
}