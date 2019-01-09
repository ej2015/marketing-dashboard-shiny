source("ggplot_setting.r")

campaign_average_position_point_plot <- function(campaigns) {
  campaigns %>%
    filter(`Avg. position` > 1, `Avg. CPC` > 0) %>%
    ggplot() +
    geom_point(aes(`Avg. CPC`, `Avg. position`, size = Impressions, color = ctr), alpha = 0.5, stat = 'identity') +
    facet_wrap( ~ Campaign, nrow = 2) +
    ylab("Average Position") +
    xlab("Campaign") +
    theme_minimal()
}

campaign_cost_by_type_bar_plot <- function(campaign_by_types){
  campaign_by_types %>%
    ggplot() +
    geom_bar(aes(`Campaign type`, avg_cost), stat = 'identity', fill = 'steelblue') +
    labs(title = "cost by type") +
    ylab("cost") +
    xlab("Type") +
    ggplot_theme 
}

campaign_ctr_by_type_bar_plot <- function(campaign_by_types){
  campaign_by_types %>% 
    ggplot() +
    geom_bar(aes(`Campaign type`, ctr), stat = 'identity', fill = 'steelblue') +
    labs(title = "CTR by type") +
    ylab("CTR") +
    xlab("Type") +
    ggplot_theme 
}

campaign_by_hour_smooth_plot <- function(campaign_by_hour) {
  campaign_by_hour %>%
    ggplot() +
    geom_smooth(aes(`Hour of the day`, Impr.), fill='steelblue') +
    geom_smooth(aes(`Hour of the day`, Interactions), color='red') +
    scale_y_log10(limits = c(-5, 8000)) +
    facet_wrap( ~ Campaign, nrow = 2) +
    ylab("Impressions/Clicks") +
    xlab("hour") +
    theme_minimal()
}

positive_roi_keyword_bar_plot <- function(keywords) {
  keywords %>%
    filter(roi_cat == 'positive') %>%
    ggplot() +
    geom_bar(aes(reorder(Keyword, roi), roi), stat = 'identity', fill='steelblue') +
    scale_y_log10(limits = c(-5, 2100)) +
    coord_flip() +
    theme_minimal()
}

campaign_roi_bar_plot <- function(keywords) {
  campaign_roi <- keywords %>%
    group_by(Campaign) %>%
    summarize(revenue = sum(total_roi), cost = sum(cost)) %>%
    mutate(roi = (revenue - cost)/cost) %>%
    filter(!is.na(roi))
  
  campaign_roi %>%
    ggplot() +
    geom_bar(aes(reorder(Campaign, roi), roi), stat='identity', fill='steelblue') +
    coord_flip() +
    ylab("ROI") +
    xlab("Campaign") +
    labs(title = "Campaign ROI") +
    ggplot_theme
}

roi_by_keyword_match_type_bar_plot <- function(keywords) {
  keywords %>%
    mutate(match_type = replace(match_type, str_detect(Keyword, "[+]"), "BMM")) %>%
    mutate(Keyword = str_replace(Keyword, "^[+]", "")) %>%
    mutate(Keyword = str_replace_all(Keyword, "[+]", " ")) %>%
    mutate(Keyword = str_replace_all(Keyword, "[:punct:]", "")) %>%
    filter(roi < 2000) %>%
    ggplot() +
    geom_point(aes(match_type, roi, size=Interactions), position=position_jitter(width=0.1, height=0.1), alpha=1/3) +
    labs(title='ROI vs. Keyword Match Type') +
    ylab("ROI") +
    xlab("Match Type") +
    ggplot_theme
}

campaign_by_day_of_week_smooth_plot <- function(campaigns) {
  weeks = c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun" )
  campaigns_with_day <- 
    campaigns %>%
    mutate(day_of_week = wday(Day))
  
  campaigns_with_day %>%
    ggplot() +
    geom_smooth(aes(day_of_week, Impressions)) +
    geom_smooth(aes(day_of_week, Clicks), color='red') +
    scale_y_log10(limits = c(-5, 8000)) +
    facet_wrap( ~ Campaign, nrow = 2) +
    ylab("Impressions/Clicks") +
    xlab("day (starts from Monday)") +
    theme_minimal()
}

roi_plot <- function(model, test_set) {
  test_predict <-  predict(model, newdata = test_set, type = 'response')
  
  p_class <- ifelse(test_predict > 0.5, 1, 0)
  p_class
  
  table(test_set$have_click, p_class)
  
  p_class_2 <- ifelse(test_predict > 133/(133+68), 1, 0)
  table(test_set$have_click, p_class_2)
  
  TPR = (28)/(11+28)
  FPR = 3/(3+9)
  
  auc_test = colAUC(test_predict, test_set$have_click, plotROC = TRUE)
  legend(0.1,0.9, round(auc_test,4), title= 'AUC', cex=.5)
  abline(a=0,b=1,lwd=2,lty=2,col="red")
  
}