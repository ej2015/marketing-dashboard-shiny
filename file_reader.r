#import and clean up data
import_campaign_data <- function(file) {
  read_csv(file, skip=2) %>%
    replace_with_na_if(.predicate = is.character, condition = ~.x == "--" ) %>%
    mutate(ctr = parse_number(CTR)/100,
           invalid_click_rate = parse_number(`Invalid click rate`),
           convertion_rate = parse_number(`Conv. rate`),
           search_exact_match_is = parse_number(`Search Exact match IS`),
           search_imp_share = parse_number(`Search Impr. share`),
           search_lost_is_rank = parse_number(`Search Lost IS (rank)`),
           search_lost_is_budget = parse_number(`Search Lost IS (budget)`)
    ) %>%
  select(-CTR, -`Invalid click rate`, -`Conv. rate`, -`Search abs. top IS`, -`Search Exact match IS`, -`Search Impr. share`, -`Search Lost IS (rank)`, -`Search Lost IS (budget)`)
}

import_campaign_hourly_data <- function(file) {
  read_csv(file, skip=2) %>%
    filter(row_number(Campaign) < 337) %>%
    filter( !str_detect(Campaign, "Inmediate" )) %>%
    filter(Campaign != 'Search | Travel | Long Tail')
}

import_campaign_summary_data <- function(file) {
  read_csv(file, skip = 2) %>% 
    mutate(total_position = `Avg. position` * Impressions) %>%
    group_by(`Campaign type`) %>%
    summarize(imp = sum(Impressions), 
            clicks = sum(Clicks),
            position = sum(total_position),
            cost = sum(Cost)
  ) %>%
  mutate(avg_position = position / imp, 
         avg_cost = cost / clicks,
         ctr = clicks / imp)
}

import_keyword_data <- function(file) {
  read_csv(file) %>%
    replace_with_na_if(.predicate = is.character, condition = (~.x == "--") ) %>%
    replace_with_na_if(.predicate = is.character, condition = (~.x == "#N/A") ) %>%
    mutate(ctr = parse_number(CTR),
           interaction_rate = parse_number(`Interaction rate`),
           convertion_rate = parse_number(`Conv. rate`),
           avg_cpc = parse_number(`Avg. CPC`),
           bounce_rate = parse_number(`Bounce rate`),
           search_imp_share = parse_number(`Search impr. share`),
           roi_per_conv = parse_number(`ROI/Conv`),
           total_roi = parse_number(`Total ROI`),
           cost = Cost,
           match_type = `Match type`,
           roi = `ROI %`,
           traffic = replace(Traffic, Traffic == 'yes', TRUE ),
           traffic = replace(traffic, traffic == 'no', FALSE),
           have_sales = replace(`Have sales`, `Have sales` == 'yes', TRUE ),
           have_sales = replace(have_sales, have_sales == 'no', FALSE),
           have_click = replace(`Have Click`, `Have Click` == 'yes', TRUE ),
           have_click = replace(have_click, have_click == 'no', FALSE),
           Competition = replace(Competition, Competition == "0", NA),
           avg_cost = parse_number(`Avg. cost`),
           max_cpc = parse_number(`Max. CPC`),
           top_of_page_bid_high = parse_number(`Top of page bid (high range)`),
           top_of_page_bid_low = parse_number(`Top of page bid (low range)`)
    ) %>%
    select(-CTR, -`Interaction rate`, -`Conv. rate`, -`Avg. CPC`, -Traffic, -`Have sales`,
           -`Bounce rate`, -`Search impr. share`, -`ROI/Conv`, -`ROI %`, -`Have Click`
    ) %>%
    mutate(roi_cat = ifelse(roi == 0, "no sale", as.character(roi))) %>%
    mutate(roi_cat = ifelse(roi > 0, "positive", as.character(roi_cat))) %>%
    mutate(roi_cat = ifelse(roi < 0, "negative", as.character(roi_cat)))
}
