library(shiny)
library(tidyverse)
library("scales")

source("file_upload_helper.r")
source("file_reader.r")
source("plots.r")
source("models.r")


sidebar <- navlistPanel(
    tabPanel("EDA",
             selectInput("eda_plot_selector", label = "Analysis",
                         choices = c("Anerage Position" = "average_position_point",
                                     "Campaign by Hour" = "campaign_by_hour_smooth",
                                     "Campaign by Day of Week" = "campaign_by_day_of_week_smooth",
                                     "Cost by Type" = "cost_by_type_bar",
                                     "CTR by Type" = "ctr_by_type_bar",
                                     "Campaign ROI" = "campaign_roi_bar",
                                     "Positive ROI Keywords" = "positive_roi_keyword_bar",
                                     "ROI by Keyword Match Type" = "roi_by_keyword_match_type_bar"
                                     ),
                         selected = "cost_by_type_bar"
                        ),
             plotOutput("eda_plot")
             ),
    tabPanel("Model",
             selectInput("model_plot_selector", label = "Analysis",
                         choices = c("Clicks LR" = "click_glm",
                                     "CTR MLR" = "ctr_lm"
                         ),
                         selected = NULL
             ),
             verbatimTextOutput("model_statistics"),
             fluidRow(
               column(3, verbatimTextOutput("model_confusion_table")),
               column(9, verbatimTextOutput("model_metrics"))
             ),
             plotOutput("roi_plot")
             ),
    tabPanel("File Upload",
             fluidRow(
               column(4, 
                      fileInput("file_campaign_summary", "Campaign Summary",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                      )
               ),
               column(4, 
                      fileInput("file_campaign_daily", "Campaign Daily Data",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                      )
               ),
               column(4, 
                      fileInput("file_campaign_hour", "Campaign Hourly Data",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                      )
               )
             ),
             fluidRow(
               column(4, fileInput("file_keyword", "Keyword Data",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"))
                       )
             )
    ),
    tabPanel("Prediction",
             fluidRow(
               column(4, fileInput("file_new_keywords", "Submit Keywords",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
               )
               ),
               column(4, actionButton("run_analysis", "Analyse", class="predict-button"))
             ),
             verbatimTextOutput("keyword_prediction_results")
    )
    
  )

ui <- fluidPage(
  theme = "style.css",
  fluidRow(
    column(1,  tags$img(class="logo", src="LogoIM.png") ),
    column(2,   titlePanel("InsuranceMarket.sg"), class="title" )
  ),
    sidebar
)