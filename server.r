server <-  function(input, output) {
  
  campaign_daily_data <- reactive({ df_from_file(input$file_campaign_daily, import_campaign_data)  }) 
  
  campaign_summary_data <- reactive({ df_from_file(input$file_campaign_summary, import_campaign_summary_data )})
  
  campaign_hourly_data <- reactive({ df_from_file(input$file_campaign_hour, import_campaign_hourly_data )})
  
  keyword_data <- reactive({ df_from_file(input$file_keyword, import_keyword_data )})
  
  new_keyword_data <- reactive({ df_from_file(input$file_new_keywords, import_keyword_data )})
  
  eda_plots <- list("average_position_point" = list("func" = campaign_average_position_point_plot, "arg" = campaign_daily_data),
                    "cost_by_type_bar" = list("func" = campaign_cost_by_type_bar_plot, "arg" = campaign_summary_data),
                    "ctr_by_type_bar" = list("func" = campaign_ctr_by_type_bar_plot, "arg" = campaign_summary_data),
                    "positive_roi_keyword_bar" =  list("func" = positive_roi_keyword_bar_plot, "arg" = keyword_data),
                    "campaign_roi_bar" = list("func" = campaign_roi_bar_plot, "arg" = keyword_data),
                    "roi_by_keyword_match_type_bar" =  list("func" = roi_by_keyword_match_type_bar_plot, "arg" = keyword_data),
                    "campaign_by_hour_smooth" = list("func" = campaign_by_hour_smooth_plot, "arg" = campaign_hourly_data),
                    "campaign_by_day_of_week_smooth" = list("func" = campaign_by_day_of_week_smooth_plot, "arg" = campaign_daily_data)
                    )

  output$eda_plot <- renderPlot({
    l <- eda_plots[[input$eda_plot_selector]]
    l[["func"]](isolate(l[["arg"]]()))
  })
  
  keyword_glm_sets <- reactive({
    get_keyword_glm_sets(keyword_data())
    
  })
  
  glm_model <- reactive({ 
    train_glm(isolate(keyword_glm_sets()$train_set))
  })
  
  click_glm_threshold <- reactive({
    total_true_row <- sum(keyword_data()$traffic == TRUE)
    total_row <-  nrow(keyword_data())
    total_true_row / total_row
  })
  
  lm_model <- reactive({ 
    train_ctr_lm(isolate(keyword_data()))
  })
  
  
  observeEvent( input$model_plot_selector, {
    if(input$model_plot_selector == "click_glm"){
      output$model_statistics <- renderPrint({
        summary(glm_model())
      })

      output$model_confusion_table <- renderPrint({
        test_set <- keyword_glm_sets()$test_set
        train_set <- keyword_glm_sets()$train_set
        test_predict <-  predict(glm_model(), newdata = test_set, type = 'response')
        p_class <- ifelse(test_predict > click_glm_threshold(), 1, 0)
        #p_class <- ifelse(test_predict > 0.5, 1, 0)
        c_table <-  table(test_set$have_click, p_class)
        c_table
      })
      
      output$model_metrics <- renderPrint ({
        test_set <- keyword_glm_sets()$test_set
        train_set <- keyword_glm_sets()$train_set
        test_predict <-  predict(glm_model(), newdata = test_set, type = 'response')
        p_class <- ifelse(test_predict > click_glm_threshold(), 1, 0)
        #p_class <- ifelse(test_predict > 0.5, 1, 0)
        c_table <-  table(test_set$have_click, p_class)
        c_table
        data.frame(
          TPR = percent(c_table[4]/(c_table[3] + c_table[4])),
          FPR = percent(c_table[2]/(c_table[1] + c_table[2])),
          TNR = percent(c_table[1]/(c_table[1] + c_table[2])),
          FNR = percent(c_table[3]/(c_table[3] + c_table[4])),
          ACCURACY = percent((c_table[1] + c_table[4])/sum(c_table))
        )

      })
      output$roi_plot <- renderPlot({
        roi_plot(glm_model(), isolate(keyword_glm_sets()$test_set))
      })
      
    } else if(input$model_plot_selector == "ctr_lm"){
        
        output$model_statistics <- renderPrint({
          summary(lm_model())
        })
        
        output$model_confusion_table <- renderPrint({
          list("R Squared" =  summary(lm_model())$r.squared)
        })
        
        output$model_metrics <- renderPrint ({
          res = residuals(lm_model())
          shapiro.test(res)
        })
        
        output$roi_plot <- renderPlot({
          par(mfrow=c(2,2))
          plot(lm_model())
        })
      
    }
  })
  
  observeEvent(input$run_analysis, {
    output$keyword_prediction_results <- renderPrint({
      keywords <- new_keyword_data() %>%
        mutate(match_type = replace(match_type, str_detect(Keyword, "[+]"), "BMM")) %>%
        mutate(Keyword = str_replace(Keyword, "^[+]", "")) %>%
        mutate(Keyword = str_replace_all(Keyword, "[+]", " ")) %>%
        mutate(Keyword = str_replace_all(Keyword, "[:punct:]", ""))
      click_predict <- predict(glm_model(), newdata = keywords, type = 'response')
      click_p_class <-ifelse(click_predict > click_glm_threshold(), 1, 0)
      ctr_predict <-  predict(lm_model(), newdata = keywords, type = 'response')
      data.frame(keyword = keywords$Keyword, click = click_p_class, ctr = percent(ctr_predict/100))
   })
  })


  # glm_model <- reactive({ 
  #   if(input$model_plot_selector == "glm"){
  #     train_glm(keyword_data()$train_set)
  #   }
  # })
  # 

  
}