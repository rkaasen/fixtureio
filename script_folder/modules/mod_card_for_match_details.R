

card_for_match_details_UI <- function(id) {
  
  fluidRow(
    
    actionButton(inputId = NS(id, "external_toggle"), label = "Match Stats", class = "reactable-button"),
    
    hidden(
      div(
        id = NS(id, "card_div"),
        align = "left",
        
        card(
          id = NS(id, "my_card"),
          full_screen = TRUE,
          class = "p-3",
          # card_header(h3("Teams- and Match details")),
          card_body(
            navs_tab_card(
              id = NS(id,"tabs"),  
              # HOME TEAM TAB
              nav(value = NS(id,"home_team_tab"), h4(textOutput(NS(id,"home_team"))), class = "my-gradient-background-white-start",
                  div( align = "center",
                       uiOutput(NS(id,"logo_ht")),
                       div(style = "height: 10px;"),
                       reactableOutput( NS(id,"home_last_10"))
                  )
                  
              ),
              
              # MATCH TAB
              nav(value = NS(id,"the_match_tab"), h4("SELECTED MATCH"), class = "my-gradient-background-white-start",
                  div( align = "center",
                       div(style = "height: 20px;"),
                       match_data_row_UI(NS(id,"in_main_card")),
                       
                       div(style = "height: 10px;"),
                       
                       # fair offs row
                       fluidRow(
                         column(12, offset = 0,  
                                class = "rounded-column",
                                style = "background: #7283b9;",
                                fluidRow(
                                  column(4, align = "left", div(style = "height: 7px;"),
                                         h5("'Fair' odds based on estimation:", style = "color: white;"),
                                  ),
                                  column(4,
                                         fluidRow(
                                         column(2, offset = 1, align = "middle", h3(textOutput(NS(id,"fair_home")), style = "color: white;")),
                                         column(2, offset = 2, align = "middle", h3(textOutput(NS(id,"fair_draw")), style = "color: white;")),
                                         column(2, offset = 2, align = "middle", h3(textOutput(NS(id,"fair_away")), style = "color: white;")),
                                  ))

                                  

                                  
                                )
                         )
                       ),
                       
                       # Your odds row
                       fluidRow(
                         column(12, offset = 0,  
                                class = "rounded-column",
                                style = "background: #505c83;",
                                fluidRow(
                                  column(4, align = "left", div(style = "height: 9px;"),
                                         h5("Your current odds (Placeholder values):", style = "color: white;"),
                                  ),
                                  column(4,
                                         fluidRow(
                                           column(2, offset = 1, align = "middle", actionButton(NS(id,"home_odds"), 2.13, class = "odds-button-same")),
                                           column(2, offset = 2, align = "middle", actionButton(NS(id,"draw_odds"), 3.25, class = "odds-button-better")),
                                           column(2, offset = 2, align = "middle", actionButton(NS(id,"away_odds"), 3.59, class = "odds-button-worse"))
                                         ))
                                  
                                  
                                )
                         )
                       )
                  )  # "#505c83", "#282d3e"
              ),
              
              # AWAY TEAM TAB
              nav(value = NS(id,"away_team_tab"),h4(textOutput(NS(id,"away_team"))), class = "my-gradient-background-white-start",
                  div( align = "center",
                       uiOutput(NS(id,"logo_at")),
                       div(style = "height: 10px;"),
                       reactableOutput( NS(id,"away_last_10"))
                  )
              )
              
              
            )
          )
        )
      )
    )
  )
  
}


card_for_match_details_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
    
    match_data_row_Server("in_main_card", r6, expl_stats = F)
    
    # toggle the card:
    
    observeEvent(list(input$external_toggle, watch("open_analytics_card")), {
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-the_match_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    observeEvent(watch("open_analytics_card_home"), {
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-home_team_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    observeEvent(watch("open_analytics_card_away"), {
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-away_team_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    
    observeEvent(watch("full_screen_card_closed"), {
      shinyjs::hide("card_div")
    })
    
    
    # code for server:
    
    output$logo_ht <- renderUI({
      img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "15%", width = "15%")
    })
    output$logo_at <- renderUI({
      img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "15%", width = "15%")
    })
    
    
    observeEvent(list(input$external_toggle,watch("open_analytics_card_home"), watch("open_analytics_card_away"), watch("open_analytics_card")), ignoreInit = T, {
      home_team <- r6$selected_home_team
      away_team <- r6$selected_away_team
      
      output$home_team <- renderText(toupper(home_team))
      output$away_team <- renderText(toupper(away_team))
      
      output$home_last_10 <- renderReactable(
        f_last_10_table(r6$data$filtered, home_team)
      )
      output$away_last_10 <- renderReactable(
        f_last_10_table(r6$data$filtered, away_team)
      )

    })
    
    
    observeEvent(watch("update_main_prediction"), ignoreInit = T, {
      metrics <- r6$metrics
      
      # Calculate overall percentages
      overall_percentages <-
        f_calculate_overall_percentages(
          metrics,
          Home_label = r6$selected_home_team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short),
          Away_label = r6$selected_away_team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short),
          return_only_percentage_df = TRUE
        )
      
      home_perc = overall_percentages[1]
      draw_perc = overall_percentages[2]
      away_perc = overall_percentages[3]
      
      output$fair_home <- renderText(round(100/home_perc,2))
      output$fair_draw <- renderText(round(100/draw_perc,2))
      output$fair_away <- renderText(round(100/away_perc,2))
      
      
    })
    
    
  })
}

