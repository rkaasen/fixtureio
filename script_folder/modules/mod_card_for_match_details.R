

card_for_match_details_UI <- function(id) {
  
  fluidRow(
    
    hidden(
      div(class = "full-page-spinner", tags$img(src = "spinner.gif"))
    ),
    
    
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
                                         )),
                                  column(2, offset = 0, align = "right", div(style = "height: 12px;"),
                                         h5("Not live yet: ", style = "color: white;"),
                                         
                                  ),
                                  column(1, offset = 0, align = "left",div(style = "height: 3px;"),
                                         
                                         # disabled(
                                         actionButton(NS(id,"bet_btn"), " BET " ,class = "reactable-button", width = "75%", style = "padding-top: 8px;" )
                                         # )
                                  )
                                  
                                  
                                  
                                  
                                )
                         )
                       ),
                       
                       # ~~~~~~~~~~~~~~~~~~~~~~~~~~
                       # Your odds row
                       hidden(
                         fluidRow( id = NS(id,"your_odds_row"),
                                   
                                   column(12, offset = 0,  
                                          class = "rounded-column",
                                          style = "background: #505c83;",
                                          fluidRow(
                                            column(4, align = "left", div(style = "height: 9px;"),
                                                   h5("Your current odds (Placeholder values):", style = "color: white;"),
                                            ),
                                            column(4,
                                                   fluidRow(
                                                     column(2, offset = 1, align = "middle", actionButton(NS(id,"btn_home_odds"), textOutput(NS(id,"fair_home_dummy")), class = "odds-button-same")),
                                                     column(2, offset = 2, align = "middle", actionButton(NS(id,"btn_draw_odds"), textOutput(NS(id,"fair_draw_dummy")), class = "odds-button-better")),
                                                     column(2, offset = 2, align = "middle", actionButton(NS(id,"btn_away_odds"), textOutput(NS(id,"fair_away_dummy")), class = "odds-button-worse"))
                                                   ))
                                            
                                            
                                          )
                                   ),
                                   
                                   div(style = "height: 9px;"),
                                   
                                   fluidRow(
                                     style = "display: flex;",
                                     
                                     column(4, align = "center",
                                            class = "rounded-column",
                                            style = "background: linear-gradient(to bottom, #f7f7f7, #ececed); margin-left: 30px;",
                                            plotlyOutput(NS(id,"pie_chart"))
                                     ),
                                     
                                     column(7, align = "Left", id = NS(id, "user_info"),
                                            class = "rounded-column",
                                            style = "background: linear-gradient(to bottom, #f7f7f7, #ececed); margin-left: 20px;",
                                            h3("Place Bet:"),
                                            hidden(
                                              
                                              fluidRow(
                                                id = NS(id,"place_bet_row"),
                                                style = "display: flex;",
                                                column(6,
                                                       h5("Game result:"),
                                                       fluidRow(
                                                         style = "display: flex; align-items: center;",  # Use flexbox for alignment
                                                         div(style = "flex: 1; display: flex; align-items: flex-end;",  # This div will take all available space
                                                             h2(textOutput(NS(id, "odds_string")), 
                                                                style = 'color: #14499F; font-weight: bold; padding-right: 10px;'),
                                                             tags$h5(id = NS(id, "to_win_text"), " to win", style = ' padding-bottom: 5px;')
                                                         ),
                                                         
                                                       )
                                                ),
                                                column(2,
                                                       h5("Odds:", style = 'font-weight: bold; padding-bottom: 5px; margin-bottom: 0px;'), 
                                                       h1(textOutput(NS(id,"odds_value")), style = 'font-weight: bold; color: #14499F; padding-top: 0px;')
                                                ),
                                                column(1, div(style = "height: 25px;"), align = "middle",
                                                       actionButton(NS(id,"btn_place_bet"), label = icon("check-circle"), class = "odds-button-place-bet")
                                                ),
                                                column(1, div(style = "height: 25px;"), 
                                                       actionButton(NS(id,"btn_cancel_bet"), "Cancel", class = "odds-button-worse")
                                                ),
                                                
                                              )
                                              
                                            ),
                                            fluidRow(
                                              id = NS(id,"chose_bet_row"),
                                              h4("Choose your bet", style = 'color: #14499F; font-weight: bold;')
                                            ),
                                            
                                            div(style = "height: 10px;"),
                                            
                                            hr(),
                                            h3("Your bets on this match"),
                                            div(style = "height: 15px;"),
                                            reactableOutput( NS(id,"your_bets_table"))
                                     ),
                                     # hidden(
                                     column(7, id = NS(id, "no_user_info"),
                                            h3("Log in to use betting")
                                     )
                                     # )
                                   )
                         )
                       )
                  ),
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
    
    fair_home_dummy_val <- reactiveVal(NULL)
    fair_draw_dummy_val <- reactiveVal(NULL)
    fair_away_dummy_val <- reactiveVal(NULL)
    
    to_win <- reactiveVal(NULL)
    to_win_odds <- reactiveVal(NULL)
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
    
    match_data_row_Server("in_main_card", r6, expl_stats = F)
    
    # toggle the card:
    
    observeEvent(list(input$external_toggle, watch("open_analytics_card")), ignoreInit = T, {
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-the_match_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    observeEvent(watch("open_analytics_card_home"), ignoreInit = T,{
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-home_team_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    observeEvent(watch("open_analytics_card_away"), ignoreInit = T,{
      shinyjs::show("card_div")
      nav_select(id = "tabs", selected = "top_of_estimation_tab-card_module-away_team_tab")
      session$sendCustomMessage("toggleCardFullscreen", session$ns("my_card"))
    })
    
    
    observeEvent(watch("full_screen_card_closed"), {
      shinyjs::hide("card_div")
    })
    
    
    # code for server:
    
    output$logo_ht <- renderUI({
      img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "10%", width = "10%")
    })
    output$logo_at <- renderUI({
      img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "10%", width = "10%")
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
      
      shinyjs::hide("your_odds_row")
      
      if(r6$user_info$logged_in){
        shinyjs::show("user_info")
        shinyjs::hide("no_user_info")
      } else {
        shinyjs::hide("user_info")
        shinyjs::show("no_user_info")
      }
      
      
    })
    
    
    observeEvent(watch("update_main_prediction"), ignoreInit = T, {
      metrics <- r6$metrics
      
      # Calculate overall percentages
      overall_percentages <-
        f_calculate_overall_percentages(
          metrics,
          Home_label = r6$selected_home_team_short,
          Away_label = r6$selected_away_team_short,
          return_only_percentage_df = TRUE
        )
      
      home_perc = overall_percentages[1]
      draw_perc = overall_percentages[2]
      away_perc = overall_percentages[3]
      
      output$fair_home <- renderText(round(100/home_perc,2))
      output$fair_draw <- renderText(round(100/draw_perc,2))
      output$fair_away <- renderText(round(100/away_perc,2))
      
      # Needs to be calculated better:
      fair_home_dummy_val((100/home_perc)-0.04)
      fair_draw_dummy_val((100/draw_perc)+0.27)
      fair_away_dummy_val((100/away_perc)-0.16)
      
      output$fair_home_dummy <- renderText(round(as.numeric(fair_home_dummy_val()),2))
      output$fair_draw_dummy <- renderText(round(as.numeric(fair_draw_dummy_val()),2))
      output$fair_away_dummy <- renderText(round(as.numeric(fair_away_dummy_val()),2))
      
    })
    
    observeEvent(input$bet_btn, {
      
      shinyjs::show("your_odds_row")
      
      output$pie_chart <- renderPlotly({
        f_pie_n_bets(
          list_n_bets = c(440, 200, 700), 
          list_labels = c(r6$selected_home_team_short, "Draw", r6$selected_away_team_short)
        )
      })
      
      if(r6$user_info$logged_in){
        
        output$your_bets_table <- renderReactable({
          f_your_bets_table(r6)
        })
        
      }
      
    })
    
    observeEvent(input$btn_cancel_bet, {
      shinyjs::hide("place_bet_row")
      shinyjs::show("chose_bet_row")
    })
    observeEvent(list(input$btn_home_odds, input$btn_draw_odds, input$btn_away_odds), ignoreInit = T, {
      shinyjs::hide("chose_bet_row")
      shinyjs::show("place_bet_row")
    })
    
    observeEvent(input$btn_home_odds, {
      
      to_win(r6$selected_home_team)
      to_win_odds(as.numeric(fair_home_dummy_val()))
      
      output$odds_string <- renderText(paste0(r6$selected_home_team))
      output$odds_value  <- renderText(round(as.numeric(fair_home_dummy_val()),2))
      shinyjs::show("to_win_text")
    })
    observeEvent(input$btn_draw_odds, {
      
      to_win("DRAW")
      to_win_odds(as.numeric(fair_draw_dummy_val()))
      
      output$odds_string <- renderText(paste0("DRAW"))
      output$odds_value  <- renderText(round(as.numeric(fair_draw_dummy_val()),2))
      shinyjs::hide("to_win_text")
    })
    observeEvent(input$btn_away_odds, {
      
      to_win(r6$selected_away_team)
      to_win_odds(as.numeric(fair_away_dummy_val()))
      
      output$odds_string <- renderText(paste0(r6$selected_away_team))
      output$odds_value  <- renderText(round(as.numeric(fair_away_dummy_val()),2))
      shinyjs::show("to_win_text")
    })
    
    
    
    observeEvent(input$btn_place_bet, {
      
      match_id_chosen = paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending)
      
      # betting conditions
      active_bets_total <- r6$user_info$bets %>% filter(is.na(bet_concluded)) %>% nrow()
      bets_on_match <- format_bets_for_match_bets(r6$user_info$bets,match_id_chosen) %>% nrow()
      
      utc_time_now <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      utc_time_schedule <- r6$data$pl_schedule %>% 
        filter(
          HomeTeam == r6$selected_home_team,
          AwayTeam == r6$selected_away_team
        ) %>% 
        pull(utc_date)
      
      
      if( bets_on_match >2 ){
        showNotification("Max number of bets per match is 3", type = "error", duration = 5)
      } 
      # else if(active_bets_total > 9 ) {
      #   showNotification("Max number of total bets is 10", type = "error", duration = 5)
      # } 
      else if (utc_time_now > utc_time_schedule){
        showNotification("Game already started", type = "error", duration = 5)
      }
      else {
        
        shinyjs::show(selector = ".full-page-spinner") 
        
        
        write_data_to_db_bets(bet = to_win(), 
                              odds = round(to_win_odds(),2), 
                              match_id = paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending), 
                              user_id = r6$user_info$user_id)
        
        r6$user_info$bets <- fetch_table_all_bets(r6)
        update_n_bets_in_db(r6$user_info$user_id, r6$user_info$bets, r6$user_info$bets_week_starting, match_id_chosen)
        
        
        output$your_bets_table <- renderReactable({
          f_your_bets_table(r6)
        })
        
        shinyjs::hide(selector = ".full-page-spinner") 
        
      }
      
    })
    
    
    observeEvent(input$cancel, ignoreInit = T, {
      
      utc_time_now <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      utc_time_schedule <- r6$data$pl_schedule %>% 
        filter(
          HomeTeam == r6$selected_home_team,
          AwayTeam == r6$selected_away_team
        ) %>% 
        pull(utc_date)
      
      if (utc_time_now > utc_time_schedule){
        showNotification("Game already started", type = "error", duration = 5)
      }
      else {
        
        
        shinyjs::show(selector = ".full-page-spinner") 
        
        #CANCEL BET
        cancel_bet_in_db(bet_id = input$cancel)
        
        r6$user_info$bets <- fetch_table_all_bets(r6)
        update_n_bets_in_db(r6$user_info$user_id, r6$user_info$bets, r6$user_info$bets_week_starting, match_id_chosen)
        
        output$your_bets_table <- renderReactable({
          f_your_bets_table(r6)
        })
        
        shinyjs::hide(selector = ".full-page-spinner") 
        
      }
      
    })
    
    
    
  })
}