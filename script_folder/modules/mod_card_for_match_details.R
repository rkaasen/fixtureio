

card_for_match_details_UI <- function(id) {
  
  fluidRow(
    
    # hidden(
    #   div(class = "full-page-spinner", tags$img(src = "spinner.gif"))
    # ),
    
    # 
    # actionButton(inputId = NS(id, "external_toggle"), label = "Match Stats", class = "reactable-button"),
    
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
                                           column(2, offset = 1, align = "middle", h4(textOutput(NS(id,"fair_home")), style = "color: white;")),
                                           column(2, offset = 2, align = "middle", h4(textOutput(NS(id,"fair_draw")), style = "color: white;")),
                                           column(2, offset = 2, align = "middle", h4(textOutput(NS(id,"fair_away")), style = "color: white;")),
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
                                                     column(2, offset = 1, align = "middle", actionButton(NS(id,"btn_home_odds"), textOutput(NS(id,"home_odds")))),
                                                     column(2, offset = 2, align = "middle", actionButton(NS(id,"btn_draw_odds"), textOutput(NS(id,"draw_odds")))),
                                                     column(2, offset = 2, align = "middle", actionButton(NS(id,"btn_away_odds"), textOutput(NS(id,"away_odds"))))
                                                   ))
                                            
                                            
                                          )
                                   ),
                                   
                                   div(style = "height: 9px;"),
                                   
                                   fluidRow(
                                     style = "display: flex;",
                                     
                                     column(4, align = "center", id = NS(id, "pie_chart_show"),
                                            class = "rounded-column",
                                            style = "background: linear-gradient(to bottom, #f7f7f7, #ececed); margin-left: 30px;",
                                            fluidRow(plotlyOutput(NS(id,"pie_chart"))),
                                            fluidRow(
                                              column(12, h4("Bets changed since opening: ")),
                                              column(4, h6(textOutput(NS(id,"home_team_odds_change")))),
                                              column(4, h6(textOutput(NS(id,"draw_team_odds_change")))),
                                              column(4, h6(textOutput(NS(id,"away_team_odds_change"))))
                                            )
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
                                     column(12, id = NS(id, "no_user_info"),
                                            h3(HTML("Please <span id='go_to_login' style='color: #14499F; text-decoration: underline; cursor: pointer;'>log in</span> to use this feature")),
                                     ),
                                     column(12, id = NS(id, "betting_closed"),
                                            div(style = "height: 50px;"),
                                            h3("Betting not open for this match")
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
    
    home_odds <- reactiveVal(NULL)
    draw_odds <- reactiveVal(NULL)
    away_odds <- reactiveVal(NULL)
    
    fair_home_odds <- reactiveVal(NULL)
    fair_draw_odds <- reactiveVal(NULL)
    fair_away_odds <- reactiveVal(NULL)
    
    to_win <- reactiveVal(NULL)
    to_win_odds <- reactiveVal(NULL)
    
    match_id_chosen <- reactiveVal(NULL)
    
    l_bets <- reactiveVal(NULL)
    
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
      
      shinyjs::removeClass(id = "top_of_estimation_tab-card_module-in_main_card-ggplot_overall_percent_div", class = "clickable-border", asis = T)
      shinyjs::removeClass(id = "top_of_estimation_tab-in_main_page-ggplot_overall_percent_div", class = "clickable-border", asis = T)
      
      home_team <- r6$selected_home_team
      away_team <- r6$selected_away_team
      
      match_id_chosen(paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending))
      
      output$home_team <- renderText(toupper(home_team))
      output$away_team <- renderText(toupper(away_team))
      
      output$home_last_10 <- renderReactable(
        f_last_10_table(r6$data$filtered, home_team)
      )
      output$away_last_10 <- renderReactable(
        f_last_10_table(r6$data$filtered, away_team)
      )
      
      # hide the odds each time the card is opened
      shinyjs::hide("your_odds_row")
      
      # Reset the 3 buttons, so they dont show last game's values
      output$home_odds <- renderText("NA")
      output$away_odds <- renderText("NA")
      output$draw_odds <- renderText("NA")
      
      # text under the buttons not to show last selected team (from last card)
      shinyjs::hide("place_bet_row")
      shinyjs::show("chose_bet_row")
      
      # check if betting is open
      
      # match_id_chosen = paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending)
      
      bet_open <- f_match_open_for_betting() %>% 
        filter(match_id == match_id_chosen()) %>% 
        pull(bet_open)
      
      
      if(r6$user_info$logged_in){
        
        shinyjs::hide("no_user_info")
        
        if(bet_open){
          shinyjs::show("user_info")
          shinyjs::show("pie_chart_show")
          
          shinyjs::hide("betting_closed")
        } else{
          shinyjs::hide("user_info")
          shinyjs::hide("pie_chart_show")
          
          shinyjs::show("betting_closed")
        }
        
      } else {
        shinyjs::show("no_user_info")
        
        shinyjs::hide("user_info")
        shinyjs::hide("pie_chart_show")
        if(bet_open){
          shinyjs::hide("betting_closed")
        }else {
          shinyjs::show("betting_closed")
        }
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
      
      fair_home_odds(round(100/home_perc,2))
      fair_draw_odds(round(100/draw_perc,2))
      fair_away_odds(round(100/away_perc,2))
      
      output$fair_home <- renderText(fair_home_odds())
      output$fair_draw <- renderText(fair_draw_odds())
      output$fair_away <- renderText(fair_away_odds())
      
    })
    
    observeEvent(input$bet_btn, {
      
      shinyjs::show("your_odds_row")
      
      # IS BET OPEN?
      
      bet_open <- f_match_open_for_betting() %>% 
        filter(match_id == match_id_chosen()) %>% 
        pull(bet_open)

      if(bet_open){
        if(r6$user_info$logged_in){
          
          l_bets_fetched <- fetch_total_bets_on_match(match_id_chosen())
          l_bets(l_bets_fetched)
        }
        
        trigger("set_odds_and_update_pie")
      }
      
    })
    
    observeEvent(watch("set_odds_and_update_pie"), ignoreInit = T, {
      
      odds_row <- r6$odds$pl %>% 
        filter(
          HomeTeam == r6$selected_home_team,
          AwayTeam == r6$selected_away_team
        )
      
      home_perc_bet <- odds_row %>% pull(`Home Win Probability (%)`)
      away_perc_bet <- odds_row %>% pull(`Away Win Probability (%)`)
      draw_perc_bet <- odds_row %>% pull(`Draw Probability (%)`)
      
      # Needs to be calculated better:
      home_odds((100/home_perc_bet))
      away_odds((100/away_perc_bet))
      draw_odds((100/draw_perc_bet))
      
      # print(home_odds())
      # print(fair_home_odds())
      
      output$home_odds <- renderText(round(as.numeric(home_odds()),2))
      output$away_odds <- renderText(round(as.numeric(away_odds()),2))
      output$draw_odds <- renderText(round(as.numeric(draw_odds()),2))
      
      if(home_odds()-fair_home_odds() > 0.2){
        shinyjs::addClass(id = "btn_home_odds", class = "odds-button-better")
      } else if(home_odds()-fair_home_odds() < -0.2){
        shinyjs::addClass(id = "btn_home_odds", class = "odds-button-worse")
      } else{
        shinyjs::addClass(id = "btn_home_odds", class = "odds-button-same")
      }
      
      if(draw_odds()-fair_draw_odds() > 0.2){
        shinyjs::addClass(id = "btn_draw_odds", class = "odds-button-better")
      } else if(draw_odds()-fair_draw_odds() < -0.2){
        shinyjs::addClass(id = "btn_draw_odds", class = "odds-button-worse")
      } else{
        shinyjs::addClass(id = "btn_draw_odds", class = "odds-button-same")
      }
      
      if(away_odds()-fair_away_odds() > 0.2){
        shinyjs::addClass(id = "btn_away_odds", class = "odds-button-better")
      } else if(away_odds()-fair_away_odds() < -0.2){
        shinyjs::addClass(id = "btn_away_odds", class = "odds-button-worse")
      } else{
        shinyjs::addClass(id = "btn_away_odds", class = "odds-button-same")
      }
      
      if(r6$user_info$logged_in){
        
        output$pie_chart <- renderPlotly({
          f_pie_n_bets(
            list_n_bets = l_bets(), 
            list_labels = c(r6$selected_home_team_short, "Draw", r6$selected_away_team_short)
          )
        })
        
        output$your_bets_table <- renderReactable({
          f_your_bets_table(r6)
        })
        
        
        output$home_team_odds_change <- renderText(paste0(r6$selected_home_team_short, ": \n", round(odds_row %>% pull(changed_since_start_home),3)))
        output$draw_team_odds_change <- renderText(paste0("DRAW", ": \n", round(odds_row %>% pull(changed_since_start_draw),3)))
        output$away_team_odds_change <- renderText(paste0(r6$selected_away_team_short, ": \n", round(odds_row %>% pull(changed_since_start_away),3)))
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
      to_win_odds(as.numeric(home_odds()))
      
      output$odds_string <- renderText(paste0(r6$selected_home_team))
      output$odds_value  <- renderText(round(as.numeric(home_odds()),2))
      shinyjs::show("to_win_text")
    })
    observeEvent(input$btn_draw_odds, {
      
      to_win("DRAW")
      to_win_odds(as.numeric(draw_odds()))
      
      output$odds_string <- renderText(paste0("DRAW"))
      output$odds_value  <- renderText(round(as.numeric(draw_odds()),2))
      shinyjs::hide("to_win_text")
    })
    observeEvent(input$btn_away_odds, {
      
      to_win(r6$selected_away_team)
      to_win_odds(as.numeric(away_odds()))
      
      output$odds_string <- renderText(paste0(r6$selected_away_team))
      output$odds_value  <- renderText(round(as.numeric(away_odds()),2))
      shinyjs::show("to_win_text")
    })
    
    
    
    observeEvent(input$btn_place_bet, {
      
      # match_id_chosen = paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending)
      
      # betting conditions
      active_bets_total <- r6$user_info$bets %>% filter(is.na(bet_concluded)) %>% nrow()
      bets_on_match <- format_bets_for_match_bets(r6$user_info$bets,match_id_chosen()) %>% nrow()
      
      bet_open <- f_match_open_for_betting() %>% 
        filter(match_id == match_id_chosen()) %>% 
        pull(game_15_started)
      
      
      if( bets_on_match >2 ){
        showNotification("Max number of bets per match is 3", type = "error", duration = 5)
      } 
      else if(active_bets_total > 9 ) {
        showNotification("Max number of total bets is 10", type = "error", duration = 5)
      }
      else if (bet_open){
        showNotification("Game already started, or too close to kickoff", type = "error", duration = 5)
      }
      else {
        
        shinyjs::show(selector = ".full-page-spinner") 
        
        
        write_data_to_db_bets(bet = to_win(), 
                              odds = round(to_win_odds(),2), 
                              match_id = paste0(r6$selected_home_team, "-", r6$selected_away_team, "-", current_season_ending), 
                              user_id = r6$user_info$user_id)
        
        # r6$user_info$bets <- fetch_table_all_bets(r6)
        
        l_after_update <- full_update_after_bet_place(r6$user_info$user_id, 
                                                      r6$user_info$bets, 
                                                      r6$user_info$bets_week_starting, 
                                                      match_id_chosen())
        r6$odds$pl <- l_after_update[[2]]
        r6$user_info$bets <-l_after_update[[3]]
        
        output$your_bets_table <- renderReactable({
          f_your_bets_table(r6)
        })
        
        # update pie
        # l_bets <- fetch_total_bets_on_match(match_id_chosen())
        l_bets(l_after_update[[1]])
        trigger("set_odds_and_update_pie")
        
        shinyjs::hide(selector = ".full-page-spinner") 
        
      }
      
    })
    
    
    observeEvent(input$cancel, ignoreInit = T, {
      
      bet_open <- f_match_open_for_betting() %>% 
        filter(match_id == match_id_chosen()) %>% 
        pull(game_15_started)
      
      
      if (bet_open){
        showNotification("Game already started, or too close to kickoff", type = "error", duration = 5)
      }
      else {
        
        shinyjs::show(selector = ".full-page-spinner") 
        
        #CANCEL BET
        cancel_bet_in_db(bet_id = input$cancel)
        
        # r6$user_info$bets <- fetch_table_all_bets(r6)
        l_after_update <- full_update_after_bet_place(r6$user_info$user_id, r6$user_info$bets, r6$user_info$bets_week_starting, match_id_chosen())
        r6$odds$pl <- l_after_update[[2]]
        r6$user_info$bets <-l_after_update[[3]]
        
        # update_n_bets_in_db(r6$user_info$user_id, r6$user_info$bets, r6$user_info$bets_week_starting, match_id_chosen)
        
        output$your_bets_table <- renderReactable({
          f_your_bets_table(r6)
        })
        
        # update pie
        # l_bets <- fetch_total_bets_on_match(match_id_chosen)
        l_bets(l_after_update[[1]])
        trigger("set_odds_and_update_pie")
        
        
        shinyjs::hide(selector = ".full-page-spinner") 
        
      }
      
    })
    
    
    
  })
}