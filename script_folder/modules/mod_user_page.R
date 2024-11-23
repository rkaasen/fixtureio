# login_module.R

library(shiny)
library(shinyjs)

user_page_UI <- function(id) {
  
  
  
  
  
  
  fluidPage(
    
    style = "min-height: 90vh; display: flex; flex-direction: column; justify-content: space-between;", # Main container for full height
    useShinyjs(),
    fluidRow(
      
      style = "display: flex; align-items: flex-start; justify-content: center; padding-top: 20px;",
      
      fluidRow( style = "display: flex; align-items: center;",
                column(width = 4, offset = 0,
                       h1(textOutput(NS(id, "user_names_text")))
                ),
                column(5, offset = 3, 
                       fluidRow( style = "display: flex; align-items: center;",
                                 column(3, offset=7, align = 'right',
                                        actionButton(NS(id, "to_analysis_bn"), "To Analysis",class = "reactable-button" )
                                 ),
                                 column(2, align = 'left',
                                        tagList(
                                          actionButton(NS(id, "log_out_btn"), "Log out",class = "reactable-button" ),
                                        )
                                 ),
                       ),
                       
                ),
                
                div(style  = "height: 40px;"),
                
                column(2, class = "rounded-column", style = "margin-left: 15px; margin-top: -10px; background: #dee2e6;",
                       h3("Select dates"),
                       div(style  = "height: 15px;"),
                       sliderTextInput(
                         inputId = NS(id, "date_range"), 
                         label = NULL,
                         choices = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "1 day"),
                         selected = c(as.Date("2022-03-01"), as.Date("2022-03-17")),
                       )
                ),
                
                column(2, offset = 0, 
                       uiOutput(NS(id, "betting_return_box"))
                ),
                
                column(2, offset = 0, 
                       uiOutput(NS(id, "bets_won_box"))
                ),
                
                column(2, offset = 3, align = "right",
                       uiOutput(NS(id, "bets_available_box"))
                ),
                
                
                
                div(style  = "height: 40px;"),
                
                fluidRow( style = "display: flex; align-items: center;",
                          column(4,
                                 
                                 plotlyOutput(NS(id,"winnings_ts_plot"))
                          ),
                          column(5,
                                 reactableOutput( NS(id,"all_bets_table")),
                          )
                          
                          
                )
                
      )
    )
    
  )
  
  
  
  
  
  
}



user_page_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$log_out_btn, ignoreInit =  T, {
      
      r6$user_info$logged_in <- FALSE
      r6$user_info$user_id <- NULL
      r6$user_info$username <- NULL
      r6$user_info$bets <- NULL
      r6$user_info$bets_week_starting <- NULL
      
      trigger("user_logged_out")
      
    })
    
    observeEvent(input$to_analysis_bn, ignoreInit = T, {
      trigger("button_app")
    })
    
    
    observeEvent(watch("user_logged_in"), ignoreInit = T, {
      
      output$user_names_text <- renderText(paste0("Welcome, ", r6$user_info$username))
      
      bet_dates <- r6$user_info$bets %>% select(match_id) %>% unique() %>% 
        left_join(r6$data$pl_historic %>% 
                    mutate(match_id = paste0(HomeTeam, "-", AwayTeam, "-", Season_ending_year)) %>% 
                    select(Date, match_id),
                  by = c("match_id")) %>% 
        filter(!is.na(Date)) %>% 
        pull(Date)
      
      bet_dates <- format(as.Date(bet_dates, format = "%d/%m/%Y"), "%Y-%m-%d") %>% as.Date()
      
      
      if(length(bet_dates)>0){
        first_bet =  format(as.Date(bet_dates %>% min(), format = "%d/%m/%Y"), "%Y-%m-%d") %>% as.Date() - 7
        last_bet =  today() %>% as.Date() + 14
      } else{
        first_bet =  today() %>% as.Date() - 7
        last_bet =  today() %>% as.Date() + 14
      }
      
      updateSliderTextInput(
        session = session,
        inputId = "date_range",
        choices = seq(first_bet, last_bet, by = "1 day"),
        selected = c(first_bet, last_bet),
        
      )
    })
    
    observeEvent(input$date_range, ignoreInit = T, {
      
      all_bets_from_user <-  f_format_all_bets(r6)
      
      output$all_bets_table <- renderReactable({
        f_all_bets_table(all_bets_from_user, input$date_range)
      })
      

      output$winnings_ts_plot <- renderPlotly({
        f_win_loss_bars_plotly(all_bets_from_user, input$date_range)
      })
      
      
      # VALUE BOXES
      
      output$betting_return_box <- renderUI({
        # Compute betting returns
        total_wins <- all_bets_from_user %>%
          filter(
            Date >= as.Date(input$date_range[1]),
            Date <= as.Date(input$date_range[2]),
            !is.na(bet_concluded)
          ) %>%
          pull(bet_concluded) %>%
          sum()
        
        total_wins %>% print()
        
        # Create the value box
        value_box(
          title = "Betting Returns",
          value = paste0(round(total_wins, 2)),
          showcase = bs_icon("bank2"), 
          theme = value_box_theme(
            bg = "#dee2e6", fg = "#14499F"   
          ),
          height = "138px"
        )
      })
      
      output$bets_won_box <- renderUI({
        
        number_wins <- all_bets_from_user %>%
          filter(
            Date >= as.Date(input$date_range[1]),
            Date <= as.Date(input$date_range[2]),
            !is.na(bet_concluded)
          ) %>%
          mutate(
            bet_won = ifelse(bet_concluded == -1, "No", "Yes")
          )
        
        losses <- number_wins %>% filter(bet_won == "No") %>% nrow()
        wins <- number_wins %>% filter(bet_won == "Yes") %>% nrow()
        total = losses + wins
        
        # Create the value box
        value_box(
          title = "Bets won",
          value = paste0(wins, " / ", total),
          showcase = bs_icon("bank2"), 
          theme = value_box_theme(
            bg = "#dee2e6", fg = "#14499F"   
          ),
          height = "138px"
        )
      })
      
      output$bets_available_box <- renderUI({
        
        n_bets_used <- r6$user_info$bets %>% filter(is.na(bet_concluded)) %>% nrow()
        
        # Create the value box
        value_box(
          title = "Bets placed this week",
          value = paste0(n_bets_used, " / ", round(r6$user_info$bets_week_starting,0)),
          showcase = bs_icon("bank2"), 
          theme = value_box_theme(
            bg = "#dee2e6", fg = "#14499F"   
          ),
          height = "138px"
        )
      })
      
      
    })
    
  })
}
