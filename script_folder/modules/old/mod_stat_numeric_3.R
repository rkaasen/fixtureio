




if(F){
  metric_name =  "Goals_Scored_lx"
}


estimation_page_stats_numeric_UI <- function(id, header = "DEFAULT_HEADER", h2h = F, home_away = F) {
  
  
  if(header == "DEFAULT_HEADER"){
    header = id
  }
  
  div(
    id = NS(id, "hide_all"),
    
    hidden(
      div(  
        id = NS(id, "hide_stat"),  # Attach the id here for shinyjs::show/hide functionality
        fluidRow( 
          
          
          style = "margin-top: -15px; margin-bottom: -15px; padding-top = 0px;",  
          
          column(10, offset = 1,  
                 
                 # div(
                 class = "rounded-column",
                 style = if(h2h) {
                   "background: linear-gradient(to bottom, #f3f6f8, #ffb3b3); padding-top: 0px;"
                 } else {
                   "background: linear-gradient(to bottom, #f3f6f8, #c1c3c4); padding-top: 0px;"
                 },
                 
                 fluidRow(
                   column(1, 
                          
                          align = "center",
                          div(style = "height: 20px;"),
                          if(h2h){} else{
                            div(style="display:inline-block;text-align: center;",
                                actionButton(NS(id,"compare_btn"), label = icon("chart-line"), width = "60px",style = "height: 35px",class = "reactable-button" ))
                          }
                   ),
                   
                   column(width = 2, offset = 0, align = "left",
                          column(10, offset = 2, 
                                 # h6(textOutput(NS(id,"n_matches_string"))),
                                 div(
                                   style = "margin-bottom: 0px;",  # adjust margin
                                   
                                   if(h2h | home_away){
                                     radioButtons(NS(id,"n_matches"), label = "",
                                                  choices = list("Latest 3 Games" = 3, "Latest 5 Games" = 5),
                                                  selected = 3)
                                   } else{
                                     radioButtons(NS(id,"n_matches"), label = "",
                                                  choices = list("Latest 5 Games" = 5, "Latest 10 Games" = 10),
                                                  selected = 5)
                                   }
                                 )
                          )
                   ),
                   column(width = 6, offset = 0, align = "center",
                          column(width = 12,align = "center",
                                 plotOutput(NS(id,"ggplot_divergent"), width = "80%", height = "70px")
                          ),
                          
                   ),
                   column(width = 3, offset =  0, align = "left",
                          column(6, offset = 2,
                                 # h6("Weight of stat:"),
                                 div(style = "height: 20px;"),
                                 sliderTextInput(NS(id,"Slider"),choices = paste0(seq(0,200,25), "%"), label = NULL, selected = "100%", width = "120%"),
                          ),
                   ),
                 ),
                 
                 # ) # end of div
                 
          )
        )
      )
    ),
    
    div(
      id = NS(id, "not_enough_data"),  # Set the ID here on a simpler container (div)
      fluidRow(
        style = "margin-top: -15px; margin-bottom: -15px; padding-top: 0px;",  # Adjust as needed
        column(10, offset = 1, align = "center",
               class = "rounded-column",
               style = "background: linear-gradient(to bottom, #f3f6f8, #ffb3b3); padding-top: 0px;",
               div(style = "height: 6px;"),
               h6("Not enough data for:"),
               h6(textOutput(NS(id,"not_enough_data_text"))),
        )
      )
    )
    
  )
  
}



estimation_page_stats_numeric_Server <- function(id, r6, 
                                                 metric_name =  "NO METIRC",
                                                 positive_metric = T,
                                                 header = "DEFAULT",
                                                 h2h = F,
                                                 home_away = F) {
  moduleServer(id, function(input, output, session) {
    
    if (header == "DEFAULT") {
      header <- id
    }
    # init lis. Will be overwritten
    l_perc_results <- list()
    
    # SERVER:
    observeEvent(list(input$n_matches,watch("button_to_estimation"), watch("new_stats_chosen")),ignoreInit = T, {
      
      if(id %in% r6$chosen_stats_list){
        
        # Check for h2h that we have enough data:
        
        # calculate all metrics
        n_matches = input$n_matches %>% as.numeric()
        
        if(h2h){
          h2h_teams = c(r6$selected_home_team, r6$selected_away_team)
        } else{ h2h_teams = "SKIP" }
        
        metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches, h2h_teams = h2h_teams, home_away = home_away) %>%
          filter(metric == metric_name)
        
        # filter to metriC and hometeam
        
        if(home_away){
          home_team_matches  = metrics_df %>%
            filter(
              Team==r6$selected_home_team,
              home_away == "home"
            ) %>% pull(n_matches_in_sample)
          
          # filter to metriC and awayteam
          away_team_matches  = metrics_df %>%
            filter(
              Team==r6$selected_away_team,
              home_away == "away"
            ) %>% pull(n_matches_in_sample)
          
        } else{
          home_team_matches  = metrics_df %>%
            filter(
              Team==r6$selected_home_team
            ) %>% pull(n_matches_in_sample)
          
          # filter to metriC and awayteam
          away_team_matches  = metrics_df %>%
            filter(
              Team==r6$selected_away_team
            ) %>% pull(n_matches_in_sample)
          
        }
        
        if(length(away_team_matches) == 0){
          away_team_matches = 0
        }
        if(length(home_team_matches) == 0){
          away_team_matches = 0
        }
        
        if((away_team_matches + 1) > n_matches && (home_team_matches + 1) > n_matches) {
          
          include_in_calc = T
          shinyjs::show("hide_all")
          shinyjs::show("hide_stat")
          shinyjs::hide("not_enough_data")
          
        } else{
          
          include_in_calc = F
          shinyjs::show("hide_all")
          shinyjs::hide("hide_stat")
          shinyjs::show("not_enough_data")
          
          output$not_enough_data_text = renderText(glue('"{header}"'))
          
        }
        
        
      } else{
        # If stat not chosen
        include_in_calc = F
        shinyjs::hide("hide_all")
      }
      
      if(include_in_calc){
        
        # Make string for header
        output$n_matches_string = renderText(glue("Latest {input$n_matches} Matches"))
        
        # Take n_matches as numeric
        n_matches = input$n_matches %>% as.numeric()
        
        if(h2h){
          h2h_teams = c(r6$selected_home_team, r6$selected_away_team)
        } else{ h2h_teams = "SKIP" }
        
        # calculate all metrics
        metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches, h2h_teams = h2h_teams, home_away= home_away) %>% 
          filter(metric == metric_name)
        
        if(home_away) {
          # filter to metriC and hometeam
          home_team_df  = metrics_df %>%
            filter(
              Team==r6$selected_home_team,
              home_away == "home"
            )
          
          # filter to metriC and awayteam
          away_team_df  = metrics_df %>% 
            filter(
              Team==r6$selected_away_team,
              home_away == "away"
            )
        } else{
          # filter to metriC and hometeam
          home_team_df  = metrics_df %>%
            filter(
              Team==r6$selected_home_team,
            )
          
          # filter to metriC and awayteam
          away_team_df  = metrics_df %>% 
            filter(
              Team==r6$selected_away_team,
            )
        }
        
        # ~~~~~~~~~~~~~~~~~~~~
        # MAKE GGPLOTS
        
        # RUN FUNCTION FOR PERCENTAGES + PLOT:
        l_perc_results <- f_plot_winning_prediction_percent(Home_rank = home_team_df$rank, 
                                                            Away_rank = away_team_df$rank, 
                                                            range = (metrics_df$rank %>% max - metrics_df$rank %>% min), 
                                                            Home_label = home_team_df$Team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short), 
                                                            Away_label = away_team_df$Team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short),
                                                            return_plot = F
                                                            
        )
        
        # MAIN PLOT:
        
        output$ggplot_divergent = renderPlot({
          f_plot_divergent_bar(metrics_df = metrics_df,
                               home_team = r6$selected_home_team, 
                               away_team = r6$selected_away_team,
                               title_stat = header,
                               home_away = home_away
          )
        }, bg="transparent")
      }
      
      
      
      if(include_in_calc){
        r6$metrics[[id]]$weight <- input$Slider
        r6$metrics[[id]]$home_team_perc <- l_perc_results$home_team_perc
        r6$metrics[[id]]$away_team_perc <- l_perc_results$away_team_perc
        r6$metrics[[id]]$draw_team_perc <- 100 - l_perc_results$away_team_perc - l_perc_results$home_team_perc
        trigger("update_main_prediction")
      } else {
        r6$metrics[[id]]$weight <- "0%"
        r6$metrics[[id]]$home_team_perc <-0
        r6$metrics[[id]]$away_team_perc <- 0
        r6$metrics[[id]]$draw_team_perc <- 0
        trigger("update_main_prediction")
        
      }
      
    })
    
    observeEvent(input$Slider, ignoreInit = T,{
      r6$metrics[[id]]$weight <- input$Slider
      trigger("update_main_prediction")
    })
    
    # modal:
    observeEvent( input$compare_btn, {
      
      tags$style(HTML("
        .modal-dialog {
          width: 80vw !important;  /* Set desired width percentage */
          max-width: 80vw !important;  /* Set maximum width */
        }
      "))
      
      
      # Take n_matches as numeric
      n_matches = input$n_matches %>% as.numeric()
      
      if(h2h){
        h2h_teams = c(r6$selected_home_team, r6$selected_away_team)
      } else{ h2h_teams = "SKIP" }
      
      # calculate all metrics
      metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches, h2h_teams = h2h_teams, home_away = home_away) %>% 
        filter(metric == metric_name)

      
      if(home_away){
        home_team_df  = metrics_df %>%
          filter(
            Team==r6$selected_home_team,
            home_away == "home"
          )
        
        home_team_df %>% print()
        
        # filter to metriC and awayteam
        away_team_df  = metrics_df %>% 
          filter(
            Team==r6$selected_away_team,
            home_away == "away"
          )
        
        away_team_df %>% print()
        
        output$ggplot_bar_home = renderPlot({
          f_plot_ranks(metrics_df = metrics_df,
                       home_team = r6$selected_home_team,
                       away_team = r6$selected_away_team,
                       positive_metric_plot = positive_metric,
                       home_away_value = "home"
          )
        }, bg="transparent")
        
        output$ggplot_bar_away = renderPlot({
          f_plot_ranks(metrics_df = metrics_df,
                       home_team = r6$selected_home_team,
                       away_team = r6$selected_away_team,
                       positive_metric_plot = positive_metric,
                       home_away_value = "away"
          )
        }, bg="transparent")
        
        
      } else{
        home_team_df  = metrics_df %>%
          filter(
            Team==r6$selected_home_team,
          )
        
        # filter to metriC and awayteam
        away_team_df  = metrics_df %>% 
          filter(
            Team==r6$selected_away_team,
          )
        
        output$ggplot_bar = renderPlot({
          f_plot_ranks(metrics_df = metrics_df,
                       home_team = r6$selected_home_team,
                       away_team = r6$selected_away_team,
                       positive_metric_plot = positive_metric
          )
        }, bg="transparent")
        
      }
      
      # RUN FUNCTION FOR PERCENTAGES + PLOT:
      l_perc_results <- f_plot_winning_prediction_percent(Home_rank = home_team_df$rank, 
                                                          Away_rank = away_team_df$rank, 
                                                          range = (metrics_df$rank %>% max - metrics_df$rank %>% min), 
                                                          Home_label = home_team_df$Team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short), 
                                                          Away_label = away_team_df$Team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short)
                                                          
      )
      
      output$ggplot_percent = renderPlot({
        l_perc_results$plot
      }, bg="transparent")
      
      
      output$logo_ht <- renderUI({
        img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "100%", width = "100%")
      })
      output$logo_at <- renderUI({
        img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "100%", width = "100%")
      })
      
      showModal(modalDialog(
        title = paste0("Details for  ", header),
        
        if(home_away){
          column(width = 12, align = "center",
                 h5(paste0(header, "  at HOME compared to league at HOME:")),
                 fluidRow(
                   column(2, 
                          uiOutput(session$ns("logo_ht"), width = "65%")
                   ),
                   column(10,
                          plotOutput(session$ns("ggplot_bar_home"), width = "100%", height = "250px"),
                   )
                 ),
                 br(),
                 h5(paste0(header, "  AWAY compared to league at AWAY:")),
                 fluidRow(
                   column(2, 
                          uiOutput(session$ns("logo_at"), width = "65%")
                   ),
                   column(10,
                          plotOutput(session$ns("ggplot_bar_away"), width = "100%", height = "220"),
                   )
                 ),
                 
                 
          )
        }else {
          
          column(width = 12, align = "center",
                 h5(paste0("Comparison of ", header, " to league:")),
                 plotOutput(session$ns("ggplot_bar"), width = "100%", height = "300px")
          )
        },
        
        hr(),
        column(width = 12, align = "center",
               h5(paste0("Contribution of ", header, " only:")),
               plotOutput(session$ns("ggplot_percent"), width = "75%", height = "125px"),
        ),
        easyClose = TRUE,  # Allows closing the modal by clicking outside
        footer = tagList(
          modalButton("Close"),  # A button to close the modal
        ),
        size = "l"
        
      ))
      
    }, ignoreInit = T
    )
    
    
    
    # }
    
    # }) # observe on output trigger
    
  }) # module
}
