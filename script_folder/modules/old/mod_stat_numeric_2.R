




if(F){
  metric_name =  "Goals_Scored_lx"
}


estimation_page_stats_numeric_UI <- function(id, header = "DEFAULT_HEADER", default_on = F) {
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css")
  # )
  
  style = "background: linear-gradient(to bottom, #f3f6f8, #c1c3c4);"

  
  fluidRow( id = NS(id, "everything"),
            column(10, offset = 1,  
                   class = "rounded-column",
                   style = style,

                   # header row
                   fluidRow(
                     column(width = 3, offset = 0, align = "left",
                            column(7, offset = 5, 
                                   hidden(h4(style = "font-weight: bold;",header, id = NS(id,"header_string"))),
                                   h6(textOutput(NS(id,"n_matches_string"))),
                                   
                                   ##
                                   hidden(
                                     radioButtons(NS(id,"n_matches"), label = "",
                                                  choices = list("Latest 5 Games" = 5, "Latest 10 Games" = 10), 
                                                  selected = 5)
                                   ),
                            )
                     ),
                     column(width = 5, offset = 0, align = "center",
                            fluidRow(id = NS(id, "rank_row"),
                                     column(12, offset = 0,align = "center",
                                            column(width = 12,align = "center",
                                                   plotOutput(NS(id,"ggplot_divergent"), width = "550px", height = "90px")
                                            ),
                                     )
                            ),
                            
                            ##
                            column(10, offset = 1, 
                                   hidden(
                                     fluidRow(id = NS(id, "ggplot_rank_row"), 
                                              div(style = "height: 5px;"),
                                              plotOutput(NS(id,"ggplot_rank"), width = "500px", height = "160px")
                                     )
                                   )
                            )
                     ),
                     column(width = 3, offset =  0, align = "left",
                            fluidRow(
                              column(7, offset = 3, id = NS(id, "include_button_col"), 
                                     div(style = "height: 15px;"),
                                     materialSwitch(inputId = NS(id,"include"), label = NULL, status = "primary", value = default_on ),
                              ),
                              column(7, offset = 3, id = NS(id, "weight_col"), 
                                     div(style = "height: 5px;"),
                                     h4(style = "font-weight: bold;", textOutput(NS(id,"weight"))),
                              ),
                              
                              ##
                              column(6, offset = 2,
                                     hidden(
                                       fluidRow(id = NS(id, "slider_row"),
                                                div(style = "height: 25px;"),
                                                sliderTextInput(NS(id,"Slider"),choices = paste0(seq(0,200,25), "%"), label = NULL, selected = "100%", width = "80%"),
                                       ))),
                              
                            ),
                            fluidRow(
                              column(width = 11,offset = 1,
                                     hidden(
                                       plotOutput(NS(id,"ggplot_percent"), width = "60%", height = "70px")
                                     )
                              )
                            )
                     ),
                     column(width = 1,
                     )
                   ),
                   
                   
                   
            ),column(1)
  )
  
  # ~~~~~~~~~~~
  # } else {}
}



estimation_page_stats_numeric_Server <- function(id, r6, 
                                                 metric_name =  "NO METIRC",
                                                 positive_metric = T,
                                                 header = "DEFAULT") {
  moduleServer(id, function(input, output, session) {
    
    if (header == "DEFAULT") {
      header <- id
    }
    
    # init lis. Will be overwritten
    l_perc_results <- list()
    
    # Controls for "output mode"
    observeEvent(watch("output_mode_trigger"), {
      
      if(r6$output_mode) {
        if(input$include == F){
          # output on, include off
          hide("everything")

        } else {
          # include on, output on
          show("rank_row")
          show("weight_col")
          
          hide("ggplot_rank_row")
          hide("include_button_col")
          hide("n_matches")
          hide("slider_row")
          hide("ggplot_percent")
          hide("header_string")
        }
        
      } else {
        # output off
        show("everything")
        
        hide("weight_col")
        
        # show the "include = ON" as normal
        if ( input$include ){
          # output off, include on
          hide("rank_row")
          
          show("ggplot_rank_row")
          show("include_button_col")
          show("n_matches")
          show("slider_row")
          # show("header_string")
        }
        
        
        # SERVER:
        
        observeEvent(list(input$n_matches,watch("button_to_estimation"), input$include),ignoreInit = T, {
          
          if (input$include ){
            
            # print("include")
            
            show("ggplot_rank_row")
            show("slider_row")
            show("n_matches")
            # show("ggplot_percent")
            hide("rank_row")
            # show("header_string")
            
            trigger("update_main_prediction")
          } else {
            
            # print("exclude")
            
            hide("ggplot_rank_row")
            hide("slider_row")
            hide("n_matches")
            hide("ggplot_percent")
            show("rank_row")
            hide("header_string")
            
          }
          
          
          # Make string for header
          output$n_matches_string = renderText(glue("Latest {input$n_matches} Matches"))
          output$weight = renderText(glue("{input$Slider}"))
          
          # Take n_matches as numeric
          n_matches = input$n_matches %>% as.numeric()
          
          # calculate all metrics
          metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches) %>% 
            filter(metric == metric_name) %>% 
            mutate(counter = 1) %>% 
            arrange(rank) %>% 
            mutate(
              label_x =  paste0(value, "\n", "(", rank %>% toOrdinal(), ")")
            )
          
          # filter to metriC and hometeam
          home_team_df  = metrics_df %>%
            filter(
              Team==r6$selected_home_team,
            )
          output$rank_ht  = renderText(home_team_df$rank  %>% toOrdinal())
          output$value_ht = renderText(home_team_df$value)
          
          # filter to metriC and awayteam
          away_team_df  = metrics_df %>% 
            filter(
              Team==r6$selected_away_team,
            )
          output$rank_at  = renderText(away_team_df$rank %>% toOrdinal())
          output$value_at = renderText(away_team_df$value)
          
          
          
          # ~~~~~~~~~~~~~~~~~~~~
          # MAKE GGPLOTS
          
          output$ggplot_rank = renderPlot({
            f_plot_ranks(metrics_df = metrics_df,
                         home_team = r6$selected_home_team, 
                         away_team = r6$selected_away_team,
                         team_list_league = r6$team_list$league,
                         title_stat = header
            )
          }, bg="transparent")
          
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
          
          
          output$ggplot_divergent = renderPlot({
            f_plot_divergent_bar(metrics_df = metrics_df,
                                 home_team = r6$selected_home_team, 
                                 away_team = r6$selected_away_team,
                                 title_stat = header
            )
          }, bg="transparent")
          
          
          
          
          if(input$include){
            r6$metrics[[metric_name]]$weight <- input$Slider
            r6$metrics[[metric_name]]$home_team_perc <- l_perc_results$home_team_perc
            r6$metrics[[metric_name]]$away_team_perc <- l_perc_results$away_team_perc
            r6$metrics[[metric_name]]$draw_team_perc <- 100 - l_perc_results$away_team_perc - l_perc_results$home_team_perc
            trigger("update_main_prediction")
          } else {
            r6$metrics[[metric_name]]$weight <- "0%"
            r6$metrics[[metric_name]]$home_team_perc <-0
            r6$metrics[[metric_name]]$away_team_perc <- 0
            r6$metrics[[metric_name]]$draw_team_perc <- 0
            trigger("update_main_prediction")
            
          }
          
        })
        
        observeEvent(input$Slider, ignoreInit = T,{
          if(input$include){
            r6$metrics[[metric_name]]$weight <- input$Slider
            trigger("update_main_prediction")
          } else {
            r6$metrics[[metric_name]]$weight <- "0%"
            trigger("update_main_prediction")
          }
        })
        
        
        
        
      }
      
    }) # observe on output trigger
    
  }) # module
}
