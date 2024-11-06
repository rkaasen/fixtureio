





estimation_page_stats_overall_UI <- function(id) {
  
  fluidPage(
    
    tags$style(HTML("
    .gradient-bg-overall{
      background: linear-gradient(to bottom, #d3b440, #efe3b7);
      height: 100vh;
    }
  ")),
    div(class = "gradient-bg-overall",
        
        fluidRow(
          br(),
          column(1),
          column(10,
                 sliderInput(NS(id, "change_overall"), "Change Overall", min = 0, max = 100, value = 60, step = 5),
          ),
          column(1),
        ),
        
    )
    
  )
  
  # ~~~~~~~~~~~
}


estimation_page_stats_overall_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$change_overall , {
      r6$val_overall <-  input$change_overall
      trigger("change_val_overall")
    })
    
  })
}
