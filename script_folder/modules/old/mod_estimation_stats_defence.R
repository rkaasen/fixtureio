

estimation_page_stats_defence_UI <- function(id) {
  
  fluidPage(
    tags$style(HTML("
    .gradient-bg-defence{
      background: linear-gradient(to bottom, #6db543, #b6daa1); 
      height: 100vh;
    }
  ")),
    div(class = "gradient-bg-defence",
        
        # home and away team
        fluidRow(
          br(),
          column(1),
          column(10,
                 sliderInput(NS(id, "change_defence"), "Change Defence", min = 0, max = 100, value = 40, step = 5),
          ),
          column(1),
        ),
        
    )
    
  )
  
  # ~~~~~~~~~~~
}


estimation_page_stats_defence_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$change_defence , {
      r6$val_defence <-  input$change_defence
      trigger("change_val_defence")
    })
    
  })
}