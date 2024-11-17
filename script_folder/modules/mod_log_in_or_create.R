# login_module.R

login_UI <- function(id) {
  fluidPage(
    
    hidden(
      div(class = "full-page-spinner", tags$img(src = "spinner.gif"))
    ),
    
    style = "min-height: 90vh; display: flex; flex-direction: column; justify-content: space-between;", # Main container for full height
    useShinyjs(),
    fluidRow(
      
      style = "display: flex; align-items: flex-start; justify-content: center; padding-top: 20px;",
      fluidRow(
        column(
          width = 6, 
          offset = 3,
          style = "background-color: #eef2f5;",
          class = "rounded-column",
          
          # Login UI
          div(
            id = NS(id, "login_div"),
            h2("Welcome Back!"),
            tags$form(
              textInput(NS(id, "username"), "Username"),      # Using default Shiny input
              passwordInput(NS(id, "password"), "Password"),  # Using default Shiny input
              actionButton(NS(id, "login_btn"), "Log In", class = "reactable-button")
            ),
            br(), div(style = "height: 10px;"),
            actionLink(NS(id, "show_signup"), "Not a user yet? Sign up"),
            br(), div(style = "height: 10px;"),
            h4(textOutput(NS(id, "login_message")), style = "color: red;")
          ),
          
          # Signup UI
          div(
            id = NS(id, "signup_div"),
            style = "display: none;",
            h2("Create an Account"),
            tags$form(
              textInput(NS(id, "new_username"), "Choose a Username"),       # Default Shiny input
              passwordInput(NS(id, "new_password"), "Choose a Password"),   # Default Shiny input
              textInput(NS(id, "email"), "Email (optional)", placeholder = "For password recovery and security")
            ),
            div(class = "note-text", "We respect your privacy. No spam, just for account recovery."),
            actionButton(NS(id, "signup_btn"), "Sign Up", class = "btn-success btn-lg btn-block"),
            br(), div(style = "height: 10px;"),
            actionLink(NS(id, "show_login"), "Already have an account? Log in"),
            br(), div(style = "height: 10px;"),
            h4(textOutput(NS(id, "signup_message")), style = "color: red;")
          )
        )
      )
      
    )
  )
}




login_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    # Create a reactive value to hold the message
    login_message <- reactiveVal("")
    signup_message <- reactiveVal("")
    
    # Helper function to set a message temporarily
    show_message_login <- function(message_text) {
      
      login_message(message_text)
      output$login_message <- renderText({
        login_message()
      })
      showNotification(message_text, type = "error", duration = 3)
      
      shinyjs::delay(3000, { login_message("") })  
      output$login_message <- renderText({
        login_message()
      })
      
    }
    
    show_message_signup <- function(message_text) {
      
      signup_message(message_text)
      output$signup_message <- renderText({
        signup_message()
      })
      showNotification(message_text, type = "error", duration = 3)
      
      shinyjs::delay(3000, { signup_message("") }) 
      output$signup_message <- renderText({
        signup_message()
      })
      
    }
    
    
    # Toggle visibility for login and signup forms
    observeEvent(input$show_signup, {
      shinyjs::hide("login_div")
      shinyjs::show("signup_div")
    })
    
    observeEvent(input$show_login, {
      shinyjs::hide("signup_div")
      shinyjs::show("login_div")
    })
    
    # Log in process
    observeEvent(input$login_btn, {
      req(input$username, input$password)

      shinyjs::show(selector = ".full-page-spinner") 
      
      con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = Sys.getenv("DB_PORT", "5432")
      )
      on.exit({
        dbDisconnect(con)
      }, add = TRUE)
      
      query <- "SELECT user_id, password_hash, username, bets_week_starting FROM users WHERE username = $1 AND is_active = TRUE"
      user_data <- dbGetQuery(con, query, list(input$username))
      
      if (nrow(user_data) == 1 && bcrypt::checkpw(input$password, user_data$password_hash)) {
        r6$user_info$logged_in <- TRUE
        r6$user_info$user_id <- user_data$user_id
        r6$user_info$username <- user_data$username
        r6$user_info$bets_week_starting <- user_data$bets_week_starting

        r6$user_info$bets <- fetch_table_all_bets(r6)
        
        update_last_logged_in_db(user_id)
        
        trigger("user_logged_in")
        
      } else {
        show_message_login("Invalid username or password.")
      }
      
      shinyjs::hide(selector = ".full-page-spinner") # Hide spinner when done
      user_data = NULL
      
    })
    
    # Sign up process
    observeEvent(input$signup_btn, ignoreInit = T, {
      req(input$new_username, input$new_password, input$email)
      
      shinyjs::show(selector = ".full-page-spinner") 
      
      con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = Sys.getenv("DB_PORT", "5432")
      )
      on.exit(dbDisconnect(con), add = TRUE)
      
      query <- "SELECT COUNT(*) FROM users WHERE username = $1"
      user_exists <- dbGetQuery(con, query, list(input$new_username))$count > 0
      
      if (user_exists) {
        
        show_message_signup("Username or email already exists.")
        
      } else {
        hashed_password <- bcrypt::hashpw(input$new_password)
        write_data_to_db_users(username = input$new_username, password_hash = hashed_password, email = input$email, role = "user")
        
        shinyjs::hide("signup_div")
        shinyjs::show("login_div")
        
        showModal(
          modalDialog(
            title = ("Sign Up Succelssfull!"),
            h5("WELCOME TO FIXTURE IO")
          ))
        
      }
      
      shinyjs::hide(selector = ".full-page-spinner") # Hide spinner when done
      user_exists = NULL
      
    })
  })
}


