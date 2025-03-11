library(shiny)
library(bslib)
library(plotly)
library(shinyjs)
library(shinyWidgets)  # Added for progressBar

# Initialize global variables
initial_points <- 5
initial_health <- 100
egg_price_base <- 10
max_health <- 100
health_decay_rate <- 5 # Health lost per tick
health_gained_per_egg <- 10
points_per_tick <- 5
tick_interval_ms <- 500 # 5 seconds

# Initialize game state
game_state <- reactiveVal(list(
  egg_price = egg_price_base,
  price_history = data.frame(
    timestamp = Sys.time(),
    price = egg_price_base,
    action = "start"
  ),
  timestamp = Sys.time()
))

ui <- page_sidebar(
  title = "The Game of Eggs",
  
  sidebar = sidebar(
    width = 300,
    
    card(
      card_header("Game Controls"),
      actionButton("buy_eggs", "Buy Eggs", class = "btn-primary btn-lg btn-block"),
      hr(),
      h4("Current Egg Price:"),
      textOutput("current_price"),
      hr(),
      h4("Game Instructions:"),
      p("1. You earn points every 5 seconds"),
      p("2. Your health decreases over time"),
      p("3. Buy eggs to restore health"),
      p("4. Egg prices fluctuate over time"),
      p("5. If your health reaches 0, game over!")
    )
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Points",
      value = textOutput("points_value"),
      showcase = icon("coins"),
      theme = "primary"
    ),
    value_box(
      title = "Health",
      value = textOutput("health_value"),
      showcase = icon("heart"),
      theme = "danger",
      full_screen = FALSE,
      p(
        "Health:",
        shinyWidgets::progressBar("health_bar", value = 100, display_pct = TRUE, status = "danger")
      )
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header("Egg Price History"),
    card_body(
      min_height = 400,
      layout_column_wrap(
        style = css(grid_template_columns = "3fr 1fr"),
      plotlyOutput("price_history_plot", height = "400px"),
      plotlyOutput("price_history_plot_prediction", height = "400px")
      )
    )
    
  ),
  
  # Add JavaScript for periodic updates
  useShinyjs(),
  tags$script(HTML(
    "
    let gameInterval;
    $(document).ready(function() {
      gameInterval = setInterval(function() {
        Shiny.setInputValue('tick', Math.random());
      }, 5000);
    });
    "
  ))
)

server <- function(input, output, session) {
  # Player state
  player <- reactiveValues(
    points = initial_points,
    health = initial_health
  )
  
  # Update time-based events
  observeEvent(input$tick, {
    # Update player state
    player$points <- player$points + points_per_tick
    player$health <- max(0, player$health - health_decay_rate)
    
    # Update egg price
    current_state <- game_state()
    
    # Calculate new price with some randomness (between 70% and 130% of base price)
    new_price <- egg_price_base * runif(1, 0.7, 1.3) |> round(2) |> mean(current_state$egg_price)
    
    # Update game state
    new_state <- list(
      egg_price = new_price,
      price_history = rbind(
        current_state$price_history,
        data.frame(
          timestamp = Sys.time(),
          price = new_price,
          action = "update"
        )
      ),
      timestamp = Sys.time()
    )
    
    game_state(new_state)
    
    # Check if health is zero (game over)
    if (player$health <= 0) {
      showModal(modalDialog(
        title = "Game Over!",
        "Your health reached zero. Game over!",
        footer = actionButton("restart", "Restart Game", class = "btn-primary")
      ))
    }
  })
  
  # Buy eggs
  observeEvent(input$buy_eggs, {
    current_state <- game_state()
    current_price <- current_state$egg_price
    
    # Check if player has enough points
    if (player$points >= current_price) {
      # Deduct points
      player$points <- player$points - current_price
      
      # Increase health
      player$health <- min(max_health, player$health + health_gained_per_egg)
      
      # Add purchase to history
      new_state <- list(
        egg_price = current_price,
        price_history = rbind(
          current_state$price_history,
          data.frame(
            timestamp = Sys.time(),
            price = current_price,
            action = "purchase"
          ),
          data.frame(
            timestamp = Sys.time() + 1,
            price = current_price + 2,
            action = "update"
          )
        ),
        timestamp = current_state$timestamp
      )
      
      game_state(new_state)
      
      # Notify the user
      showNotification(
        paste("You bought eggs for", current_price, "points and gained health!"),
        type = "message"
      )
    } else {
      # Not enough points
      showNotification(
        paste("Not enough points! You need", current_price, "points to buy eggs."),
        type = "error"
      )
    }
    
  })
  
  # Restart game
  observeEvent(input$restart, {
    player$points <- initial_points
    player$health <- initial_health
    removeModal()
  })
  
  # UI Updates
  output$points_value <- renderText({
    paste0(player$points)
  })
  
  output$health_value <- renderText({
    paste0(round(player$health), "%")
  })
  
  output$current_price <- renderText({
    paste0(game_state()$egg_price, " points")
  })
  
  # Update health bar
  observe({
    shinyWidgets::updateProgressBar(
      session = session,
      id = "health_bar",
      value = player$health,
      status = if(player$health < 30) "danger" else if(player$health < 70) "warning" else "success"
    )
    
    print(game_state()$price_history)
  })
  
  # Price history plot
  output$price_history_plot <- renderPlotly({
    history <- game_state()$price_history |> 
      tail(20)
    
    purchaces <- history |> 
      filter(action == "purchase")
    
    # Create different colors for different actions
    colors <- ifelse(history$action == "purchase", "red", 
                    ifelse(history$action == "update", "steelblue", "black"))
    
    p <- plot_ly(history, x = ~timestamp, y = ~price, 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = 'steelblue'),
                marker = list(color = colors, size = 10)) |> 
      layout(title = "Egg Price History",
             xaxis = list(title = "Time"),
             yaxis = list(title = "Price (points)",
                          range = c(0, max(history$price) + 5)),
             hovermode = "closest") |> 
      add_annotations(
        x = purchaces$timestamp,
        y = purchaces$price,
        text = "Buy",
        showarrow = TRUE,
        arrowhead = 4,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = "red",
        ax = 20,
        ay = -40
      )
    
    p
  })
  
  output$price_history_plot_prediction <- renderPlotly({
    history <- game_state()$price_history |> 
      tail(20)
    
    
    prediction =data.frame(price=rnorm(n = 100, mean = egg_price_base * runif(1, 0.7, 1.3),sd = 1), 
                           time = Sys.time() + 1)
    
    prediction |> 
      ggplot(aes(y = price, x = time)) +
      geom_rug(length  = unit(10, "mm")) +
      theme_minimal() -> gg
    
      p <- ggplotly(gg) |> 
        layout(
          title = "Egg Price Prediction",
          xaxis = list(title = "Time"),
          yaxis = list(
          title = "Price (points)",
          range = c(0, max(history$price) + 5)
        ))
    
    p
  })
  
}

shinyApp(ui, server)
