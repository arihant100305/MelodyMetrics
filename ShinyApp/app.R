### Importing the necessary packages
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shinyjs)
library(scales)
library(waiter)
library(tibble)
library(fmsb)
library(viridis)
library(plotly)

#load the dataset 
#load the dataset 
load("normalized_tracks_features.RData")
load("artist_metadata.RData")
load("all_top_tracks_final_data.RData")
load("recents.RData")
load("Top90s.RData")
load("Top80s.RData")
load("Top70s.RData")
load("Top60s.RData")
load("Top00s.RData")
load("Love_90s.RData")
load("Love_80s.RData")
load("Love_70s.RData")
load("Love_60s.RData")
load("Love_00s.RData")
load("Love_recents.RData")
load("Dance1_90s.RData")
load("Dance2_90s.RData")
load("Dance_00s.RData")
load("Dance_recents.RData")


# Data Preparation - Shift to other file
All00s = bind_rows(Top00s, Love_00s, Dance_00s) %>% distinct()
Allrecents = bind_rows(Recents, Love_recents, Dance_recents) %>% distinct()
All90s = bind_rows(Top90s, Love_90s, Dance1_90s, Dance2_90s) %>% distinct()
All80s = bind_rows(Top80s, Love_80s) %>% distinct()
All70s = bind_rows(Top70s, Love_70s) %>% distinct()
All60s = bind_rows(Top60s, Love_60s) %>% distinct()

All = bind_rows(Allrecents, All00s, All90s, All80s, All70s, All60s) %>% distinct()


# Prepare feature data function
prepare_feature_data <- function(track_id, features_df, feature_list) {
  # Error Handling
  # Input validation
  if (!is.data.frame(features_df)) {
    stop("features_df must be a data frame")
  }
  if (!all(feature_list %in% names(features_df))) {
    missing_features <- setdiff(feature_list, names(features_df))
    stop("Missing features in features_df: ", paste(missing_features, collapse = ", "))
  }
  if (!"track_id" %in% names(features_df)) {
    stop("features_df must contain a 'track_id' column")
  }
  
  # Filter and select data
  track_data <- features_df %>% filter(track_id == !!track_id) %>%  
    select(all_of(feature_list))
  
  # Check if any data was returned
  if (nrow(track_data) == 0) {
    warning(sprintf("No data found for track_id: %s", track_id))
    return(NULL)
  }
  
  # Prepare data for plot
  plot_data <- track_data %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>%
    mutate(feature = factor(feature, levels = feature_list))
  
  return(plot_data)
}


# Create spider plot function with plotly
create_spider_plot <- function(plot_data, title) {
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # Ensure data is ordered correctly and add a row to close the loop for radar plot
  plot_data <- rbind(plot_data, plot_data[1, ])
  
  # Create radar plot
  fig <- plot_ly(
    type = 'scatterpolar',
    r = plot_data$value,               # Feature values
    theta = plot_data$feature,          # Feature names (angles)
    fill = 'toself',                    # Fill in the radar chart
    fillcolor = 'rgba(1, 50, 32)', # Custom fill color with transparency
    marker = list(color = 'green'),  # color of the markers
    line = list(color = "#1DB954", width = 4) # Line color and thickness
  ) %>%
    layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0,1)),
        bgcolor = '#282828'
      ),
      paper_bgcolor = "#282828",
      plot_bgcolor = "white"
    )
  
  # Customize layout
  fig <- fig %>%
    layout(
      polar = list(
        
        radialaxis = list(
          visible = TRUE,
          range = c(0, 1),  # Set range of radial axis (0 to 1 or 0 to 100%)
          tickvals = seq(0, 1, by = 0.2),  # Tick marks every 20%
          ticktext = paste0(seq(0, 100, by = 20), "%"),  # Labels in percent format
          showline = TRUE,
          linecolor = 'white'
        ),
        angularaxis = list(
          tickangle = 0,
          showline = TRUE,
          linecolor = 'white',
          ticks = 'outside',
          tickfont = list(
            family = 'Arial', 
            size = 14, 
            color = 'white'  # Set color of feature labels (e.g., tomato red)
          )
        )
      ),
      title = list(
        text = "Song Feature Comparison",  # Title text
        font = list(
          family = 'Arial',  # Font family
          size = 20,         # Font size
          color = '#1DB954'  # Title color (tomato red)
        ),
        x = 0.5,  # Center title horizontally
        xanchor = 'center'  # Title alignment (centered)
      )
    )
  
  return(fig)
}

create_feature_trends_plot <- function(data) {
  p <- ggplot(data, aes(x = Released, y = Popularity) +
    geom_point(size = 2) +
    labs(title = "Variation of Features Over Time",
         x = "Year", y = "Popularity") +
    theme_minimal()
  )
  return(p)
}



# UI of the app
ui <- dashboardPage(
  skin = "black",  # Dark theme closer to Spotify's look
  
  dashboardHeader(title = "MelodyMetrics"),
  
  dashboardSidebar(
    # Sidebar with Spotify-like style
    tags$style(HTML("
      .skin-black .main-sidebar { 
        background-color: #121212; 
        color: #b3b3b3; 
      }
      .skin-black .sidebar a {
        color: #b3b3b3;
      }
      .skin-black .sidebar a:hover {
        color: #1DB954;
      }
      .skin-black .sidebar .active a {
        background-color: #1DB954;
        color: #ffffff;
      }
      .skin-black .sidebar-menu > li:hover > a, 
      .skin-black .sidebar-menu > li.active > a {
        color: #ffffff;
        background: #1DB954;
      }
    ")),
    conditionalPanel(
      condition = "input.mainMenu == 'home'",
      sidebarMenu(id = "mainMenu",
                  menuItem("Home", tabName = "home", icon = icon("house")),
                  menuItem("Artist Explorer", tabName = "artist", icon = icon("music")),
                  menuItem("Recommendations", tabName = "recommendations", icon = icon("thumbs-up")),
                  menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
      )
    ),
    
    # Sidebar for artist, recommendations, and analysis pages
    conditionalPanel(
      condition = "input.mainMenu == 'artist'",
      sidebarMenu(id = "artistMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house")),
                  selectInput("artist_select", "Select Artist:", choices = NULL, selectize = TRUE),
                  sliderInput("num_top_tracks", label = "Number of Top tracks to display", min = 1, max = 10, value = 5,  step = 1)
      )
    ),
    conditionalPanel(
      condition = "input.mainMenu == 'recommendations'",
      sidebarMenu(id = "recommendationsMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house"))
      )
    ),
    conditionalPanel(
      condition = "input.mainMenu == 'analysis'",
      sidebarMenu(id = "analysisMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house")),
                  h3("Select Decade"),
                  selectInput("decade", "Choose a Decade:", 
                              choices = c("All Decades", "00s", "90s", "80s", "70s", "60s"), 
                              selected = "All Decades"),
                  h3("Choose Analysis Type"),
                  radioButtons("analysis_type", "Select Analysis Type:", 
                               choices = c("Popularity vs Acousticness", 
                                           "Correlation Matrix", 
                                           "Violin Plot", 
                                           "Density Plot"),
                               selected = "Popularity vs Acousticness")
      )
    )
  ),
  
  dashboardBody(
    useWaiter(),
    useShinyjs(),
    
    # CSS for Spotify-like styling
    tags$head(
      tags$style(HTML("        
        .content-wrapper { background-color: #121212; color: #b3b3b3; }
        .box-header {
          background-color: #1DB954 !important;  
          color: white !important;
        }
        .box {
          background-color: #282828 !important;
          color: #b3b3b3;
          border-radius: 8px;
          box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
        }
        .artist-card, .feature-box, .stats-box {
          background: #282828;
          color: #ffffff;
          border-radius: 8px;
          padding: 15px;
          transition: transform 0.2s ease;
        }
        .feature-box:hover, .artist-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
        }
        .feature-title, .feature-description, .stats-label {
          color: #ffffff;
        }
        .feature-title {
          font-size: 20px;
          font-weight: bold;
        }
        .track-item {
          background: #333;
          border-radius: 5px;
          padding: 10px;
          cursor: pointer;
          transition: background 0.2s ease;
        }
        .track-item:hover, .track-item.selected {
          background: #1DB954;
        }
        .feature-icon {
          font-size: 48px;
          color: #1DB954;
        }
      "))
    ),
    
    tabItems(
      # Home Page
      tabItem(tabName = "home",
              fluidRow(
                column(12, 
                       h1("Welcome to MelodyMetrics", class = "text-center", 
                          style = "margin-bottom: 40px; color: #1DB954;")
                )
              ),
              fluidRow(
                column(4,
                       actionButton(
                         "goToArtist",
                         div(
                           tags$i(class = "fa fa-music feature-icon"),
                           div(class = "feature-title", "Artist Explorer"),
                           div(class = "feature-description", 
                               "Get information about your favorite artists")
                         ),
                         class = "feature-box btn-block",
                         style = "border: none; background: #282828;"
                       )
                ),
                column(4,
                       actionButton(
                         "goToRecommendations",
                         div(
                           tags$i(class = "fa fa-thumbs-up feature-icon"),
                           div(class = "feature-title", "Recommendations"),
                           div(class = "feature-description",
                               "Get personalized music recommendations")
                         ),
                         class = "feature-box btn-block",
                         style = "border: none; background: #282828;"
                       )
                ),
                column(4,
                       actionButton(
                         "goToAnalysis",
                         div(
                           tags$i(class = "fa fa-chart-line feature-icon"),
                           div(class = "feature-title", "Analysis"),
                           div(class = "feature-description",
                               "Deep dive into music trends and patterns")
                         ),
                         class = "feature-box btn-block",
                         style = "border: none; background: #282828;"
                       )
                )
              )
      ),
      
      # Artist Page 
      tabItem(tabName = "artist",
              fluidRow(
                column(12,
                       box(width = NULL,
                           class = "artist-card",
                           fluidRow(
                             column(4, uiOutput("artist_image")),
                             column(8, 
                                    uiOutput("artist_details"),
                                    uiOutput("artist_stats")
                             )
                           )
                       )
                )
              ),
              
              fluidRow(
                column(4,
                       box(width = NULL,
                           title = "Top Tracks",
                           status = "primary",
                           solidHeader = TRUE,
                           uiOutput("top_tracks_list")
                       )
                ),
                column(8,
                       box(width = NULL,
                           title = "Track Analysis",
                           status = "primary",
                           solidHeader = TRUE,
                           uiOutput("track_details"),
                           plotlyOutput("feature_spider_plot", height = "500px")
                       )
                )
              )
      ),
      
      # Recommendations Page
      tabItem(tabName = "recommendations",
              h2("Music Recommendations")
              # Add your recommendations content here
              
              
      ),
      
      # Analysis Page
      tabItem(tabName = "analysis",
              h2("Analysis of Popularity of songs with respect to various parameters"),
              mainPanel(
                plotOutput("try"),
                plotlyOutput("feature_plot"),
                plotOutput("analysis_plot"),
                downloadButton("download_plot", "Download Plot")
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  # Navigation handlers
  observeEvent(input$goToArtist, {
    updateTabItems(session, "mainMenu", "artist")
  })
  
  observeEvent(input$goToRecommendations, {
    updateTabItems(session, "mainMenu", "recommendations")
  })
  
  observeEvent(input$goToAnalysis, {
    updateTabItems(session, "mainMenu", "analysis")
  })
  
  # Track current tab for conditional sidebar
  observe({
    if (!is.null(input$mainMenu)) {
      updateTextInput(session, "selectedTab", value = input$mainMenu)
    }
  })
  
  # Loading screen
  w <- Waiter$new(id = "loading-screen", html = spin_dots(), color = "rgba(255,255,255,0.9)")
  
  # Reactive values and select input options
  selected_track <- reactiveVal(NULL)
  
  observe({
    updateSelectInput(session, "artist_select", choices = sort(unique(artist_metadata$name)))
  })
  
  # Render artist image and details
  output$artist_image <- renderUI({
    req(input$artist_select)
    artist <- artist_metadata %>% filter(name == input$artist_select)
    div(style = "text-align: center;", img(src = artist$image_url, style = "max-width: 100%; border-radius: 10px;"))
  })
  
  output$artist_details <- renderUI({
    req(input$artist_select)
    artist <- artist_metadata %>% filter(name == input$artist_select)
    div(
      h3(artist$name, style = "margin-top: 0; color: #1DB529;"),
      p(artist$summary, style = "font-family: Lucida Console, color: white; line-height: 1.6; text-align: justify"),
      tags$div(
        style = "margin-top: 15px;",
        tags$span(class = "badge bg-primary", style = "margin-right: 5px;", artist$genres)
      )
    )
  })
  
  # Render artist statistics
  output$artist_stats <- renderUI({
    req(input$artist_select)
    artist <- artist_metadata %>% filter(name == input$artist_select)
    fluidRow(
      column(6, div(class = "stats-box", div(class = "stats-value", format(artist$followers, big.mark = ",", scientific = FALSE)), div(class = "stats-label", "Followers"))),
      column(6, div(class = "stats-box", div(class = "stats-value", paste0(artist$popularity, "%")), div(class = "stats-label", "Popularity")))
    )
  })
  
  # Render top tracks list
  output$top_tracks_list <- renderUI({
    req(input$artist_select)
    top_tracks <- all_top_tracks %>% 
      filter(artist_name == input$artist_select) %>% 
      arrange(desc(popularity)) %>% 
      distinct(track_id, .keep_all = TRUE) %>%  
      head(input$num_top_tracks)
    
    div(
      lapply(1:nrow(top_tracks), function(i) {
        track <- top_tracks[i, ]
        div(
          class = sprintf("track-item %s", if (!is.null(selected_track()) && selected_track() == track$track_id) "selected" else ""),
          id = track$track_id,
          onclick = sprintf("Shiny.setInputValue('selected_track_id', '%s');", track$track_id),
          div(style = "display: flex; justify-content: space-between; align-items: center;", h4(style = "margin: 0;", track$track_name))
        )
      })
    )
  })
  
  observeEvent(input$selected_track_id, {
    selected_track(input$selected_track_id)
  })
  
  observeEvent(input$artist_select, {
    selected_track(NULL)
  })
  
  # Render track details and feature bar plot
  output$track_details <- renderUI({
    req(selected_track())
    track <- all_top_tracks %>% filter(track_id == selected_track()) %>% slice(1)
    
    div(
      h3(track$track_name, style = "margin-top: 0; color: white;"),
      tags$p(tags$span(style = "font-weight: bold; color: #1DB954;", "Album: "), track$album_name),
      tags$p(tags$span(style = "font-weight: bold; color: #1DB954;", "Release Year: "), track$release_year),
      tags$p(tags$span(style = "font-weight: bold; color: #1DB954;", "Popularity: "), paste0(track$popularity, "%"))
    )
  })
  
  # Preparing the data for the Radar Plot
  plot_data <- reactive({
    req(selected_track())
    prepare_feature_data(
      track_id = selected_track(),
      features_df = features_df,
      feature_list = c("danceability", "energy", "loudness", "speechiness", "valence", "tempo")
    )
  })
  
  # Radar Plot for the selected track
  output$feature_spider_plot <- renderPlotly({
    create_spider_plot(plot_data(), title = "Feature Analysis")
  })
  
  # Analysis section
  corr_matrix <- function(df) {
    n <- ncol(df)
    cor_mat <- matrix(0, n - 2, n - 2)
    
    for (i in 3:n) {
      x <- df[[i]]
      for (j in 3:n) {
        y <- df[[j]]
        cor_mat[i - 2, j - 2] <- cor(x, y, use = "complete.obs")
      }
    }
    
    colnames(cor_mat) <- names(df)[3:n]
    rownames(cor_mat) <- names(df)[3:n]
    
    return(cor_mat)
  }
  
  filtered_data <- reactive({
    switch(input$decade,
           "00s" = All00s,
           "90s" = All90s,
           "80s" = All80s,
           "70s" = All70s,
           "60s" = All60s,
           All
    )
  })
  
  
  output$analysis_plot <- renderPlot({
    data <- filtered_data()
    
    if (input$analysis_type == "Popularity vs Acousticness") {
      ggplot(data, aes(x = Popularity, y = acousticness, color = loudness)) + 
        geom_point(aes(size = energy), alpha = 1) +  
        geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
        scale_color_viridis_c(option = "plasma") +
        scale_size_continuous(range = c(1, 6)) +
        labs(
          title = paste("Factors Affecting Popularity in", input$decade),
          subtitle = "Popularity has the highest correlation with acousticness and loudness, the size denotes energy",
          x = "Popularity",
          y = "Acousticness",
          color = "Loudness",
          size = "Energy"
        )
      
    } else if (input$analysis_type == "Correlation Matrix") {
      cor_mat <- corr_matrix(data)
      correlation <- expand.grid(rows = rownames(cor_mat), columns = colnames(cor_mat))
      correlation$value <- as.vector(cor_mat)
      
      ggplot(correlation, aes(x = rows, y = columns, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)),
                  color = ifelse(abs(correlation$value) > 0.5, "white", "black"),
                  size = 3) +
        scale_fill_gradient2(
          low = "blue",
          mid = "white",
          high = "red",
          midpoint = 0,
          limits = c(-1, 1)
        )
      
    } else if (input$analysis_type == "Violin Plot") {
      df <- data
      df$decade <- floor(df$Released / 10) * 10
      ggplot(df, aes(x = acousticness, y = Popularity, fill = as.factor(decade))) +
        geom_violin() +
        facet_wrap(~decade, scales = "free_x") +
        labs(x = "Acousticness", y = "Popularity", fill = "Decade")
      
    } else if (input$analysis_type == "Density Plot") {
      ggplot(data, aes(x = acousticness, y = Popularity)) +
        geom_density_2d_filled(alpha = 0.6) +
        geom_point(aes(size = energy), alpha = 0.4) +
        scale_fill_viridis_d(name = "Density Level") +
        scale_size_continuous(name = "Energy", range = c(0, 3), breaks = seq(0, 1, by = 0.2)) +
        labs(
          title = "2D Density Distribution",
          subtitle = "Darker colors = Higher density of points",
          caption = "Point size represents energy level"
        )
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("analysis_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 10, height = 8, dpi = 300)
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)
