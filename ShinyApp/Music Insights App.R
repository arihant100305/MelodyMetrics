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
load("MASTER_DATA_5.RData")
load("MASTER_Songs.RData")

Master_df = df_unnested

# Global variables
options(spinner.color="#0275d8")

# Data Preparation
All00s = bind_rows(Top00s, Love_00s, Dance_00s) %>% distinct()
Allrecents = bind_rows(Recents, Love_recents, Dance_recents) %>% distinct()
All90s = bind_rows(Top90s, Love_90s, Dance1_90s, Dance2_90s) %>% distinct()
All80s = bind_rows(Top80s, Love_80s) %>% distinct()
All70s = bind_rows(Top70s, Love_70s) %>% distinct()
All60s = bind_rows(Top60s, Love_60s) %>% distinct()

# Prepare feature data function
prepare_feature_data <- function(track_id, features_df, feature_list) {
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
  track_data <- features_df %>% 
    filter(track_id == !!track_id) %>%  
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

# Create a horizontal bar plot function
create_bar_plot <- function(plot_data, title) {
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # Create the bar plot
  p <- ggplot(plot_data, aes(x = value, y = feature)) +
    geom_bar(stat = "identity", fill = "#013220", alpha = 0.7) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(title = title, x = "Feature Value (0-100%)", y = "Feature") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.ticks.y = element_blank()
    )
  
  return(p)
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

# UI
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Music Insights"),
  
  dashboardSidebar(
    # Home page sidebar
    conditionalPanel(
      condition = "input.mainMenu == 'home'",
      sidebarMenu(id = "mainMenu",
                  menuItem("Home", tabName = "home", icon = icon("house")),
                  menuItem("Artist Explorer", tabName = "artist", icon = icon("music")),
                  menuItem("Recommendations", tabName = "recommendations", icon = icon("thumbs-up")),
                  menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
                  menuItem("Top Artists by Decade", tabName = "topartists", icon = icon("crown"))
      )
    ),
    # Artist page sidebar
    conditionalPanel(
      condition = "input.mainMenu == 'artist'",
      sidebarMenu(id = "artistMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house")),
                  selectInput("artist_select", "Select Artist:", choices = NULL, selectize = TRUE),
                  sliderInput("num_top_tracks", label = "Number of Top tracks to display", min = 1, max = 10, value = 5,  step = 1)
      )
    ),
    # Recommendations page sidebar
    conditionalPanel(
      condition = "input.mainMenu == 'recommendations'",
      sidebarMenu(id = "recommendationsMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house")),
                  h3("Songs type"),
                  radioButtons("songs_filter", "Select Type :",
                                     choices = c("Quiet", "Balanced", "Happy", "Dance"),
                                     selected = "Balanced"),
                  h3("Filters"),
                  checkboxGroupInput("era_filter", "Select Era(s):",
                                     choices = c("Evergreen", "90s", "Modern", "All"),
                                     selected = "Modern"),
                  radioButtons("popularity_filter", "Recommendation Type:",
                               choices = c("Familiar (High Popularity)", 
                                           "Discover (Low Popularity)"),
                               selected = "Familiar (High Popularity)"),
                  h3("Display Options"),
                  selectInput("num_recommendations", 
                              "Number of Recommendations:",
                              choices = c(5, 10, 15, 20), 
                              selected = 10)
      )
    ),
    
    # Analysis page sidebar with options for decade and analysis type
    conditionalPanel(
      condition = "input.mainMenu == 'analysis'",
      sidebarMenu(id = "analysisMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house")),
                  h3("Select Decade"),
                  selectInput("decade", "Choose a Decade:", 
                              choices = c("All", "00s", "90s", "80s", "70s", "60s"), 
                              selected = "All"),
                  h3("Select Feature"),
                  selectInput("feature", "Choose Feature:", 
                              choices = c("Released", "acousticness", "danceability", "energy", 
                                          "instrumentalness", "liveness", "loudness", "speechiness", 
                                          "tempo", "valence"),
                              selected = "acousticness"),
                  h3("Choose Analysis Type"),
                  radioButtons("analysis_type", "Select Analysis Type:", 
                               choices = c("Scatter Plot", "Correlation Matrix", "Violin Plot", "Density Plot"),
                               selected = "Scatter Plot")
                  
      )
    ),
    
    conditionalPanel(
      condition = "input.mainMenu == 'topartists'",
      sidebarMenu(id = "topArtistsMenu",
                  menuItem("Back to Home", tabName = "home", icon = icon("house")),
                  h3("Select Parameters"),
                  selectInput("decade_select", "Choose Decade:", 
                              choices = c("1960s" = 1960, 
                                          "1970s" = 1970,
                                          "1980s" = 1980,
                                          "1990s" = 1990,
                                          "2000s" = 2000,
                                          "2010s" = 2010),
                              selected = 1990),
                  sliderInput("num_artists", 
                              "Number of Top Artists:", 
                              min = 5, 
                              max = 20, 
                              value = 10,
                              step = 5)
      )
    )
  ),
  
  dashboardBody(
    useWaiter(),
    useShinyjs(),
    
    # CSS
    tags$head(
      tags$style(HTML("        
        .content-wrapper { background-color: #f4f4f8; }
      
      .box-header {
        background-color: #9CCC65 !important;  
        color: white !important;
      }

      .box {
        background-color: #e8f5e9 !important;
        border-radius: 15px !important;
        padding: 5px;
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
        margin-bottom: 20px;
        transition: all 0.3s ease;
      }
      
        .artist-card {
          background: #fif8e9;
          border-radius: 15px;
          padding: 20px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
          margin-bottom: 20px;
          transition: all 0.3s ease;
        }
        .artist-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
        }
        .track-item {
          padding: 12px;
          margin: 8px 0;
          background: #ffffff;
          border-radius: 8px;
          cursor: pointer;
          transition: all 0.2s ease;
        }
        .track-item:hover {
          background: #f0f4c3;
          transform: scale(1.02);
        }
        .track-item.selected {
          background: #c8e6c9;
          border-left: 4px solid #388e3c;
        }
        .feature-chart {
          background: #ffffff;
          border-radius: 15px;
          padding: 15px;
          height: 400px;
        }
        .stats-box {
          text-align: center;
          padding: 15px;
          background: #ffffff;
          border-radius: 10px;
          margin: 10px 0;
        }
        .stats-value {
          font-size: 24px;
          font-weight: bold;
          color: #388e3c;
        }
        .stats-label {
          font-size: 14px;
          color: #6c757d;
        }
        
        .feature-box {
          padding: 20px;
          border-radius: 15px;
          background: #ffffff;
          margin-bottom: 20px;
          text-align: center;
          transition: all 0.3s ease;
          cursor: pointer;
        }
        .feature-box:hover {
          transform: translateY(-5px);
          box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
        }
        .feature-icon {
          font-size: 48px;
          color: #388e3c;
          margin-bottom: 15px;
        }
        .feature-title {
          font-size: 24px;
          font-weight: bold;
          margin-bottom: 10px;
          color: #2e7d32;
        }
        .feature-description {
          color: #6c757d;
        }
      "))
    ),
    
    tabItems(
      # Home Page
      tabItem(tabName = "home",
              fluidRow(
                column(12, 
                       h1("Welcome to Music Insights", class = "text-center", 
                          style = "margin-bottom: 40px; color: #2e7d32;")
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
                         style = "border: none; background: white;"
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
                         style = "border: none; background: white;"
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
                         style = "border: none; background: white;"
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
                           plotlyOutput("feature_spider_plot", height = "400px")
                       )
                )
              )
      ),
      
      # Recommendations Page
      tabItem(tabName = "recommendations",
              fluidRow(
                column(12,
                       box(width = NULL,
                           title = "Recommended Tracks",
                           status = "primary",
                           solidHeader = TRUE,
                           div(
                             class = "recommendation-info",
                             style = "margin-bottom: 20px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
                             textOutput("recommendation_summary")
                           ),
                           tableOutput("recommended_tracks")
                       )
                )
              )
      ),
      
      # Analysis Page
      tabItem(tabName = "analysis",
              h2("Analysis of Popularity of songs with respect to various parameters"),
              mainPanel(
                plotOutput("analysis_plot"),
                downloadButton("download_plot", "Download Plot")
              )
      ),
      
      tabItem(tabName = "topartists",
              fluidRow(
                column(12,
                       box(width = NULL,
                           title = "Top Artists by Decade",
                           status = "primary",
                           solidHeader = TRUE,
                           plotlyOutput("top_artists_plot", height = "600px")
                       )
                )
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
  
  # ARTIST PAGE OUTPUTS
  output$artist_image <- renderUI({
    req(input$artist_select)
    artist <- artist_metadata %>% filter(name == input$artist_select)
    div(style = "text-align: center;", 
        img(src = artist$image_url, style = "max-width: 100%; border-radius: 10px;"))
  })
  
  output$artist_details <- renderUI({
    req(input$artist_select)
    artist <- artist_metadata %>% filter(name == input$artist_select)
    div(
      h3(artist$name, style = "margin-top: 0; color: #2c3e50;"),
      p(artist$summary, style = "color: #34495e; line-height: 1.6;"),
      tags$div(
        style = "margin-top: 15px;",
        tags$span(class = "badge bg-primary", style = "margin-right: 5px;", artist$genres)
      )
    )
  })
  
  output$artist_stats <- renderUI({
    req(input$artist_select)
    artist <- artist_metadata %>% filter(name == input$artist_select)
    fluidRow(
      column(6, div(class = "stats-box", 
                    div(class = "stats-value", format(artist$followers, big.mark = ",", scientific = FALSE)), 
                    div(class = "stats-label", "Followers"))),
      column(6, div(class = "stats-box", 
                    div(class = "stats-value", paste0(artist$popularity, "%")), 
                    div(class = "stats-label", "Popularity")))
    )
  })
  
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
          class = sprintf("track-item %s", 
                          if (!is.null(selected_track()) && selected_track() == track$track_id) 
                            "selected" else ""),
          id = track$track_id,
          onclick = sprintf("Shiny.setInputValue('selected_track_id', '%s');", track$track_id),
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              h4(style = "margin: 0;", track$track_name))
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
  
  output$track_details <- renderUI({
    req(selected_track())
    track <- all_top_tracks %>% filter(track_id == selected_track()) %>% slice(1)
    
    div(
      h3(track$track_name, style = "margin-top: 0; color: #2c3e50;"),
      tags$p(tags$span(style = "font-weight: bold; color: #6c757d;", "Album: "), 
             track$album_name),
      tags$p(tags$span(style = "font-weight: bold; color: #6c757d;", "Release Year: "), 
             track$release_year),
      tags$p(tags$span(style = "font-weight: bold; color: #6c757d;", "Popularity: "), 
             paste0(track$popularity, "%"))
    )
  })
  
  plot_data <- reactive({
    req(selected_track())
    prepare_feature_data(
      track_id = selected_track(),
      features_df = features_df,
      feature_list = c("danceability", "energy", "loudness", "speechiness", 
                       "valence", "tempo")
    )
  })
  
  output$feature_spider_plot <- renderPlotly({
    create_spider_plot(plot_data(), title = "Feature Values for Selected Track")
  })
  
  # ANALYSIS PAGE
  filtered_data <- reactive({
    switch(input$decade,
           "00s" = All00s,
           "90s" = All90s,
           "80s" = All80s,
           "70s" = All70s,
           "60s" = All60s,
           All_r
    )
  })
  
  output$analysis_plot <- renderPlot({ 
    data <- filtered_data()
    
    if (input$analysis_type == "Scatter Plot") {
      feature <- input$feature
      ggplot(data, aes_string(x = "Popularity", y = feature, color = "loudness")) + 
        geom_point(aes(size = energy), alpha = 1) +  
        geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
        scale_color_viridis_c(option = "plasma") +
        scale_size_continuous(range = c(1, 6)) +
        labs(
          title = paste("Popularity vs", feature, "in", input$decade),
          subtitle = paste("Popularity vs", feature, 
                           "with loudness as color and size denoting energy"),
          x = "Popularity",
          y = feature,
          color = "Loudness",
          size = "Energy"
        )
      
    } else if (input$analysis_type == "Correlation Matrix") {
      cor_mat <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")
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
      feature <- input$feature 
      data$decade <- floor(data$Released / 10) * 10
      ggplot(data, aes_string(x = feature, y = "Popularity", 
                              fill = "as.factor(decade)")) +
        geom_violin() +
        facet_wrap(~decade, scales = "free_x") +
        labs(x = feature, y = "Popularity", fill = "Decade")
      
    } else if (input$analysis_type == "Density Plot") {
      feature <- input$feature 
      ggplot(data, aes_string(x = feature, y = "Popularity")) +
        geom_density_2d_filled(alpha = 0.6) +
        geom_point(aes(size = energy), alpha = 0.4) +
        scale_fill_viridis_d(name = "Density Level") +
        scale_size_continuous(name = "Energy", range = c(0, 3), 
                              breaks = seq(0, 1, by = 0.2)) +
        labs(
          title = paste("2D Density Distribution: Popularity vs", feature),
          subtitle = "Darker colors = Higher density of points",
          x = feature,
          y = "Popularity",
          caption = "Point size represents energy level"
        )
    }
  })
  
  # RECOMMENDATIONS PAGE
  # Check if the required columns exist in the dataset
  if(!all(c("normalized_valence", "energy", "acousticness", "danceability", "era", "Popularity", "Track_Name", "Released") %in% colnames(All_r))) {
    stop("Required columns are missing from the dataset!")
  }
  
  # Create mood score based on features
  All_r$mood_score <- 0.35 * All_r$normalized_valence +
    0.25 * All_r$energy +
    0.1 * All_r$acousticness +
    0.2 * All_r$danceability +
    0.1 * All_r$normalized_tempo  # Assuming 'normalized_tempo' is already in [0, 1]
  All_r$Released <- as.integer(All_r$Released)
  
  
  # Reactive function for track recommendations
  recommended_tracks <- reactive({
    req(input$songs_filter, input$era_filter, input$popularity_filter)
    
    songs_filter <- input$songs_filter
    if(songs_filter=="Quiet"){
      mood_filter = 0.4
    }
    else if (songs_filter=="Balanced"){
      mood_filter=0.6
    }
    else if (songs_filter=="Happy"){
      mood_filter = 0.7
    }
    else{
      mood_filter <- 0.85
    }
    
    
    tolerance <- 0.15
    
    # Filter data based on mood score
    filtered_data <- All_r[All_r$mood_score >= (mood_filter - tolerance) & 
                             All_r$mood_score <= (mood_filter + tolerance), ]
    
    # Era filtering
    if (!"All" %in% input$era_filter) {
      filtered_data <- filtered_data[filtered_data$era %in% input$era_filter, ]
    }
    
    # Popularity filtering
    if (input$popularity_filter == "Familiar (High Popularity)") {
      filtered_data <- filtered_data %>% 
        arrange(desc(Popularity))
    } else {
      filtered_data <- filtered_data %>% 
        arrange(Popularity)
    }
    
    # Select top N recommendations
    n_recommendations <- as.integer(input$num_recommendations)
    filtered_data <- head(filtered_data, n = n_recommendations)
    
    # Select display columns
    filtered_data %>% 
      select(Track_Name, Released,  Popularity) %>%
      mutate(
        Released = as.integer(Released),
        Popularity = paste0(round(Popularity), "%")
      )
  })
  
  # Render recommendations summary
  output$recommendation_summary <- renderText({
    mood_level <- case_when(
      input$mood_slider < 0.4 ~ "melancholic",
      input$mood_slider < 0.6 ~ "balanced",
      input$mood_slider < 0.8 ~ "upbeat",
      TRUE ~ "very upbeat"
    )
    
    popularity_type <- if(input$popularity_filter == "Familiar (High Popularity)") 
      "popular" else "lesser-known"
    
    eras <- if("All" %in% input$era_filter) "all eras" else 
      paste(input$era_filter, collapse = ", ")
    
    sprintf("Showing %s %s tracks from %s with a %s mood", 
            input$num_recommendations, 
            popularity_type,
            eras,
            mood_level)
  })
  
  output$recommended_tracks <- renderTable({
    df <- recommended_tracks()
    if (nrow(df) == 0) {
      return(data.frame(Message = "No tracks match your criteria."))
    }
    df
  }, striped = TRUE, hover = TRUE, spacing = "l", align = 'l')
  # Download handler for analysis plots
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("analysis-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 7)
    }
  )
  
  observeEvent(input$goToTopArtists, {
    updateTabItems(session, "mainMenu", "topartists")
  })
  
  # Add reactive for top artists data
  top_artists_data <- reactive({
    req(input$decade_select)
    selected_decade <- as.numeric(input$decade_select)
    
    # Unnest and process data
    df_unnested <- Master_df %>%
      mutate(decade = floor(Released / 10) * 10) %>%
      filter(decade == selected_decade)
    
    # Calculate mean popularity for each artist
    artist_popularity <- df_unnested %>%
      group_by(Artists) %>%
      summarize(
        mean_popularity = sum(Popularity, na.rm = TRUE),
        total_tracks = n()
      ) %>%
      filter(total_tracks >= 3) %>%  # Filter artists with at least 3 tracks
      ungroup() %>%
      arrange(desc(mean_popularity)) %>%
      head(input$num_artists)
    
    # Reorder factor levels based on popularity
    artist_popularity$Artists <- factor(
      artist_popularity$Artists,
      levels = artist_popularity$Artists[order(artist_popularity$mean_popularity)]
    )
    
    artist_popularity
  })
  
  # Add plotly output for top artists
  output$top_artists_plot <- renderPlotly({
    data <- top_artists_data()
    
    plot_ly(data, 
            x = ~mean_popularity, 
            y = ~Artists, 
            type = 'bar', 
            orientation = 'h',
            marker = list(
              color = ~mean_popularity,
              colorscale = 'Viridis'
            ),
            hoverinfo = 'text',
            text = ~sprintf(
              "Artist: %s<br>Mean Popularity: %.1f<br>Total Tracks: %d",
              Artists, mean_popularity, total_tracks
            )) %>%
      layout(
        title = list(
          text = sprintf("Top %d Artists of the %s s", 
                         input$num_artists, 
                         input$decade_select),
          font = list(size = 20)
        ),
        xaxis = list(
          title = "Mean Popularity Score"
        ),
        yaxis = list(
          title = "",
          automargin = TRUE
        ),
        showlegend = FALSE,
        plot_bgcolor = '#e8f5e9',
        paper_bgcolor = '#e8f5e9'
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
