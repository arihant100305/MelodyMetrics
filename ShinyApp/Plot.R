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

library(tibble)
library(dplyr)
library(ggplot2)
library(fmsb)
library(plotly)
library(tidyr)

All00s = bind_rows(Top00s, Love_00s, Dance_00s) %>% distinct()
Allrecents = bind_rows(Recents, Love_recents, Dance_recents) %>% distinct()
All90s = bind_rows(Top90s, Love_90s, Dance1_90s, Dance2_90s) %>% distinct()
All80s = bind_rows(Top80s, Love_80s) %>% distinct()
All70s = bind_rows(Top70s, Love_70s) %>% distinct()
All60s = bind_rows(Top60s, Love_60s) %>% distinct()

plot00s = ggplot(All00s, aes(x = Popularity, y = acousticness, color = loudness)) + 
  geom_point(size = 5*All00s$energy, alpha = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(1, 6)) +
  labs(
    title = "Factors affecting popularity in 00s",
    subtitle = "Popularity has the highest correlation with acousticness and loudness, the size denotes energy",
    x = "Popularity",
    y = "Acousticness",
    color = "Loudness",
    size = "Energy"
  ) 

ggsave("plot.png", plot = plot00s, width = 10, height = 8, dpi = 300)

corr_matrix <- function(df) {
  n = ncol(df)
  cor_mat = matrix(0, n-2, n-2)
  
  for (i in 3:n) {
    x = df[[i]]  
    for (j in 3:n) {
      y = df[[j]]  
      cor_mat[i-2, j-2] = cor(x, y)  
    }
  }
  colnames(cor_mat) = names(df)[3:n]
  rownames(cor_mat) = names(df)[3:n]
  
  return(cor_mat)
}

create_corrmat = function(df){
  
  cor_mat = corr_matrix(df)
  
  correlation = expand.grid(
    rows = rownames(cor_mat),
    columns = colnames(cor_mat)
  )
  correlation$value <- as.vector(cor_mat)
  
  # Create the heatmap
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
}

plotrecents = ggplot(Allrecents, aes(x = Popularity, y = acousticness, color = loudness)) + 
  geom_point(size = 5*All00s$energy, alpha = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(1, 6)) +
  labs(
    title = "Factors affecting popularity in 00s",
    subtitle = "Popularity has the highest correlation with acousticness and loudness, the size denotes energy",
    x = "Popularity",
    y = "Acousticness",
    color = "Loudness",
    size = "Energy"
  ) 

ggsave("plotrecents.png", plot = plot00s, width = 10, height = 8, dpi = 300)

All = bind_rows(Allrecents, All00s, All90s, All80s, All70s, All60s) %>% distinct()

violin = function(df){
  
  df$decade <- floor(df$Released / 10) * 10
  if(nrow(df)<1000){
    df$decade = names(which.max(table(df$decade)))
  }
  # Create violin plot
  ggplot(df, aes(x = acousticness, y = Popularity, fill = as.factor(decade))) +
    geom_violin() +
    facet_wrap(~decade, scales = "free_x") +
    labs(x = "Acousticness", y = "Popularity", fill = "Decade")
}

violin_plot = violin(All)
ggsave("Violin_plot.png", plot = violin_plot, width = 10, height = 8, dpi = 300)

density_plot = ggplot(All, aes(x = acousticness, y = Popularity, size = energy)) +
  geom_density_2d_filled(alpha = 0.6) +
  geom_point(alpha = 0.4) +
  scale_fill_viridis_d(name = "Density Level") +
  scale_size_continuous(name = "Energy",
                        range = c(0,3),
                        breaks = seq(0, 1, by = 0.2)) +  # custom breaks for energy
  labs(title = "2D Density Distribution",
       subtitle = "Darker colors = Higher density of points",
       caption = "Point size represents energy level")

ggsave("density_plot.png", plot = density_plot, width = 10, height = 8, dpi = 300)

artist_metadata
artist_names <- artist_metadata$name
songs <- vector("list", length = length(artist_names))

# Loop through each artist in artist_names
for (i in seq_along(songs)) {
  # Find rows in All where artist_names[i] is present in All$Artists
  matching_rows <- sapply(All$Artists, function(artist_list) artist_names[i] %in% artist_list)
  
  # Extract Track_Name values where the artist is present
  songs[[i]] <- All$Track_Name[matching_rows]
}
names(songs) = artist_names
artist_names <- all_top_tracks$artist_name
songs <- vector("list", length = length(artist_names))

# Loop through each artist in artist_names
for (i in seq_along(songs)) {
  # Find rows in All where artist_names[i] is present in All$Artists
  matching_rows <- sapply(All$Artists, function(artist_list) artist_names[i] %in% artist_list)
  
  # Extract Track_Name values where the artist is present
  songs[[i]] <- All$Track_Name[matching_rows]
}

Alldance = bind_rows(Dance_00s,Dance_recents,Dance1_90s,Dance2_90s) %>% distinct()
Alllove = bind_rows(Love_00s,Love_recents,Love_90s,Love_80s,Love_70s,Love_60s) %>% distinct()

create_corrmat(Dance1_90s[3:14])
create_corrmat(Love_60s[2:14])

average_loudness_by_year <- All_r %>%
  group_by(.data[["Released"]]) %>%
  summarize(avg_loudness = mean(valence, na.rm = TRUE))

fit <- lm(avg_loudness ~ poly(Released, 3), data = average_loudness_by_year)

# Generate predicted values for the fitted curve
average_loudness_by_year <- average_loudness_by_year %>%
  mutate(fitted_loudness = predict(fit, newdata = average_loudness_by_year))

# Create the interactive plot with plotly
interactive_plot <- plot_ly(data = average_loudness_by_year, x = ~Released, y = ~avg_loudness, type = 'scatter', mode = 'markers+lines',
                            marker = list(color = 'red', size = 1),
                            hoverinfo = 'x+y', name = 'Average Valence') %>%
  add_lines(x = ~Released, y = ~fitted_loudness, line = list(color = 'green'), name = 'Polynomial Fit') %>%
  layout(
    title = "Average Valence of Songs by Year of Release",
    xaxis = list(
      title = "Year of Release"
    ),
    yaxis = list(title = "Average Valence"),
    showlegend = FALSE
  )

# Display the interactive plot
interactive_plot

dance_songs <- Happy %>% mutate(song_type = 'Happy')
love_songs <- Sad %>% mutate(song_type = 'Sad')

# Combine the data frames
combined_songs <- bind_rows(dance_songs, love_songs)

# Create the violin plot
fig <- plot_ly(combined_songs, y = ~valence, x = ~song_type, color = ~song_type,
               type = 'violin', box = list(visible = TRUE), 
               meanline = list(visible = TRUE))

# Customize the layout
fig <- fig %>% layout(
  title = "Valence Distribution in Happy and Sad Songs",
  yaxis = list(title = "Valence"),
  xaxis = list(title = "Song Type")
)

# Display the plot
fig

##Singer wise
df_unnested <- All %>%
  unnest(Artists) %>%  # This expands the artist column into multiple rows for each artist
  mutate(decade = floor(Released / 10) * 10)  # Add a decade column

df_unnested = df_unnested %>% filter(decade == 2020)

# Calculate the mean popularity for each artist per decade
artist_popularity <- df_unnested %>%
  group_by(decade, Artists) %>%
  summarize(mean_popularity = sum(Popularity, na.rm = TRUE)) %>%
  ungroup()

# Get the top 10 artists per decade based on mean popularity
top_artists_decade <- artist_popularity %>%
  top_n(10, mean_popularity) %>%
  ungroup()

# Create the plot using Plotly
fig <- plot_ly(top_artists_decade, x = ~mean_popularity, y = ~Artists, 
               type = 'bar', orientation = 'h', 
               hoverinfo = 'x+y+text',
               text = ~paste('Decade:',decade)) %>%
  layout(
    title = "Top 10 Artists by Decade",
    xaxis = list(title = "Popularity"),
    yaxis = list(title = "", automargin = TRUE),
    barmode = 'stack',
    plot_bgcolor = 'rgb(30, 30, 30)',  # Dark background for the plot area
    paper_bgcolor = 'rgb(20, 20, 20)',  # Dark background for the entire plot
    font = list(color = 'white')  # White text for readability
  )

# Display the plot
fig
save(df_unnested, file="MASTER_Songs.RData")
save(Alldance, file="MASTER_Songs.RData")
save(Alllove, file="MASTER_Songs.RData")
a = paste("f")
