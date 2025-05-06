
library(httr2)
library(tibble)
library(dplyr)
library(ggplot2)

CLIENT_ID <- "2c802eccd6c54cf0bc8d809945279d9d"
CLIENT_SECRET <- "d58594d1ec0047cc80ec6920fc323416"

generate_access_token <- function(CLIENT_ID, CLIENT_SECRET){
  req_access_token <- request("https://accounts.spotify.com/api/token") |> 
    req_method("POST") |> 
    req_body_raw(paste0("grant_type=client_credentials&client_id=", 
                        CLIENT_ID,
                        "&client_secret=", 
                        CLIENT_SECRET), 
                 "application/x-www-form-urlencoded") |> 
    req_perform() |>
    resp_body_json()
  return(req_access_token$access_token)
}

access_token <- generate_access_token(CLIENT_ID, CLIENT_SECRET)
access_token

get_playlist <- function(PLAYLIST_ID){
  playlist <- request(paste0("https://api.spotify.com/v1/playlists/", PLAYLIST_ID)) |> 
    req_method("GET") |> 
    req_headers(
      Authorization = paste0("Bearer ", access_token),
    ) |> 
    req_perform() |>
    resp_body_json()
  return(playlist)
}

# Feature extraction on that playlist
get_features <- function(playlist){
  N <- length(playlist$tracks$items) # No. of songs in the playlist
  track_names <- vector(length = N)
  track_ids <- vector(length = N)
  release_year <- vector(length = N)
  popularity <- numeric(length = N)
  acousticness <- numeric(length = N)
  danceability <- numeric(length = N)
  energy <- numeric(length = N)
  instrumentalness <- numeric(length = N)
  liveness <- numeric(length = N)
  loudness <- numeric(length = N)
  speechiness <- numeric(length = N)
  tempo <- numeric(length = N)
  valence <- numeric(length = N)
  
  for(i in 1:N){
    track_names[i] <- playlist$tracks$items[[i]]$track$name
    track_ids[i] <- playlist$tracks$items[[i]]$track$id
    popularity[i] <- playlist$tracks$items[[i]]$track$popularity
    
    # Extracting that particular song
    song <- request(paste0("https://api.spotify.com/v1/audio-features/", track_ids[i])) |> 
      req_method("GET") |> 
      req_headers(
        Authorization = paste0("Bearer ", access_token),
      ) |> 
      req_perform() |>
      resp_body_json()
    
    track <- request(paste0("https://api.spotify.com/v1/tracks/", track_ids[i])) |> 
      req_method("GET") |> 
      req_headers(
        Authorization = paste0("Bearer ", access_token),
      ) |> 
      req_perform() |>
      resp_body_json()
    
    release_year[i] <- as.numeric(substr(track$album$release_date, 1, 4))
    
    # Performing feature extraction on each song
    acousticness[i] <- song$acousticness
    danceability[i] <- song$danceability
    energy[i] <- song$energy
    instrumentalness[i] <- song$instrumentalness
    liveness[i] <- song$liveness
    loudness[i] <- song$loudness
    speechiness[i] <- song$speechiness
    tempo[i] <- song$tempo
    valence[i] <- song$valence
  }
  data <- tibble(Track_Name = track_names, Track_ID = track_ids, Released = release_year, Popularity = popularity, 
                 acousticness, danceability, energy, instrumentalness, liveness,
                 loudness, speechiness, tempo, valence)
  return(data)
}


nineties <- get_playlist("37i9dQZF1DXa2huSXaKVkW")
love_90s <- get_playlist("37i9dQZF1DXa6iPZDThhLh")
dance1_90s <- get_playlist("37i9dQZF1DXdcRZAcc2QFU")
dance2_90s <- get_playlist("37i9dQZF1DWWQ4RVrwACVr")
eighties <- get_playlist("37i9dQZF1DX5rOEFf3Iycd")
love_80s <- get_playlist("37i9dQZF1DX3NU3NvyoJUz")
seventies <- get_playlist("37i9dQZF1DX9kVlnA5Si6s")
love_70s <- get_playlist("37i9dQZF1DX57WIZsVQSIn")
sixties <- get_playlist("37i9dQZF1DXa1eCiO3E6Rr")
love_60s <- get_playlist("37i9dQZF1DX6q19gm5UQXx")
twoThousands <- get_playlist("37i9dQZF1DWZNJXX2UeBij")
twoThousands_love <- get_playlist("37i9dQZF1DWVq1SXCH6uFn")
twoThousands_dance <- get_playlist("37i9dQZF1DWTUfv2yzHEe7")
recents <- get_playlist("37i9dQZF1DWVDvBpGQbzXj")
recents_love <- get_playlist("37i9dQZF1DWX76Z8XDsZzF")
recents_dance <- get_playlist("37i9dQZF1DX8xfQRRX1PDm")
happy_songs <- get_playlist("37i9dQZF1DWTwbZHrJRIgD")
sad_songs <- get_playlist("37i9dQZF1DXdFesNN9TzXT")

Top60s <- get_features(sixties)
Top70s <- get_features(seventies)
Top80s <- get_features(eighties)
Top90s <- get_features(nineties)
Top00s<- get_features(twoThousands)
Recents<- get_features(recents)
Love_00s <- get_features(twoThousands_love)
Dance_00s <- get_features(twoThousands_dance)
Love_recents <- get_features(recents_love)
Dance_recents <- get_features(recents_dance)
Love_90s <- get_features(love_90s)
Dance1_90s <- get_features(dance1_90s)
Dance2_90s <- get_features(dance2_90s)
Love_80s <- get_features(love_80s)
Love_70s <- get_features(love_70s)
Love_60s <- get_features(love_60s)
## Do this for the remaining

save(Recents, file = "recents.RData")
save(Top90s, file = "Top90s.RData")
save(Top80s, file = "Top80s.RData")
save(Top70s, file = "Top70s.RData")
save(Top60s, file = "Top60s.RData")
save(Top00s, file = "Top00s.RData")
save(Love_90s, file = "Love_90s.RData")
save(Love_00s, file = "Love_00s.RData")
save(Love_recents, file = "Love_recents.RData")
save(Love_80s, file = "Love_80s.RData")
save(Love_70s, file = "Love_70s.RData")
save(Love_60s, file = "Love_60s.RData")
save(Dance1_90s, file = "Dance1_90s.RData")
save(Dance2_90s, file = "Dance2_90s.RData")
save(Dance_00s, file = "Dance_00s.RData")
save(Dance_recents, file = "Dance_recents.RData")
## Also saved it and upload it on github
