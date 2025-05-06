library(httr2)
library(tibble)

CLIENT_ID <- "0e07531f66be4e9a938211933cb999c4"
CLIENT_SECRET <- "aa3b0d04a45c4b4981b4c7cddcd559aa"

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
  data <- tibble(Track_Name = track_names, Track_ID = track_ids, Popularity = popularity, 
                 acousticness, danceability, energy, instrumentalness, liveness,
                 loudness, speechiness, tempo, valence)
  return(data)
}


nineties <- get_playlist("37i9dQZF1DXa2huSXaKVkW")
twoThousands <- get_playlist("37i9dQZF1DWZNJXX2UeBij")
twoTens <- get_playlist("37i9dQZF1DWVDvBpGQbzXj")

NINETIES <- get_features(nineties)
TWO_THOUSANDS <- get_features(twoThousands)
TWO_TENS <- get_features(twoTens)

save(NINETIES, file="Nineties_hits.RData")
save(TWO_THOUSANDS, file = "TwoThousands_hits.RData")
save(TWO_TENS, file = "TwoTens_hits.RData")
