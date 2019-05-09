library(furrr)
library(spotifyr)
library(tidyverse)
library(RColorBrewer)
library(highcharter)
plan(multiprocess)

albums <- c('Bossanova', 'Trompe le Monde', 'Come On Pilgrim', 'Doolittle', 'Surfer Rosa (Remastered)')

pixies_audio_features <- get_artist_audio_features('pixies', dedupe_albums = F) %>% 
    filter(album_name %in% albums,
           album_id != '5dBIhAet5YfCd1TSvMRbzX') %>% 
    mutate(row_number = row_number())

track_audio_analyses <- pixies_audio_features %>% 
    pull(track_id) %>% 
    future_map(get_track_audio_analysis, .progress = TRUE)

filter(pixies_audio_features, track_name == 'Where Is My Mind?')
filter(pixies_audio_features, track_name == 'Tame')
filter(pixies_audio_features, track_name == "I’ve Been Tired")
filter(pixies_audio_features, track_name == "Bone Machine")
filter(pixies_audio_features, track_name == "Bird Dream of the Olympus Mons")

View(pixies_audio_features)

track <- track_audio_analyses[[65]]

plot_df <- track$segments %>% 
    select(start, loudness_start) %>%
    rowwise() %>% 
    mutate(section_start = track$sections$start[start >= track$sections$start & start <= track$sections$start + track$sections$duration],
           section_loudness = track$sections$loudness[start >= track$sections$start & start < track$sections$start + track$sections$duration]) %>% 
    ungroup() %>% 
    mutate(section = dense_rank(section_start)) %>% 
    group_by(section) %>% 
    mutate(section_end = max(start)) %>% 
    ungroup()

sections <- plot_df %>% 
    mutate(color = colorize(section_loudness, brewer.pal(3, 'YlOrRd'))) %>% 
    select(section_start, section_end, section_loudness, section, color) %>% 
    unique()

plotlines <- map(sections$section_start, function(x) {
    list(color = 'grey', value = x, width = 1, zIndex = 5)
})

plotbands <- map(1:nrow(sections), function(x) {
    list(
        from = sections$section_start[x],
        to = sections$section_end[x],
        color = sections$color[x]
    )
})

plot_df %>% 
    hchart(hcaes(x = start, y = loudness_start), type = 'line') %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_colors('black')

plot_df %>% 
    hchart(hcaes(x = start, y = loudness_start), type = 'line') %>% 
    hc_xAxis(plotLines = plotlines) %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_colors('black')

plot_df %>% 
    hchart(hcaes(x = start, y = loudness_start), type = 'line') %>% 
    hc_xAxis(plotBands = plotbands, gridLineColor = 'transparent') %>% 
    hc_yAxis(gridLineColor = 'transparent') %>% 
    hc_colors('black') %>% 
    hc_add_theme(hc_theme_smpl())
