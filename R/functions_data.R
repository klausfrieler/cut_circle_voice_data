
metadata_from_fname <- function(fname){
  map_dfr(basename(fname) %>% tools::file_path_sans_ext() %>% str_split("_"), function(elts){
    tibble(day = elts[2], condition = elts[3], track = elts[4], voice = elts[5])
  }) %>% mutate(id = basename(fname) %>% tools::file_path_sans_ext() )
  
}

list_csv_files <- function(dir){
  list.files(dir, pattern = ".csv", full.names = T)
}

read_note_tracks_by_list <- function(file_list){
  map_dfr(file_list, function(fn){
    #browser()
    tmp <- readr::read_csv(fn, col_names = c("onset", "pitch", "duration")) %>%
      mutate(pos = 1:nrow(.),
             onset_hms = hms::hms(seconds = onset) ,
             pitch_midi = hz_to_midi(pitch))  %>% 
      mutate(ioi = c(diff(onset), NA), 
             int_midi = c(diff(pitch_midi), NA)) %>% bind_cols(metadata_from_fname(fn))
    
    pitch_out <- tmp %>% pull(pitch_midi) %>%  boxplot() %>% pluck("out")
    messagef("[%s] Found %d pitch outliers", fn, length(pitch_out))
    tmp <- tmp %>% filter(!(pitch_midi %in% pitch_out))
    
    duration_out <- tmp %>% pull(duration) %>%  boxplot() %>% pluck("out")
    duration_out <- duration_out[duration_out < mean(tmp$duration)]
    messagef("[%s] Found %d duration outliers", fn, length(duration_out))
    tmp %>% filter(!(duration %in% duration_out))
    
  }) %>% 
    mutate(across(c(day, condition, track, voice), str_extract, "[0-9]+")) %>% 
    select(pos, onset, onset_hms, pitch = pitch_midi, ioi, int = int_midi, day, condition, track, voice, id)
  
  
}
read_note_tracks <- function(data_dir){
  read_note_tracks_by_list(list.files(data_dir, pattern = ".csv", full.names = T))
}
