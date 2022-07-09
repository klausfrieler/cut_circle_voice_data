
metadata_from_fname <- function(fname){
  pieces <- c("j1" = "josquin-virgo", 
              "j2" = "josquin-dung", 
              "a1" = "dufay-agnus1", 
              "a2" = "dufay-agnus2", 
              "a3" = "dufay-agnus3",
              "m1" = "dufay-kyrie1", 
              "m2" = "dufay-kyrie2", 
              "m3" = "dufay-gloria" 
  )
  voices <- c("hs7" = "sopran1", 
              "hs8" = "sopran1",
              "hs1" = "alt1",
              "hs2" = "alt2",
              "hs5" = "tenor1",
              "hs6" = "tenor1",
              "hs3" = "bass1",
              "hs4" = "bass2")
              
  map_dfr(basename(fname) %>% tools::file_path_sans_ext() %>% str_split("_"), function(elts){
    #"j1-2"   "take10" "hs3"  
    tmp <- str_split(elts[1], "-")[[1]]
    tibble(piece = pieces[tmp[1]], 
           repetition = as.numeric(tmp[2]),
           take = elts[2],
           voice_type = str_extract(voices[elts[3]], "^[a-z]+"),
           voice_no = str_extract(voices[elts[3]], "[0-9]+$"),
           headset = elts[3] 
           )
  }) %>% mutate(id = basename(fname) %>% tools::file_path_sans_ext() )
  
}

list_csv_files <- function(dir){
  list.files(dir, pattern = ".csv", full.names = T)
}

read_note_tracks_by_list <- function(file_list){
  map_dfr(file_list, function(fn){
    #browser()
    tmp <- readr::read_csv(fn, col_names = c("onset", "pitch", "duration", "dummy", "pos")) %>%
      mutate(onset_hms = hms::hms(seconds = onset),
             pos = as.character(pos),
             pitch_hz = pitch,
             pitch = hz_to_midi(pitch_hz))  %>% 
      mutate(ioi = c(diff(onset), NA), 
             int_midi = c(diff(pitch), NA)) %>% bind_cols(metadata_from_fname(fn)) %>% 
      rename()
    
    pitch_out <- tmp %>% pull(pitch) %>%  boxplot() %>% pluck("out")
    messagef("[%s] Found %d pitch outliers", fn, length(pitch_out))
    tmp <- tmp %>% filter(!(pitch %in% pitch_out))
    
    duration_out <- tmp %>% pull(duration) %>%  boxplot() %>% pluck("out")
    duration_out <- duration_out[duration_out < mean(tmp$duration)]
    messagef("[%s] Found %d duration outliers", fn, length(duration_out))
    print(tmp)
    browser()
    tmp %>% filter(!(duration %in% duration_out))
    
  }) %>% 
    select(pos, onset, onset_hms, pitch = pitch_midi, pitch_hz = pitch, ioi, int = int_midi, everything() )
  
  
}
read_note_tracks <- function(data_dir){
  read_note_tracks_by_list(list.files(data_dir, pattern = ".csv", full.names = T))
}
