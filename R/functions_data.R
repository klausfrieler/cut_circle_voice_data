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
            "hs8" = "sopran2",
            "hs1" = "alt1",
            "hs2" = "alt2",
            "hs5" = "tenor1",
            "hs6" = "tenor2",
            "hs3" = "bass1",
            "hs4" = "bass2")

score_metadata_from_fname <- function(fname){
  map_dfr(basename(fname) %>% tools::file_path_sans_ext() %>% str_split("_"), function(elts){
    #"j1-2"   "take10" "hs3"  
    tmp <- str_split(elts[2], "-")[[1]]
    tibble(piece = pieces[elts[1]], 
           voice_type = str_extract(voices[tmp[1]], "^[a-z]+")
    )
  }) %>% mutate(id = basename(fname) %>% tools::file_path_sans_ext() )
  
}

note_track_metadata_from_fname <- function(fname){
  map_dfr(basename(fname) %>% tools::file_path_sans_ext() %>% str_split("_"), function(elts){
    #"j1-2"   "take10" "hs3"  
    tmp <- str_split(elts[1], "-")[[1]]
    tibble(piece = pieces[tmp[1]], 
           repetition = as.numeric(tmp[2]),
           take = elts[2],
           voice_type = str_extract(voices[ substr(elts[3], 1, 3) ], "^[a-z]+"),
           voice_no = str_extract(voices[substr(elts[3], 1, 3) ], "[0-9]+$"),
           headset = substr(elts[3], 1, 3) 
           )
  }) %>% mutate(id = str_remove(basename(fname) %>% tools::file_path_sans_ext(), "x$") )
  
}

list_csv_files <- function(dir){
  list.files(dir, pattern = ".csv", full.names = T)
}

read_note_tracks_by_list <- function(file_list){
  outliers <- tibble()
  col_types <- cols(
    onset = col_double(),
    pitch = col_double(),
    duration = col_double(),
    pos = col_character(),
    dummy = col_integer()
  )
  ret <- 
    map_dfr(file_list, function(fn){
    #browser()
    #
    tmp <- readr::read_csv(fn, 
                           col_names = c("onset", "pitch", "duration", "dummy", "pos"),
                           col_types = col_types) %>%
      mutate(onset_hms = hms::hms(seconds = onset),
             pos = as.character(pos),
             pitch_hz = pitch,
             pitch = hz_to_midi(pitch_hz))  %>% 
      mutate(ioi = c(diff(onset), NA),
             int_midi = c(diff(pitch), NA)) %>%
      bind_cols(note_track_metadata_from_fname(fn)) %>% 
      select(-dummy)
    if(all(is.na(tmp$pos))){
      outliers <<- bind_rows(outliers, 
                             tmp %>% slice(1) %>% mutate(outlier_type = "missing_col"))
      
    }
    pitch_out <- tmp %>% pull(pitch) %>%  boxplot() %>% pluck("out")
    messagef("[%s] Found %d pitch outliers", fn, length(pitch_out))
    if(length(pitch_out) > 0){
      outliers <<- bind_rows(outliers, 
                             tmp %>% filter(pitch %in% pitch_out) %>% mutate(outlier_type = "pitch"))
    }
    tmp <- tmp %>% filter(!(pitch %in% pitch_out))
    
    duration_out <- tmp %>% pull(duration) %>%  boxplot() %>% pluck("out")
    duration_out <- duration_out[duration_out < mean(tmp$duration)]
    messagef("[%s] Found %d duration outliers", fn, length(duration_out))
    if(length(duration_out) > 0){
      outliers <<- bind_rows(outliers, 
                             tmp %>% filter(pitch %in% pitch_out) %>% mutate(outlier_type = "duration"))
    }
    tmp %>% filter(!(duration %in% duration_out)) 
  }) %>% 
    mutate(note_label = str_extract(pos, "^[0-9]+[a-c]?"),
                                   pos_spec = str_extract(pos, "[^0-9]$") %>% tolower() %>% str_replace("-", "~"),
           pos = as.numeric(str_extract(pos, "^[0-9]+")),
           note_id = sprintf("%s:%s", id, note_label)) %>% 
    select(pos, onset, onset_hms, pitch, pitch_hz, ioi, int = int_midi, everything() ) 
  
  assign("outliers", outliers, globalenv())
  ret
}

read_note_tracks <- function(data_dir, pattern = ".csv"){
  read_note_tracks_by_list(list.files(data_dir, pattern = pattern, full.names = T))
}

read_score_files <- function(score_dir = "data/note_data_csv", pattern = "*.csv"){
  col_types <- cols(
    note_label = col_character(),
    measure_number = col_double(),
    beat_pos = col_double(),
    midi_pitch = col_double(),
    duration = col_double(),
    time_signature = col_double(),
    onset = col_double()
  )
  scores <- list.files(score_dir, pattern = pattern, full.names = T)
  ret <- 
    map_dfr(scores, function(fn){
    readr::read_csv(fn, 
                    col_types = col_types) %>% 
      rename(nom_pitch = midi_pitch, 
             nom_onset = onset,
             nom_duration = duration,
             bar = measure_number) %>% 
      bind_cols(score_metadata_from_fname(fn))
  })
  browser()
  #Josquins all a whole tone too low?!
  ret[str_detect(ret$piece, "jos"),]$nom_pitch <- ret[str_detect(ret$piece, "jos"),]$nom_pitch - 2 
  ret
}

annotate_note_tracks <- function(note_tracks, scores){
  ret <- 
    note_tracks %>% 
    left_join(scores %>% select(-id), by = c("piece", "voice_type", "note_label")) %>% 
    filter(!is.na(pos), is.na(pos_spec) | pos_spec != "~", is.na(pos_spec) | pos_spec != "x") %>% 
    mutate(d_pitch = pitch - nom_pitch)
  ret
}