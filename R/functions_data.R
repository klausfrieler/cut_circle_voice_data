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
           repetition = ifelse(is.na(tmp[2]), 1, as.numeric(tmp[2])),
           take = str_remove(elts[2], "b"),
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
    messagef("Reading note track %s", fn)
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
      #browser()
      outliers <<- bind_rows(outliers, 
                             tmp %>% slice(1) %>% mutate(type = "missing_col"))
      
    }
    tmp
  }) %>% 
    mutate(note_label = str_extract(pos, "^[0-9]+[a-c]?"),
           pos_spec = str_extract(pos, "[^0-9]$") %>% tolower() %>% str_replace("-", "~"),
           pos = as.numeric(str_extract(pos, "^[0-9]+")),
           note_id = sprintf("%s:%s", id, note_label)) %>% 
    select(pos, onset, onset_hms, pitch, pitch_hz, ioi, int = int_midi, everything() ) 
  ret[is.na(ret$pos_spec), ]$pos_spec <- ""
  
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

annotate_note_tracks <- function(note_tracks, scores, track_info){
  track_info <- readxl::read_excel(track_info) %>% 
    filter(!is.na(piece)) %>% 
    select(take, condition) %>% 
    mutate(take = sprintf("take%s", take), 
           condition = c("tc" = "touch", "ntc" = "no-touch", "ntf" = "far-apart")[condition])
  note_tracks[is.na(note_tracks$repetition),]$repetition <- 1
  ret <- 
    note_tracks %>% 
    left_join(scores %>% select(-id), by = c("piece", "voice_type", "note_label")) %>% 
    filter(!is.na(pos), is.na(pos_spec) | pos_spec != "~", is.na(pos_spec) | pos_spec != "x") %>% 
    mutate(d_pitch = pitch - nom_pitch, 
           piece_take = sprintf("%s:%s:r%d", take, piece, repetition),
           voice_type = factor(voice_type, levels = c("sopran", "alt", "tenor", "bass")))
  ret[is.na(ret$pos_spec),]$pos_spec <- ""

  ret  <- ret %>% left_join(track_info, by = "take")
  ret
}

retune_all <- function(pitch_data){
  map_dfr(unique(pitch_data$piece_take), function(tp){
    tmp <- pitch_data %>% 
      filter(piece_take == tp)
    offset <- tmp %>% 
      pull(pitch) %>% 
      find_global_tuning_offset(epsilon = .01)  
    messagef("Retuning %s with offset %f", tp, offset)
    tmp %>% mutate(pitch_u = pitch, pitch = pitch - offset)
  })
}

end_to_end_diff <- function(x){
  x[length(x)] - x[1]
}

remove_linear_trend <- function(pitch_data, slope = NULL, intercept = NULL){
  if(is.null(slope) || is.null(intercept)){
    model <- lm(d_pitch ~ onset, data = pitch_data) 
    intercept <- coef(model)[1]
    slope <- coef(model)[2]    
    residuals <- pitch_data$d_pitch - (intercept + pitch_data$onset * slope)
  }
  else{
    residuals <- residuals(model)
  }
  pitch_data <- pitch_data %>% mutate(d_pitch_res = residuals)
  pitch_data
}

remove_all_linear_trends <- function(pitch_data, max_error = 1){
  pitch_data <- pitch_data %>% filter(abs(d_pitch) < max_error)
  map_dfr(unique(pitch_data$piece_take), function(tp){
    tmp <- pitch_data %>% 
      filter(piece_take == tp)
    messagef("Removing linear trend for %s", tp)
    tmp %>% remove_linear_trend()
  })
  
}
get_linear_trend <- function(pitch_data, type = "position"){
  if(type == "position"){
    model <- lm(d_pitch ~ pos, data = pitch_data) 
  }
  else{
    model <- lm(d_pitch ~ onset, data = pitch_data) 
  }
  ret <- model %>% broom::tidy() %>% 
    mutate(type = type, 
           total_drift = predict(model) %>% end_to_end_diff())
  ret
}

analyze_drift_and_tuning <- function(pitch_data){
  map_dfr(unique(pitch_data$piece_take), function(tp){
    messagef("Analyzing drift: %s", tp)
    tmp <- pitch_data %>% 
      filter(piece_take == tp)

    global_offset <- tmp %>% 
      pull(pitch) %>% 
      find_global_tuning_offset(epsilon = .01)  
    
    total <- map_dfr(c("position", "onset"), ~{get_linear_trend(tmp, .x)}) %>% 
      mutate(scope = "total", 
             voice_type = "all", 
             offset = global_offset,
             piece_take = tp, 
             condition = tmp$condition[1])
    
    map_dfr(unique(tmp$headset), function(hs){
      tmp2 <- tmp %>% 
        filter(headset == hs)
      
      offset <- tmp2 %>% 
        pull(pitch) %>% 
        find_global_tuning_offset(epsilon = .01)  
      
      map_dfr(c("position", "onset"), ~{ get_linear_trend(tmp2, .x) })%>% 
        mutate(scope = hs, 
               voice_type = tmp$voice_type[1], 
               offset = offset, 
               piece_take = tp,
               condition = tmp$condition[1])
      
    }) %>% 
      bind_rows(total) %>% 
      select(piece_take, type, scope, voice_type, condition, term, everything())
  })
}

get_synchrony_indicators <- function(pitch_data){
  
  map_dfr(unique(pitch_data$piece_take), function(tp){
    tmp <- pitch_data %>% filter(piece_take == tp) 
    
    map_dfr(unique(tmp$voice_type), function(vt){
      messagef("Gettin synchronies for %s (%s)", tp, vt)
      if(tmp %>% filter(voice_type == vt) %>% pull(headset) %>% unique() %>% length() != 2){
        #browser()
        return(NULL)
      }
      tmp2 <- tmp %>% filter(voice_type == vt) %>% 
        distinct(pos, headset, .keep_all = T) %>% 
        pivot_wider(id_cols = c(piece_take, pos), 
                    names_from = c(voice_no), 
                    values_from = onset, names_prefix = "onset") 
      
      browser()
      tmp2 %>% mutate(d_onset = onset1  - mean(onset1[1:5]) - onset2 + mean(onset2[1:5])) %>% 
        summarise(synchrony = mean(d_onset, na.rm = T), 
                  abs_phase = mean(abs(d_onset), na.rm = T), 
                  phase_prop = mean(sign(d_onset), na.rm = T))  %>% 
        mutate(piece_take = tp, voice_type = vt)
      
    })
  }) %>% left_join(pitch_data %>% distinct(piece_take, condition), by = "piece_take")
}