library(tidyverse)
library(readr)
library(performance)
library(broom.mixed)
library(tuneR)
library(tictoc)

messagef <- function(...) message(sprintf(...))

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
           take = sprintf("take%02d", str_remove(elts[2], "b") %>% str_extract("[0-9]+$") %>% as.integer()),
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
    messagef("Reading  %s", fn)
    tmp <- readr::read_csv(fn, 
                           col_names = c("onset", "pitch", "duration", "dummy", "pos"),
                           col_types = col_types) 
    if(length(tmp) == 3){
      tmp$pos <- NA
      tmp$dummy <- NA
    }
    tmp <- tmp %>% 
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
    mutate(pos_orig = pos,
           note_label = str_extract(pos, "^[0-9]+[a-cx]?"),
           pos_spec = str_extract(pos, "[^0-9]+$") %>% 
             tolower() %>% 
             str_replace("-", "~") %>% 
             str_replace("x~", "x") %>% 
             str_replace("~x", "x") %>% 
             str_replace("bx", "x"),
           pos = as.numeric(str_extract(pos, "^[0-9]+")),
           voice_type = factor(voice_type, levels = c("sopran", "alt", "tenor", "bass")),           
           note_id = sprintf("%s:%s", id, note_label)) %>% 
    select(pos, onset, onset_hms, pitch, pitch_hz, ioi, int = int_midi, everything() ) 

    ret[is.na(ret$repetition),]$repetition <- 1
  ret[is.na(ret$pos_spec), ]$pos_spec <- ""
  ret <- ret %>% 
    mutate(piece_take = sprintf("%s:%s:r%d", take, piece, repetition),
           error = as.integer(pos_spec %in% c("n", "x", "~")))  

  
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
  }) %>% 
    mutate(voice_type = factor(voice_type, levels = c("sopran", "alt", "tenor", "bass")))
  browser()
  #Josquins all a whole tone too low?!
  ret[str_detect(ret$piece, "jos"),]$nom_pitch <- ret[str_detect(ret$piece, "jos"),]$nom_pitch - 2 
  ret
}

read_track_info <- function(path){
  readxl::read_excel(path) %>% 
    filter(!is.na(piece)) %>% 
    select(take, condition, day) %>% 
    mutate(take = sprintf("take%02d", as.integer(take)), 
           condition = c("tc" = "touch", "ntc" = "no-touch", "ntf" = "far-apart")[condition])
  
}

annotate_note_tracks <- function(note_tracks, scores, track_info){
  ret <- 
    note_tracks %>% 
    left_join(scores %>% select(-id), by = c("piece", "voice_type", "note_label")) %>% 
    filter(!is.na(pos),  pos_spec != "~", pos_spec != "x", pos_spec != "n", pos_spec != "g") %>% 
    mutate(d_pitch = pitch - nom_pitch) %>% 
    select(-error)

  ret  <- ret %>% left_join(track_info, by = "take") %>% mutate(day = sprintf("day-%d", day))
  
  sync_points <- scores %>% 
    group_by(piece, nom_onset) %>% 
    summarise(sync_point = n() == 4, .groups = "drop") %>% 
    filter(sync_point) %>% 
    select(piece, nom_onset)
  ret <- ret %>% left_join(sync_points %>% mutate(sync = TRUE)) 
  ret[is.na(ret$sync),]$sync <- FALSE
  ret
}


end_to_start_diff <- function(x){
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

get_pitch_stats <- function(data){
  data %>% group_by(piece_take, headset) %>% 
    mutate(MAPE = mean(abs(d_pitch_res), na.rm = T),
           MPP  = mean(sd(d_pitch_res, na.rm = T)),
           LMAPE = -log(MAPE),
           LMPP  = -log(MPP)) %>% 
    ungroup() %>% 
    distinct(piece, take, piece_take, day, condition, headset, repetition, MAPE, MPP, LMAPE, LMPP)
}

get_pitch_stats_inner_voice <- function(data){
  #browser()
  data %>% 
    select(piece_take, voice_type, voice_no, condition, day, piece, pos, pitch) %>% 
    pivot_wider(id_cols = c(piece_take, voice_type, condition, day, piece, pos), 
                names_from = voice_no, 
                values_from = pitch, names_prefix = "voice") %>% 
    mutate(d_voice = voice2 - voice1) %>% 
    group_by(piece_take, day, piece, condition, voice_type) %>% 
    summarise(
      MAPE = mean(abs(d_voice), na.rm = T),
      MPP  = mean(sd(d_voice, na.rm = T)),
      LMAPE = -log(MAPE),
      LMPP  = -log(MPP), .groups = "drop")
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
           total_drift = predict(model) %>% end_to_start_diff())
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
             day = tmp$day[1], 
             piece = tmp$piece[1],
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
               day = tmp$day[1], 
               piece = tmp$piece[1],
               piece_take = tp,
               headset = hs,
               condition = tmp$condition[1])
      
    }) %>% 
      bind_rows(total) %>% 
      select(piece_take, type, scope, voice_type, condition, day, piece, headset, term, everything())
  })
}

sig_stars <- Vectorize(
  function(p_value){
    if(is.na(p_value)){
      return(NA)
    }
    stars <- ""
    if (p_value < 0.1) stars <- "."
    if (p_value < 0.05) stars <- "*"
    if (p_value < 0.01) stars <- "**"
    if (p_value < 0.001) stars <- "***" 
    sprintf("%.3f%s", p_value, stars)
  }
)

make_nice_lmer <- function(lmer_mod, DV = ""){
  broom.mixed::tidy(lmer_mod) %>% 
    mutate(DV = !!DV, 
           p_val_str = sig_stars(p.value)) %>% 
    bind_cols(broom.mixed::glance(lmer_mod)) %>% 
    bind_cols(performance::r2_nakagawa(lmer_mod) %>% as.data.frame() %>% select(1, 2) %>% as_tibble()) 
    
}

get_lmer_model <- function(pitch_stats, 
                           dv = "LMAPE", 
                           fixed = "condition", 
                           ranef = c("piece", "day", "headset"), 
                           reduced = T){
  map_dfr(dv, function(v){
    form <- sprintf("%s ~ %s + %s", v, 
                    fixed, 
                    paste(sprintf("(1|%s)", ranef), collapse = " + "))
    mod <- lmerTest::lmer(form, data = pitch_stats) %>% make_nice_lmer(v)
    if(reduced){
      mod %>% 
        filter(effect == "fixed") %>% 
        select(term, DV, estimate, p_val_str, R2_conditional, R2_marginal) 
    }
    else{
      mod %>% select(effect, term, DV, estimate, p_val_str, R2_conditional, R2_marginal, everything())
    }
    
  })
}
get_bootstrap_sample <- function(data){
  data[sample(1:nrow(data), replace = T),]
}

get_permutation <- function(data, target_var = "day"){
  stopifnot(length(target_var) == 1)
  data %>% mutate(!!sym(target_var) := sample(!!sym(target_var)))
}

bootstrap_lm <- function(data, 
                         dv = "LMAPE", 
                         iv = "day", 
                         size = 1, 
                         type = c("bootstrap", "permutation")){
  type = match.arg(type)      
  map_dfr(dv, function(v1){
    browser()
    form <- sprintf("%s ~ %s", v1, paste(iv, collapse = " + ")) %>% as.formula()
    map_dfr(1:size, function(n){
      browser()
      if(type == "bootstrap"){
        tmp <- get_bootstrap_sample(data)
      }
      else{
        tmp <- get_permutation(data, iv)
      }
      tmp %>% 
        lm(form, data = .) %>% 
        broom::tidy() %>% 
        mutate(iter = n)
    })
    
  })
}

get_intonation_model_single <- function(pitch_stats, reduced = T){
  get_lmer_model(pitch_stats, 
                 dv = c("LMAPE", "LMPP"), 
                 fixed = "condition", 
                 ranef = c("piece", "headset", "day"))  %>% 
    mutate(model_type = "intonation_single_voice")
}

get_intonation_model_inner_voice <- function(pitch_stats_inner){
  get_lmer_model(pitch_stats_inner, 
                 dv = c("LMAPE", "LMPP"), fixed = "condition", 
                 ranef = c("piece", "voice_type", "day"))  %>% 

    mutate(model_type = "intonation_inner_voice")
}

get_permutation_tests <- function(pitch_stats, dv = c("LMAPE", "LMPP"), iv = c("condition", "day", "headset", "piece")){
  map_dfr(dv, function(v1){
    map_dfr(iv, function(v2){
      form <- sprintf("%s ~ factor(%s)", v1, v2) %>% as.formula()
      ipt <- coin::independence_test(form, data = pitch_stats)
      tibble(dv = v1, iv = v2, 
             statistic = coin::statistic(ipt) %>% as.numeric(), 
             p.value = coin::pvalue(ipt) %>% as.numeric())
    })
  })  
}

check_main_effects <- function(pitch_stats, pitch_stats_inner){
  bind_rows(get_permutation_tests(pitch_stats) %>% mutate(type = "intonation_single"),
            get_permutation_tests(pitch_stats_inner, 
                                  iv = c("condition", "day", "voice_type", "piece"))
            %>% mutate(type = "intonation_inner")
            )  %>% mutate(p_val_str = sig_stars(p.value))
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
      
      #browser()
      tmp2 %>% 
        mutate(d_onset = onset1  - mean(onset1[1:5]) - onset2 + mean(onset2[1:5])) %>% 
        summarise(synchrony = mean(d_onset, na.rm = T), 
                  abs_phase = mean(abs(d_onset), na.rm = T), 
                  phase_prop = mean(sign(d_onset), na.rm = T))  %>% 
        mutate(piece_take = tp, voice_type = vt)
      
    })
  }) %>% left_join(pitch_data %>% distinct(piece_take, condition), by = "piece_take")
}

get_vertical_indicators <- function(pitch_data){
  indicators <- 
    pitch_data %>% 
    filter(sync)%>% 
    group_by(piece_take, nom_onset, condition, day, piece, repetition) %>%
    summarise(MOP = sd(onset), 
              MAPE = mean(abs(d_pitch_res)),
              LMOP = -log(MOP),
              LMAPE = -log(MAPE),
              .groups = "drop") 
  indicators
}

get_vertical_models <- function(vertical_indicators){
  model.lmape  <- vertical_indicators %>% 
    lmerTest::lmer(LMAPE ~ condition + (1|piece) + (1|day), data = .)
  model.lmop <- vertical_indicators %>% 
    lmerTest::lmer(LMOP ~ condition + (1|piece) + (1|day), data = .)
  bind_rows(make_nice_lmer(model.lmape, "VERT_LMAPE"), 
            make_nice_lmer(model.lmop, "VERT_LMOP"))
  
}

get_singing_errors <- function(note_tracks, note_tracks_annotated){
  tmp <- note_tracks %>%
    left_join(note_tracks_annotated %>% 
                distinct(piece_take, headset, condition, piece, repetition, day), 
              by = c("piece_take", "headset", "piece", "repetition")) %>% 
    filter(!is.na(pos))
  
  error_log <- 
    lme4::glmer(error ~ condition + (1|piece) + (1|headset) +(1|day), 
                family = binomial, 
                data = tmp )
  
  error_log %>% 
    make_nice_lmer(DV = "error") %>% 
    filter(effect == "fixed") %>% 
    select(term, DV, estimate, p_val_str, R2_conditional, R2_marginal) 
}

plot_error_rates <- function(note_tracks, note_tracks_annotated){
  browser()
  tmp <- note_tracks %>%
    left_join(note_tracks_annotated %>% 
                distinct(piece_take, headset, condition, piece, repetition, day), 
              by = c("piece_take", "headset", "piece", "repetition")) %>% 
    filter(!is.na(pos)) %>% 
    group_by(condition, piece, day, headset) %>% 
    summarise(error_rate = mean(error),
              NLER = -log(error_rate),
              .groups = "drop")
  plot_main_effect(tmp, dv = "error_rate")
    
}

pad_to_next_power_of_two <- function(x, pad = 0){
  next_pow2 <- 2^ceiling(log2(length(x)))
  c(x, rep(pad, next_pow2 - length(x)))
}

next_power_of_two <- function(x){
  2^round(log2(x))  
}

fft_cache <- list()

get_cc <-function(target, query){
  target <- pad_to_next_power_of_two(target)
  query <- pad_to_next_power_of_two(query)
  lc <- length(target)
  ls <- length(query)
  size <- lc - ls
  stopifnot(size >= 0)
  query <- c(query, rep(0, size))

  hash <- digest::digest(target)
  if(hash %in% names(fft_cache)){
    #messagef("Found FFT in cache (%s)", hash)
    fft_target <- fft_cache[[hash]] 
  }
  else{
    w_target <- scale(target) * e1071::hamming.window(length(target))
    fft_target <- fft(w_target)
    fft_cache[[hash]] <<- fft_target
  }
  w_query <- scale(query) * e1071::hamming.window(length(query))
  
  corr <- fft(fft_target* Conj(fft(w_query)), inverse = T)
  corr %>% abs()
}

find_snippet <- function(container, snippet, max_win_sec = 2, sr = 48000){
  max_win_size <- round(sr * max_win_sec) %>% next_power_of_two() %>% as.integer()
  
  target <- pad_to_next_power_of_two(container@left)
  query <- pad_to_next_power_of_two(snippet@left)
  if(length(query) > max_win_size){
    query <- query[1:max_win_size]  
  }
  else{
    max_win_size <- length(query)  
  }
  lt <- length(target)
  lq <- length(query)
  size <- lt - lq
  stopifnot(size >= 0)
  max_windows <- 2 * ceiling(lt/max_win_size) %>% as.integer()
  hop_size <- as.integer(max_win_size / 2)
  #browser()
  messagef("Starting snippet find with %d windows of size %d/%.3f and hop size %d/%.3f (target length: %.3f s, query length: %.3f sec)", 
           max_windows, max_win_size, max_win_size/sr, hop_size, hop_size/sr, lt/sr, length(snippet@left)/sr)
  map_dfr(seq(0, max_windows - 1), function(i){
    if(i %% 10 == 0) message(sprintf("Window %d @offset %d", i, hop_size * i))  
    #browser()
    t <- target[seq(1, max_win_size) + hop_size * i]
    cc <- get_cc(t, query)
    arg_max <- which.max(cc)
    tibble(window = i, 
           window_offset = hop_size * i,
           window_offset_sec = hop_size * i / sr,
           arg_max =  arg_max, 
           max_val = cc[arg_max], 
           total_offset =  window_offset + arg_max, 
           total_offset_sec = total_offset/sr
           )
  }) %>% mutate(is_max = max_val == max(max_val))
}


find_snippet2 <- function(container, snippet, sr = 48000){
  tmp <- get_cc(container@left, snippet@left)
  tibble(offset = which.max(tmp) - 1, offset_sec = offset/sr)
}

read_take_audios <- function(audio_dir = "e:/projects/science/MPIEA/projects/cut_circle_voice_data/audios", 
                            day = 1, 
                            take = 5, 
                            headset = 1){
  #browser()
  take_dir <- file.path(audio_dir, sprintf("day_%d/take_%02d", day, take))
  full  <- readWave(file.path(take_dir, sprintf("%02d_H-Set%d.wav", headset, headset)))
  file_list <- list.files(file.path(take_dir, sprintf("Annotation Take %d/hs%d", take, headset)), pattern = "*.wav", full.names = T)
  file_list <- file_list[str_detect(file_list, "_take")] 
  snippets <- map(file_list, function(fn) readWave(fn))
  names(snippets) <-  file_list %>% basename() %>% tools::file_path_sans_ext() %>% str_replace("5b_", "5_") 
  list(full = full, snippets = snippets)
}

align_all_audios <- function(audio_dir = "audios", 
                             days = 1:3,
                             exclude_takes = 9, 
                             headsets = 1:8,
                             save_dir = "data/metadata"){
  map_dfr(days, function(n){
    day <- sprintf("day_%d", n)
    takes <- list.files(file.path(audio_dir, day)) %>% str_remove("take_") %>% as.integer()
    takes <- setdiff(takes, exclude_takes)
      map_dfr(takes, function(tk){
        pos <- 
          map_dfr(headsets, function(hs){
            messagef("*** Aligning: Day %d, Take %d, Headset %d ***", n, tk, hs)
            pos <- align_take_snippets2(audio_dir, day = n, take = tk, headset = hs) %>% 
              mutate(take = tk, headset = hs)
            saveRDS(pos, file = file.path(save_dir, sprintf("take%02d_hs%d_offsets.rds", tk, hs)))
            pos
          }) 
        saveRDS(pos, file = file.path(save_dir, sprintf("take%02d_offsets.rds", tk)))
        pos
    })
  })
}

align_take_snippets <- function(audio_dir = "audios", 
                                day = 1, 
                                take = 5, 
                                headset = 1, 
                                win_size = 20){
  audios <- read_take_audios(audio_dir, day = day, take = take, headset = headset)
  take_pos <- map_dfr(names(audios$snippets), function(n){
    find_snippet2(audios$full, 
                 audios$snippets[[n]], 
                 max_win_sec = win_size) %>% 
      mutate(snippet = n)})
}

align_take_snippets2 <- function(audio_dir = "e:/projects/science/MPIEA/projects/cut_circle_voice_data/audios", 
                                day = 1, 
                                take = 5, 
                                headset = 1){
  audios <- read_take_audios(audio_dir, day = day, take = take, headset = headset)
  take_pos <- map_dfr(names(audios$snippets), function(n){
    messagef("    Snippet: %s", n)
      find_snippet2(audios$full, 
                    audios$snippets[[n]]) %>% 
        mutate(snippet = n)
    }) %>% arrange(offset)
  fft_cache <<- list()
  take_pos
}