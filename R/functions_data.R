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
voices <- c("hs7" = "soprano1", 
            "hs8" = "soprano2",
            "hs1" = "alto1",
            "hs2" = "alto2",
            "hs5" = "tenor1",
            "hs6" = "tenor2",
            "hs3" = "bass1",
            "hs4" = "bass2")

voice_types <- str_remove(voices, "[0-9]$") %>% unique()

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
section_from_fname <- function(fname){
  note_track_metadata_from_fname(fname) %>% 
    mutate(section = sprintf("%s:%s:r%d", take, piece, repetition)) %>% 
    pull(section)
}
list_csv_files <- function(dir){
  list.files(dir, pattern = ".csv", full.names = T)
}

read_note_tracks_by_list <- function(file_list){
  outliers <- tibble()
  #browser()
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
           voice_type = factor(voice_type, levels = voice_types),           
           note_id = sprintf("%s:%s", id, note_label)) %>% 
    select(pos, onset, onset_hms, pitch, pitch_hz, ioi, int = int_midi, everything() ) 

  ret[is.na(ret$repetition),]$repetition <- 1
  ret[is.na(ret$pos_spec), ]$pos_spec <- ""
  ret <- ret %>% 
    mutate(section = sprintf("%s:%s:r%d", take, piece, repetition),
           error = as.integer(pos_spec %in% c("n", "x", "~")))  

  
  assign("outliers", outliers, globalenv())
  ret
}

read_note_tracks <- function(data_dir, pattern = ".csv"){
  #browser()
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
    mutate(voice_type = factor(voice_type, levels = voice_types))
  #browser()
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

read_piece_info <- function(path){
  readxl::read_excel(path, sheet = "music_metadata") %>% 
    filter(!is.na(piece)) %>% 
    select(piece_short = piece, full_name) %>% 
    mutate(piece = pieces[piece_short]) %>% 
    select(piece, full_name)
  
}

annotate_note_tracks <- function(note_tracks, scores, track_info, offsets){
  ret <- 
    note_tracks %>% 
    left_join(scores %>% select(-id), by = c("piece", "voice_type", "note_label")) %>% 
    filter(!is.na(pos),  pos_spec != "~", pos_spec != "x", pos_spec != "n", pos_spec != "g") %>% 
    mutate(d_pitch = pitch - nom_pitch) %>% 
    select(-error)
  ret  <- ret %>% left_join(track_info, by = "take") %>% mutate(day = sprintf("day-%d", day))
  #browser()
  ret <- ret %>% left_join(offsets %>% 
                             select(offset_sec, 
                                    section, 
                                    headset) %>% 
                             mutate(headset = sprintf("hs%d", headset)), 
                           by = c("section", "headset")) %>% 
    mutate(real_onset = onset + offset_sec)  

  scores_voice_count <- scores %>% distinct(piece, voice_type) %>% count(piece, name = "no_voices")
  scores <- scores %>% left_join(scores_voice_count, by = "piece")
  
  sync_points <- scores %>% 
    group_by(piece, nom_onset, no_voices) %>% 
#    summarise(sync_point = n() == no_voices, 
    summarise(sync_point = n() > 1, 
              .groups = "drop") %>% 
    filter(sync_point) %>% 
    distinct(piece, nom_onset)
  ret <- ret %>% left_join(sync_points %>% mutate(sync = TRUE)) 
  ret[is.na(ret$sync),]$sync <- FALSE
  ret
}


end_to_start_diff <- function(x){
  x[length(x)] - x[1]
}

remove_linear_trend <- function(pitch_data, slope = NULL, intercept = NULL){
  browser()
  if(is.null(slope) || is.null(intercept)){
    model <- lm(d_pitch ~ real_onset, data = pitch_data) 
    intercept <- coef(model)[1]
    slope <- coef(model)[2]    
    residuals <- pitch_data$d_pitch - (intercept + pitch_data$real_onset * slope)
  }
  else{
    residuals <- residuals(model)
  }
  pitch_data <- pitch_data %>% mutate(d_pitch_res = residuals)
  pitch_data
}

get_pitch_stats <- function(data){
  data %>% group_by(section, headset) %>% 
    mutate(MAPE = mean(abs(d_pitch_res), na.rm = T),
           MPP  = mean(sd(d_pitch_res, na.rm = T)),
           LMAPE = -log(MAPE),
           LMPP  = -log(MPP)) %>% 
    ungroup() %>% 
    distinct(piece, take, section, day, condition, headset, repetition, MAPE, MPP, LMAPE, LMPP)
}

get_pitch_stats_inner_voice <- function(data){
  ret <-
    data %>% 
    select(section, voice_type, voice_no, condition, day, piece, pos, pitch) %>% 
    pivot_wider(id_cols = c(section, voice_type, condition, day, piece, pos), 
                names_from = voice_no, 
                values_from = pitch, names_prefix = "voice") 
  #browser()
  ret <- ret %>% 
    mutate(d_voice = voice2 - voice1) %>% 
    group_by(section, day, piece, condition, voice_type) %>% 
    summarise(
      MAPE = mean(abs(d_voice), na.rm = T),
      MPP  = mean(sd(d_voice, na.rm = T)),
      LMAPE = -log(MAPE),
      LMPP  = -log(MPP), .groups = "drop")
}

get_onset_stats_inner_voice <- function(data, max_diff = .3, only_error = F){
  # ret1 <- data %>%
  #   group_by(section, voice_type, nom_onset, piece, condition, day) %>% 
  #   summarise(d_onset = abs(diff(real_onset)), .groups = "drop")
  # browser()
  ret2 <- data %>% 
    select(section, voice_type, voice_no, condition, day, piece, pos, real_onset) %>% 
    pivot_wider(id_cols = c(section, voice_type, condition, day, piece, pos), 
                names_from = voice_no, 
                values_from = real_onset, names_prefix = "voice") %>% 
    mutate(d_voice = voice2 - voice1)
  d_voice <- ret2 %>% pull(d_voice)
  messagef("Removed %d from %d events", length(d_voice) - length(d_voice[abs(d_voice) < max_diff]), length(d_voice))
  browser()
  if(only_error){
    if(only_error){
      tmp <- ret2 %>% mutate(error = (abs(d_voice) > max_diff))
      error_log <- 
        lme4::glmer(error ~ condition + (1|piece) +  (1|day) + (1|voice_type), 
                    family = binomial, 
                    data = tmp )  %>%  
        make_nice_lmer(DV = "error") %>% 
        filter(effect == "fixed") %>% 
        select(term, DV, estimate, p_val_str, R2_conditional, R2_marginal) 
      
      return(error_log)
    }
    
  }
  ret2 %>%    
    group_by(section, day, piece, condition, voice_type) %>% 
    summarise(
      MOE = mean(abs(d_voice[abs(d_voice) < max_diff]), na.rm = T),
      MOP  = mean(sd(d_voice[abs(d_voice) < max_diff], na.rm = T)),
      LMOE = -log(MOE),
      LMOP = -log(MOP), .groups = "drop") 
  # %>% 
  #   filter(section != "take21:dufay-agnus2:r2")
  
}

remove_all_linear_trends <- function(pitch_data, max_error = 1){
  pitch_data <- pitch_data %>% filter(abs(d_pitch) < max_error)
  map_dfr(unique(pitch_data$section), function(tp){
    browser()
    tmp <- pitch_data %>% 
      filter(section == tp)
    messagef("Removing linear trend for %s", tp)
    tmp %>% remove_linear_trend()
  })
  
}

get_linear_trend <- function(pitch_data, dv = "d_pitch", iv = "pos"){
  model <- lm(as.formula(sprintf("%s ~ %s", dv, iv)), data = pitch_data) 
  ret <- model %>% broom::tidy() %>% 
    mutate(type = iv, 
           total_drift = predict(model) %>% end_to_start_diff())
  ret
}

analyze_drift_and_tuning <- function(pitch_data){
  map_dfr(unique(pitch_data$section), function(tp){
    messagef("Analyzing drift: %s", tp)
    tmp <- pitch_data %>% 
      filter(section == tp)

    global_offset <- tmp %>% 
      pull(pitch) %>% 
      find_global_tuning_offset(epsilon = .01)  
    
    total <- map_dfr(c("pos", "real_onset"), ~{get_linear_trend(tmp, dv = "d_pitch", iv = .x)}) %>% 
      mutate(scope = "total", 
             voice_type = "all", 
             tuning_offset = global_offset,
             section = tp, 
             day = tmp$day[1], 
             piece = tmp$piece[1],
             condition = tmp$condition[1])
    
    map_dfr(unique(tmp$headset), function(hs){
      tmp2 <- tmp %>% 
        filter(headset == hs)
      
      offset <- tmp2 %>% 
        pull(pitch) %>% 
        find_global_tuning_offset(epsilon = .01)  
      
      map_dfr(c("pos", "real_onset"), ~{ get_linear_trend(tmp2, dv = "d_pitch", iv = .x) })%>% 
        mutate(scope = hs, 
               voice_type = tmp$voice_type[1], 
               tuning_offset = offset, 
               day = tmp$day[1], 
               piece = tmp$piece[1],
               section = tp,
               headset = hs,
               condition = tmp$condition[1])
      
    }) %>% 
      bind_rows(total) %>% 
      select(section, type, scope, voice_type, condition, day, piece, headset, term, everything())
  })
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
    #browser()
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

get_onset_model_sync <- function(onset_stats, reduced = T){
  get_lmer_model(onset_stats, 
                 dv = c("LMOP"), 
                 fixed = "condition", 
                 ranef = c("piece",  "day"))  %>% 
    mutate(model_type = "onset_sync")
}

get_onset_model_inner_voice <- function(onset_stats_inner){
  get_lmer_model(onset_stats_inner, 
                 dv = c("LMOP", "LMOE"), 
                 fixed = "condition", 
                 ranef = c("piece", "voice_type", "day"))  %>% 
    
    mutate(model_type = "onset_inner_voice")
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

check_main_effects <- function(stats, stats_inner, type = c("pitch", "onset")){
  type <- match.arg(type)
  if(type == "pitch") {
    ret <- bind_rows(
      get_permutation_tests(stats) %>% 
        mutate(type = "intonation_single"),
      get_permutation_tests(stats_inner, iv = c("condition", "day", "voice_type", "piece"))
      %>% mutate(type = "intonation_inner")
      ) 
  }
  else{
    ret <- bind_rows(
      get_permutation_tests(stats, dv = c("LMOP"), iv = c("condition", "day", "piece")) %>% 
        mutate(type = "onset_sync"),
      get_permutation_tests(stats_inner,
                            dv = c("LMOP", "LMOE"),
                            iv = c("condition", "day", "voice_type", "piece"))
              %>% mutate(type = "onset_inner")
    )
    
  }
  ret %>% mutate(p_val_str = sig_stars(p.value))
}

get_synchrony_indicators <- function(pitch_data){
  map_dfr(unique(pitch_data$section), function(tp){
    tmp <- pitch_data %>% filter(section == tp) 
    
    map_dfr(unique(tmp$voice_type), function(vt){
      messagef("Gettin synchronies for %s (%s)", tp, vt)
      if(tmp %>% filter(voice_type == vt) %>% pull(headset) %>% unique() %>% length() != 2){
        #browser()
        return(NULL)
      }
      tmp2 <- tmp %>% filter(voice_type == vt) %>% 
        distinct(pos, headset, .keep_all = T) %>% 
        pivot_wider(id_cols = c(section, pos), 
                    names_from = c(voice_no), 
                    values_from = onset, names_prefix = "onset") 
      
      #browser()
      tmp2 %>% 
        mutate(d_onset = onset1  - mean(onset1[1:5]) - onset2 + mean(onset2[1:5])) %>% 
        summarise(synchrony = mean(d_onset, na.rm = T), 
                  abs_phase = mean(abs(d_onset), na.rm = T), 
                  phase_prop = mean(sign(d_onset), na.rm = T))  %>% 
        mutate(section = tp, voice_type = vt)
      
    })
  }) %>% left_join(pitch_data %>% distinct(section, condition), by = "section")
}

get_onset_stats <- function(pitch_data, remove_outlier = T, only_error = F){
  indicators <- 
    pitch_data %>% 
    filter(sync)%>% 
    group_by(section, nom_onset, condition, day, piece, repetition) %>%
    summarise(OP = sd(real_onset), 
              LMOP = -log(OP),
              .groups = "drop")
  browser()
  if(only_error){
    tmp <- indicators %>% mutate(error = (OP %in% (boxplot(indicators$OP) %>% pluck("out"))))
    error_log <- 
      lme4::glmer(error ~ condition + (1|piece) +  (1|day), 
                  family = binomial, 
                  data = tmp )  %>%  
      make_nice_lmer(DV = "error") %>% 
      filter(effect == "fixed") %>% 
      select(term, DV, estimate, p_val_str, R2_conditional, R2_marginal) 
    
    return(error_log)
  }
  
  if(remove_outlier){
    before <- nrow(indicators)
    indicators <- remove_outlier(indicators, "OP")
    messagef("Removed %d events from %d", before - nrow(indicators), before)
  }  
  #browser()
  indicators <- indicators %>%
    group_by(section, condition, day, piece, repetition) %>%
    summarise(MOP = mean(OP, na.rm = T),
              LMOP = -log(MOP),
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
                distinct(section, headset, condition, piece, repetition, day), 
              by = c("section", "headset", "piece", "repetition")) %>% 
    filter(!is.na(pos))
  
  tmp <- tmp %>% 
    inner_join(note_tracks_annotated %>% 
                 select(note_id, d_pitch) %>% 
                 filter(!is.na(d_pitch)), 
               by = "note_id") %>% mutate(error = error | abs(d_pitch) > 1) 
  error_log <- 
    lme4::glmer(error ~ condition + (1|piece) + (1|headset) + (1|day), 
                family = binomial, 
                data = tmp )
  
  error_log %>% 
    make_nice_lmer(DV = "error") %>% 
    filter(effect == "fixed") %>% 
    select(term, DV, estimate, p_val_str, R2_conditional, R2_marginal) 
}

get_drift_model <- function(drift_data){
  model <- drift_data %>% 
    filter(type == "pos", 
           term != "(Intercept)", 
           scope == "total")  %>% 
    get_lmer_model(dv = c("estimate"), 
                   fixed = "condition", ranef = c("piece", "day"))
  model
}

get_tempo_model <- function(tempo_data){
  tempo_data %>% 
    mutate(log_abs_drift = -log(abs(dt)), 
           log_abs_total_drift = -log(abs(total_drift)),
           log_mean_tempo = -log(mean_tempo)) %>% 
    get_lmer_model(dv = c("log_abs_drift", 
                          "mean_tempo", 
                          "log_abs_total_drift"), 
                   fixed = "condition", 
                   ranef = c("piece", "day"))
  
}

plot_error_rates <- function(note_tracks, note_tracks_annotated){
  tmp <- note_tracks %>%
    left_join(note_tracks_annotated %>% 
                distinct(section, headset, condition, piece, repetition, day), 
              by = c("section", "headset", "piece", "repetition")) %>% 
    filter(!is.na(pos))
  
  tmp <- tmp %>% 
    inner_join(note_tracks_annotated %>% 
                 select(note_id, d_pitch) %>% 
                 filter(!is.na(d_pitch)), 
               by = "note_id") %>% 
    mutate(error = error | abs(d_pitch) > 1) 
  #browser()
  tmp <- tmp %>% group_by(condition, piece, day, headset) %>% 
    summarise(error_rate = mean(error),
              NLER = -log(error_rate),
              .groups = "drop")
  plot_main_effect(tmp, dv = "error_rate")
    
}

read_offsets <- function(data_dir = "data/metadata"){
  fnames <-  list.files(data_dir, "*offsets.rds", full.names = T)  
  offsets <- fnames[!str_detect(fnames, "hs") & !str_detect(fnames, "all")] %>% map_dfr(~{readRDS(.x)}) %>% 
    mutate(voice_type = ceiling(headset/2),
           voice_no = (headset - 1) %% 2 + 1,
           piece = str_remove(snippet, "_take[0-9]+_hs[0-9]$")) %>% 
    mutate(section = section_from_fname(snippet))
  offsets[offsets$take == 33 & offsets$piece == "a1-1" & offsets$headset == 1, ]$offset <- 0
  offsets[offsets$take == 33 & offsets$piece == "a1-1" & offsets$headset == 1, ]$offset_sec <- 0   
  offsets  %>% arrange(take, headset, offset)
}

analyze_tempo_and_drift <- function(clean_nt){
  tmp <- clean_nt %>% 
    group_by(section, headset) %>% 
    mutate(d_pos = c(diff(pos), NA), 
           ioi = c(diff(real_onset), NA), 
           nom_ioi = c(diff(nom_onset), NA), 
           T = ioi/nom_ioi) %>%  
    ungroup() %>% 
    filter(d_pos == 1, nom_ioi %in% c(2, 4))
  map_dfr(c(FALSE, TRUE), function(remove_outlier){
    map_dfr(unique(tmp$section), function(pt) {
      tmp2 <- tmp %>% filter(section == pt)
      if(remove_outlier){
        tmp2 <- tmp2 %>% filter(!(T %in% (boxplot(tmp2$T) %>% pluck("out"))))
      }
      model <- tmp2 %>% lm(T ~ real_onset, data = .) 
      model_coef <- coef(model)
      total_drift <- end_to_start_diff(predict(model))
      bind_cols(tmp2 %>% distinct(section, condition, day, piece, repetition), 
                tibble(dt =  model_coef[2], 
                       model_tempo = model_coef[1],     
                       mean_tempo = mean(tmp2$T),
                       total_drift = total_drift,
                       remove_outlier = remove_outlier))
    })
    
  }) 
}

get_tukeys_lmer <- function(stats, dv = "LMAPE", fixef = "condition", ranef = c("piece", "day", "headset")){
  tmp <- stats %>% 
    group_by_at(ranef) %>% 
    mutate(res = scale(!!sym(dv)) %>% as.numeric()) %>% 
    distinct_at(c(fixef, ranef, "res"))
  tmp <- tmp %>% 
    lm(as.formula(sprintf("res  ~ %s", fixef)), data = .) %>% 
    aov() %>% 
    TukeyHSD() %>% 
    broom::tidy() %>% mutate(p_var_str = sig_stars(adj.p.value))
  tmp
}

get_all_tukeys <- function(pitch_stats, pitch_stats_inner, onset_stats, onset_stats_inner){
  bind_rows(
    get_tukeys_lmer(pitch_stats, dv = "LMAPE", ranef = c("piece", "day", "headset")) %>% 
      mutate(type = "intonation_model_LMAPE"),
    # get_tukeys_lmer(pitch_stats, dv = "MAPE", ranef = c("piece", "day", "headset")) %>% 
    #   mutate(type = "intonation_model_MAPE"),
    get_tukeys_lmer(pitch_stats, dv = "LMPP", ranef = c("piece", "day", "headset")) %>% 
      mutate(type = "intonation_model_LMPP"),
    get_tukeys_lmer(pitch_stats_inner, dv = "LMAPE", ranef = c("piece", "day", "voice_type")) %>% 
      mutate(type = "intonation_model_inner_LMAPE"),
    get_tukeys_lmer(pitch_stats_inner, dv = "LMPP", ranef = c("piece", "day", "voice_type")) %>% 
      mutate(type = "intonation_model_inner_LMPP"),
    get_tukeys_lmer(onset_stats, dv = "LMOP", ranef = c("piece", "day")) %>% 
      mutate(type = "onset_model_LMOP"),
    get_tukeys_lmer(onset_stats_inner, dv = "LMOP", ranef = c("piece", "day", "voice_type")) %>% 
      mutate(type = "onset_model_inner_LMOP"))
    
}
read_take_audio_anton <- function(audio_dir = "C:/Users/dassc/desktop/MPIAE/Cut Circle Project", 
                                  day = 1L,
                                  take = "5b", headset = 1L){
  take_dir <- file.path(audio_dir, sprintf("Day %s/Take %s", day, take))
  full  <- tuneR::readWave(file.path(take_dir, 
                                     sprintf("%02d_H-Set%s.wav", headset, headset)))
  
  file_list <- list.files(sprintf("%s/hs%d", take_dir, headset), pattern = "*.wav", full.names = T)
  snippets <- map(file_list, function(fn) tuneR::readWave(fn))
  names(snippets) <-  file_list %>% basename() %>% tools::file_path_sans_ext()
  list(full = full, snippets = snippets)
}

get_mean_tukeys <- function(all_tukeys){
  all_tukeys %>% 
    group_by(contrast) %>% 
    summarise(mean_d = mean(estimate),
              BF05 = mean(adj.p.value < .05)/(.05*n()),
              BF01 = mean(adj.p.value < .01)/(.01*n()),
              BF001 = mean(adj.p.value < .001)/(.001*n())) %>% 
    arrange(desc(abs(mean_d)))  
}
