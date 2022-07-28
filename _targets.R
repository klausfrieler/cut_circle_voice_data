library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("hms", "tidyverse"))

source("R/functions_util.R")
source("R/functions_data.R")
source("R/functions_plot.R")

#saveRDS(list_csv_files("data/note_tracks"), "data/note_track_idx.rds")

list(
  # Read metadata and do some preparation
  tar_target(data_dir, "data/note_tracks"),
  tar_target(metadata_dir, "data/metadata"),
  tar_target(score_dir, "data/note_data_csv"),
  tar_target(track_info_path, file.path(metadata_dir, "track_coding.xlsx"), format ="file"),
  tar_target(track_info, read_track_info(track_info_path)),
  tar_target(piece_info, read_piece_info(track_info_path)),

  # Read, annotate and preprocess actual data
  tar_target(scores, 
             read_score_files(score_dir)),
  
  tar_target(offsets, 
             readRDS(file.path(metadata_dir, "all_offsets.rds"))),
  
  tar_target(note_tracks, 
             read_note_tracks(data_dir)),
  
  tar_target(note_tracks_annotated, 
             annotate_note_tracks(note_tracks, scores, track_info, offsets)),
  
  tar_target(clean_note_tracks, 
             remove_all_linear_trends(note_tracks_annotated, max_error = 1)),
  
  # PITCH: get performance indicators
  tar_target(pitch_stats, 
             get_pitch_stats(clean_note_tracks)),

  tar_target(pitch_stats_inner, 
             get_pitch_stats_inner_voice(clean_note_tracks)),
  
  tar_target(main_effects_pitch, 
             check_main_effects(pitch_stats, pitch_stats_inner)),
  
  # PITCH: plots for performance indicators
  tar_target(main_effects_LMAPE, 
             plot_main_effect(pitch_stats)),
  
  tar_target(main_effects_LMPP, 
             plot_main_effect(pitch_stats, dv = "LMPP")),
  
  # PITCH: Regression models  
  tar_target(intonation_model_single, 
             get_intonation_model_single(pitch_stats)),
  
  tar_target(intonation_model_inner, 
             get_intonation_model_inner_voice(pitch_stats_inner)),

  # TIMING: performance indicators  
  tar_target(onset_stats, 
             get_onset_stats(clean_note_tracks)),
  
  tar_target(onset_stats_inner, 
             get_onset_stats_inner_voice(clean_note_tracks)),
  
  tar_target(main_effects_onset, 
             check_main_effects(onset_stats, onset_stats_inner, type = "onset")),

  # TIMING: performance indicators plots  
  tar_target(main_effects_LMOP, 
             plot_main_effect(onset_stats, dv = "LMOP", vars = c("day", "condition", "piece"))),
  
  tar_target(main_effects_LMOP_inner, 
             plot_main_effect(onset_stats_inner, dv = "LMOP", vars = c("day", "condition", "piece", "voice_type"))),
  
  # TIMING: regression models  
  tar_target(onset_model_sync, 
             get_onset_model_sync(onset_stats)),
  
  tar_target(onset_model_inner, 
             get_onset_model_inner_voice(onset_stats_inner)),
  
  # PITCH & TIMING ERRORS: logistic regressions   
  tar_target(singing_error_model, get_singing_errors(note_tracks, note_tracks_annotated)),
  tar_target(onset_error_model_sync, get_onset_stats(clean_note_tracks, only_error = T)),
  tar_target(onset_error_model_inner, get_onset_stats_inner_voice(clean_note_tracks, only_error = T)),
  
  tar_target(main_effects_error, 
             plot_error_rates(note_tracks, note_tracks_annotated)),
  
  # PITCH: Drift
  tar_target(drift_data, analyze_drift_and_tuning(clean_note_tracks)),
  tar_target(drift_model, get_drift_model(drift_data)),
  
  # TEMPO: Drift
  tar_target(tempo_analysis, analyze_tempo_and_drift(clean_note_tracks)),
  tar_target(tempo_model, get_tempo_model(tempo_analysis)),            
  
  # EFFECT SIZES for pitch and timing
  tar_target(all_tukeys, get_all_tukeys(pitch_stats, pitch_stats_inner, onset_stats, onset_stats_inner)),
  tar_target(mean_tukeys, get_mean_tukeys(all_tukeys))
)
