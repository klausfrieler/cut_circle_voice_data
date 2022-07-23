library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("hms", "tidyverse"))

source("R/functions_util.R")
source("R/functions_data.R")
source("R/functions_plot.R")

#saveRDS(list_csv_files("data/note_tracks"), "data/note_track_idx.rds")

list(
  tar_target(data_dir, "data/note_tracks"),
  tar_target(metadata_dir, "data/metadata"),
  #tar_target(data_index, "data/metadata/note_track_idx.rds", format = "file"),
  tar_target(score_dir, "data/note_data_csv"),
  tar_target(track_info_path, file.path(metadata_dir, "track_coding.xlsx"), format ="file"),
  tar_target(track_info, read_track_info(track_info_path)),
  #tar_target(file_list, list_csv_files(data_dir)),
  tar_target(scores, 
             read_score_files(score_dir)
  ),
  tar_target(offsets, 
             readRDS(file.path(metadata_dir, "all_offsets.rds"))
             ),
  tar_target(note_tracks, 
             read_note_tracks(data_dir)
             ),
  tar_target(note_tracks_annotated, annotate_note_tracks(note_tracks, scores, track_info, offsets)),
  
  tar_target(clean_note_tracks, remove_all_linear_trends(note_tracks_annotated, max_error = 1)),
  tar_target(pitch_stats, get_pitch_stats(clean_note_tracks)),
  tar_target(pitch_stats_inner, get_pitch_stats_inner_voice(clean_note_tracks)),
  tar_target(main_effects, check_main_effects(pitch_stats, pitch_stats_inner)),
  tar_target(main_effects_LMAPE, plot_main_effect(pitch_stats)),
  tar_target(main_effects_LMPP, plot_main_effect(pitch_stats, dv = "LMPP")),
  tar_target(main_effects_error, plot_error_rates(note_tracks, note_tracks_annotated)),
  tar_target(intonation_model_single, get_intonation_model_single(pitch_stats)),
  tar_target(intonation_model_inner, get_intonation_model_inner_voice(pitch_stats_inner)),
  tar_target(singing_error_model, get_singing_errors(note_tracks, note_tracks_annotated))
  
)
