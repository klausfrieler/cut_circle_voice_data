library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("hms", "tidyverse"))

source("R/functions_util.R")
source("R/functions_data.R")
source("R/functions_plot.R")

saveRDS(list_csv_files("data/note_tracks"), "data/note_track_idx.rds")

list(
  tar_target(data_dir, "data/note_tracks"),
  tar_target(data_index, "data/metadata/note_track_idx.rds", format = "file"),
  tar_target(score_dir, "data/note_data_csv"),
  tar_target(track_info_path, "data/metadata/track_coding.xlsx", format ="file"),
  tar_target(track_info, read_track_info(track_info_path)),
  tar_target(file_list, list_csv_files(data_dir)),
  tar_target(scores, 
             read_score_files(score_dir)
  ),
  tar_target(note_tracks, 
             read_note_tracks(data_dir)
             ),
  tar_target(data, annotate_note_tracks(note_tracks, scores, track_info)),
  tar_target(data_red, remove_all_linear_trends(data, max_error = 1)),
  tar_target(pitch_stats, get_pitch_stats(data_red)),
  tar_target(pitch_stats_inner, get_pitch_stats_inner_voice(data_red)),
  tar_target(intonation_model_single, get_intonation_model_single(pitch_stats)),
  tar_target(intonation_model_inner, get_intonation_model_inner_voice(pitch_stats_inner))
  
)
