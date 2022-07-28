library(tidyverse)
library(tuneR)
library(digest)
library(e1071)

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


find_snippet2 <- function(container, snippet, sr = 48000, dry_run = F){
  if(dry_run){
    return(tibble(offset = NA, offset_sec = NA))
  }
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
                             save_dir = "data/metadata", 
                             dry_run = F){
  map_dfr(days, function(n){
    day <- sprintf("day_%d", n)
    takes <- list.files(file.path(audio_dir, day)) %>% str_remove("take_") %>% as.integer()
    takes <- setdiff(takes, exclude_takes)
    map_dfr(takes, function(tk){
      pos <- 
        map_dfr(headsets, function(hs){
          messagef("*** Aligning: Day %d, Take %d, Headset %d ***", n, tk, hs)
          pos <- align_take_snippets2(audio_dir, day = n, take = tk, headset = hs, dry_run = dry_run) %>% 
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
                                dry_run = F){
  audios <- read_take_audios(audio_dir, day = day, take = take, headset = headset)
  take_pos <- map_dfr(names(audios$snippets), function(n){
    find_snippet2(audios$full, 
                  audios$snippets[[n]],
                  dry_run = dry_run) %>% 
      mutate(snippet = n)})
}

align_take_snippets2 <- function(audio_dir = "e:/projects/science/MPIEA/projects/cut_circle_voice_data/audios", 
                                 day = 1, 
                                 take = 5, 
                                 headset = 1, 
                                 dry_run = F){
  #browser()
  audios <- read_take_audios(audio_dir, day = day, take = take, headset = headset)
  take_pos <- map_dfr(names(audios$snippets), function(n){
    messagef("    Snippet: %s", n)
    find_snippet2(audios$full, 
                  audios$snippets[[n]], 
                  dry_run = dry_run) %>% 
      mutate(snippet = n)
  }) %>% arrange(offset)
  fft_cache <<- list()
  take_pos
}
