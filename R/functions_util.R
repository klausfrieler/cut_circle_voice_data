messagef <- function(...) message(sprintf(...))

midi_to_hz <-function(midi){
  2^((midi - 69)/12)*440  
}

hz_to_midi <- function(hz){
  12 * log2(hz/440)  + 69
}

hz_to_cents <- function(hz, base = 69){
  1200 * log2(hz/midi_to_hz(base))
}

integer_deviation <- function(x){
  mean(abs(round(x) - x))
}  

find_global_tuning_offset <- function(pitch_vector, epsilon = .1){
  step <- .1
  search_seq <- seq(-.5, .5,  step)  
  dev <- Inf
  new_step <- step  
  offset <- Inf
  browser()
  while(dev > epsilon && new_step > 1e-6){
    messagef("Running search with step = %f, seq len = %d, current dev = %f, current offset = %f.", new_step, length(search_seq), dev, offset)
    tmp <- 
      map_dfr(search_seq, function(offset){
      tibble(offset = offset, dev = integer_deviation(pitch_vector - offset))
    }) %>% 
      arrange(dev) %>% 
      slice(1) 
    offset <- tmp  %>% pull(offset)
    dev <- tmp %>% pull(dev)
    search_seq <- seq(offset - new_step/2, offset + new_step/2, new_step/10)
    new_step <- new_step/10
    
  }
  offset
}