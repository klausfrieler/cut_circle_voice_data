messagef <- function(...) message(sprintf(...))

midi_to_hz <-function(midi){
  2^((midi - 69)/12)*440  
}

hz_to_midi <- function(hz){
  if(!is.numeric(hz)){
    browser()
    hz <- str_extract("[0-9.-]+")
  }
  12 * log2(hz/440)  + 69
}

hz_to_cents <- function(hz, base = 69){
  1200 * log2(hz/midi_to_hz(base))
}

integer_deviation <- function(x){
  mean(abs(round(x) - x))
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
    #browser()
    form <- sprintf("%s ~ %s", v1, paste(iv, collapse = " + ")) %>% as.formula()
    map_dfr(1:size, function(n){
      #browser()
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
find_global_tuning_offset <- function(pitch_vector, epsilon = .1, verbose = F){
  step <- .1
  search_seq <- seq(-.5, .5,  step)  
  dev <- Inf
  new_step <- step  
  offset <- Inf
  #browser()
  while(dev > epsilon && new_step > 1e-6){
    if(verbose){
      messagef("Running search with step = %f, seq len = %d, current dev = %f, current offset = %f.", new_step, length(search_seq), dev, offset)
    }
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