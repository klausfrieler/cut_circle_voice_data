pitch_ridge_plot <- function(data, bw = .2){
  q <- data %>% ggplot(aes(x = (pitch - offset), y = id, fill = id)) 
  q <- q + ggridges::geom_density_ridges(bandwidth = bw)
  q <- q + theme_minimal()
  q <- q + scale_fill_viridis_d()
  q <- q + guides(fill = guide_none())
  q <- q + scale_x_continuous(breaks = seq(53, 72, 1), labels = parkR:::pc_labels_flat[(seq(53, 72, 1) %%12) + 1])
  q
  
}

plot_main_effect <- function(pitch_stats, 
                             dv = "LMAPE", 
                             vars = c("day", "condition", "headset", "piece")){
  labels <- c("MAPE" = "Mean Absolute Pitch Error", 
              "MPP" = "Mean Pitch Standard Deviation",
              "LMAPE" = "Pitch Accuracy",
              "LMPP" = "Pitch Precision",
              "MOP" = "Mean Onset Standard Deviation",
              "LMOP" = "Mean Onset Precision")

  tmp <- pitch_stats %>% 
    select(all_of(c(dv, vars))) %>% 
    pivot_longer(-all_of(dv))
  q <- tmp %>% 
    ggplot(aes(x = value, 
               y = !!sym(dv), 
               fill = name)) 
  q <- q + geom_boxplot() 
  q <- q + geom_jitter(width =.2, alpha = .2) 
  q <- q + theme_minimal() 
  q <- q + facet_wrap(~name, scale = "free_y") 
  q <- q + coord_flip()  
  q <- q + theme(legend.position = "none") 
  q <- q  + labs(x = "", y = labels[dv])  
  q
}