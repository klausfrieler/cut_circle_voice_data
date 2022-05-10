pitch_ridge_plot <- function(data, bw = .2){
  q <- data %>% ggplot(aes(x = (pitch - offset), y = id, fill = id)) 
  q <- q + ggridges::geom_density_ridges(bandwidth = bw)
  q <- q + theme_minimal()
  q <- q + scale_fill_viridis_d()
  q <- q + guides(fill = guide_none())
  q <- q + scale_x_continuous(breaks = seq(53, 72, 1), labels = parkR:::pc_labels_flat[(seq(53, 72, 1) %%12) + 1])
  q
  
}
