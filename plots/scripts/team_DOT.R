library(doMC)
registerDoMC()
getDoParWorkers() #This lists how many workers you have (hopefully more than 1!)

yr <- 2019
tic()

ddply(
  freq_passers, .variables = as.quoted(), .fun = function(x) {
    #do your plotting now 
    p_new <- ggplot(x, aes(y = tm_ADOT_order, x = air_yards)) +
      geom_image(data = team_df, aes(image = logo, x = 17.5, y = 2.8), size = 0.80) +
      geom_image(data = team_df, aes(image = logo, x = 17.5, y = 2.8), size = 0.80, color = 'white', alpha = 0.8) +
      geom_image(data = headshot_df, aes(image = pff_url, x = 38, y = tm_ADOT_order), size = 0.22 * img_size_adj, nudge_y = 0.5) +
      geom_density_ridges2(aes(y = tm_ADOT_order, fill = posteam, color = posteam), scale = 1.7, size = 0.2, na.rm = T, show.legend = F, bandwidth = 2, panel_scaling = F) +
      # geom_text(aes(label = full_name, x = 35), nudge_y = .75, hjust = 1, size = 0.8 * img_size_adj, color = 'darkblue') +
      # geom_text(data = team_df, aes(label = posteam, x = -1, y = 4.8), angle = 45, size = 1.5 * img_size_adj, color = 'darkblue') +
      # geom_text(aes(label = 0, x = 0, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
      # geom_text(aes(label = 10, x = 10, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
      # geom_text(aes(label = 20, x = 20, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
      # geom_text(aes(label = 30, x = 30, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
      geom_shadowtext(aes(label = full_name, x = 35), nudge_y = .75, hjust = 1, size = 0.8 * img_size_adj, color = 'darkblue', bg.color = 'white', bg.r = 0.15) +
      geom_shadowtext(data = team_df, aes(label = posteam, x = -1, y = 4.8), angle = 45, size = 1.5 * img_size_adj, color = 'darkblue', bg.color = 'white', bg.r = 0.1) +
      geom_shadowtext(aes(label = 0, x = 0, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
      geom_shadowtext(aes(label = 10, x = 10, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
      geom_shadowtext(aes(label = 20, x = 20, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
      geom_shadowtext(aes(label = 30, x = 30, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
      scale_color_manual(values = outline_col) + 
      scale_fill_manual(values = fill_col) + 
      facet_wrap(~ posteam, nrow = 8, shrink= F) +
      labs(title = 'Receiver Depth of Target Distribution', 
           subtitle = paste0(yr, " NFL Regular Season"), 
           x = 'Air Yards') +
      scale_x_continuous(limits = c(-5,40), expand = c(0,0)) +
      scale_y_discrete(expand = expansion(add = c(0.3, 1))) +
      theme_bw() +
      theme(
        text = element_text(color='darkblue'),
        plot.background = element_rect(fill = 'grey95', color = 'grey95'),
        panel.border = element_rect(color = 'darkblue'),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 5),
        legend.background = element_rect(fill = 'grey90',color = 'darkblue'),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0.1, "lines")
      )
    #save your plot
    ggsave(paste0("plots/desktop/", yr, "_team_ridge_DOT.png"), plot = p_new, width = 14, height = 25, dpi = 100, limitsize = FALSE)
  },
  .parallel = TRUE
)
toc()