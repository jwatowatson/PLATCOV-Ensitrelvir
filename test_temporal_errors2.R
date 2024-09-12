ind_res <-1

n_id <- stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$n_id
ind_start <- stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$ind_start

post_beta_hat <- rstan::extract(out, "beta_hat")[[1]]
post_Beta_hat <- rstan::extract(out, "Beta_hat")[[1]]
post_Beta_hat_trt <- rstan::extract(out, "Beta_hat_trt")[[1]]
post_slope <- rstan::extract(out, "slope")[[1]]


post_beta_hat_summarize <- apply(post_beta_hat, 2, quantile, c(0.025, 0.5, 0.975))
post_Beta_hat_summarize <- apply(post_Beta_hat, 2, quantile, c(0.025, 0.5, 0.975))
post_Beta_hat_trt_summarize <- apply(post_Beta_hat_trt, 2, quantile, c(0.025, 0.5, 0.975))
post_slope_summarize <- apply(post_slope, 2, quantile, c(0.025, 0.5, 0.975))

data_for_plot_slope <-  platcov_dat_analysis_list[[model_settings$dataset[ind_res]]][ind_start,]

data_for_plot_slope$slope_low <- post_slope_summarize[1,]
data_for_plot_slope$slope_med <- post_slope_summarize[2,]
data_for_plot_slope$slope_up <- post_slope_summarize[3,]

data_for_plot_slope$slope_trt_low <- post_Beta_hat_trt_summarize[1,]
data_for_plot_slope$slope_trt_med <- post_Beta_hat_trt_summarize[2,]
data_for_plot_slope$slope_trt_up <- post_Beta_hat_trt_summarize[3,]

data_for_plot_slope$beta_hat <- post_Beta_hat_summarize[2,]
data_for_plot_slope$beta_hat_low <- post_Beta_hat_summarize[1,]
data_for_plot_slope$beta_hat_up <- post_Beta_hat_summarize[3,]
#########################################################################
if(model_settings$Dmax[ind_res] == 5.5){
  lab <- expression(bold(paste("Viral clearance rates, ",alpha["0-5"]," (log"["10"]," genomes mL"^-1, " day"^-1,")")));
  max <- "5d"
} else {
  lab <- expression(bold(paste("Viral clearance rates, ",alpha["0-7"]," (log"["10"]," genomes mL"^-1, " day"^-1,")")));
  max <- "7d"
}

Sp_all <- ggplot(data_for_plot_slope, aes(x = Rand_date, y = slope_med)) +
  geom_point(size = 3, alpha = 0.5, aes(col = Trt)
  ) +
  theme_bw() +
  # geom_errorbar(aes(ymin = slope_low, ymax = slope_up), width = 0,
  #               alpha = 0.5, col = "grey", linewidth = 0.25) +
  geom_point(aes(x = Rand_date, y = beta_hat), 
             #linewidth = 1.75, 
             alpha = 0.7, col = "red") +
  xlab("Randomisation date") +
  ylab(lab) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        panel.spacing = unit(1, "lines")) +
  coord_cartesian(ylim=c(-3,0))

Sp_all
