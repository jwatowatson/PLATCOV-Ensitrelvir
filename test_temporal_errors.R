library(rstan)
library(ggplot2)
library(dplyr)
library(scales)
load('Rout/model_run_setup_Unblinded_all.RData')

ID_map <- stan_inputs[[1]]$ID_map

trts = levels(as.factor(platcov_dat_analysis_list[[1]]$Trt))
trts = trts[-1]
nrow(platcov_dat_analysis_list[[1]])

ind_res <- 1
model_settings[ind_res,]
list.files('Rout_batch/Rout/', pattern = paste0('.RData'),)

ff <- paste0('Rout_batch/Rout/', 'model_fits_temporal_spline_', ind_res, '.RData')
my_probs <- c(0.025,0.10,0.50, 0.90, 0.975)

effect_ests=list()
for(i in 1:length(ff)){
  load(ff[i])
  effect_ests[[i]] = 
    summary(out, pars='trt_effect',use_cache=F,probs=my_probs)$summary[,c('2.5%','10%','50%','90%','97.5%'),drop=F]
  rownames(effect_ests[[i]]) = trts
  
  print(traceplot(out))
}


exp(effect_ests[[1]])

n_id <- stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$n_id
ind_start <- stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$ind_start

post_beta_hat <- rstan::extract(out, "beta_hat")[[1]]
post_trt_slope <- rstan::extract(out, "trt_slope")[[1]]
post_trt_theta_rand_id <- rstan::extract(out, "theta_rand_id")[[1]]
post_trt_beta_cov <- rstan::extract(out, "beta_cov")[[1]]

slope_ALL <- matrix(NA, ncol = n_id, nrow = 2000)
slope_trt <- matrix(NA, ncol = n_id, nrow = 2000)


for (j in 1:n_id) {
    slope_ALL[, j]  <-
      post_beta_hat[, j] * exp(post_trt_slope[, ind_start[j]] +  post_trt_theta_rand_id[, j, 2] +  post_trt_beta_cov[, ind_start[j]])
    slope_trt[, j] <-
      post_beta_hat[,j] * exp(post_trt_slope[, ind_start[j]])
}
  
slope_summarize <- apply(slope_ALL, 2, quantile, c(0.025, 0.5, 0.975))
slope_trt_summarize <- apply(slope_trt, 2, quantile, c(0.025, 0.5, 0.975))
post_beta_hat_summarize <- apply(post_beta_hat, 2, quantile, c(0.025, 0.5, 0.975))


slope_trt_for_summarize <- list("data" = platcov_dat_analysis_list[[model_settings$dataset[ind_res]]][ind_start,], "slope" = t(slope_trt))

data_for_plot_slope <- platcov_dat_analysis_list[[model_settings$dataset[ind_res]]][ind_start,]
data_for_plot_slope$slope_low <- slope_summarize[1,]
data_for_plot_slope$slope_med <- slope_summarize[2,]
data_for_plot_slope$slope_up <- slope_summarize[3,]

data_for_plot_slope$slope_trt_low <- slope_trt_summarize[1,]
data_for_plot_slope$slope_trt_med <- slope_trt_summarize[2,]
data_for_plot_slope$slope_trt_up <- slope_trt_summarize[3,]

data_for_plot_slope$beta_hat <- post_beta_hat_summarize[2,]
data_for_plot_slope$beta_hat_low <- post_beta_hat_summarize[1,]
data_for_plot_slope$beta_hat_up <- post_beta_hat_summarize[3,]


#data_for_plot_slope <- data_for_plot_slope[data_for_plot_slope$beta_hat != max(data_for_plot_slope$beta_hat),] 

#########################################################################
if(model_settings$Dmax[ind_res] == 5.5){
  lab <- expression(bold(paste("Viral clearance rates, ",alpha["0-5"]," (log"["10"]," genomes mL"^-1, " day"^-1,")")));
  max <- "5d"
} else {
  lab <- expression(bold(paste("Viral clearance rates, ",alpha["0-7"]," (log"["10"]," genomes mL"^-1, " day"^-1,")")));
  max <- "7d"
}


Sp_all <- ggplot(data_for_plot_slope %>% filter(!Trt %in% c("Evusheld", "Regeneron")), aes(x = Rand_date, y = slope_med)) +
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
