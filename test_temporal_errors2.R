library(ggplot2)
library(dplyr)
my_probs = c(0.025, 0.1, .5, .9, .975)

load('Rout/model_run_setup_Unblinded_all.RData')

ff = list.files('Rout/', pattern = paste0('model_fits_temporal_spline'),)
ff = ff[grep(pattern = 'model_fits_',x = ff, ignore.case = T)]
if(!length(ff)==nrow(model_settings)) stop('not all outputs are ready for all model settings')
ff = paste0('Rout/',ff)

ID_map <- stan_inputs[[1]]$ID_map

trts = levels(as.factor(platcov_dat_analysis_list[[1]]$Trt))
trts = trts[-1]
nrow(platcov_dat_analysis_list[[1]])

load(ff)

ind_res <-1

n_id <- stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$n_id
ind_start <- stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$ind_start

post_beta_hat <- rstan::extract(out, "beta_hat")[[1]]
post_Beta_hat <- rstan::extract(out, "Beta_hat")[[1]]
#post_Beta_hat_trt <- rstan::extract(out, "Beta_hat_trt")[[1]]
post_slope <- rstan::extract(out, "slope")[[1]]
post_Trt_slope <- rstan::extract(out, "Trt_slope")[[1]]

post_beta_hat_summarize <- apply(post_beta_hat, 2, quantile, c(0.025, 0.5, 0.975))
post_Beta_hat_summarize <- apply(post_Beta_hat, 2, quantile, c(0.025, 0.5, 0.975))
#post_Beta_hat_trt_summarize <- apply(post_Beta_hat_trt, 2, quantile, c(0.025, 0.5, 0.975))
post_slope_summarize <- apply(post_slope, 2, quantile, c(0.025, 0.5, 0.975))

#data_for_plot_slope <- data_for_plot_slope[ID_map$ID_stan,]

# data_for_plot_slope$slope_low <- post_slope_summarize[1,]
# data_for_plot_slope$slope_med <- post_slope_summarize[2,]
# data_for_plot_slope$slope_up <- post_slope_summarize[3,]
# 
# 
# data_for_plot_slope$beta_hat <- post_Beta_hat_summarize[2,]
# data_for_plot_slope$beta_hat_low <- post_Beta_hat_summarize[1,]
# data_for_plot_slope$beta_hat_up <- post_Beta_hat_summarize[3,]


data_for_plot_slope <-  platcov_dat_analysis_list[[model_settings$dataset[ind_res]]][ind_start,]
t12_output = data.frame(slope_med = apply(post_slope,2,median),
                        Beta_hat = apply(post_Beta_hat,2,median),
                        beta_hat = apply(post_beta_hat,2,median),
                        Trt_slope = apply(post_Trt_slope,2,median),
                        ID_stan = stan_inputs[[model_settings$dataset[ind_res]]]$analysis_data_stan$id[ind_start],
                        i = 1:nrow(ID_map),
                        ID_map_key = ID_map$ID_key,
                        ID_map_i = ID_map$ID_stan)

t12_output = merge(t12_output, ID_map, by.x = 'ID_stan', by.y = "ID_stan")
data_for_plot_slope = merge(data_for_plot_slope, t12_output, by.x = 'ID', by.y = 'ID_key')
#########################################################################
if(model_settings$Dmax[ind_res] == 5.5){
  lab <- expression(bold(paste("Viral clearance rates, ",alpha["0-5"]," (log"["10"]," genomes mL"^-1, " day"^-1,")")));
  max <- "5d"
} else {
  lab <- expression(bold(paste("Viral clearance rates, ",alpha["0-7"]," (log"["10"]," genomes mL"^-1, " day"^-1,")")));
  max <- "7d"
}

Sp_all <- ggplot(data_for_plot_slope, aes(x = Rand_date, y = slope_med)) +
  geom_point(size = 3, alpha = 0.5, aes(col = Trt)) +
 # geom_text(aes(label = i)) +
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

#########################################################################
# post_Trt_slope <- rstan::extract(out, "Trt_slope")[[1]]
# post_post_Trt_slope_summarize <- apply(post_Trt_slope, 2, quantile, c(0.025, 0.5, 0.975))
# data_for_plot_slope$Trt_slope <- post_post_Trt_slope_summarize[2,]

ggplot(data_for_plot_slope, aes(x = Trt, y = Trt_slope)) +
  geom_point()
#########################################################################
post_preds_summarize <- rstan::extract(out, "preds")[[1]] %>% apply(2, quantile, c(0.025, 0.5, 0.975))
plat_dat <- platcov_dat_analysis_list[[1]]

plat_dat$preds <- post_preds_summarize[2,]

#########################################################################
data_for_plot_slope %>%
  filter(slope_med < -2.5) %>%
  pull(ID)


plot_list <- list()

for(i in 1:nrow(ID_map)){
  IDD <- ID_map$ID_key[i]
  plot_dat <- plat_dat %>% filter(ID == IDD)
  labs <- paste0(plot_dat$ID[1], "\n", plot_dat$Trt[1])
  
  plot_list[[i]] <- ggplot(plat_dat %>% filter(ID == IDD), aes(x = Time)) +
    geom_point(aes(y = log10_viral_load)) +
    geom_line(aes(y = preds), col = "red") +
    geom_point(aes(y = preds), col = "red", size = 3) +
    ggtitle(labs) +
    geom_text(data = data_for_plot_slope %>% filter(ID == IDD),  mapping   = aes(x = 4, y = 6,label = round(slope_med, 1))) +
    ylim(0,9) +
    theme_bw() +
    xlab("") +
    ylab("")
  
  
}
#########################################################################
library(ggpubr)
library(grid)
ind_plot_all <- ggarrange(plotlist =  plot_list, nrow = 4, ncol = 4, common.legend = T, legend = "none")


for(i in 1:length(ind_plot_all)){
  fname <- paste0("Plots/Individual_plot_meta/", intervention, "_analysis_individual_", i, ".png")
  
  png(fname, width = 8, height = 8, units = "in", res = 300)
  print(annotate_figure( ind_plot_all[[i]], bottom = textGrob("Time since randomisation (days)", vjust = 0.5, gp = gpar(cex = 1.2, fontface="bold")),
                         left = textGrob("Viral densities (genomes/mL)", rot = 90, gp = gpar(cex = 1.2, fontface="bold"))))
  dev.off()
  
}





