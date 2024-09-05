sympt_dat
sympt_dat$Trt[sympt_dat$Trt=='Nirmatrelvir + Ritonavir']='Nirmatrelvir'


col_yn <- colnames(sympt_dat)[grepl('^sq.*yn$', colnames(sympt_dat))]
col_20_sym <- col_yn[-c(1,22)]

sympt_dat_20 <- sympt_dat[,col_20_sym]
sympt_dat_20[is.na(sympt_dat_20)] <- 0

freq_symp20 <- apply(sympt_dat_20,2,sum)
all_freq <- sum(freq_symp20)
wieght_symp20 <- freq_symp20/all_freq

score_calculation <- function(row){
  return(row * wieght_symp20)
}

a <- apply(sympt_dat_20, 2, score_calculation)

sum(sympt_dat_20[1,] * wieght_symp20)

sympt_dat$symp20_score <-  apply(sympt_dat_20, 1, function(row) sum(row * wieght_symp20))
sympt_dat

sympt_dat_med <- sympt_dat %>%
  group_by(Timepoint_ID, Trt) %>%
  summarise(med = median(symp20_score),
            Q1 = quantile(symp20_score, 0.25),
            Q3 = quantile(symp20_score, 0.75))


ggplot(sympt_dat, aes(x = Timepoint_ID, y = symp20_score)) +
  geom_jitter(alpha = 0.25, width = 0.1, aes(col = Trt), shape = 21) +
  geom_line(data =  sympt_dat_med, aes(x = Timepoint_ID, y = med, col = Trt), size = 1) +
  geom_point(data =  sympt_dat_med, aes(x = Timepoint_ID, y = med, fill = Trt), size = 4, shape = 24) +
  scale_color_manual(values = trt_colors[unique(sympt_dat$Trt)], name = "") +
  scale_fill_manual(values = trt_colors[unique(sympt_dat$Trt)], name = "") +
  ylim(0,1) +
  xlim(-0.5,15) +
  theme_bw(base_size = 14) +
  ylab("Composite symptom scores [0 to 1]") +
  xlab("Time since randomisation (days)")



ggplot(sympt_dat, aes(x = Timepoint_ID)) +
  geom_jitter(alpha = 0.1, width = 0.1, aes(y = symp20_score),  shape = 21) +
  geom_line(data =  sympt_dat_med, aes(x = Timepoint_ID, y = med, col = Trt), size = 1) +
  geom_errorbar(data =  sympt_dat_med, aes(x = Timepoint_ID, ymin = Q1, ymax = Q3, col = Trt), width = 0) +
  geom_point(data =  sympt_dat_med, aes(x = Timepoint_ID, y = med, fill = Trt), size = 4, shape = 24) +
  scale_color_manual(values = trt_colors[unique(sympt_dat$Trt)], name = "") +
  scale_fill_manual(values = trt_colors[unique(sympt_dat$Trt)], name = "") +
  ylim(0,1) +
  xlim(-0.5,15) +
  theme_bw(base_size = 14) +
  ylab("Composite symptom scores [0 to 1]") +
  xlab("Time since randomisation (days)") +
  facet_grid(.~Trt)
