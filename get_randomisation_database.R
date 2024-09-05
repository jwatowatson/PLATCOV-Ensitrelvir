data.TH1 <- read.csv("~/Dropbox/PLATCOV/data-TH1.csv")
data.TH1$Date = as.POSIXct(data.TH1$Date,format='%a %b %d %H:%M:%S %Y')
data.TH1 = data.TH1[data.TH1$Date <= '2024-04-22',  ]

data.LA8 <- read.csv("~/Dropbox/PLATCOV/data-LA08.csv")
data.LA8$Date = as.POSIXct(data.LA8$Date,format='%a %b %d %H:%M:%S %Y')
data.LA8 =  data.LA8[data.LA8$Date <= '2024-04-22',  ]

data.BR3 <- read.csv("~/Dropbox/PLATCOV/data-BR3.csv")
data.BR3$Date = as.POSIXct(data.BR3$Date,format='%a %b %d %H:%M:%S %Y')
data.BR3 = data.BR3[data.BR3$Date <= '2024-04-22',  ]

data.PK1 <- read.csv("~/Dropbox/PLATCOV/data-PK01.csv")
data.PK1$Date = as.POSIXct(data.PK1$Date,format='%a %b %d %H:%M:%S %Y')
data.PK1 =  data.PK1[data.PK1$Date <= '2024-04-22',  ]

data.TH1$ID = paste('PLT-TH1-',data.TH1$randomizationID,sep='')
data.LA8$ID = paste('PLT-LA8-',data.LA8$randomizationID,sep='')
data.BR3$ID = paste('PLT-BR3-',data.BR3$randomizationID,sep='')
data.PK1$ID = paste('PLT-PK1-',data.PK1$randomizationID,sep='')

xx = rbind(data.TH1[, c('ID', 'Treatment')],
           data.LA8[, c('ID', 'Treatment')],
           data.BR3[, c('ID', 'Treatment')],
           data.PK1[, c('ID', 'Treatment')]
           )

xx <- xx %>% filter(!Treatment %in% c('Nirmatrelvir + Ritonavir + Molnupiravir',
                               'Hydroxychloroquine',
                               'Nitazoxanide'))

library(stringr)
for(i in 1:nrow(xx)){
  id = unlist(strsplit(xx$ID[i],split = '-'))
  id[3] = str_pad(id[3], 3, pad = "0")
  id = paste(id, collapse = '-')
  xx$ID[i]=id
}

table(xx$Treatment)

write.csv(x = xx, file = 'ITT_population_meta.csv')

