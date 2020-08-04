#### Modelos

pred_ll <- read.csv2('resultado/pred_ll.csv', sep = ';')

pred_ll_m1 <- read.csv2('resultado/pred_ll_m1.csv', sep = ';')

mundo <- read.csv2('resultado/covid19gravidade.csv', sep = ';')

colnames(mundo) <- c('local', 'casos', 'novos_casos', 'obitos', 'novos_obitos', 'recuperados', 'ativos', 'criticos', 'casos_milhao', 'obitos_milhao', 'concluidos', 'testes', 'testes_milhao', 'txConc', 'txConcObitos', 'txCriticos', 'txPopObitos', 'data_extracao', 'lat', 'long')

mundo$obitos_milhao <- as.integer(as.character(mundo$obitos_milhao))

belgicaCasosMilhao <- mundo %>% filter(local == 'Belgium') %>% select(casos_milhao) %>% as.integer

belgicaMortesMilhao <- mundo %>% filter(local == 'Belgium') %>% select(obitos_milhao) %>% as.integer

brasilCasosMilhao <- mundo %>% filter(local == 'Brazil') %>% select(casos_milhao) %>% as.integer

brasilMortesMilhao <- mundo %>% filter(local == 'Brazil') %>% select(obitos_milhao) %>% as.integer

espanhaCasosMilhao <- mundo %>% filter(local == 'Spain') %>% select(casos_milhao) %>% as.integer

espanhaMortesMilhao <- mundo %>% filter(local == 'Spain') %>% select(obitos_milhao) %>% as.integer

####CORRELAÇÕES
iso_in <- read.csv('resultado/inlocoMob.csv', sep = ";")
colnames(iso_in) <- c('data', 'cidade', 'estado', 'media_cidade', 'iso_estado', 'iso_cidade', 'iso_inloco')

iso_ap <- read.csv('resultado/appleMob.csv', sep = ";")
iso_ap <- iso_ap[,-c(4,5)]
colnames(iso_ap) <- c('tipo', 'estado', 'tipo_transporte', 'pais', 'data', 'mob', 'iso_apple')

iso_go <- read.csv('resultado/googleMob.csv', sep = ";")
iso_go <- iso_go[,-c(4)]
colnames(iso_go) <- c('code_pais', 'pais', 'estado', 'data', 'iso_var_rec', 'iso_sup_farm','iso_park', 'iso_tran', 'iso_trab', 'iso_res', 'iso_google')

