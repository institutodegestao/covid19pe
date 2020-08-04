library(dplyr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(shiny)
library(zoo)
library(lubridate)
library(DT)

###################### PREDIÇÃO LEITOS #########################

base_predicao_leitos <- read.csv2('resultado/base_predicao_leitos.csv', sep = ',')

base_predicao_leitos$data <- base::as.Date(base_predicao_leitos$data, format = "%Y-%m-%d")

modelo4 <- readRDS("resultado/modelo4.rda")

modelo4_importancia <- data.frame('peso' = round(summary(modelo4)[["coefficients"]][, "Estimate"][-1], 3), 'importancia' = round(summary(modelo4)[["coefficients"]][, "t value"][-1],3))

#rownames(modelo4_importancia) <- c('tendencia', 'obitos_7dias', 'int_uti_7dias', 'leitos_uti_oc_7dias', 'casos_srag_7dias')

modelo4_importancia$importancia_relativa <- round(modelo4_importancia$importancia / sum(modelo4_importancia$importancia),3)

######################## PERNAMBUCO ############################

######################## QUADRO SINTETICO ######################
rep_bas <- read.csv2('resultado/r0.csv', sep = ';', dec = '.')

quadro_sint <- read.csv2('resultado/evolucao_geral.csv', sep = ';')

quadro_sint <- quadro_sint %>% replace(is.na(.), 0)

quadro_sint$data <- base::as.Date(quadro_sint$dt_referencia, format = "%d/%m/%Y")

quadro_sint <- quadro_sint %>%
  mutate(testesMM7 = round(rollmean(x = testes_novos, 7, align = "right", fill = NA),2))

quadro_sint <- quadro_sint %>% replace(is.na(.), 0)

######################## CASOS + SRAG ######################
srag_municipio <- read.csv2('resultado/srag_municipio.csv', sep = ';')

srag_municipio$data <- base::as.Date(srag_municipio$data, format = "%Y-%m-%d")

srag_municipio$SRAG <- srag_municipio$DESCARTADO + srag_municipio$CONFIRMADO

srag_pernambuco <- srag_municipio %>% group_by(data = data) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

srag_pernambuco$semana <- epiweek(srag_pernambuco$data)

max_srag <- max(srag_pernambuco$data)

srag_pernambuco <- srag_pernambuco %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

srag_pernambuco <- srag_pernambuco %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

srag_pernambuco <- srag_pernambuco %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

srag_pernambuco <- srag_pernambuco %>% replace(is.na(.), 0)

######################## OBITOS ######################
obitos_municipio <- read.csv2('resultado/obitos_municipio.csv', sep = ';')

obitos_municipio$data <- base::as.Date(obitos_municipio$data, format = "%Y-%m-%d")

obitos_municipio$SRAG <- obitos_municipio$DESCARTADO + obitos_municipio$CONFIRMADO

obitos_pernambuco <- obitos_municipio %>% group_by(data = data) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

obitos_pernambuco$semana <- epiweek(obitos_pernambuco$data)

max_obitos <- max(obitos_pernambuco$data)

obitos_pernambuco <- obitos_pernambuco %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

obitos_pernambuco <- obitos_pernambuco %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

obitos_pernambuco <- obitos_pernambuco %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

obitos_pernambuco <- obitos_pernambuco %>% replace(is.na(.), 0)

######################## SOLICITACOES ######################
solicitacoes_municipios <- read.csv2('resultado/solicitacoes_municipios.csv', sep = ';')

solicitacoes_municipios$data <- base::as.Date(solicitacoes_municipios$data, format = "%Y-%m-%d")

solicitacoes_municipios$total <- solicitacoes_municipios$ENFERMARIA + solicitacoes_municipios$UTI

solicitacoes_pernambuco <- solicitacoes_municipios %>% group_by(data = data) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

solicitacoes_pernambuco$semana <- epiweek(solicitacoes_pernambuco$data)

max_solicitacoes <- max(solicitacoes_pernambuco$data)

solicitacoes_pernambuco <- solicitacoes_pernambuco %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

solicitacoes_pernambuco <- solicitacoes_pernambuco %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

solicitacoes_pernambuco <- solicitacoes_pernambuco %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

solicitacoes_pernambuco <- solicitacoes_pernambuco %>% replace(is.na(.), 0)

######################## FILA TOTAL ######################

fila_total_municipios <- read.csv2('resultado/fila_total_municipios.csv', sep = ';')

fila_total_municipios$data <- as.Date(fila_total_municipios$data, format = "%Y-%m-%d")

fila_total_municipios$total <- fila_total_municipios$UTI + fila_total_municipios$ENFERMARIA

fila_total_pernambuco <- fila_total_municipios %>% group_by(data = data) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_total_pernambuco$semana <- epiweek(fila_total_pernambuco$data)

max_fila_total <- max(fila_total_pernambuco$data)

fila_total_pernambuco <- fila_total_pernambuco %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_total_pernambuco <- fila_total_pernambuco %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_total_pernambuco <- fila_total_pernambuco %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_total_pernambuco <- fila_total_pernambuco %>% replace(is.na(.), 0)

######################## FILA DIA ######################

fila_nova_municipios <- read.csv2('resultado/fila_nova_municipios.csv', sep = ';')

fila_nova_municipios$data <- as.Date(fila_nova_municipios$data, format = "%Y-%m-%d")

fila_nova_municipios$total <- fila_nova_municipios$UTI + fila_nova_municipios$ENFERMARIA

fila_nova_pernambuco <- fila_nova_municipios %>% group_by(data = data) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_nova_pernambuco$semana <- epiweek(fila_nova_pernambuco$data)

max_fila_nova <- max(fila_nova_pernambuco$data)

fila_nova_pernambuco <- fila_nova_pernambuco %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_nova_pernambuco <- fila_nova_pernambuco %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_nova_pernambuco <- fila_nova_pernambuco %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_nova_pernambuco <- fila_nova_pernambuco %>% replace(is.na(.), 0)

######################## Internacoes ######################

internacoes_pernambuco <- read.csv2('resultado/internacoes_pernambuco.csv', sep = ';')

internacoes_pernambuco$data <- as.Date(internacoes_pernambuco$data, format = "%Y-%m-%d")

internacoes_pernambuco <- internacoes_pernambuco %>%
  mutate(intUTIMM7 = round(rollmeanr(x = UTI, 7, align = "right", fill = NA),2))

internacoes_pernambuco <- internacoes_pernambuco %>%
  mutate(intENFMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

internacoes_pernambuco <- internacoes_pernambuco %>%
  mutate(intGLMM7 = round(rollmean(x = geral, 7, align = "right", fill = NA),2))

internacoes_pernambuco <- internacoes_pernambuco %>% replace(is.na(.), 0)

######################## Leitos ######################

leitos_pernambuco <- read.csv2('resultado/leitos_pernambuco.csv', sep = ';')

leitos_pernambuco$data <- as.Date(leitos_pernambuco$data, format = "%Y-%m-%d")

leitos_pernambuco$UTI_TX <- round(1- leitos_pernambuco$UTI_LIVRE / leitos_pernambuco$UTI_DSP,2)

leitos_pernambuco$ENFERMARIA_TX <- round(1- leitos_pernambuco$ENFERMARIA_LIVRE / leitos_pernambuco$ENFERMARIA_DSP,2)

leitos_pernambuco$GERAL_TX <- round(1- leitos_pernambuco$GERAL_LIVRE / leitos_pernambuco$GERAL_DSP,2)

leitos_pernambuco <- leitos_pernambuco %>% replace(is.na(.), 0)

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(txUTIMM7 = round(rollmean(x = UTI_TX, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(txENFMM7 = round(rollmean(x = ENFERMARIA_TX, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(txTOTALMM7 = round(rollmean(x = GERAL_TX, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(dspUTIMM7 = round(rollmean(x = UTI_DSP, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(dspENFMM7 = round(rollmean(x = ENFERMARIA_DSP, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(dspTOTALMM7 = round(rollmean(x = GERAL_DSP, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(totalUTIMM7 = round(rollmean(x = UTI_TOTAL, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(totalENFMM7 = round(rollmean(x = ENFERMARIA_TOTAL, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>%
  mutate(totalGERALMM7 = round(rollmean(x = GERAL_TOTAL, 7, align = "right", fill = NA),2))

leitos_pernambuco <- leitos_pernambuco %>% replace(is.na(.), 0)