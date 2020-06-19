library(dplyr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(shiny)
library(zoo)

########## GERES #############
########## CASOS GERES #############

srag_geres <- srag_municipio %>% group_by(data = data, geres = geres) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

srag_geres$semana <- epiweek(srag_geres$data)

srag_geres <- srag_geres %>% group_by(geres) %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

srag_geres <- srag_geres %>% group_by(geres) %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

srag_geres <- srag_geres %>% group_by(geres) %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

srag_geres <- srag_geres %>% replace(is.na(.), 0)

########## OBITOS GERES #############

obitos_geres <- obitos_municipio %>% group_by(data = data, geres = geres) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

obitos_geres$semana <- epiweek(obitos_geres$data)

obitos_geres <- obitos_geres %>% group_by(geres) %>%
  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

obitos_geres <- obitos_geres %>% group_by(geres) %>%
  mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

obitos_geres <- obitos_geres %>% group_by(geres) %>%
  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

obitos_geres <- obitos_geres %>% replace(is.na(.), 0)

########## SOLICITACOES MACRO #############

solicitacoes_geres <- solicitacoes_municipios %>% group_by(data = data, geres = geres) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

solicitacoes_geres$semana <- epiweek(solicitacoes_geres$data)

solicitacoes_geres <- solicitacoes_geres %>% group_by(geres) %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

solicitacoes_geres <- solicitacoes_geres %>% group_by(geres) %>% mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

solicitacoes_geres <- solicitacoes_geres %>% group_by(geres) %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

solicitacoes_geres <- solicitacoes_geres %>% replace(is.na(.), 0)

######################## FILA TOTAL MACRO ######################

fila_total_geres <- fila_total_municipios %>% group_by(data = data, geres = geres) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_total_geres$semana <- epiweek(fila_total_geres$data)

fila_total_geres <- fila_total_geres %>% group_by(geres) %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_total_geres <- fila_total_geres %>% group_by(geres) %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_total_geres <- fila_total_geres %>% group_by(geres) %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_total_geres <- fila_total_geres %>% replace(is.na(.), 0)

######################## FILA NOVA MACRO ######################

fila_nova_geres <- fila_nova_municipios %>% group_by(data = data, geres = geres) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_nova_geres$semana <- epiweek(fila_nova_geres$data)

fila_nova_geres <- fila_nova_geres %>% group_by(geres) %>%
  mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_nova_geres <- fila_nova_geres %>% group_by(geres) %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_nova_geres <- fila_nova_geres %>% group_by(geres) %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_nova_geres <- fila_nova_geres %>% replace(is.na(.), 0)

########## INTERNACOES GERES#############
internacoes_geres <- read.csv2('resultado/internacoes_geres.csv', sep = ';')

internacoes_geres$data <- as.Date(internacoes_geres$data, format = "%Y-%m-%d")

internacoes_geres <- internacoes_geres %>% 
  group_by(geres) %>%
  mutate(intUTIMM7 = round(rollmeanr(x = UTI, 7, align = "right", fill = NA),2))

internacoes_geres <- internacoes_geres %>% group_by(geres) %>%
  mutate(intENFMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

internacoes_geres <- internacoes_geres %>% group_by(geres) %>%
  mutate(intGLMM7 = round(rollmean(x = geral, 7, align = "right", fill = NA),2))

internacoes_geres <- internacoes_geres %>% replace(is.na(.), 0)

########## GERES #############
########## LEITOS GERES#############
leitos_geres <- read.csv2('resultado/leitos_geres.csv', sep = ';')

leitos_geres$data <- as.Date(leitos_geres$data, format = "%Y-%m-%d")

leitos_geres$UTI_TX <- round(1- leitos_geres$UTI_LIVRE / leitos_geres$UTI_DSP,2)

leitos_geres$ENFERMARIA_TX <- round(1- leitos_geres$ENFERMARIA_LIVRE / leitos_geres$ENFERMARIA_DSP,2)

leitos_geres$GERAL_TX <- round(1- leitos_geres$GERAL_LIVRE / leitos_geres$GERAL_DSP,2)

leitos_geres <- leitos_geres %>% replace(is.na(.), 0)

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(txUTIMM7 = round(rollmean(x = UTI_TX, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(txENFMM7 = round(rollmean(x = ENFERMARIA_TX, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(txTOTALMM7 = round(rollmean(x = GERAL_TX, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(dspUTIMM7 = round(rollmean(x = UTI_DSP, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(dspENFMM7 = round(rollmean(x = ENFERMARIA_DSP, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(dspTOTALMM7 = round(rollmean(x = GERAL_DSP, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(totalUTIMM7 = round(rollmean(x = UTI_TOTAL, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(totalENFMM7 = round(rollmean(x = ENFERMARIA_TOTAL, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% group_by(geres) %>%
  mutate(totalGERALMM7 = round(rollmean(x = GERAL_TOTAL, 7, align = "right", fill = NA),2))

leitos_geres <- leitos_geres %>% replace(is.na(.), 0)
