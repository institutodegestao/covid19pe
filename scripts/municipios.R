library(dplyr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(shiny)
library(zoo)

########## municipio #############

########## Casos municipio #############

srag_municipio_gb <- srag_municipio %>% group_by(data = data, municipio = municipio) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

srag_municipio_gb$semana <- epiweek(srag_municipio_gb$data)

srag_municipio_gb <- srag_municipio_gb %>% group_by(municipio) %>%  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

srag_municipio_gb <- srag_municipio_gb %>% group_by(municipio) %>% mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

srag_municipio_gb <- srag_municipio_gb %>% group_by(municipio) %>%  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

srag_municipio_gb <- srag_municipio_gb %>% replace(is.na(.), 0)

########## Obitos municipio #############

obitos_municipio_gb <- obitos_municipio %>% group_by(data = data, municipio = municipio) %>% summarise(DESCARTADO = sum(DESCARTADO), CONFIRMADO = sum(CONFIRMADO), SRAG = sum(SRAG))

obitos_municipio_gb$semana <- epiweek(obitos_municipio_gb$data)

obitos_municipio_gb <- obitos_municipio_gb %>% group_by(municipio) %>%  mutate(confMM7 = round(rollmean(x = CONFIRMADO, 7, align = "right", fill = NA),2))

obitos_municipio_gb <- obitos_municipio_gb %>% group_by(municipio) %>% mutate(descfMM7 = round(rollmean(x = DESCARTADO, 7, align = "right", fill = NA),2))

obitos_municipio_gb <- obitos_municipio_gb %>% group_by(municipio) %>%  mutate(sragfMM7 = round(rollmean(x = SRAG, 7, align = "right", fill = NA),2))

obitos_municipio_gb <- obitos_municipio_gb %>% replace(is.na(.), 0)

########## Solicitacoes municipio #############

solicitacoes_municipio_gb <- solicitacoes_municipios %>% group_by(data = data, municipio = municipio) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

solicitacoes_municipio_gb$semana <- epiweek(solicitacoes_municipio_gb$data)

solicitacoes_municipio_gb <- solicitacoes_municipio_gb %>% group_by(municipio) %>% mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

solicitacoes_municipio_gb <- solicitacoes_municipio_gb %>% group_by(municipio) %>% mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

solicitacoes_municipio_gb <- solicitacoes_municipio_gb %>% group_by(municipio) %>% mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

solicitacoes_municipio_gb <- solicitacoes_municipio_gb %>% replace(is.na(.), 0)

########## Fila Total Municipio  ######################

fila_total_municipio_gb <- fila_total_municipios %>% group_by(data = data, municipio = municipio) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_total_municipio_gb$semana <- epiweek(fila_total_municipio_gb$data)

fila_total_municipio_gb <- fila_total_municipio_gb %>% group_by(municipio) %>% mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_total_municipio_gb <- fila_total_municipio_gb %>% group_by(municipio) %>% mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_total_municipio_gb <- fila_total_municipio_gb %>% group_by(municipio) %>% mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_total_municipio_gb <- fila_total_municipio_gb %>% replace(is.na(.), 0)

########## Fila Nova Municipio  ######################

fila_nova_municipio_gb <- fila_nova_municipios %>% group_by(data = data, municipio = municipio) %>% summarise(ENFERMARIA = sum(ENFERMARIA), UTI = sum(UTI), total = sum(total))

fila_nova_municipio_gb$semana <- epiweek(fila_nova_municipio_gb$data)

fila_nova_municipio_gb <- fila_nova_municipio_gb %>% group_by(municipio) %>% mutate(geralMM7 = round(rollmean(x = total, 7, align = "right", fill = NA),2))

fila_nova_municipio_gb <- fila_nova_municipio_gb %>% group_by(municipio) %>% mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

fila_nova_municipio_gb <- fila_nova_municipio_gb %>% group_by(municipio) %>% mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

fila_nova_municipio_gb <- fila_nova_municipio_gb %>% replace(is.na(.), 0)

########## Internacoes Municipio #############
internacoes_municipio <- read.csv2('resultado/internacoes_municipio.csv', sep = ';')

internacoes_municipio$data <- as.Date(internacoes_municipio$data, format = "%Y-%m-%d")

internacoes_municipio <- internacoes_municipio %>% 
  group_by(municipio) %>%
  mutate(intUTIMM7 = round(rollmeanr(x = UTI, 7, align = "right", fill = NA),2))

internacoes_municipio <- internacoes_municipio %>% group_by(municipio) %>%
  mutate(intENFMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

internacoes_municipio <- internacoes_municipio %>% group_by(municipio) %>%
  mutate(intGLMM7 = round(rollmean(x = geral, 7, align = "right", fill = NA),2))

internacoes_municipio <- internacoes_municipio %>% replace(is.na(.), 0)

########## Leitos Municipio #############

municipios_leitos <- read.csv2('resultado/leitos_municipios.csv', sep = ';')

municipios_leitos$data <- as.Date(municipios_leitos$data, format = "%Y-%m-%d")

municipios_leitos$UTI_TX <- round(1- municipios_leitos$UTI_LIVRE / municipios_leitos$UTI_DSP,2)

municipios_leitos$ENFERMARIA_TX <- round(1- municipios_leitos$ENFERMARIA_LIVRE / municipios_leitos$ENFERMARIA_DSP,2)

municipios_leitos$GERAL_TX <- round(1- municipios_leitos$GERAL_LIVRE / municipios_leitos$GERAL_DSP,2)

municipios_leitos <- municipios_leitos %>% replace(is.na(.), 0)

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(txUTIMM7 = round(rollmean(x = UTI_TX, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(txENFMM7 = round(rollmean(x = ENFERMARIA_TX, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(txTOTALMM7 = round(rollmean(x = GERAL_TX, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(dspUTIMM7 = round(rollmean(x = UTI_DSP, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(dspENFMM7 = round(rollmean(x = ENFERMARIA_DSP, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(dspTOTALMM7 = round(rollmean(x = GERAL_DSP, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(totalUTIMM7 = round(rollmean(x = UTI_TOTAL, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(totalENFMM7 = round(rollmean(x = ENFERMARIA_TOTAL, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>%
  group_by(municipio) %>%
  mutate(totalGERALMM7 = round(rollmean(x = GERAL_TOTAL, 7, align = "right", fill = NA),2))

municipios_leitos <- municipios_leitos %>% replace(is.na(.), 0)